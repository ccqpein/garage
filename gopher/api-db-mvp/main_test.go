package main

import (
	"api-db-mvp/apis"
	"api-db-mvp/db"
	"database/sql"
	"fmt"
	"io"
	"log"
	"math/rand"
	"net/http"
	"net/http/httptest"
	"os"
	"strconv"
	"testing"

	_ "github.com/lib/pq"
	"github.com/ory/dockertest/v3"
	"github.com/ory/dockertest/v3/docker"
)

var dbconn *sql.DB
var mockurl string

func TestMain(m *testing.M) {
	pool, err := dockertest.NewPool("")
	if err != nil {
		log.Fatalf("Could not connect to docker: %s", err)
	}

	opts := dockertest.RunOptions{
		Repository: "postgres",
		Tag:        "13.3",
		Env: []string{
			"POSTGRES_USER=ccQ",
			"POSTGRES_DB=postgres",
			"POSTGRES_PASSWORD=secret",
		},
		ExposedPorts: []string{"5432"},
		PortBindings: map[docker.Port][]docker.PortBinding{
			"5432": {
				{HostIP: "0.0.0.0", HostPort: "5432"},
			},
		},
	}

	resource, err := pool.RunWithOptions(&opts)
	if err != nil {
		log.Fatalf("Could not start resource: %s", err)
	}

	config := fmt.Sprintf("host=%s port=%d user=%s password=secret dbname=%s sslmode=disable", "localhost", 5432, "ccQ", "postgres")
	if err = pool.Retry(func() error {
		var err error
		dbconn, err = sql.Open("postgres", config)
		if err != nil {
			return err
		}
		return dbconn.Ping()
	}); err != nil {
		log.Fatalf("Could not connect to docker: %s", err)
	}

	// make table
	dbconn.Exec(`CREATE TABLE IF NOT EXISTS test
(
    key bigint NOT NULL,
    value bigint NOT NULL,
    CONSTRAINT test_pkey PRIMARY KEY (key)
)`)

	// start to mock apis
	conn := db.NewPGConnWithInstance(dbconn)
	mux := http.NewServeMux()
	mux.Handle("/fib", apis.NewFibApi(conn))
	mux.Handle("/less", apis.NewLessThanApi(conn))
	mux.Handle("/clean", apis.NewCleanApi(conn))

	ts := httptest.NewServer(mux)
	mockurl = ts.URL
	defer ts.Close()

	code := m.Run()

	if err = pool.Purge(resource); err != nil {
		log.Fatalf("Could not purge resource: %s", err)
	}

	os.Exit(code)
}

func TestFibApi(t *testing.T) {
	res, err := http.Get(mockurl + "/fib?input=10")
	if err != nil {
		log.Fatal(err)
	}

	body, err := io.ReadAll(res.Body)
	res.Body.Close()
	if string(body) != "result: 55\n" {
		t.Fatalf("/fib?input=10 get wrong answer: %s", string(body))
	}

	res, err = http.Get(mockurl + "/fib?input=12")
	if err != nil {
		log.Fatal(err)
	}
	body, err = io.ReadAll(res.Body)
	res.Body.Close()
	if string(body) != "result: 144\n" {
		t.Fatalf("/fib?input=10 get wrong answer %s", string(body))
	}
}

func TestLessApi(t *testing.T) {
	res, err := http.Get(mockurl + "/less?than=120")
	if err != nil {
		log.Fatal(err)
	}

	body, err := io.ReadAll(res.Body)
	res.Body.Close()
	if string(body) != "there are 12 records less than 120" {
		t.Fatalf("/less?than=120 get wrong answer: %s", string(body))
	}
}

func TestCleanApi(t *testing.T) {
	res, err := http.Get(mockurl + "/clean")
	if err != nil {
		log.Fatal(err)
	}

	body, err := io.ReadAll(res.Body)
	res.Body.Close()
	if string(body) != "Done clean" {
		t.Fatalf("/clean get wrong answer: %s", string(body))
	}

	// check if the table is empty
	res, err = http.Get(mockurl + "/less?than=0")
	if err != nil {
		log.Fatal(err)
	}

	body, err = io.ReadAll(res.Body)
	res.Body.Close()
	if string(body) != "there are 0 records less than 0" {
		t.Fatal("Doesn't clean successfully")
	}
}

func BenchmarkFibApiMemoizes(b *testing.B) {
	// remember from 0 to 120
	http.Get(mockurl + "/fib?input=120")
	for i := 0; i < b.N; i++ {
		http.Get(mockurl + "/fib?input=" + strconv.Itoa(rand.Intn(121)))

	}
}

func BenchmarkFibApiStep(b *testing.B) {
	// clean
	http.Get(mockurl + "/clean")
	this := 10
	for i := 0; i < b.N; i++ {
		http.Get(mockurl + "/fib?input=" + strconv.Itoa(this))
		this += 10
	}
}
