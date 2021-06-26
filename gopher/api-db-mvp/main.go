package main

import (
	"api-db-mvp/apis"
	"api-db-mvp/db"
	"encoding/json"
	"flag"
	"fmt"
	"io"
	"log"
	"net/http"
	"os"
)

type DBConfig struct {
	Host      string `json:"host"`
	Port      int    `json:"port"`
	User      string `json:"user"`
	Password  string `json:"password"`
	DBName    string `json:"dbname"`
	TableName string `json:"tablename"`
}

func main() {
	dbconfigPath := flag.String("dbconfig", "./dbconfig.json", "File of DB configuration")
	dbconfig := DBConfig{}

	if f, err := os.Open(*dbconfigPath); err != nil {
		log.Panicln("Cannot open config of db")
		panic(err)
	} else {
		if b, err := io.ReadAll(f); err != nil {
			panic(err)
		} else {
			json.Unmarshal(b, &dbconfig)
		}
	}

	config := fmt.Sprintf("host=%s port=%d user=%s password=%s dbname=%s sslmode=disable", dbconfig.Host, dbconfig.Port, dbconfig.User, dbconfig.Password, dbconfig.DBName)

	// connect db
	conn, err := db.NewPGConn(config)
	if err != nil {
		panic(err)
	}

	http.Handle("/fib", apis.NewFibApi(conn))
	http.Handle("/less", apis.NewLessThanApi(conn))
	http.Handle("/clean", apis.NewCleanApi(conn))

	log.Fatal(http.ListenAndServe(":8080", nil))
}
