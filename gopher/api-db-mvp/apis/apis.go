package apis

import (
	"api-db-mvp/db"
	"api-db-mvp/service"
	"fmt"
	"log"
	"net/http"
	"strconv"
)

// fetch Fibonacci of this number
type FibApi struct {
	dbconn *db.PGConn
}

func NewFibApi(conn *db.PGConn) *FibApi {
	return &FibApi{dbconn: conn}
}

// return (if find input, the value of input, records, and error)
func (fa *FibApi) query(input int) (bool, uint64, [3]*service.Record, error) {
	ll, err := fa.dbconn.Query(
		`select key, value from 
(select key, value from "test" order by key desc) cache
where (key <= $1) limit 3`,
		input)
	if err != nil {
		log.Printf("Err happend on calling db: %+v\n", err)
		return false, 0, [3]*service.Record{}, err
	}

	records := [3]*service.Record{}

	for k, v := range ll {
		if v.K == input {
			return true, v.V, records, nil
		}
		records[k] = &service.Record{K: v.K, V: v.V}

	}

	log.Printf("return records: %+v\n", records)

	return false, 0, records, nil
}

func (fa *FibApi) run(input int) uint64 {
	var b bool
	var v uint64
	var records [3]*service.Record
	var err error

	b, v, records, err = fa.query(input)
	if err != nil {
		log.Println(err)
	}

	if b {
		return v
	}

	all := map[int]uint64{}
	result := service.FibWithRecordPlus(input, records, all)
	fa.dbconn.InsertPlus("test", all)

	return result
}

// /fib?input={v}
func (fa *FibApi) ServeHTTP(w http.ResponseWriter, req *http.Request) {
	values := req.URL.Query()
	var input int

	// check
	if v, ok := values["input"]; !ok {
		w.Write([]byte(`have to has input parameter`))
		return
	} else if len(v) > 1 {
		w.Write([]byte(`only accept one parameter`))
		return
	} else if nn, err := strconv.Atoi(v[0]); err != nil {
		w.Write([]byte(`only accept number`))
		return
	} else {
		input = nn
	}

	fmt.Fprintf(w, "result: %d\n", fa.run(input))
}

// less than a given value
type LessThanApi struct {
	dbconn *db.PGConn
}

func NewLessThanApi(conn *db.PGConn) *LessThanApi {
	return &LessThanApi{dbconn: conn}
}

func (lt *LessThanApi) run(than int) (int, error) {
	ll, err := lt.dbconn.Count(
		`select count(key) from "test" where (value <= $1)`, than,
	)
	if err != nil {
		log.Printf("Err happend on calling db: %+v\n", err)
		return 0, err
	}

	return ll, nil

}

func (lt *LessThanApi) ServeHTTP(w http.ResponseWriter, req *http.Request) {
	values := req.URL.Query()
	var than int

	// check
	if v, ok := values["than"]; !ok {
		w.Write([]byte(`have to has than parameter`))
		return
	} else if len(v) > 1 {
		w.Write([]byte(`only accept one parameter`))
		return
	} else if nn, err := strconv.Atoi(v[0]); err != nil {
		w.Write([]byte(`only accept number`))
		return
	} else {
		than = nn
	}

	if n, err := lt.run(than); err != nil {
		fmt.Fprintf(w, "there are some errors happen")
	} else {
		fmt.Fprintf(w, "there are %d records less than %d", n, than)
	}
}

// clear
type CleanApi struct {
	dbconn *db.PGConn
}

func NewCleanApi(conn *db.PGConn) *CleanApi {
	return &CleanApi{dbconn: conn}
}

func (cln *CleanApi) run() error {
	if err := cln.dbconn.Exec("delete from test"); err != nil {
		return err
	}
	return nil
}

func (cln *CleanApi) ServeHTTP(w http.ResponseWriter, req *http.Request) {
	if err := cln.run(); err != nil {
		fmt.Fprintf(w, "Done failed")
	} else {
		fmt.Fprintf(w, "Done clean")
	}
}
