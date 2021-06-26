package db

import (
	"database/sql"
	"fmt"
	"log"
	"strings"

	_ "github.com/lib/pq"
)

type PGConn struct {
	db *sql.DB
}

const INSERTSTMT = `insert into "%s" (key, value) values ($1, $2)`

const INSERTSTMTPLUS = `insert into "%s" (key, value) values`

func NewPGConnWithInstance(db *sql.DB) *PGConn {
	return &PGConn{
		db: db,
	}
}

func NewPGConn(config string) (*PGConn, error) {
	db, err := sql.Open("postgres", config)
	if err != nil {
		return nil, err
	}

	if db.Ping() == nil {
		if _, err := db.Exec(`CREATE TABLE IF NOT EXISTS test
(
    key bigint NOT NULL,
    value bigint NOT NULL,
    CONSTRAINT test_pkey PRIMARY KEY (key)
)`); err != nil {
			log.Println(err)
		}
	}

	return &PGConn{
		db: db,
	}, nil
}

func (conn *PGConn) Insert(tablename string, k int, v uint64) error {
	if _, err := conn.db.Exec(fmt.Sprintf(INSERTSTMT, tablename), k, v); err != nil {
		return err
	}
	return nil
}

func (conn *PGConn) InsertPlus(tablename string, all map[int]uint64) error {
	var b strings.Builder

	log.Printf("insert %d records", len(all))

	fmt.Fprintf(&b, INSERTSTMTPLUS, tablename)
	for k, v := range all {
		fmt.Fprintf(&b, " (%d, %d),", k, v)
	}

	log.Println("insert many:", strings.TrimRight(b.String(), ","))

	if _, err := conn.db.Exec(strings.TrimRight(b.String(), ",")); err != nil {
		return err
	}
	return nil
}

func (conn *PGConn) Query(stmt string, args ...interface{}) ([]struct {
	K int
	V uint64
}, error) {
	rows, err := conn.db.Query(stmt, args...)
	if err != nil {
		return nil, err
	}

	values := []struct {
		K int
		V uint64
	}{}
	for rows.Next() {
		var key int
		var va uint64
		err = rows.Scan(&key, &va)
		if err != nil {
			return nil, err
		}
		values = append(values, struct {
			K int
			V uint64
		}{
			K: key,
			V: va,
		},
		)
	}

	log.Printf("query from db: %+v\n", values)
	return values, nil
}

func (conn *PGConn) Count(stmt string, args ...interface{}) (int, error) {
	rows, err := conn.db.Query(stmt, args...)
	if err != nil {
		return 0, err
	}
	var c int = 0
	if rows.Next() {
		err = rows.Scan(&c)
		if err != nil {
			return c, err
		}
	}

	return c, nil
}

func (conn *PGConn) Exec(stmt string) error {
	if _, err := conn.db.Exec(stmt); err != nil {
		return err
	}
	return nil
}
