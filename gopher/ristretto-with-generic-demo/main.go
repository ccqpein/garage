package main

import "github.com/dgraph-io/ristretto/v2"

type typeCache[K ristretto.Key] struct {
}

type Cache[K ristretto.Key] struct {
	typeTable map[string]*ristretto.Cache[K]
}

type keyAble interface {
	ristretto.Key
}

type cacheAble[K ristretto.Key] interface {
	acceptAbleKey() K
	cacheTypeName() string
}

func InsertTypeCache[K ristretto.Key, V cacheAble[K]](v V) {
	cache, err := ristretto.NewCache(&ristretto.Config[K, V]{
		NumCounters: 1e7,     // number of keys to track frequency of (10M).
		MaxCost:     1 << 30, // maximum cost of cache (1GB).
		BufferItems: 64,      // number of keys per Get buffer.
	})
	if err != nil {
		panic(err)
	}
	defer cache.Close()
}

func GetCache[K ristretto.Key, V cacheAble[K]](c *Cache, k K, v V) {

}

// /////////////////////
type A struct{}
type B struct{}

func (a *A) acceptAbleKey() string {
	return "a"
}

func (a *A) cacheTypeName() string {
	return "a"
}

func Test[K ristretto.Key](v cacheAble[K]) {}

///////////////////////

func main() {
	cache, err := ristretto.NewCache(&ristretto.Config[string, string]{
		NumCounters: 1e7,     // number of keys to track frequency of (10M).
		MaxCost:     1 << 30, // maximum cost of cache (1GB).
		BufferItems: 64,      // number of keys per Get buffer.
	})
	if err != nil {
		panic(err)
	}

	defer cache.Close()

	Test(&A{})
}
