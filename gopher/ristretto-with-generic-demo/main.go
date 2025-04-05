package main

import "github.com/dgraph-io/ristretto/v2"

type A struct{}
type B struct{}

type Cache struct {
}

type keyAble interface{}

type cacheAble[K ristretto.Key] interface {
	acceptAbleKey() K
}

func GetCache[K ristretto.Key](c *Cache, k K, v cacheAble[K]) {

}

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

}
