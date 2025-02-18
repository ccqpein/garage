package main

import (
	"fmt"
	"internal/weak"
)

// Blob is a large byte slice.
type Blob []byte

func (b Blob) String() string {
	return fmt.Sprintf("Blob(%d KB)", len(b)/1024)
}

// newBlob returns a new Blob of the given size in KB.
func newBlob(size int) *Blob {
	b := make([]byte, size*1024)
	for i := range size {
		b[i] = byte(i) % 255
	}
	return (*Blob)(&b)
}

// func checkPreventGarbage() {
// 	heapSize := getAlloc()
// 	b := newBlob(1000)

// 	fmt.Println("value before GC =", b)
// 	runtime.GC()
// 	fmt.Println("value after GC =", b)
// 	fmt.Printf("heap size delta = %d KB\n", heapDelta(heapSize))
// }

func main() {
	// b := newBlob(1000) // 1000 KB
	// fmt.Println(b)

	wb := weak.Make(newBlob(1000)) // 1000 KB
	//fmt.Println(wb.Value())        // there is no value of this
	wb.Strong().String()
}
