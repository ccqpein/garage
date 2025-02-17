package main

import "fmt"

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

func main() {

}
