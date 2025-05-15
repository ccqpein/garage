package main

import (
	"bytes"
	"errors"
	"io"
	"strings"
)

func copyBuffer(dst io.Writer, src io.Reader, buf []byte) (written int64, err error) {
	if buf == nil {
		size := 32 * 1024
		if l, ok := src.(*io.LimitedReader); ok && int64(size) > l.N {
			if l.N < 1 {
				size = 1
			} else {
				size = int(l.N)
			}
		}
		buf = make([]byte, size)
	}
	for {
		nr, er := src.Read(buf)
		if nr > 0 {
			nw, ew := dst.Write(buf[0:nr])
			if nw < 0 || nr < nw {
				nw = 0
				if ew == nil {
					ew = errors.New("aa")
				}
			}
			written += int64(nw)
			if ew != nil {
				err = ew
				break
			}
			if nr != nw {
				err = errors.New("bb")
				break
			}
		}
		if er != nil {
			if er != io.EOF {
				err = er
			}
			break
		}
	}
	return written, err
}

// func tryCopyBuffer() {
// 	limit := int64(10)
// 	src := strings.NewReader("This is a test string longer than the limit.")
// 	limitedSrc := io.LimitReader(src, limit)
// 	dst := &bytes.Buffer{}

// 	copyBuffer(dst, limitedSrc, nil)
// }

func tryCopyBufferIONative() (int64, error) {
	//limit := int64(10)
	src := strings.NewReader("This is a test string longer than the limit.")
	//limitedSrc := io.LimitReader(src, limit)
	dst := &bytes.Buffer{}

	return copyBuffer(dst, src, nil)
}

// func tryCopyBufferIONativeBuffer() (int64, error) {
// 	//limit := int64(10)
// 	src := strings.NewReader("This is a test string longer than the limit.")
// 	//limitedSrc := io.LimitReader(src, limit)
// 	dst := &bytes.Buffer{}
// 	var buf = make([]byte, 1024)

// 	return io.CopyBuffer(dst, src, buf)
// }

func main() {
	//tryCopyBuffer()
	tryCopyBufferIONative()
}
