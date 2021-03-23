package main

import "fmt"

func main() {
	a := []int{0, 1, 2, 3, 4, 5, 6, 7}

	fmt.Printf("Address of a: %p\n", &a)

	b := a
	fmt.Printf("Address of b: %p\n", &b)

	var c []int
	c = a
	fmt.Printf("Address of c: %p\n", &c)
	/// all addresses are different

	c[0] = 1
	fmt.Printf("a: %v, a: %v\n", a, c) // a: [1 1 2 3 4 5 6 7], a: [1 1 2 3 4 5 6 7]

	b[0] = 2
	fmt.Printf("b: %v, a: %v, c: %v\n", b, a, c) // b: [2 1 2 3 4 5 6 7], a: [2 1 2 3 4 5 6 7], c: [2 1 2 3 4 5 6 7]

	d := append(a, 8) // cap expand here
	d[0] = 0
	fmt.Printf("d address is: %p, d: %v, a: %v\n", &d, d, a) // d: [0 1 2 3 4 5 6 7 8], a: [2 1 2 3 4 5 6 7]

	fmt.Printf("d cap is %d\n", cap(d)) // d cap is 16
	e := append(d, 9)
	fmt.Printf("d cap is %d after e, value: %v\n", cap(d), d) // d cap is 16 after e, value: [0 1 2 3 4 5 6 7 8]
	e[0] = 1
	fmt.Printf("e: %v, d: %v\n", e, d) // e: [1 1 2 3 4 5 6 7 8 9], d: [1 1 2 3 4 5 6 7 8]

	////=====================================
	a = []int{0, 1, 2, 3}
	b = a[1:3]
	fmt.Printf("b: %v\n", b) // b: [1 2]
	c = a[1:3:4]
	fmt.Printf("c: %v, c len: %d, c cap: %d\n", c, len(c), cap(c)) // c: [1 2], c len: 2, c cap: 3

	b[0] = 0
	fmt.Printf("b: %v, a: %v\n", b, a) // b: [0 2], a: [0 0 2 3]

	c[0] = 1
	fmt.Printf("c: %v, a: %v\n", c, a) // c: [1 2], a: [0 1 2 3]

	//c = a[1:3:5] // panic 5 > cap
	//fmt.Printf("c: %v, c len: %d, c cap: %d\n", c, len(c), cap(c))
	c = append(c, 4)
	fmt.Printf("c: %v, a: %v\n", c, a) // c: [1 2 4], a: [0 1 2 4]

	c = append(c, 5)
	c[0] = 0
	fmt.Printf("c: %v, a: %v\n", c, a) // c: [0 2 4 5], a: [0 1 2 4]
}
