package main

func main() {
	a := make(map[int]int)
	v, ok := a[1]
	println("v and ok", v, ok)

	b := map[int]int{}
	v, ok = b[1]
	println("v and ok", v, ok)

	b[1] = 1
	v, ok = b[1]
	println("v and ok", v, ok)

	delete(b, 1)
	v, ok = b[1]
	println("v and ok", v, ok)
}
