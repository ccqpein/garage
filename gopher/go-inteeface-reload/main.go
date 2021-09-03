package main

type A interface {
	AMethod() int
}

type WrapA interface {
	W() int
	A
}

type Ainstance struct {
}

func (*Ainstance) AMethod() int { return 1 }

type WrapAInstance struct {
	innerA Ainstance
}

func (*WrapAInstance) W() int { return 2 }

// both are fine
//func (*WrapAInstance) AMethod() int    { return 3 }
func (wa *WrapAInstance) AMethod() int { return wa.innerA.AMethod() }

//
func useWrapA(v WrapA) int {
	return v.AMethod()
}

func main() {
	print(useWrapA(&WrapAInstance{Ainstance{}}))
}
