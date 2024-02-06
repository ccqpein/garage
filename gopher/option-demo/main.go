package main

type Some[T any] struct {
	V T
}

// failed with point
func (s *Some[T]) IsSome() bool {
	return true
}

// success without point
// func (s Some[T]) IsSome() bool {
// 	return true
// }

type None struct{}

type Option[T any] interface {
	//*Some[T] | None | Some[T] // two some for testing
}

type OptionAble[T any] interface {
	Option[T]
	IsSome() bool
}

func test1[T OptionAble[int]](a T) {}

func main() {
	test1(&Some[int]{V: 0})
}
