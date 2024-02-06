package optiondemo

type Some[T any] struct {
	V T
}

func (s *Some[T]) IsSome() bool {
	return true
}

type None struct{}

func (*None) IsSome() bool {
	return false
}

type Option[T any] interface {
	*Some[T] | *None
}

type OptionAble[T any] interface {
	Option[T]
	IsSome() bool
}
