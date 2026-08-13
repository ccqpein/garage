package main

import (
	"fmt"
	"reflect"
	"uuid"

	gu "github.com/google/uuid"
)

func useGoogleUUID(id gu.UUID) {}

func useStdUUID(id uuid.UUID) {}

func SameKind(x, y any) bool {
	return reflect.TypeOf(x).Kind() == reflect.TypeOf(y).Kind()
}

func main() {
	guid := gu.New()
	stduid := uuid.NewV4()

	fmt.Printf("type is same? %+v\n", SameKind(guid, stduid))

	//useGoogleUUID(stduid)          // compiler error
	useGoogleUUID(gu.UUID(stduid))

	//useStdUUID(guid)            // compiler error
	useStdUUID(uuid.UUID(guid))
}
