package subpkg

import "testing"

func TestA(t *testing.T) {
	testcase0 := map[string]int{"a": 1}
	if A(testcase0) != 1 {
		t.Fail()
	}
}
