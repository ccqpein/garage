package service

import "testing"

func TestFibWithRecord(t *testing.T) {
	re := [2]*Record{
		{K: 1, V: 1}, nil,
	}

	if FibWithRecord(10, re) != 55 {
		t.Fatal("Wrong number")
	}

	re = [2]*Record{nil, nil}

	if FibWithRecord(12, re) != 144 {
		t.Fatal("Wrong number")
	}

	re = [2]*Record{{11, 89}, {10, 55}}
	if FibWithRecord(12, re) != 144 {
		t.Fatal("Wrong number")
	}
}

func TestFibWithRecordPlus(t *testing.T) {
	re := [3]*Record{
		{K: 1, V: 1}, nil, nil,
	}
	all := map[int]uint64{}

	if FibWithRecordPlus(10, re, all) != 55 || len(all) != 10 {
		t.Logf("%+v\n", all)
		t.Fatal("Wrong number")
	}

	re = [3]*Record{nil, nil, nil}
	all = map[int]uint64{}

	if FibWithRecordPlus(12, re, all) != 144 || len(all) != 13 {
		t.Logf("%+v\n", all)
		t.Fatal("Wrong number")
	}

	re = [3]*Record{{11, 89}, {10, 55}}
	all = map[int]uint64{}
	if FibWithRecordPlus(12, re, all) != 144 || len(all) != 1 {
		t.Logf("%+v\n", all)
		t.Fatal("Wrong number")
	}
}
