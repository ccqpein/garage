package service

type Record struct {
	K int
	V uint64
}

func FibWithRecord(n int, record [2]*Record) uint64 {
	if record[0] != nil && n == record[0].K {
		return record[0].V
	}

	if record[1] != nil && n == record[1].K {
		return record[1].V
	}

	if n < 2 {
		return uint64(n)
	}

	return (FibWithRecord(n-1, record) + FibWithRecord(n-2, record))
}

func FibWithRecordPlus(n int, record [3]*Record, all map[int]uint64) uint64 {
	if record[0] != nil && n == record[0].K {
		return record[0].V
	}

	if record[1] != nil && n == record[1].K {
		return record[1].V
	}

	if record[2] != nil && n == record[2].K {
		return record[2].V
	}

	if v, ok := all[n]; ok {
		return v
	}

	if n < 2 {
		all[n] = uint64(n)
		return uint64(n)
	}

	a := FibWithRecordPlus(n-1, record, all) + FibWithRecordPlus(n-2, record, all)
	all[n] = a

	return a
}
