package datamine

import (
	"encoding/binary"
	"math"
	"reflect"
	"strings"
)

func SwapBytes(bytes []byte) []byte {
	newbytes := bytes
	for si := 0; si < 2; si++ {
		tmp := bytes[si]
		newbytes[si] = newbytes[7-si]
		newbytes[7-si] = tmp
	}

	return newbytes
}

func NumberFromBytes(bytes []byte) float64 {
	if len(bytes) == 4 {
		bits := binary.LittleEndian.Uint32(bytes)
		float := math.Float32frombits(bits)

		return float64(float)
	}

	bits := binary.LittleEndian.Uint64(bytes)
	float := math.Float64frombits(bits)

	return float
}

func StringFromBytes(bytes []byte, size int) string {
	if size == 1 {
		s := string(bytes)
		s = strings.TrimSpace(s)
		return s
	}

	n := len(bytes) / 8
	bits := make([]byte, n*4)
	for i := 0; i < n; i++ {
		m := i * 4
		k := i * 8
		bits[m] = bytes[k]
		bits[m+1] = bytes[k+1]
		bits[m+2] = bytes[k+2]
		bits[m+3] = bytes[k+3]
	}
	s := string(bits)
	s = strings.TrimSpace(s)

	return s
}

func NumberBytes[T Number](num T) []byte {
	x := reflect.TypeOf(num).String()
	if x == "float32" {
		bits := math.Float32bits(float32(num))
		bytes := make([]byte, 4)
		binary.LittleEndian.PutUint32(bytes, bits)
	}

	bits := math.Float64bits(float64(num))
	bytes := make([]byte, 8)
	binary.LittleEndian.PutUint64(bytes, bits)

	return bytes
}
