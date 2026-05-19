package datamine

import (
	"bufio"
	"fmt"
	"os"
)

func ReadDM(filename string) (*Data, error) {
	// Assume file is 32bit
	byteSize := 1

	// Open file
	file, err := os.Open(filename)
	if err != nil {
		return nil, fmt.Errorf("could not open input file: %s", filename)
	}
	defer file.Close()

	reader := bufio.NewReaderSize(file, 2048)

	// Establish if it is 32 or 64 bit format.
	// NOTE: Documentation and observation show that if it is double precision,
	// the 25th 64-bit word (of 1 to n) will be a double precision value = 456789.0
	// If not 456789.0 it should be 987654.0, but old files may not have set it.
	// the 25th 32-bit word (of 1 to n) should be 987654.0, but old files may not
	// have set it.  Word 25 was unused prior to introduction of double precision.
	header, _ := reader.Peek(200)
	fmt64 := NumberFromBytes(header[24*8 : 24*8+8])
	if fmt64 == 456789.0 {
		byteSize = 2
	}

	// Read header
	name := make([]byte, byteSize*8)
	reader.Read(name)
	directory := make([]byte, byteSize*8)
	reader.Read(directory)
	description := make([]byte, byteSize*64)
	reader.Read(description)
	owner := make([]byte, byteSize*8)
	reader.Read(owner)
	ownerPerms := make([]byte, byteSize*4)
	reader.Read(ownerPerms)
	otherPerms := make([]byte, byteSize*4)
	reader.Read(otherPerms)
	modifyDate := make([]byte, byteSize*4)
	reader.Read(modifyDate)
	numFields := make([]byte, byteSize*4)
	reader.Read(numFields)
	numPages := make([]byte, byteSize*4)
	reader.Read(numPages)
	recsLastPage := make([]byte, byteSize*4)
	reader.Read(recsLastPage)

	fmt.Println(StringFromBytes(name, byteSize), StringFromBytes(directory, byteSize))
	fmt.Println(StringFromBytes(description, byteSize))

	// Check if byte swapped
	x := NumberFromBytes(modifyDate)
	fmt.Println("Modify date:", x)
	if x < 720101 || x > 99991231 {
		fmt.Println("Byte swapped")
	}

	n := int(NumberFromBytes(numFields))
	if n > 64 || n <= 0 {
		panic("invalid file")
	}

	np := int(NumberFromBytes(numPages))
	rlp := int(NumberFromBytes(recsLastPage))

	// read metadata
	fmeta := []metadata{}
	var bits []byte
	pname := ""
	nn := 0
	nf := 0
	for i := 0; i < n; i++ {
		var m metadata

		bits = make([]byte, byteSize*8)
		reader.Read(bits)
		m.FieldName = StringFromBytes(bits, byteSize)

		bits = make([]byte, byteSize*4)
		reader.Read(bits)
		m.Type = string(bits[:1])

		bits = make([]byte, byteSize*4)
		reader.Read(bits)
		m.LogicalRecPos = int(NumberFromBytes(bits))

		bits = make([]byte, byteSize*4)
		reader.Read(bits)
		m.WordNumber = int(NumberFromBytes(bits))

		bits = make([]byte, byteSize*4)
		reader.Read(bits)
		m.Unit = int(NumberFromBytes(bits))

		if m.Type == "N" {
			bits = make([]byte, byteSize*4)
			reader.Read(bits)
			m.Default = NumberFromBytes(bits)
		} else {
			bits := make([]byte, byteSize*4)
			reader.Read(bits)
			m.Default = StringFromBytes(bits, byteSize)
		}

		m.Size = 4 * byteSize
		if pname == m.FieldName {
			fmeta[nn-1].Size += 4 * byteSize
			fmeta[nn-1].Default = fmt.Sprintf("%s%s", fmeta[nn-1].Default, m.Default)
		} else {
			pname = m.FieldName
			nn += 1
			fmeta = append(fmeta, m)
		}

		if m.LogicalRecPos > 0 {
			nf += 1
		}
	}

	for i := range fmeta {
		fmt.Printf("{Field: %s, Type: %s, Size: %d, Default: %v, Implicit: %v}\n", fmeta[i].FieldName, fmeta[i].Type, fmeta[i].Size, fmeta[i].Default, fmeta[i].LogicalRecPos)
	}

	// Skipping unnecessary bytes
	//rb := byteSize * (1936 - (n * 28))
	reader.Discard(reader.Buffered())

	// Number of records per page
	nrp := 0
	if nf > 0 {
		//There is only 508 bytes per page (table is 512 with 4 reserved)
		nrp = 508 / nf
	}

	// Number of data records
	nd := (np-2)*nrp + rlp

	// Number of bytes remaining per page
	//nrb := byteSize * (2048 - (nf * nrp * 4))

	// Number of remaining pages
	np = np - 1
	if np < 0 {
		np = 0
	}

	fmt.Printf("Number of rows: %d\n", nd)

	// Initialize data
	var data Data
	data.Names = make([]string, nn)
	data.Types = make([]string, nn)
	data.Data = make([][]any, nn)
	for i := range data.Data {
		data.Data[i] = make([]any, nd)
	}

	// Reading data
	r := 0
	for p := 0; p < np; p++ {
		if p+1 == np {
			nrp = rlp
		}
		for pr := 0; pr < nrp; pr++ {
			for i, v := range fmeta {
				data.Names[i] = v.FieldName
				data.Types[i] = v.Type
				// check if variable is implicit or not
				if v.LogicalRecPos != 0 {
					if v.Type == "N" {
						bits = make([]byte, v.Size)
						reader.Read(bits)
						data.Data[i][r] = NumberFromBytes(bits)
					} else {
						bits = make([]byte, v.Size)
						reader.Read(bits)
						data.Data[i][r] = StringFromBytes(bits, byteSize)
					}
				} else {
					data.Data[i][r] = v.Default
				}
			}
			r += 1
		}
		reader.Discard(reader.Buffered())
	}

	return &data, nil
}
