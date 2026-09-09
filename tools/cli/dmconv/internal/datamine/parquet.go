package datamine

import (
	"log"

	"github.com/apache/arrow/go/arrow"
	"github.com/apache/arrow/go/arrow/array"
	"github.com/apache/arrow/go/arrow/memory"
	"github.com/xitongsys/parquet-go-source/local"
	"github.com/xitongsys/parquet-go/writer"
)

func WriteParquet(data *Data, filename string) {
	fw, err := local.NewLocalFileWriter(filename)
	if err != nil {
		log.Fatal("Could not create file", err)
	}

	mem := memory.NewCheckedAllocator(memory.NewGoAllocator())
	fields := []arrow.Field{}
	for i, name := range data.Names {
		var fld arrow.Field
		if data.Types[i] == "N" {
			fld = arrow.Field{Name: name, Type: arrow.PrimitiveTypes.Float64}
		} else {
			fld = arrow.Field{Name: name, Type: arrow.BinaryTypes.String}
		}
		fields = append(fields, fld)
	}

	schema := arrow.NewSchema(fields, nil)
	b := array.NewRecordBuilder(mem, schema)
	defer b.Release()

	for idx := range schema.Fields() {
		if data.Types[idx] == "N" {
			y := make([]float64, len(data.Data[idx]))
			for i, v := range data.Data[idx] {
				if floatVal, ok := v.(float64); ok {
					y[i] = floatVal
				}
			}
			b.Field(idx).(*array.Float64Builder).AppendValues(y, nil)
		} else {
			y := make([]string, len(data.Data[idx]))
			for i, v := range data.Data[idx] {
				if stringVal, ok := v.(string); ok {
					y[i] = stringVal
				}
			}
			b.Field(idx).(*array.StringBuilder).AppendValues(y, nil)
		}
	}
	rec := b.NewRecord()

	w, err := writer.NewArrowWriter(schema, fw, 1)
	if err != nil {
		log.Println("Can't create parquet writer", err)
		return
	}
	if err = w.WriteArrow(rec); err != nil {
		log.Println("WriteArrow error", err)
		return
	}
	if err = w.WriteStop(); err != nil {
		log.Println("WriteStop error", err)
		return
	}
	log.Println("Write Finished")
	fw.Close()
}
