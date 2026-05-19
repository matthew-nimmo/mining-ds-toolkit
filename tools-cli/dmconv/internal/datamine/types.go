package datamine

type metadata struct {
	FieldName     string
	Type          string
	LogicalRecPos int
	WordNumber    int
	Unit          int
	Default       any
	Size          int
}

type Data struct {
	Names []string
	Types []string
	Data  [][]any
}

type Number interface {
	float32 | float64
}
