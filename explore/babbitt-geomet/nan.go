package main

import (
	"embed"
	_ "embed"
	"encoding/csv"
	"flag"
	"fmt"
	"io"
	"math"
	"os"
	"path/filepath"
	"sort"
	"strconv"
	"time"
)

//go:embed include/usage.txt
var usage_txt string

//go:embed include/help.txt
var help_txt string

//go:embed include/test.csv
var testdata string

//go:embed include/proto.csv
var f embed.FS

type datafields struct {
	cu  string
	xc  string
	yc  string
	zc  string
	dom string
}

func main() {
	var (
		datafile string
		colcu    string
		colx     string
		coly     string
		colz     string
		coldom   string
	)
	mySet := flag.NewFlagSet("", flag.ExitOnError)
	mySet.StringVar(&datafile, "f", "test.csv", "The data file to enrich")
	mySet.StringVar(&colcu, "cu", "CU", "Cu column")
	mySet.StringVar(&colx, "x", "XC", "X coord")
	mySet.StringVar(&coly, "y", "YC", "Y coord")
	mySet.StringVar(&colz, "z", "ZC", "Z coord")
	mySet.StringVar(&coldom, "dom", "Domain", "Domain")

	mycmd := "usage"
	if len(os.Args) > 1 {
		mycmd = os.Args[1]
		mySet.Parse(os.Args[2:])
	}

	switch mycmd {
	case "pred":
		p := datafields{colcu, colx, coly, colz, coldom}
		cmd_pred(datafile, p)
	case "test":
		cmd_test()
	case "help":
		fmt.Println(help_txt)
	default:
		fmt.Println(usage_txt)
	}
}

func runSpinner(runString string, stop chan bool) {
	// Define the character sequence for the animation
	//frames := []string{"⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏"}
	frames := []string{"⢿", "⣻", "⣽", "⣾", "⣷", "⣯", "⣟", "⡿"}
	//frames := []string{".   ", " .  ", "  . ", "   .", "  . ", " .  "}
	i := 0

	for {
		select {
		case <-stop:
			return
		default:
			// \r moves the cursor back to the start of the line
			fmt.Printf("\r%s %s", frames[i], runString)
			i = (i + 1) % len(frames)
			time.Sleep(100 * time.Millisecond)
		}
	}
}

func cmd_test() {
	ex, err := os.Executable()
	if err != nil {
		panic(err)
	}
	exPath := filepath.Dir(ex)

	test_file := exPath + "/test.csv"

	if _, err := os.Stat(test_file); err == nil {
		fmt.Printf("File test.csv already exists!\n")
	} else {
		fmt.Printf("File test.csv does not exist!\n")
		fmt.Println("Creating test.csv data.")

		f, err := os.Create(test_file)
		if err == nil {
			f.WriteString(testdata)
		}

		fmt.Println("This data contains the fields CU, XC, YC, ZC, and Domain.")
		fmt.Println("To use: nan -f test.csv")
	}
}

func cmd_pred(filename string, flds datafields) {
	fmt.Println("Predicting values ...")
	fmt.Println()

	fmt.Println("File name: " + filename)
	fmt.Println("CU: " + flds.cu)
	fmt.Println("XC: " + flds.xc)
	fmt.Println("YC: " + flds.yc)
	fmt.Println("ZC: " + flds.zc)
	fmt.Println("Domain: " + flds.dom)
	fmt.Println()

	start := time.Now()

	ex, err := os.Executable()
	if err != nil {
		panic(err)
	}
	exPath := filepath.Dir(ex)
	infile := exPath + "/" + filename
	outfile := exPath + "/e" + filename

	// Spinner
	fmt.Print("\033[?25l")
	defer fmt.Print("\033[?25h")

	// read proto data
	//stopChan := make(chan bool)
	//go runSpinner("Loading training data", stopChan)
	protoMatrix := [][]string{}
	proto, err := f.Open("include/proto.csv")
	if err != nil {
		panic(err)
	}
	defer proto.Close()
	reader := csv.NewReader(proto)
	reader.Comma = ','
	reader.LazyQuotes = true
	proto_header, err := reader.Read()
	if err != nil {
		panic(err)
	}
	for {
		record, err := reader.Read()
		if err == io.EOF {
			break
		} else if err != nil {
			panic(err)
		}
		protoMatrix = append(protoMatrix, record)
	}
	//stopChan <- true
	//fmt.Print("\033[2K\r")
	//fmt.Println("✓ Prototypes loaded")

	//stopChan = make(chan bool)
	//go runSpinner("Preparing model", stopChan)
	// Scale matrix
	proto_center := [5]float64{0.182408, 2298366.0, 420094.1, 748.3813, 0.491}
	proto_scale := [5]float64{0.3928515, 3494.0607610, 2138.2136765, 553.5672143, 0.499969}

	// split data into explaining and explained variables
	X := [][]float64{}
	Y := [][]string{}
	for _, data := range protoMatrix {
		// convert str slice data into float slice data
		temp := []float64{}
		for i := range 5 {
			parsedValue, err := strconv.ParseFloat(data[i], 64)
			if err != nil {
				panic(err)
			}
			parsedValue = (parsedValue - proto_center[i]) / proto_scale[i]
			temp = append(temp, parsedValue)
		}
		// explaining variables
		X = append(X, temp)

		// explained variables
		Y = append(Y, data[9:])
	}

	// training
	knn := KNN{}
	knn.k = 1
	knn.fit(X, Y)

	//stopChan <- true
	//fmt.Print("\033[2K\r")
	//fmt.Println("✓ Model prepared")

	// read data
	//stopChan := make(chan bool)
	//go runSpinner("Loading data\n", stopChan)
	dataMatrix := [][]string{}
	inputMatrix := [][]float64{}
	data, err := os.Open(infile)
	if err != nil {
		panic(err)
	}
	defer data.Close()
	r := csv.NewReader(data)
	r.Comma = ','
	r.LazyQuotes = true
	data_header, err := r.Read()
	if err != nil {
		panic(err)
	}
	colids := [5]int{
		getIndex(data_header, flds.cu),
		getIndex(data_header, flds.xc),
		getIndex(data_header, flds.yc),
		getIndex(data_header, flds.zc),
		getIndex(data_header, flds.dom),
	}
	for {
		record, err := r.Read()
		if err == io.EOF {
			break
		} else if err != nil {
			panic(err)
		}
		temp := []float64{}
		for i := range 5 {
			parsedValue, err := strconv.ParseFloat(record[colids[i]], 64)
			if err != nil {
				panic(err)
			}
			parsedValue = (parsedValue - proto_center[i]) / proto_scale[i]
			temp = append(temp, parsedValue)
		}
		dataMatrix = append(dataMatrix, record)
		inputMatrix = append(inputMatrix, temp)
	}
	//stopChan <- true
	//fmt.Print("\033[2K\r")
	//fmt.Println("✓ Data loaded")

	// Predict labels
	stopChan := make(chan bool)
	go runSpinner("Generating predictions", stopChan)
	predicted := knn.predict(inputMatrix)

	// Result
	fmt.Println("... Outputing results")
	var res = make([][]string, len(dataMatrix))
	for i := 0; i < len(dataMatrix); i++ {
		res[i] = append(res[i], dataMatrix[i]...)
		res[i] = append(res[i], predicted[i]...)
	}

	// Write enriched data
	f, err := os.Create(outfile)
	if err != nil {
		panic(err)
	}
	writer := csv.NewWriter(f)
	defer f.Close()
	head := data_header
	for _, h := range proto_header[9:] {
		head = append(head, h)
	}
	e := writer.Write(head)
	if e != nil {
		fmt.Println("Error writing record to CSV:", e)
	}
	e = writer.WriteAll(res)
		stopChan <- true
	fmt.Print("\033[2K\r")
	fmt.Println("✓ Finished predicting")

	t := time.Now()
	fmt.Println("Done!")
	fmt.Println(fmt.Sprintf("duration: %s", t.Sub(start)))
}

type KNN struct {
	k      int
	data   [][]float64
	labels [][]string
}

func (knn *KNN) fit(X [][]float64, Y [][]string) {
	//read data
	knn.data = X
	knn.labels = Y
}

func getIndex(header []string, value string) int {
	indx := -1
	for i := range len(header) {
		if header[i] == value {
			indx = i
		}
	}
	return (indx)
}

// calculate euclidean distance betwee two slices
func Dist(source, dest []float64) float64 {
	val := 0.0
	for i := range source {
		val += math.Pow(source[i]-dest[i], 2)
	}
	return math.Sqrt(val)
}

func transpose(slice [][]string) [][]string {
	xl := len(slice[0])
	yl := len(slice)
	result := make([][]string, xl)
	for i := range result {
		result[i] = make([]string, yl)
	}
	for i := 0; i < xl; i++ {
		for j := 0; j < yl; j++ {
			result[i][j] = slice[j][i]
		}
	}
	return result
}

func (knn *KNN) predict(X [][]float64) [][]string {
	predictedLabel := [][]string{}
	for _, source := range X {
		var (
			distList   []float64
			nearLabels [][]string
		)

		// calculate distance between predict target data and surpervised data
		for _, dest := range knn.data {
			distList = append(distList, Dist(source, dest))
		}

		// take top k nearest item's index
		s := NewFloat64Slice(distList)
		sort.Sort(s)
		targetIndex := s.idx[:knn.k]

		// get the index's label
		for _, ind := range targetIndex {
			nearLabels = append(nearLabels, knn.labels[ind])
		}

		// get label frequency
		nearLabels = transpose(nearLabels)
		p := []string{}
		for _, row := range nearLabels {
			labelFreq := Counter(row)
			// the most frequent label is the predict target label
			a := List{}
			for k, v := range labelFreq {
				e := Entry{k, v}
				a = append(a, e)
			}
			sort.Sort(a)
			p = append(p, a[0].name)
		}

		//predictedLabel = append(predictedLabel, a[0].name)
		predictedLabel = append(predictedLabel, p)
	}

	return predictedLabel
}

// argument sort
type Slice struct {
	sort.Interface
	idx []int
}

func (s Slice) Swap(i, j int) {
	s.Interface.Swap(i, j)
	s.idx[i], s.idx[j] = s.idx[j], s.idx[i]
}

func NewSlice(n sort.Interface) *Slice {
	s := &Slice{Interface: n, idx: make([]int, n.Len())}
	for i := range s.idx {
		s.idx[i] = i
	}
	return s
}

func NewFloat64Slice(n []float64) *Slice { return NewSlice(sort.Float64Slice(n)) }

// map sort
type Entry struct {
	name  string
	value int
}
type List []Entry

func (l List) Len() int {
	return len(l)
}

func (l List) Swap(i, j int) {
	l[i], l[j] = l[j], l[i]
}

func (l List) Less(i, j int) bool {
	if l[i].value == l[j].value {
		return l[i].name < l[j].name
	} else {
		return l[i].value > l[j].value
	}
}

// count item frequence in slice
func Counter(target []string) map[string]int {
	counter := map[string]int{}
	for _, elem := range target {
		counter[elem] += 1
	}
	return counter
}
