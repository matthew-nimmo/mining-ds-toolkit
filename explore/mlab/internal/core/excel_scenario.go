package core

import (
	"encoding/json"
	"fmt"
	"os"
	"strconv"
	"strings"

	"github.com/xuri/excelize/v2"
)

func atoi(s string) int {
    i, _ := strconv.Atoi(s)
    return i
}

func atof(s string) float64 {
    f, _ := strconv.ParseFloat(s, 64)
    return f
}

func rowVal(row []string, idx int) string {
	if idx >= len(row) {
		return ""
	}
	return strings.TrimSpace(row[idx])
}

// loadParameterMetadata reads the ParameterMetadata sheet into a map keyed by parameter_key.
func loadParameterMetadata(f *excelize.File) (map[string]ParamMeta, error) {
	rows, err := f.GetRows("ParameterMetadata")
	if err != nil {
		return nil, fmt.Errorf("missing ParameterMetadata sheet: %w", err)
	}
	if len(rows) < 2 {
		return nil, fmt.Errorf("ParameterMetadata has no data rows")
	}

	meta := make(map[string]ParamMeta)

	for i, row := range rows {
		if i == 0 {
			// header
			continue
		}
		if len(row) == 0 || rowVal(row, 0) == "" {
			continue
		}
		key := rowVal(row, 0)
		pm := ParamMeta{
			Key:         key,
			Category:    rowVal(row, 1),
			Name:        rowVal(row, 2),
			Unit:        rowVal(row, 3),
			Type:        rowVal(row, 4),
			Default:     atof(rowVal(row, 5)),
			Min:         atof(rowVal(row, 6)),
			Max:         atof(rowVal(row, 7)),
			Description: rowVal(row, 8),
		}
		meta[key] = pm
	}

	return meta, nil
}

// loadPenalties reads the Penalties sheet into []PenaltyRule.
// If the sheet is absent, it returns an empty slice (no error).
func loadPenalties(f *excelize.File) ([]PenaltyRule, error) {
	rows, err := f.GetRows("Penalties")

	if err != nil {
		// no penalties defined; treat as empty
		return []PenaltyRule{}, nil
	}
	if len(rows) < 2 {
		return []PenaltyRule{}, nil
	}

	var rules []PenaltyRule

	for i, row := range rows {
		if i == 0 {
			continue
		}
		if len(row) == 0 || rowVal(row, 0) == "" {
			continue
		}
		r := PenaltyRule{
			ElementCode: rowVal(row, 0),
			Unit:        rowVal(row, 1),
			Type:        rowVal(row, 2),
			Threshold:   atof(rowVal(row, 3)),
			Rate:        atof(rowVal(row, 4)),
			// Description is currently not in PenaltyRule but you can extend if needed
		}
		rules = append(rules, r)
	}

	return rules, nil
}

func loadScenarioSettings(f *excelize.File) (projectStart string, discountRate float64, err error) {
	rows, err := f.GetRows("ScenarioSettings")
	if err != nil {
		return "", 0, fmt.Errorf("missing ScenarioSettings: %w", err)
	}
	if len(rows) < 2 {
		return "", 0, fmt.Errorf("ScenarioSettings has no data row")
	}
	// Assume fixed layout: A1 "project_start", B1 "discount_rate"; row 2 values
	projectStart = rowVal(rows[1], 0)
	discountRate = atof(rowVal(rows[1], 1))
	return projectStart, discountRate, nil
}

func loadCostModelSheet(f *excelize.File) (CostModel, error) {
	rows, err := f.GetRows("CostModel")
	if err != nil {
		return CostModel{}, fmt.Errorf("missing CostModel: %w", err)
	}
	if len(rows) < 2 {
		return CostModel{}, fmt.Errorf("CostModel has no data row")
	}
	row := rows[1]
	cm := CostModel{
		MiningCostPerTonne:     atof(rowVal(row, 0)),
		ProcessingCostPerTonne: atof(rowVal(row, 1)),
		HaulCostPerTonneKm:     atof(rowVal(row, 2)),
		FixedPlantCostPerDay:   atof(rowVal(row, 3)),
	}
	return cm, nil
}

func loadPriceDeckSheet(f *excelize.File) ([]PricePoint, error) {
	rows, err := f.GetRows("PriceDeck")
	if err != nil {
		return nil, fmt.Errorf("missing PriceDeck: %w", err)
	}
	if len(rows) < 2 {
		return nil, fmt.Errorf("PriceDeck has no data rows")
	}

	header := rows[0]
	// find price:* columns
	priceCols := map[string]int{} // elementCode -> colIdx
	for idx, h := range header {
		h = strings.TrimSpace(h)
		if strings.HasPrefix(h, "price:") {
			code := strings.TrimPrefix(h, "price:")
			priceCols[code] = idx
		}
	}

	var out []PricePoint
	for i, row := range rows {
		if i == 0 {
			continue
		}
		if len(row) == 0 || rowVal(row, 0) == "" {
			continue
		}
		pp := PricePoint{
			Date:   rowVal(row, 0),
			Prices: make(map[string]float64),
		}
		for code, idx := range priceCols {
			val := rowVal(row, idx)
			if val != "" {
				pp.Prices[code] = atof(val)
			}
		}
		out = append(out, pp)
	}
	return out, nil
}

func loadResourcesSheet(f *excelize.File) ([]ResourceJSON, error) {
	rows, err := f.GetRows("Resources")
	if err != nil {
		return nil, fmt.Errorf("missing Resources: %w", err)
	}
	if len(rows) < 2 {
		return nil, nil
	}
	var out []ResourceJSON
	for i, row := range rows {
		if i == 0 {
			continue
		}
		if len(row) == 0 || rowVal(row, 0) == "" {
			continue
		}
		r := ResourceJSON{
			ID:      rowVal(row, 0),
			Type:    rowVal(row, 1),
			Workers: atoi(rowVal(row, 2)),
			Capacity: []byte(rowVal(row, 3)), // raw JSON or empty
		}
		out = append(out, r)
	}
	return out, nil
}

func loadStockpilesSheet(f *excelize.File) ([]StockpileJSON, error) {
	rows, err := f.GetRows("Stockpiles")
	if err != nil {
		return nil, fmt.Errorf("missing Stockpiles: %w", err)
	}
	if len(rows) < 2 {
		return nil, nil
	}
	var out []StockpileJSON
	for i, row := range rows {
		if i == 0 {
			continue
		}
		if len(row) == 0 || rowVal(row, 0) == "" {
			continue
		}
		sp := StockpileJSON{
			ID:             rowVal(row, 0),
			InitialTonnes:  atof(rowVal(row, 1)),
			BlendingMethod: rowVal(row, 4),
		}
		if s := rowVal(row, 2); s != "" {
			v := atof(s)
			sp.InitialGrade = &v
		}
		if s := rowVal(row, 3); s != "" {
			v := atof(s)
			sp.MaxTonnage = &v
		}
		out = append(out, sp)
	}
	return out, nil
}

func loadConstraintsSheet(f *excelize.File) ([]ConstraintJSON, error) {
	rows, err := f.GetRows("Constraints")
	if err != nil {
		return nil, fmt.Errorf("missing Constraints: %w", err)
	}
	if len(rows) < 2 {
		return nil, nil
	}
	var out []ConstraintJSON
	for i, row := range rows {
		if i == 0 {
			continue
		}
		if len(row) == 0 || rowVal(row, 0) == "" {
			continue
		}
		var frac *float64
		if s := rowVal(row, 3); s != "" {
			v := atof(s)
			frac = &v
		}
		c := ConstraintJSON{
			Pred:       rowVal(row, 0),
			Succ:       rowVal(row, 1),
			Kind:       ConstraintKind(rowVal(row, 2)),
			Fraction:   frac,
			MinLagDays: atof(rowVal(row, 4)),
		}
		out = append(out, c)
	}
	return out, nil
}

func loadTasksSheet(f *excelize.File, meta map[string]ParamMeta) ([]TaskJSON, ScenarioOptions, error) {
	rows, err := f.GetRows("Tasks")
	if err != nil {
		return nil, ScenarioOptions{}, fmt.Errorf("missing Tasks: %w", err)
	}
	if len(rows) < 2 {
		return nil, ScenarioOptions{}, nil
	}

	header := rows[0]

	// Build maps for dynamic columns based on metadata and prefix pattern.
	gradeCols := map[string]int{}    // elementCode -> colIdx
	recoveryCols := map[string]int{} // elementCode -> colIdx
	optionCols := map[string]int{}   // optionName  -> colIdx
	paramCols := map[string]int{}    // paramName   -> colIdx

	for idx, h := range header {
		h = strings.TrimSpace(h)
		if !strings.Contains(h, ":") {
			continue
		}
		if pm, ok := meta[h]; ok {
			switch pm.Category {
			case "grade":
				gradeCols[pm.Name] = idx
			case "recovery":
				recoveryCols[pm.Name] = idx
			case "option":
				optionCols[pm.Name] = idx
			case "param":
				paramCols[pm.Name] = idx
			}
		}
	}

	var tasks []TaskJSON
	// Aggregate scenario-level options/params from all rows (for now we just take first non-empty)
	flags := map[string]bool{}
	params := map[string]float64{}

	for i, row := range rows {
		if i == 0 {
			continue
		}
		if len(row) == 0 || rowVal(row, 0) == "" {
			continue
		}

		t := TaskJSON{
			ID:       rowVal(row, 0),
			Kind:     rowVal(row, 1),
			Resource: rowVal(row, 2),
			Priority: atoi(rowVal(row, 3)),
			Metric:   atof(rowVal(row, 4)),
			Rate:     atof(rowVal(row, 5)),
		}
		if nb := rowVal(row, 6); nb != "" {
			t.NotBefore = &nb
		}

		// Dynamic per-task data: grades + recoveries
		var grades []ElementGrade
		recovery := map[string]float64{}

		for code, idx := range gradeCols {
			val := rowVal(row, idx)
			if val == "" {
				continue
			}
			key := "grade:" + code
			pm := meta[key]
			grades = append(grades, ElementGrade{
				Code:  code,
				Unit:  pm.Unit,
				Value: atof(val),
			})
		}

		for code, idx := range recoveryCols {
			val := rowVal(row, idx)
			if val == "" {
				continue
			}
			recovery[code] = atof(val)
		}

		// Options / params (scenario-level, but we read from rows)
		for name, idx := range optionCols {
			val := rowVal(row, idx)
			if val == "" {
				continue
			}
			flags[name] = strings.ToUpper(val) == "TRUE"
		}
		for name, idx := range paramCols {
			val := rowVal(row, idx)
			if val == "" {
				continue
			}
			params[name] = atof(val)
		}

		// Attach Block / Plant based on kind
		switch t.Kind {
		case "mining":
			t.Block = &BlockInfo{
				Pit:      rowVal(row, 7),
				Bench:    atof(rowVal(row, 8)),
				Grades:   grades,
				Recovery: recovery,
				Ore:      strings.ToUpper(rowVal(row, 9)) == "TRUE",
			}
		case "processing":
			t.Plant = &PlantInfo{
				FeedSource: rowVal(row, 10),
				Recovery:   recovery,
			}
		}

		tasks = append(tasks, t)
	}

	opts := ScenarioOptions{
		Flags:  flags,
		Params: params,
	}

	return tasks, opts, nil
}

func LoadScenarioFromExcel(path string) (Scenario, error) {
	f, err := excelize.OpenFile(path)
	if err != nil {
		return Scenario{}, err
	}
	defer f.Close()

	var sc Scenario

	// 1. Metadata
	meta, err := loadParameterMetadata(f)
	if err != nil {
		return sc, err
	}
	sc.ParameterMeta = meta

	// 2. Scenario settings
	ps, dr, err := loadScenarioSettings(f)
	if err != nil {
		return sc, err
	}
	sc.ProjectStart = ps
	sc.DiscountRate = dr

	// 3. Cost model
	cm, err := loadCostModelSheet(f)
	if err != nil {
		return sc, err
	}
	sc.CostModel = cm

	// 4. Price deck
	pd, err := loadPriceDeckSheet(f)
	if err != nil {
		return sc, err
	}
	sc.PriceDeck = pd

	// 5. Resources
	res, err := loadResourcesSheet(f)
	if err != nil {
		return sc, err
	}
	sc.Resources = res

	// 6. Stockpiles
	sps, err := loadStockpilesSheet(f)
	if err != nil {
		return sc, err
	}
	sc.Stockpiles = sps

	// 7. Constraints
	cs, err := loadConstraintsSheet(f)
	if err != nil {
		return sc, err
	}
	sc.Constraints = cs

	// 8. Penalties
	pens, err := loadPenalties(f)
	if err != nil {
		return sc, err
	}
	sc.Penalties = pens

	// 9. Tasks + options
	tasks, opts, err := loadTasksSheet(f, meta)
	if err != nil {
		return sc, err
	}
	sc.Tasks = tasks
	sc.Options = opts

	// 10. Write input to JSON
	jsonData, err := json.MarshalIndent(sc, "", "  ")
	if err == nil {
		err = os.WriteFile("scenario.json", jsonData, 0644)
	}

	return sc, nil
}
