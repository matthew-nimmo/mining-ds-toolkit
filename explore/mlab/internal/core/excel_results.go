package core

import (
    "fmt"

    "github.com/xuri/excelize/v2"
)

func writeTaskScheduleSheet(f *excelize.File, res Result) error {
	const sheet = "Results_TaskSchedule"

	// Reset sheet
    if ok, _ := f.GetSheetIndex(sheet); ok == -1 {
        f.DeleteSheet(sheet)
    }
	f.NewSheet(sheet)

	// Header
	_ = f.SetSheetRow(sheet, "A1", &[]string{
		"id", "resource", "start", "end", "metric", "worker",
	})

	// Rows
	for i, t := range res.Tasks {
		row := []interface{}{
			t.ID, t.Resource, t.Start, t.End, t.Metric, t.Worker,
		}
		cell := fmt.Sprintf("A%d", i+2)
		_ = f.SetSheetRow(sheet, cell, &row)
	}

	return nil
}

func writeStockpileSheet(f *excelize.File, res Result) error {
	const sheet = "Results_Stockpiles"

	if ok, _ := f.GetSheetIndex(sheet); ok == -1 {
		f.DeleteSheet(sheet)
	}
	f.NewSheet(sheet)

	// Header
	_ = f.SetSheetRow(sheet, "A1", &[]string{
		"stockpile", "time", "tonnes", "element_code", "grade",
	})

	r := 2
	for _, sp := range res.Stockpiles {
		for _, snap := range sp.TimeSeries {
			for code, g := range snap.Grades {
				row := []interface{}{
					sp.ID,
					snap.Time.Format("2006-01-02"),
					snap.Tonnage,
					code,
					g,
				}
				cell := fmt.Sprintf("A%d", r)
				_ = f.SetSheetRow(sheet, cell, &row)
				r++
			}
		}
	}

	return nil
}

func writeObjectiveSheet(f *excelize.File, res Result) error {
	const sheet = "Results_Objective"

	if ok, _ := f.GetSheetIndex(sheet); ok == -1 {
		f.DeleteSheet(sheet)
	}
	f.NewSheet(sheet)

	_ = f.SetSheetRow(sheet, "A1", &[]string{"NPV", "Undiscounted"})
	_ = f.SetSheetRow(sheet, "A2", &[]interface{}{
		res.Objective.NPV,
		res.Objective.UndiscountedCashFlow,
	})

	return nil
}

func writeScenarioOptionsSheet(f *excelize.File, sc Scenario) error {
	const sheet = "Results_Options"

	if ok, _ := f.GetSheetIndex(sheet); ok == -1 {
		f.DeleteSheet(sheet)
	}
	f.NewSheet(sheet)

	_ = f.SetSheetRow(sheet, "A1", &[]string{"option", "value"})

	r := 2
	for k, v := range sc.Options.Flags {
		row := []interface{}{k, v}
		cell := fmt.Sprintf("A%d", r)
		_ = f.SetSheetRow(sheet, cell, &row)
		r++
	}

	for k, v := range sc.Options.Params {
		row := []interface{}{k, v}
		cell := fmt.Sprintf("A%d", r)
		_ = f.SetSheetRow(sheet, cell, &row)
		r++
	}

	return nil
}

func writePenaltySheet(f *excelize.File, sc Scenario) error {
	const sheet = "Results_Penalties"

	if ok, _ := f.GetSheetIndex(sheet); ok == -1 {
		f.DeleteSheet(sheet)
	}
	f.NewSheet(sheet)

	_ = f.SetSheetRow(sheet, "A1", &[]string{
		"element_code", "unit", "type", "threshold", "rate",
	})

	for i, p := range sc.Penalties {
		row := []interface{}{
			p.ElementCode,
			p.Unit,
			p.Type,
			p.Threshold,
			p.Rate,
		}
		cell := fmt.Sprintf("A%d", i+2)
		_ = f.SetSheetRow(sheet, cell, &row)
	}

	return nil
}

func writeDiagnosticsSheet(f *excelize.File, res Result, ctx *simContext) error {
    const sheet = "Results_Diagnostics"

    if ok, _ := f.GetSheetIndex(sheet); ok == -1 {
        f.DeleteSheet(sheet)
    }
    f.NewSheet(sheet)

    // Resource Utilization
    _ = f.SetSheetRow(sheet, "A1", &[]string{
        "resource", "worker", "busy_hours", "idle_hours", "utilization_pct",
    })

    r := 2
    for id, rs := range ctx.resources {
        for i := range rs.BusyTime {
            busy := rs.BusyTime[i].Hours()
            idle := rs.IdleTime[i].Hours()
            util := 100 * busy / (busy + idle)

            row := []interface{}{
                string(id), i, busy, idle, util,
            }
            cell := fmt.Sprintf("A%d", r)
            _ = f.SetSheetRow(sheet, cell, &row)
            r++
        }
    }

    // Penalty Summary
    r += 2
    _ = f.SetSheetRow(sheet, fmt.Sprintf("A%d", r), &[]string{
        "element_code", "total_penalty_cost",
    })
    r++

    for code, cost := range ctx.PenaltyCosts {
        row := []interface{}{code, cost}
        cell := fmt.Sprintf("A%d", r)
        _ = f.SetSheetRow(sheet, cell, &row)
        r++
    }

    return nil
}

func WriteResultsToExcel(path string, res Result, sc Scenario, ctx *simContext) error {
	f, err := excelize.OpenFile(path)
	if err != nil {
		return err
	}
	defer f.Close()

	if err := writeTaskScheduleSheet(f, res); err != nil {
		return err
	}
	if err := writeStockpileSheet(f, res); err != nil {
		return err
	}
	if err := writeObjectiveSheet(f, res); err != nil {
		return err
	}
	if err := writeScenarioOptionsSheet(f, sc); err != nil {
		return err
	}
	if err := writePenaltySheet(f, sc); err != nil {
		return err
	}
	if err := writeDiagnosticsSheet(f, res, ctx); err != nil {
		return err
	}

	return f.Save()
}
