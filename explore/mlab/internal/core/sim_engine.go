package core

import (
	"encoding/json"
	"fmt"
	"math"
	"sort"
	"strings"
	"time"
)

type TaskID string
type ResourceID string
type StockpileID string

// ---------- Scenario JSON types ----------

type ElementGrade struct {
	Code  string  `json:"code"`
	Unit  string  `json:"unit"`
	Value float64 `json:"value"`
}

type PenaltyRule struct {
	ElementCode string  `json:"element_code"` // "As", "Cu"
	Unit        string  `json:"unit"`         // "ppm", "%"
	Type        string  `json:"type"`         // "cost_per_tonne", "value_discount"
	Threshold   float64 `json:"threshold"`    // threshold in same unit as grade
	Rate        float64 `json:"rate"`         // cost per tonne OR discount fraction
}

type ParamMeta struct {
	Key         string  `json:"key"`      // "grade:cu"
	Category    string  `json:"category"` // "grade", "recovery", "penalty", "option", "param"
	Name        string  `json:"name"`     // "cu"
	Unit        string  `json:"unit"`
	Type        string  `json:"type"` // "float", "bool"
	Default     float64 `json:"default"`
	Min         float64 `json:"min"`
	Max         float64 `json:"max"`
	Description string  `json:"description"`
}

type BlockInfo struct {
	Pit      string             `json:"pit"`
	Bench    float64            `json:"bench"`
	Grades   []ElementGrade     `json:"grades"`
	Recovery map[string]float64 `json:"recovery"`
	Ore      bool               `json:"ore"`
}

type PlantInfo struct {
	FeedSource string             `json:"feed_source"` // stockpile id
	BlendRules *BlendRules        `json:"blend_rules,omitempty"`
	Recovery   map[string]float64 `json:"recovery"` // optional override
}

type ScenarioOptions struct {
	Flags  map[string]bool    `json:"flags"`  // option:gravity_circuit -> true/false
	Params map[string]float64 `json:"params"` // param:fleet_rate_multiplier -> value
}

type TaskJSON struct {
	ID        string     `json:"id"`
	Kind      string     `json:"kind"`
	Resource  string     `json:"resource"`
	Priority  int        `json:"priority"`
	Metric    float64    `json:"metric"`
	Rate      float64    `json:"rate"`
	NotBefore *string    `json:"not_before,omitempty"`
	Block     *BlockInfo `json:"block,omitempty"`
	Plant     *PlantInfo `json:"plant,omitempty"`
}

type BlendRules struct {
	TargetGrade float64 `json:"target_grade"`
	MinGrade    float64 `json:"min_grade"`
	MaxGrade    float64 `json:"max_grade"`
}

type ConstraintKind string

const (
	CKFinishToStart   ConstraintKind = "finish_to_start"
	CKPercentComplete ConstraintKind = "percent_complete"
)

type ConstraintJSON struct {
	Pred       string         `json:"pred"`
	Succ       string         `json:"succ"`
	Kind       ConstraintKind `json:"kind"`
	Fraction   *float64       `json:"fraction,omitempty"`
	MinLagDays float64        `json:"min_lag_days"`
}

type ResourceJSON struct {
	ID       string          `json:"id"`
	Type     string          `json:"type"`
	Workers  int             `json:"workers"`
	Capacity json.RawMessage `json:"capacity"` // not used in prototype
}

type StockpileJSON struct {
	ID             string   `json:"id"`
	InitialTonnes  float64  `json:"initial_tonnes"`
	InitialGrade   *float64 `json:"initial_grade,omitempty"` // legacy single-grade, optional
	MaxTonnage     *float64 `json:"max_tonnage,omitempty"`
	BlendingMethod string   `json:"blending_method"`
}

type PricePoint struct {
	Date   string             `json:"date"`
	Prices map[string]float64 `json:"prices"`
}

type CostModel struct {
	MiningCostPerTonne     float64 `json:"mining_cost_per_tonne"`
	ProcessingCostPerTonne float64 `json:"processing_cost_per_tonne"`
	HaulCostPerTonneKm     float64 `json:"haul_cost_per_tonne_km"`
	FixedPlantCostPerDay   float64 `json:"fixed_plant_cost_per_day"`
}

type Scenario struct {
	ProjectStart string  `json:"project_start"`
	DiscountRate float64 `json:"discount_rate"`

	Tasks       []TaskJSON       `json:"tasks"`
	Constraints []ConstraintJSON `json:"constraints"`
	Resources   []ResourceJSON   `json:"resources"`
	Stockpiles  []StockpileJSON  `json:"stockpiles"`
	PriceDeck   []PricePoint     `json:"price_deck"`
	CostModel   CostModel        `json:"cost_model"`

	ParameterMeta map[string]ParamMeta `json:"parameter_meta"`
	Options       ScenarioOptions      `json:"options"`
	Penalties     []PenaltyRule        `json:"penalties"`
}

// ---------- Helpers ----------

func parseDateOrPanic(s string) time.Time {
	t, err := time.Parse("2006-01-02", s)
	if err != nil {
		panic(err)
	}
	return t
}

func parsePriceDeck(deck []PricePoint) []PricePointParsed {
	out := make([]PricePointParsed, len(deck))
	for i, p := range deck {
		out[i] = PricePointParsed{
			Date:   parseDateOrPanic(p.Date),
			Prices: p.Prices,
		}
	}
	sort.Slice(out, func(i, j int) bool { return out[i].Date.Before(out[j].Date) })
	return out
}

func priceForElement(deck []PricePointParsed, code string, d time.Time) float64 {
	if len(deck) == 0 {
		return 0
	}
	var last float64
	found := false
	for _, p := range deck {
		if !p.Date.After(d) {
			if v, ok := p.Prices[code]; ok {
				last = v
				found = true
			}
		} else {
			break
		}
	}
	if !found {
		return 0
	}
	return last
}

// ---------- Internal runtime types ----------

type TaskInstance struct {
	Task            *TaskJSON
	Duration        time.Duration
	Start           time.Time
	End             time.Time
	TotalMetric     float64
	CompletedMetric float64

	Preds []*DepEdge
	Succs []*DepEdge

	WaitOnDeps     time.Duration
	WaitOnResource time.Duration
}

type DepEdge struct {
	Pred      *TaskInstance
	Succ      *TaskInstance
	Kind      ConstraintKind
	Fraction  float64
	MinLag    time.Duration
	Satisfied bool
}

type ResourceState struct {
	ID                ResourceID
	NextAvailableTime []time.Time // one per worker

	BusyTime []time.Duration
	IdleTime []time.Duration
}

type Stockpile struct {
	ID         StockpileID
	Tonnage    float64
	Grades     map[string]float64
	History    []StockpileSnapshot
	MaxTonnage float64

	StarvedDays  int
	OverflowDays int
}

type StockpileSnapshot struct {
	Time    time.Time          `json:"time"`
	Tonnage float64            `json:"tonnes"`
	Grades  map[string]float64 `json:"grades"`
}

type CashFlowEntry struct {
	Date    time.Time
	Revenue float64
	Cost    float64
}

type CashFlow struct {
	Entries []CashFlowEntry
}

// ---------- Result JSON ----------

type Result struct {
	Objective   ObjectiveResult   `json:"objective"`
	Tasks       []TaskResult      `json:"tasks"`
	Stockpiles  []StockpileResult `json:"stockpiles"`
	Plant       []PlantResult     `json:"plant"`
	Diagnostics DiagnosticsResult `json:"diagnostics"`
}

type ObjectiveResult struct {
	NPV                  float64 `json:"npv"`
	UndiscountedCashFlow float64 `json:"undiscounted_cash_flow"`
}

type TaskResult struct {
	ID       string  `json:"id"`
	Resource string  `json:"resource"`
	Start    string  `json:"start"`
	End      string  `json:"end"`
	Metric   float64 `json:"metric"`
	Worker   int     `json:"worker"`
}

type StockpileResult struct {
	ID         string              `json:"id"`
	TimeSeries []StockpileSnapshot `json:"time_series"`
}

type PlantPeriod struct {
	Start          string  `json:"start"`
	End            string  `json:"end"`
	FeedTonnes     float64 `json:"feed_tonnes"`
	FeedGrade      float64 `json:"feed_grade"`
	Recovery       float64 `json:"recovery"`
	MetalProduced  float64 `json:"metal_produced"`
	Revenue        float64 `json:"revenue"`
	ProcessingCost float64 `json:"processing_cost"`
}

type PlantResult struct {
	ID      string        `json:"id"`
	Periods []PlantPeriod `json:"periods"`
}

type DiagnosticsResult struct {
	BusiestResources []ResourceUtilization `json:"busiest_resources"`
}

type ResourceUtilization struct {
	Resource    string  `json:"resource"`
	Utilization float64 `json:"utilization"`
}

// ---------- Cash flow helpers ----------

func (cf *CashFlow) Add(date time.Time, revenue, cost float64) {
	cf.Entries = append(cf.Entries, CashFlowEntry{
		Date: date, Revenue: revenue, Cost: cost,
	})
}

func (cf *CashFlow) NPV(discountRate float64, start time.Time) float64 {
	npv := 0.0
	for _, e := range cf.Entries {
		years := e.Date.Sub(start).Hours() / (24.0 * 365.0)
		df := math.Pow(1.0+discountRate, years)
		npv += (e.Revenue - e.Cost) / df
	}
	return npv
}

func (cf *CashFlow) Undiscounted() float64 {
	sum := 0.0
	for _, e := range cf.Entries {
		sum += e.Revenue - e.Cost
	}
	return sum
}

// ---------- Stockpile logic ----------

func NewStockpile(js StockpileJSON, start time.Time) *Stockpile {
	sp := &Stockpile{
		ID:         StockpileID(js.ID),
		Tonnage:    js.InitialTonnes,
		Grades:     make(map[string]float64),
		History:    []StockpileSnapshot{},
		MaxTonnage: *js.MaxTonnage,
	}
	// legacy: single initial grade if present
	if js.InitialGrade != nil {
		sp.Grades["grade"] = *js.InitialGrade
	}
	sp.History = append(sp.History, StockpileSnapshot{
		Time:    start,
		Tonnage: sp.Tonnage,
		Grades:  copyGrades(sp.Grades),
	})
	return sp
}

func copyGrades(m map[string]float64) map[string]float64 {
	out := make(map[string]float64, len(m))
	for k, v := range m {
		out[k] = v
	}
	return out
}

func (sp *Stockpile) Add(tonnes float64, grades map[string]float64, at time.Time) {
	if sp.Tonnage > sp.MaxTonnage {
		sp.OverflowDays++
	}
	if tonnes <= 0 {
		return
	}
	totalTonnes := sp.Tonnage + tonnes
	if totalTonnes <= 0 {
		return
	}
	// update each element
	for code, gNew := range grades {
		gOld := sp.Grades[code]
		metalOld := sp.Tonnage * gOld
		metalNew := tonnes * gNew
		sp.Grades[code] = (metalOld + metalNew) / totalTonnes
	}
	sp.Tonnage = totalTonnes
	sp.History = append(sp.History, StockpileSnapshot{
		Time:    at,
		Tonnage: sp.Tonnage,
		Grades:  copyGrades(sp.Grades),
	})
}

func (sp *Stockpile) Remove(tonnes float64, at time.Time) (actualTonnes float64, grades map[string]float64) {
	if sp.Tonnage <= 0 {
		sp.StarvedDays++
	}
	if tonnes <= 0 || sp.Tonnage <= 0 {
		return 0, nil
	}
	if tonnes > sp.Tonnage {
		tonnes = sp.Tonnage
	}
	actualTonnes = tonnes
	grades = copyGrades(sp.Grades)
	sp.Tonnage -= tonnes
	// grades unchanged since we assume homogeneous blending
	sp.History = append(sp.History, StockpileSnapshot{
		Time:    at,
		Tonnage: sp.Tonnage,
		Grades:  copyGrades(sp.Grades),
	})
	return actualTonnes, grades
}

// ---------- Resource logic ----------

func NewResourceStates(resJSON []ResourceJSON, start time.Time) map[ResourceID]*ResourceState {
	out := make(map[ResourceID]*ResourceState)
	for _, r := range resJSON {
		if r.Workers <= 0 {
			r.Workers = 1
		}
		times := make([]time.Time, r.Workers)
		for i := range times {
			times[i] = start
		}
		out[ResourceID(r.ID)] = &ResourceState{
			ID:                ResourceID(r.ID),
			NextAvailableTime: times,
			BusyTime:          make([]time.Duration, r.Workers),
			IdleTime:          make([]time.Duration, r.Workers),
		}
	}
	return out
}

// ReserveSlot simulates assignment to earliest free worker.
func (rs *ResourceState) ReserveSlot(readyAt time.Time, duration time.Duration) (start, end time.Time, workerIdx int) {
	idx := 0
	earliest := rs.NextAvailableTime[0]
	for i := 1; i < len(rs.NextAvailableTime); i++ {
		if rs.NextAvailableTime[i].Before(earliest) {
			earliest = rs.NextAvailableTime[i]
			idx = i
		}
	}
	start = readyAt
	if earliest.After(start) {
		start = earliest
	}
	end = start.Add(duration)
	rs.NextAvailableTime[idx] = end
	workerIdx = idx

	idle := start.Sub(earliest)
	rs.IdleTime[idx] += idle

	busy := end.Sub(start)
	rs.BusyTime[idx] += busy

	return
}

// ---------- Simulation core ----------

type PricePointParsed struct {
	Date   time.Time
	Prices map[string]float64
}

type simContext struct {
	startTime time.Time

	tasks      map[TaskID]*TaskInstance
	resources  map[ResourceID]*ResourceState
	stockpiles map[StockpileID]*Stockpile

	priceDeck []PricePointParsed
	costModel CostModel
	cashFlow  CashFlow

	options   ScenarioOptions
	penalties []PenaltyRule
	paramMeta map[string]ParamMeta

	PenaltyCosts        map[string]float64 // elementCode -> total cost
	GravityExtraMetal   float64
	GravityExtraRevenue float64
}

func buildSimContext(sc Scenario) (*simContext, error) {
	projectStart := parseDateOrPanic(sc.ProjectStart)

	ctx := &simContext{
		startTime:    projectStart,
		tasks:        make(map[TaskID]*TaskInstance),
		resources:    NewResourceStates(sc.Resources, projectStart),
		stockpiles:   make(map[StockpileID]*Stockpile),
		priceDeck:    parsePriceDeck(sc.PriceDeck),
		costModel:    sc.CostModel,
		cashFlow:     CashFlow{},
		options:      sc.Options,
		penalties:    sc.Penalties,
		paramMeta:    sc.ParameterMeta,
		PenaltyCosts: make(map[string]float64),
	}

	// stockpiles
	for _, spjs := range sc.Stockpiles {
		ctx.stockpiles[StockpileID(spjs.ID)] = NewStockpile(spjs, projectStart)
	}

	// tasks
	for _, tjs := range sc.Tasks {
		nb := projectStart
		if tjs.NotBefore != nil {
			nb = parseDateOrPanic(*tjs.NotBefore)
		}
		// store NotBefore back to struct for convenience
		tjsCopy := tjs
		tjsCopy.NotBefore = nil // we track timing separately if needed

		// duration in days, then convert to duration
		durationDays := tjs.Metric / tjs.Rate
		dur := time.Duration(durationDays*24.0) * time.Hour

		ti := &TaskInstance{
			Task:            &tjsCopy,
			Duration:        dur,
			TotalMetric:     tjs.Metric,
			CompletedMetric: 0,
			Preds:           []*DepEdge{},
			Succs:           []*DepEdge{},
		}
		// encode not_before as Start sentinel (we'll enforce later)
		ti.Start = nb

		ctx.tasks[TaskID(tjs.ID)] = ti
	}

	// constraints
	for _, cj := range sc.Constraints {
		pred := ctx.tasks[TaskID(cj.Pred)]
		succ := ctx.tasks[TaskID(cj.Succ)]
		if pred == nil || succ == nil {
			return nil, fmt.Errorf("unknown task in constraint %+v", cj)
		}
		frac := 0.0
		if cj.Fraction != nil {
			frac = *cj.Fraction
		}
		edge := &DepEdge{
			Pred:     pred,
			Succ:     succ,
			Kind:     cj.Kind,
			Fraction: frac,
			MinLag:   time.Duration(cj.MinLagDays*24.0) * time.Hour,
		}
		pred.Succs = append(pred.Succs, edge)
		succ.Preds = append(succ.Preds, edge)
	}

	return ctx, nil
}

// onTaskProgress handles percent-complete dependencies.
func onTaskProgress(pred *TaskInstance, now time.Time) {
	for _, edge := range pred.Succs {
		if edge.Satisfied {
			continue
		}
		if edge.Kind == CKPercentComplete {
			threshold := edge.Fraction * pred.TotalMetric
			if pred.CompletedMetric >= threshold {
				if now.After(pred.Start.Add(edge.MinLag)) || now.Equal(pred.Start.Add(edge.MinLag)) {
					edge.Satisfied = true
				}
			}
		}
	}
}

// onTaskComplete handles finish-to-start dependencies.
func onTaskComplete(pred *TaskInstance) {
	for _, edge := range pred.Succs {
		if edge.Kind == CKFinishToStart {
			edge.Satisfied = true
		}
	}
}

// gravityEffectiveRecovery implements a simple Option-B gravity effect
// as an effective Au recovery boost controlled by options/params.
func gravityEffectiveRecovery(baseRecovery float64, elementCode string, opts ScenarioOptions) float64 {
	if !opts.Flags["gravity_circuit"] {
		return baseRecovery
	}
	// Only apply to Au (or gold-like) for this prototype
	if strings.ToUpper(elementCode) != "AU" {
		return baseRecovery
	}
	split := opts.Params["gravity_split_fraction"]
	if split <= 0 {
		split = 0.1 // default 10% split if not specified
	}
	boost := opts.Params["gravity_recovery_boost_au"]
	if boost <= 0 {
		boost = 0.1 // default +10% absolute recovery on split
	}
	// Effective recovery = base on full stream + extra on split
	eff := baseRecovery + boost*split
	if eff > 1.0 {
		eff = 1.0
	}
	return eff
}

// applyPenalties adjusts revenue and/or adds penalty costs.
func applyPenalties(ctx *simContext, date time.Time, feedTonnes float64, blendedGrades map[string]float64, revenue *float64) {
	for _, rule := range ctx.penalties {
		grade := blendedGrades[rule.ElementCode]

		switch rule.Type {
		case "cost_per_tonne":
			// e.g. As > threshold -> cost per tonne processed
			if grade > rule.Threshold {
				penalty := rule.Rate * feedTonnes
				ctx.cashFlow.Add(date, 0, penalty)
				ctx.PenaltyCosts[rule.ElementCode] += penalty
			}
		case "value_discount":
			// e.g. Cu < threshold -> discount on revenue
			if grade < rule.Threshold {
				*revenue *= (1.0 - rule.Rate)
				loss := (*revenue) * rule.Rate
				ctx.PenaltyCosts[rule.ElementCode] += loss
			}
		}
	}
}

// simulateTask executes a task logically in daily steps, updating cash flow.
func simulateTask(ctx *simContext, ti *TaskInstance, earliestFromDeps time.Time) (start, end time.Time) {
	// resource constraint
	rs := ctx.resources[ResourceID(ti.Task.Resource)]
	start, endTent, _ := rs.ReserveSlot(earliestFromDeps, ti.Duration)
	ti.Start = start
	ti.End = endTent

	remaining := ti.TotalMetric
	currentTime := start
	dailyRate := ti.Task.Rate

	for remaining > 0 {
		stepMetric := dailyRate
		if stepMetric > remaining {
			stepMetric = remaining
		}
		remaining -= stepMetric
		ti.CompletedMetric = ti.TotalMetric - remaining

		// progress events for successors
		onTaskProgress(ti, currentTime)

		waitDeps := earliestFromDeps.Sub(ti.Start)
		if waitDeps > 0 {
			ti.WaitOnDeps = waitDeps
		}

		waitRes := start.Sub(earliestFromDeps)
		if waitRes > 0 {
			ti.WaitOnResource = waitRes
		}

		switch ti.Task.Kind {
		case "mining":
			if ti.Task.Block != nil && ti.Task.Block.Ore {
				grades := make(map[string]float64)
				for _, g := range ti.Task.Block.Grades {
					grades[g.Code] = g.Value
				}
				// For now, send all ore to a default stockpile "STOCKPILE_1"
				sp := ctx.stockpiles[StockpileID("STOCKPILE_1")]
				if sp != nil {
					sp.Add(stepMetric, grades, currentTime)
				}
				// mining cost
				cost := ctx.costModel.MiningCostPerTonne * stepMetric
				ctx.cashFlow.Add(currentTime, 0, cost)
			}

		case "processing":
			if ti.Task.Plant != nil {
				sp := ctx.stockpiles[StockpileID(ti.Task.Plant.FeedSource)]
				if sp != nil {
					feedTonnes, feedGrades := sp.Remove(stepMetric, currentTime)
					if feedTonnes > 0 {
						revenue := 0.0
						// compute per-element recovered metal and revenue
						for code, g := range feedGrades {
							// base plant recovery
							baseRec := ti.Task.Plant.Recovery[code]
							if baseRec <= 0 {
								baseRec = 0.0
							}
							effRec := gravityEffectiveRecovery(baseRec, code, ctx.options)
							// units: assume g/t, %, ppm etc. handled externally; here we treat Value as "grade" in mass terms
							// For simplicity: recoveredMetal = tonnes * grade * effRec
							recoveredMetal := feedTonnes * g * effRec
							price := priceForElement(ctx.priceDeck, code, currentTime)
							revenue += recoveredMetal * price

							if code == "Au" {
								extra := recoveredMetal - (feedTonnes * g * baseRec)
								ctx.GravityExtraMetal += extra
								ctx.GravityExtraRevenue += extra * price
							}
						}
						// penalties
						applyPenalties(ctx, currentTime, feedTonnes, feedGrades, &revenue)
						// processing cost
						procCost := ctx.costModel.ProcessingCostPerTonne * feedTonnes
						// fixed plant cost per day (charged once per day if any processing)
						fixedCost := ctx.costModel.FixedPlantCostPerDay
						ctx.cashFlow.Add(currentTime, revenue, procCost+fixedCost)
					}
				}
			}
		}

		currentTime = currentTime.Add(24 * time.Hour)
	}

	onTaskComplete(ti)
	ti.End = currentTime
	return ti.Start, ti.End
}

// RunSimulation is the main entrypoint: it builds context, runs the schedule, and returns Result.
func RunSimulation(sc Scenario) (Result, *simContext, error) {
	ctx, err := buildSimContext(sc)
	if err != nil {
		return Result{}, &simContext{}, err
	}
	projectStart := ctx.startTime

	// dependency counts
	neededDeps := make(map[TaskID]int)
	for id, ti := range ctx.tasks {
		neededDeps[id] = len(ti.Preds)
	}
	depSatisfied := make(map[TaskID]int)

	// ready set
	ready := make(map[TaskID]*TaskInstance)
	for id, ti := range ctx.tasks {
		if neededDeps[id] == 0 {
			ready[id] = ti
		}
	}

	var scheduledOrder []*TaskInstance

	for len(ready) > 0 {
		// pick task by earliest NotBefore (encoded in Start) then priority then ID
		var chosen *TaskInstance
		for _, ti := range ready {
			if chosen == nil {
				chosen = ti
				continue
			}
			nbChosen := chosen.Start
			nbTi := ti.Start
			if nbTi.Before(nbChosen) {
				chosen = ti
			} else if nbTi.Equal(nbChosen) {
				if ti.Task.Priority < chosen.Task.Priority {
					chosen = ti
				} else if ti.Task.Priority == chosen.Task.Priority && ti.Task.ID < chosen.Task.ID {
					chosen = ti
				}
			}
		}
		delete(ready, TaskID(chosen.Task.ID))

		// earliest from deps = max(End+lag) over preds
		earliestFromDeps := projectStart
		for _, e := range chosen.Preds {
			depEnd := e.Pred.End.Add(e.MinLag)
			if depEnd.After(earliestFromDeps) {
				earliestFromDeps = depEnd
			}
		}
		// not-before constraint
		if chosen.Start.After(earliestFromDeps) {
			earliestFromDeps = chosen.Start
		}

		simulateTask(ctx, chosen, earliestFromDeps)
		scheduledOrder = append(scheduledOrder, chosen)

		// release successors
		for _, e := range chosen.Succs {
			succ := e.Succ
			if succ == nil {
				continue
			}
			count := 0
			for _, pe := range succ.Preds {
				if pe.Satisfied {
					count++
				}
			}
			depSatisfied[TaskID(succ.Task.ID)] = count
			if count == neededDeps[TaskID(succ.Task.ID)] {
				ready[TaskID(succ.Task.ID)] = succ
			}
		}
	}

	npv := ctx.cashFlow.NPV(sc.DiscountRate, projectStart)
	undisc := ctx.cashFlow.Undiscounted()

	// Build Result struct (kept intentionally simple; results_excel.go decides what to write)
	var taskResults []TaskResult
	for _, ti := range scheduledOrder {
		taskResults = append(taskResults, TaskResult{
			ID:       ti.Task.ID,
			Resource: ti.Task.Resource,
			Start:    ti.Start.Format("2006-01-02"),
			End:      ti.End.Format("2006-01-02"),
			Metric:   ti.TotalMetric,
			Worker:   0, // not tracking worker index in this prototype
		})
	}

	var spResults []StockpileResult
	for id, sp := range ctx.stockpiles {
		spResults = append(spResults, StockpileResult{
			ID:         string(id),
			TimeSeries: sp.History,
		})
	}

	res := Result{
		Objective: ObjectiveResult{
			NPV:                  npv,
			UndiscountedCashFlow: undisc,
		},
		Tasks:      taskResults,
		Stockpiles: spResults,
		Plant:      []PlantResult{}, // can be filled later with more detailed plant-period aggregation
		Diagnostics: DiagnosticsResult{
			BusiestResources: []ResourceUtilization{}, // TODO: compute utilization
		},
	}
	return res, ctx, nil
}
