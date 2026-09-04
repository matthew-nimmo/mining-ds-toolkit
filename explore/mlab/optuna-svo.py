import json
import subprocess
import tempfile

def run_go_sim(scenario: dict) -> dict:
    with tempfile.NamedTemporaryFile(suffix=".json", delete=False) as f:
        json.dump(scenario, f)
        f.flush()
        result = subprocess.run(
            ["./simulator", "--scenario", f.name],
            check=True,
            capture_output=True,
            text=True,
        )
    return json.loads(result.stdout)

def objective(trial):
    # Example: tune rate and resource counts
    scenario = {
        "project_start": "2025-01-01",
        "discount_rate": 0.08,
        "tasks": [
            {
                "id": "MINE_BLOCK_001",
                "kind": "mining",
                "resource": "FLEET_TRUCKS",
                "priority": 1,
                "metric": 100000,
                "rate": trial.suggest_float("mining_rate", 4000, 8000),
                "not_before": "2025-01-01",
                "block": {"pit": "Stage1","bench": 1000,"grade": 1.5,"recovery": 0.9,"ore": True},
            },
            {
                "id": "PROCESS_SP1",
                "kind": "processing",
                "resource": "PLANT_1",
                "priority": 10,
                "metric": 80000,
                "rate": 5000,
                "not_before": "2025-01-01",
                "plant": {"feed_source": "STOCKPILE_1","recovery": 0.9},
            },
        ],
        "constraints": [
            {
                "pred": "MINE_BLOCK_001",
                "succ": "PROCESS_SP1",
                "kind": "finish_to_start",
                "min_lag_days": 0.0,
            }
        ],
        "resources": [
            {"id": "FLEET_TRUCKS","type": "mobile_fleet","workers": trial.suggest_int("fleet_workers", 1, 5),"capacity": {}},
            {"id": "PLANT_1","type": "plant","workers": 1,"capacity": {}},
        ],
        "stockpiles": [
            {"id": "STOCKPILE_1","initial_tonnes": 0,"blending_method": "tonnage_weighted"}
        ],
        "price_deck": [{"date": "2025-01-01","metal_price": 1500}],
        "cost_model": {
            "mining_cost_per_tonne": 2.5,
            "processing_cost_per_tonne": 7.0,
            "haul_cost_per_tonne_km": 0.12,
            "fixed_plant_cost_per_day": 50000,
        },
    }

    result = run_go_sim(scenario)
    npv = result["objective"]["npv"]
    return -npv  # minimize negative NPV => maximize NPV
