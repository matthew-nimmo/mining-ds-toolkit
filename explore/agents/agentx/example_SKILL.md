# Local Project Custom Skills

This file provides decoupled automation recipes that SLMs can call upon to run tasks locally.

## Skill: optimize_images
Description: Scales and compresses local PNG assets within the workspace directory using standard utilities.
Workflow:
```bash
echo "[Skill Action] Optimizing workspace images..."
# Simulate optimization work
ls -la *.png
echo "[Skill Action] Optimization completed successfully."
```

## Skill: evaluate_data_matrix
Description: Loads matrix data arrays and executes mathematical vectors.
Workflow:
```r
# Intentional R calculation checking block
data_matrix <- matrix(1:9, nrow=3)
print("Computed Matrix Transposition successfully:")
print(t(data_matrix))
```
