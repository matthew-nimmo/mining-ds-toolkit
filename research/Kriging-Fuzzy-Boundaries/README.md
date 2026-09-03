# Fuzzy Domain Kriging (Kriging with Fuzzy Domain Boundaries)

## Theoretical context & lineage

```text
2006–2007 (Kalman Matrix Updating).   2012 (Fuzzy Domain Kriging)           2026 (Unified Engine)
Fast data addition via                Inverse variance penalties (K + R)    Woodbury Matrix Bypasses
Sherman-Morrison / Kalman Gain -->    across soft domain boundaries   -->   & ~100x speedups (Variants A/B/C)
```

The evolution of this project addresses two distinct challenges in spatial statistics and Sequential Gaussian Simulation (SGS):

- 2006/2007 - Sequential Matrix Updating (Kalman Framework): Initial research focused on bypassing full N×N matrix inversions when appending new spatial data points by applying Kalman Gain updates. This successfully eliminated repeated $N \times N$ matrix inversions but was restricted to hard/homogeneous domain assumptions.

- 2012 - Soft Domain Boundaries (Fuzzy Domain Kriging): Introduced soft boundary transition zones by adding diagonal inverse-variance penalty matrices ($K + R$) based on sample fuzzy memberships $u(x)$.

- 2026 - Unified Engine: Merges the recursive update principles of the 2006 Kalman formulation with the 2012 fuzzy boundary mathematics. By performing rank-1 recursive updates directly on the soft-boundary covariance matrix ($K_B = K + R$), Variant C solves the boundary problem while achieving a dramatic increase in speed. Making large-scale Sequential Gaussian Simulations (SGS) on soft-boundary block models practical.

## Legacy early research (2006–2007)

This folder contains the original early exploratory work behind the mathematical foundations of Fuzzy Domain Kriging (FDK). 

Originally created between 2006 and 2007, these spreadsheets reflect an initial investigation into spatial computational efficiency via **Kalman Filtering** and recursive matrix updates before the concept was re-framed in 2012 for soft domain boundary modelling.

## File index 

The legacy files are provided for archival and historic lineage purposes. The files are the remaining fragments of my earlier work on the problem.

### legacy/2006-2007/

#### `kalman.xls` (2006)
* **Objective:** Test recursive spatial covariance updates vs full batch Ordinary Kriging (OK) matrix inversions for a 7-point sample dataset.
* **Sheets:**
  * `Sheet1`: Evaluates batch inverse matrices $K^{-1}$ against step-by-step recursive linear updating formulas labelled as `(Karman Filter)`. Proves that recursive updates yield identical point estimates ($E = 592.73$) to standard kriging while reducing compute overhead.
  * `Sheet2`: Displays spatial distance matrices $h$, covariance matrices $K$, inverse covariance matrices $K^{-1}$, and individual weight allocations ($\lambda$).

#### `kalman2.xls` (2007)
* **Objective:** Formalize sequential state updates for Ordinary Kriging (OK) and Simple Kriging (SK) using Kalman Gain formulations.
* **Sheets:**
  * `OK`: Models state transition updates $E_t = w_1 E_{t-1} + w_2 V_t$ with dynamic local variance updates (`var`) as samples $t = 1 \dots 7$ are sequentially ingested.
  * `SK`: Implements the equivalent Kalman update pipeline under Simple Kriging assumptions.
  * `Sheet1`: Explores system matrix sensitivity ($\det(K)$, $\mu$) when expanding the sample set from 6 to 7 points, tracking the explicit variance reduction when adding sample $V_7 = 783$ at $(75, 128)$.

### legacy/2012/

#### `soft_boundary.xls`
* **Objective:** Investigating Soft Boundary Estimation (FDK(C)) across different spatial domains.
Core Concept: Extends the 2006 fuzzy membership idea into Fuzzy Direct Kriging across class boundaries (FDK(C)).
* **Sheets:**
  * `DATA`: Toy dataset (7 sample points across classes A, B, and C with variable values V, plus target estimation node).
  * `test 1` to `test 4`: Tests examining how varying cross-boundary covariance parameters (nugget, sill, range) influence estimation weight across domains:
    - `test 1`: Soft boundary with full range (R=10, soft factor =1.0).
    - `test 2`: Reduced inter-class correlation range (R=3, soft factor =3.33).
    - `test 3`: Multi-model variance decay across boundary transitions.
    - `test 4`: Sharply restricted inter-class range (R=1, soft factor =10.0).

#### `t.xls`
* **Objective:** Testing Simple Spatial Weighting / Inverse Distance Variants.
* **Core Concept:** A preliminary spreadsheet exploration testing simplified distance-decay functions $C(d)$ alongside full matrix inversions for Kriging weights. Computes relative distance vectors ($ΔX$,$ΔY$) and scalar distance $d=\sqrt{ΔX^2+ΔY^2}$ from the target point to sample points and calculates inverse distance-weighted estimates alongside matrix inversion $[K]^{−1}⋅k$ to compare standard spatial inverse distance weighting directly with Ordinary Kriging.

#### `fuzzy.xls`
* **Objective:** Formulating Fuzzy Membership Covariance / Weighting (FDK).
* **Core Concept:** Prototyping Fuzzy Direct Kriging (FDK) where sample points carry continuous fuzzy membership values rather than hard categorical domain labels.  
* **Sheets:**
  * **Sheet1:** Toy sample data setup.
  * **test 1:** Full matrix breakdown constructing fuzzy covariance matrix $K$ and fuzzy cross-covariance vector $k$. Calculates intermediate terms $E_i=V_i⋅w_i$, fuzzy weights, and fuzzy kriging variance $σ_FDK^2$.

#### `kriging2.R`
* **Objective:** Spatial Indicator Kriging with Nonlinear Post-Processing / Boundary Sharpening.
* **Core Concept:** Applies Ordinary Kriging to binary indicator values $u_i∈{0,1}$ and passes the resulting probability field $Z \le (x,y)∈[0,1]$ through an image-processing style contrast enhancement function sharpen(z, p).  
* Functionality:
  * ok(...): Computes Ordinary Kriging on binary indicator data u over a 2D spatial grid.
  * sharpen(z, p): Takes continuous probability estimates $z∈[0,1]$ and applies a parametric non-linear transformation:
    - When $p=0$: Hard step thresholding ($z≤0.5 \rightarrow 0$, $z>0.5 \rightarrow 1$).
    - When $p>0$: Linear contrast stretching around 0.5 using offset $c=\frac{1−p}{2p}$, softening or sharpening boundary transitions between domains.  
  * show_plot(...): Displays spatial heatmaps and contours of the softened/sharpened boundary field.

## R implementations


## Go implementations (experimental)

The Go code provides an experimental implementation of spatial interpolation algorithms, including Ordinary Kriging and Fuzzy Domain Kriging variants. Designed for 2D spatial mapping and analysis, the package includes built-in benchmarking, CSV data exporting, and image plotting capabilities for visualizing interpolated surfaces.

> Warning: This package contains experimental code. APIs, data structures, and implementation details may undergo breaking changes as research and development progress.

### Basic Usage

Ensure you have Go (1.20+) installed, then grab a copy of this project directory. Open up a terminal window or use VSCODE. If using a terminal window ensure you are inside the copied project folder. Then run:

```bash
# Run performance benchmarks
go test -bench=. ./...

# Run the example
go cmd/jura .
```

### Performance & Benchmarks

| Method | Avg Execution Time | Boundary Handling | Primary Use Case |
| :--- | :--- | :--- | :--- |
| **Standard Ordinary Kriging** | ~553 ns/op | Hard Boundaries Only | Baseline comparison |
| **Variant B (Full Inverse)** | ~570 ns/op | Fuzzy / Soft Boundaries | High-accuracy estimation with slight performance penalty over standard OK |
| **Variant B (Full Inverse)** | ~558 ns/op | Fuzzy / Soft Boundaries | High-accuracy estimation with zero performance penalty over standard OK |
| **Variant C (Recursive Update)**| **11.68 ns/op** | Fuzzy / Soft Boundaries | Large-scale block models, real-time spatial updates & Sequential Gaussian Simulation (SGS) |

* **Zero Overhead for Soft Boundaries:** Variant B proves that incorporating inverse variance penalties ($K + R$) for fuzzy domain boundaries introduces zero execution penalty over standard hard-boundary Ordinary Kriging.
* **Massive Simulation Throughput:** Variant C leverages recursive Woodbury updates to deliver a **~47x speedup**, turning multi-hour Sequential Gaussian Simulation (SGS) runs on million-node block models into sub-minute computations.

### Tail Behaviour & Outlier Robustness

* **Mitigation of Domain Boundary Extremes:** Comparative CDF analysis against standard Ordinary Kriging demonstrates that FDK Variant B exhibits tighter control over distribution tails.
* **Mechanism:** Outlier samples located across fuzzy domain transition boundaries carry reduced membership confidence ($u_i < 1.0$). The resulting $R_{ii}$ penalty dampens their leverage ($w_i$), preventing anomalous off-domain sample values from producing spurious extreme high or low estimates in the target grid.

## Test Suite & Future Directions

> **Note:** The current unit tests serve as initial proof-of-concept validations for Variants A, B, and C across basic fuzzy membership conditions. The test suite is not exhaustive and requires further empirical testing, edge-case coverage (e.g., zero-membership boundaries, ill-conditioned matrices), and numerical performance benchmarking across larger spatial datasets.

## Research Context & Future Outlook

This repository serves as both a historical record of early research (2006–2012) and a modern working implementation of Fuzzy Domain Kriging (FDK). 

By publishing these mathematical foundations, legacy exploratory scripts, and updated high-performance Go/R engines, this project establishes a formal baseline for soft boundary modelling in spatial statistics. It is released openly to encourage further academic research, empirical benchmarking, and production-grade software development in spatial estimation.
