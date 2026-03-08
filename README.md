# Extraction or Displacement?
### Labor Institutions and Surplus Distribution in India's Textile Value Chains

**Author**: Ashwin Sreekumar  
**Degree**: MA in Public Policy, CHRIST (Deemed to be University), 2026

---

## 📌 Project Overview
This repository contains the full analytical pipeline and LaTeX source code for the MA dissertation investigating formal-informal linkages in India's textile and apparel sectors. The research adjudicates between the **Modernization (Displacement)** and **Structuralist (Adverse Incorporation)** narratives by testing how labor law enforcement and formal productivity shape informal wages and employment across a 15-year horizon (2010--2024).

## 🚀 Key Research Findings
1.  **Strategic Outsourcing**: Formal productivity growth is positively associated with informal outsourcing ($\beta = 9.07 \times 10^{-4}$, $p=0.043$). This effect is restricted to high-enforcement states, indicating strategic regulatory arbitrage.
2.  **Structural Monopsony**: There is a **statistical zero wage pass-through** ($\beta = -0.014$, $p=0.610$) from formal gains to informal workers. Lead firms capture 100% of productivity rents.
3.  **Extreme Persistence**: Informal employment exhibits a persistence coefficient of **0.848** ($p < 0.001$), suggesting that informality is a self-perpetuating structural feature rather than a transitional friction.
4.  **Simulation Evidence**: An empirically-grounded Monte Carlo simulation confirms that only **direct wage floors**—not procedural factory-gate enforcement—effectively raise informal earnings.

## 📂 Project Structure
```bash
.
├── MA_Dissertation/      # LaTeX source code for the full dissertation
├── Presentation/         # Polished Writing Sample and Defense presentation (.tex & .pdf)
├── Scripts/              # R analysis pipeline (Stages 1-6)
│   ├── run_analysis.r    # Master execution script
│   └── Stage5_Policy_Simulation.r # Monte Carlo simulation engine
├── Output/               # Generated tables (.tex) and visualizations (.png)
├── Docs/                 # Project history and critical findings
├── README.md             # This file
└── .gitignore            # Excludes raw microdata and auxiliary logs
```

## 🛠️ Methodology & Data
- **Data**: State-Industry pooled panel (N=152) constructed from ASI, NSS 67th Round, NSS 73rd Round, and ASUSE (2010--2024).
- **Econometrics**: High-dimensional fixed effects (State, Industry, Year) with clustered standard errors at the state level using the `fixest` package.
- **Simulation**: 3-equation dynamic model with 10,000 Monte Carlo draws to propagate parameter uncertainty.

## 🚀 Getting Started

### 1. Analysis Pipeline (R)
Ensure R (>= 4.3) is installed with required packages (`fixest`, `ggplot2`, `dplyr`, `haven`).
```powershell
# Run the full pipeline
Rscript Scripts/run_analysis.r
```

### 2. Deliverables
- **Writing Sample**: `Presentation/writing_sample.pdf`
- **Defense Slides**: `Presentation/presentation.pdf`

## 📝 Detailed Documentation
- `Docs/CRITICAL_FINDINGS.md`: Comprehensive breakdown of regression coefficients and diagnostics.
- `MA_Dissertation/appendices/`: Robustness checks and additional structuralist tests.
