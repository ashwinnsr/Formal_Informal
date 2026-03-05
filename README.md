# Extraction or Displacement?
### Labor Institutions and Surplus Distribution in India's Textile Value Chains

**Author**: Ashwin  

---

## 📌 Project Overview
This repository contains the full analytical pipeline and LaTeX source code for the MA dissertation investigating the formal-informal linkages in India's textile and apparel sectors. The research adjudicates between the **Modernization (Displacement)** and **Structuralist (Adverse Incorporation)** narratives by testing how labor law enforcement and formal productivity shape informal wages and employment.

## 🚀 Key Research Findings
1.  **Strategic Outsourcing**: Formal productivity growth is positively associated with informal outsourcing ($\beta = 3.72 \times 10^{-6}$, $p=0.043$). This effect is **4.4x stronger** in high-enforcement states, indicating regulatory arbitrage.
2.  **Structural Monopsony**: There is a **statistical zero wage pass-through** ($\beta \approx 0$) from formal gains to informal workers. Lead firms capture 100% of productivity rents.
3.  **Extreme Persistence**: Informal employment exhibits a persistence coefficient of **0.903** ($p < 0.001$), suggesting that informality is a self-perpetuating structural feature rather than a transitional friction.
4.  **Simulation Evidence**: An empirically-grounded Monte Carlo simulation confirms that only **direct wage floors**—not procedural enforcement—effectively raise informal earnings.

## 📂 Project Structure
```bash
.
├── MA_Dissertation/      # LaTeX source code for the full dissertation
├── Scripts/              # R analysis pipeline (Stages 1-5)
│   ├── run_analysis.r    # Master execution script
│   └── Stage5_Policy_Simulation.r # Monte Carlo simulation engine
├── Output/               # Generated tables (.tex) and visualizations (.png)
├── Docs/                 # History, Critical Findings, and Data Mappings
├── ASI_Data/             # [Ignored] Unit-level Formal sector data (2021-24)
├── ASUSE_Data/           # [Ignored] Unit-level Informal sector data (2021-24)
└── README.md             # This file
```

## 🛠️ Methodology & Data
- **Data**: State-Industry pooled panel (N=141) constructed from ASI and ASUSE (2021--22 to 2023--24).
- **Econometrics**: High-dimensional fixed effects (Industry, Year) with clustered standard errors at the state level using the `fixest` package.
- **Simulation**: 3-equation dynamic model with 10,000 Monte Carlo draws to propagate parameter uncertainty.

## 🚀 Getting Started

### 1. Analysis Pipeline (R)
Ensure R (>= 4.3) is installed with required packages (`fixest`, `ggplot2`, `dplyr`, `haven`).
```powershell
Rscript Scripts/run_analysis.r
```

### 2. Dissertation Compilation (LaTeX)
The dissertation uses `biblatex` (Biber) and the `float` package for layout stability.
```bash
pdflatex main.tex
biber main
pdflatex main.tex
```

## 📝 Detailed Documentation
- `Docs/CRITICAL_FINDINGS.md`: Comprehensive breakdown of regression coefficients and diagnostics.
- `Docs/COMPLETE_PROBLEM_HISTORY.md`: Technical log of data matching and methodological pivots.
- `MA_Dissertation/appendices/`: Robustness checks and additional structuralist tests.
