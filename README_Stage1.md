# Stage 1 Dissertation Analysis: Formal-Informal Linkages

## Project Overview
This project examines the linkages between formal and informal sectors in India's Textiles (NIC-13) and Apparel (NIC-14) industries using **ASI 2023-24** and **ASUSE 2023-24** unit-level data.

## 📂 Project Structure

```bash
Formal_Informal/
├── README_Stage1.md          # This file
├── Scripts/                  # All R analysis and diagnostic scripts
│   └── Stage1_StateIndustry_Analysis.r # Main Execution Script ⭐
├── Docs/                     # Detailed reports and mapping guides
│   ├── CRITICAL_FINDINGS.md  # Significant econometric results
│   └── DATA_LIMITATION_REPORT.md # Pivot from district to state level
├── Data/                     # Raw datasets (ASI, ASUSE, External)
├── Output/                   # Regression tables (.tex, .csv) and logs
└── Presentation/             # Beamer slides and Speaker Notes
```

## 🚀 Key Result (February 2026)
The latest analysis confirms a **statistically significant positive linkage** between formal productivity and informal outsourcing:
- **Result**: Higher productivity in formal factories drives increased outsourcing intensity to informal "job work" enterprises ($p = 0.043$).
- **Sample**: 47 state-industry observations across 26 states.

## 📊 Methodology Pivot
Due to the suppression of precise geographic identifiers in public ASI data, the analysis was shifted from **District-Industry** to **State-Industry** level. 
- State codes were successfully extracted from ASI `a7` and matched with ASUSE `district` codes.
- Labor enforcement variation ($E_s$) was gathered from Ministry of Labour & Employment annual reports.

## 🛠️ Execution
To rerun the full analysis:
```powertshell
& "C:\Program Files\R\R-4.5.1\bin\x64\Rscript.exe" "Scripts/Stage1_StateIndustry_Analysis.r"
```

## 📝 Documentation
For detailed technical info, see:
- [CRITICAL_FINDINGS.md](Docs/CRITICAL_FINDINGS.md) - Regression diagnostics.
- [Variable_Mapping_Guide.md](Docs/Variable_Mapping_Guide.md) - ASI/ASUSE field definitions.
- [DATA_LIMITATION_REPORT.md](Docs/DATA_LIMITATION_REPORT.md) - Geographic constraints and solutions.
