# FINAL VARIABLE MAPPING GUIDE (Verified Feb 2026)

The following variables have been cross-checked with the ASI and ASUSE 2023-24 manuals and successfully used in the final regression pipeline.

## 🏭 ASI Variable Mappings (Formal Sector)

| Variable | Column | Block | Notes |
|----------|--------|-------|-------|
| Factory ID | `a1` | Block A | Use as character |
| **State Code** | `a7` | Block A | **Key Fix**: Extract prefix for state merging |
| NIC-2 digit | `a5` | Block A | First 2 digits (filtered for 13, 14) |
| Multiplier | `mult` | Block A | Factor to weight by factory size |
| Mandays | `EI7` | Block E | Summed across categories, then / 300 for L_formal |
| **Outsourcing** | `F7` | Block F | Verified as "Cost of contract and commission work" |
| **Output** | `J113` | Block J | Aggregated sum per factory |

## 🧵 ASUSE Variable Mappings (Informal Sector)

| Variable | Column | Block | Notes |
|----------|--------|-------|-------|
| **Job Work Flag** | `contract_manuf_service` | Block 2 | Set to `1` for subcontracted manufacturing |
| State Code | `district` | Block 2 | Consistent mapping to ASI state codes |
| Multiplier | `mlt` | Block 2 | Used for weighted wage means |
| **Hired Workers** | Item `511` | Block 4 | Counts only salaried/non-family labor |
| **Wages** | Item `559` | Block 5 | Total emoluments paid to hired workers |

---

## 🛠️ Derived Calculations in `Scripts/Stage1_StateIndustry_Analysis.r`

1. **Firm Productivity ($A_f$)**: `Total_Output / (Total_Mandays / 300)`
2. **Outsourcing Share ($Q_{out}$)**: `Outsourcing_Cost / Total_Output`
3. **Informal Wage ($w_{inf}$)**: `Total_Wages / Hired_Workers`
4. **Log Transformations**: `ln(w_inf)`, `ln(A_formal)`, `ln(Q_out + 1)`
5. **Winsorization**: Variables bounded at [1%, 99%] to remove outliers.
