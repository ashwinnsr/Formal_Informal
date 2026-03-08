# Dataset Mapping & Block Integration Summary

This document details the mapping of data blocks across ASI and all ASUSE/NSS rounds (2010–2024), documenting the schema challenges and the programmatic resolutions implemented in the analysis pipeline.

---

## 1. Block Mapping Master Table

The analysis integrates variables from three distinct survey frameworks. The following table maps the primary concepts to their respective file blocks.

| Concept | ASI (Formal) | NSS 67 (2010-11) | NSS 73 (2015-16) | ASUSE (2021-24) |
| :--- | :--- | :--- | :--- | :--- |
| **Identification/ID** | Block A | Block 1 | Block 1 | Block 2 |
| **NIC Code (Industry)**| Block A (`nic5digit`) | Block 2 (`b2_q202`) | Block 2 (`b2_q202`) | Block 2 (`major_nic_5dig`) |
| **Job-Work Filter** | N/A (Lead Firm) | Block 2 (`b2_q239`) | Block 2 (`b2_q239`) | Block 2 (`contract_manuf_service`) |
| **Hired Employment** | Block E (`EI6/tmanday`) | Block 8 (Item 801) | Block 8 (Item 801) | Block 4 (Item 511) |
| **Wages/Emoluments** | Block E (`EI8/wages`) | Block 12 (Item 1201) | Block 15 (Item 1502\_3) | Block 5 (Item 559) |
| **Outsourcing Cost** | Block F (`workdoneby`) | N/A | N/A | N/A |
| **Total Output/GVA** | Block J (`exfactval`) | Block 12 (Item 1209) | Block 15 (Item 1507\_3) | N/A |
| **Multiplier (Weight)**| Block A (`multiplier`) | Block 1 (`wgt_combined`) | Block 1 (`mlt`) | Block 2 (`mlt`) |

---

## 2. Key Problems Faced & Resolutions

### A. Schema Inconsistency across Rounds
*   **Problem**: Variable names and item codes for "Job Work" and "Wages" changed in every round (e.g., from `b2_q239` to `contract_manuf_service`).
*   **Resolution**: Implemented **Year-Specific Loaders** in `Stage1_Historical_Integration.r` and `Stage1_MultiYear_Analysis.r`. These functions act as an abstraction layer, mapping disparate raw column names to a unified internal tibble schema (`Factory_ID`, `L_formal_firm`, `w_informal_firm`, etc.).

### B. Semi-Round Data Segmentation (2015-16)
*   **Problem**: The NSS 73rd round was released in two "semi-rounds" with inconsistent multi-level file paths (using both spaces and hyphens in directory names).
*   **Resolution**: Developed a recursive mapping function (`load_s`) that iterate through both semi-round directories, cleans the mismatched file names, and concatenates the results into a single balanced cohort using `bind_rows`.

### C. Nominal vs. Real Wage Comparability
*   **Problem**: Comparing 2010 wages (avg ~50k) to 2024 wages (avg ~300k) is invalid due to inflation.
*   **Resolution**: Created a **CPI-Linked Deflator** system. We linked the CPI-IW (Base 2001) to the modern CPI (Base 2012) using a 1.95 linking factor. All financial variables are now deflated to constant **2010-11 Real Prices**.

### D. Block-Level Joining (Internal Coherence)
*   **Problem**: Joining Block 2 (ID) with Block 5 (Wages) in ASUSE often dropped observations because identifier columns (FSU Serial No, Sample Est No) were formatted differently across blocks.
*   **Resolution**: Implemented a composite **Enterprise_ID** key: `paste(fsu_serial_no, sample_est_no, sep = "_")`. This guaranteed a unique, stable link across all levels of the survey data.

### E. Geographic Suppression
*   **Problem**: Direct district-level codes are often suppressed or masked in public-use files (ASI vs. ASUSE codes don't match exactly).
*   **Resolution**: Standardized on **State-Industry (2-digit NIC)** as the primary unit of observation. This ensures sufficient sample density per cell (~200+ informal enterprises per state-industry) while maintaining geographic granularity for the Political Economy tests.

---

## 3. Cleaning & Validation Logic

1.  **Job-Work Filtering**: Strictly filtered ASUSE/NSS to enterprises where "Contract manufacturing/Job work" = 1. This isolates the "Adverse Incorporation" segment.
2.  **Productivity Calculation**: Formal productivity ($A_F$) is calculated as `Total_Output / L_total_workers` from ASI Blocks J and E.
3.  **Outlier Handling**: Residual-based outlier detection identified 6 critical state-industry-year cells (e.g., Delhi/Gujarat NIC-14) that skewed results. These are excluded in robustness specifications.
4.  **Weighting**: All aggregations use the survey-specific multipliers (`mlt` or `wgt`) to ensure state-level means are representative of the actual industrial population.
