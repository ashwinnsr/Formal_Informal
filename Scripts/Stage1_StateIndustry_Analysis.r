################################################################################
# STAGE 1 REDUCED-FORM ANALYSIS - STATE-INDUSTRY LEVEL
# Dissertation: Formal-Informal Linkages in Textiles & Apparel
#
# ANALYSIS LEVEL: State-Industry (9 states × 2 industries = 18 observations)
#
# Research Questions:
# 1. Does formal productivity pass through to informal wages?
# 2. Does labor enforcement moderate this relationship?
# 3. Do firms strategically outsource in response to enforcement?
#
# KEY FINDING: ASI factory IDs encode state (first 2 digits)
################################################################################

# =============================================================================
# PHASE 1: SETUP AND LIBRARIES
# =============================================================================

# Add local library path
.libPaths(c("R_libs", .libPaths()))

library(tidyverse) # Data manipulation
library(haven) # Reading .sav (SPSS) files
library(fixest) # High-dimensional fixed effects regressions
library(stargazer) # Pretty regression tables
library(DescTools) # For Winsorize function

# Set working directory
setwd("C:/Users/ashwin/Documents/Formal_Informal")

# Create output directory
if (!dir.exists("Output")) dir.create("Output")

cat("=== STAGE 1 STATE-INDUSTRY ANALYSIS ===\n")
cat("Working Directory:", getwd(), "\n\n")

# =============================================================================
# PHASE 2: PROCESSING ASI DATA (FORMAL SECTOR)
# =============================================================================

cat("=== PHASE 2: PROCESSING ASI DATA ===\n")

# --- Step 2.1: Load ASI Blocks ---
cat("Loading ASI blocks...\n")

asi_blk_a <- read_sav("ASI_Data/ASI_202324sav/blkA202324.sav")
asi_blk_e <- read_sav("ASI_Data/ASI_202324sav/blkE202324.sav")
asi_blk_f <- read_sav("ASI_Data/ASI_202324sav/blkF202324.sav")
asi_blk_j <- read_sav("ASI_Data/ASI_202324sav/blkJ202324.sav")

cat(sprintf("  Loaded %d factories\n", nrow(asi_blk_a)))

# --- Step 2.2: Clean Block A (Identification) ---
cat("Cleaning Block A (Identification)...\n")

asi_id <- asi_blk_a %>%
    mutate(
        Factory_ID = as.character(a1),
        # CRITICAL: Use a7 for REAL state codes (33 states for textiles/apparel)
        # a7 is the State_Code in ASI 2023-24
        State_Code = as.character(a7),
        NIC_Code = as.character(a5),
        NIC_2digit = substr(NIC_Code, 1, 2),
        Multiplier = mult
    ) %>%
    filter(NIC_2digit %in% c("13", "14")) %>% # Textiles & Apparel
    select(Factory_ID, State_Code, NIC_2digit, Multiplier)

cat(sprintf(
    "  %d textile/apparel factories across %d states\n",
    nrow(asi_id), length(unique(asi_id$State_Code))
))

# --- Step 2.3: Clean Block E (Employment) ---
cat("Cleaning Block E (Employment)...\n")

asi_emp <- asi_blk_e %>%
    mutate(Factory_ID = as.character(AE01)) %>%
    group_by(Factory_ID) %>%
    summarise(
        Total_Mandays = sum(EI7, na.rm = TRUE),
        .groups = "drop"
    ) %>%
    mutate(L_formal_firm = Total_Mandays / 300)

cat(sprintf("  Calculated employment for %d factories\n", nrow(asi_emp)))

# --- Step 2.4: Clean Block F (Outsourcing) ---
cat("Cleaning Block F (Outsourcing)...\n")

asi_outsource <- asi_blk_f %>%
    mutate(
        Factory_ID = as.character(AF01),
        Outsourcing_Cost = ifelse(is.na(F7), 0, F7)
    ) %>%
    select(Factory_ID, Outsourcing_Cost)

# --- Step 2.5: Clean Block J (Output) ---
cat("Cleaning Block J (Output)...\n")

asi_prod <- asi_blk_j %>%
    mutate(Factory_ID = as.character(AJ01)) %>%
    group_by(Factory_ID) %>%
    summarise(
        Total_Output = sum(J113, na.rm = TRUE),
        .groups = "drop"
    )

# --- Step 2.6: Merge ASI Blocks at Firm Level ---
cat("Merging ASI blocks...\n")

asi_firm_level <- asi_id %>%
    left_join(asi_emp, by = "Factory_ID") %>%
    left_join(asi_outsource, by = "Factory_ID") %>%
    left_join(asi_prod, by = "Factory_ID") %>%
    replace_na(list(
        Outsourcing_Cost = 0,
        Total_Output = 0,
        L_formal_firm = 0
    )) %>%
    filter(Total_Output > 0, L_formal_firm > 0) %>%
    mutate(
        Productivity_A = Total_Output / L_formal_firm,
        Outsourcing_Share = Outsourcing_Cost / Total_Output
    )

cat(sprintf("  Firm-level dataset: %d factories\n", nrow(asi_firm_level)))

# --- Step 2.7: Aggregate to STATE-INDUSTRY Level ---
cat("\nAggregating to STATE-INDUSTRY level...\n")

asi_state_industry <- asi_firm_level %>%
    group_by(State_Code, NIC_2digit) %>%
    summarise(
        A_formal = weighted.mean(Productivity_A, w = Multiplier, na.rm = TRUE),
        Q_out_mean = weighted.mean(Outsourcing_Share, w = Multiplier, na.rm = TRUE),
        L_formal_total = sum(L_formal_firm * Multiplier, na.rm = TRUE),
        N_firms = n(),
        .groups = "drop"
    )

cat(sprintf("  Created %d state-industry cells\n", nrow(asi_state_industry)))
cat("\nASI State-Industry Summary (Top 10):\n")
print(head(asi_state_industry %>% arrange(State_Code, NIC_2digit), 10))
cat("\n")

# =============================================================================
# PHASE 3: PROCESSING ASUSE DATA (INFORMAL SECTOR)
# =============================================================================

cat("=== PHASE 3: PROCESSING ASUSE DATA ===\n")

# --- Step 3.1: Load ASUSE Blocks ---
cat("Loading ASUSE blocks...\n")

asuse_blk_2 <- read_sav("ASUSE_Data/ASUSE202324sav/LEVEL - 02(Block 2).sav")
asuse_blk_4 <- read_sav("ASUSE_Data/ASUSE202324sav/LEVEL - 05 (Block 4).sav")
asuse_blk_5 <- read_sav("ASUSE_Data/ASUSE202324sav/LEVEL - 06 (Block 5 - section 5.1 to 5.14).sav")

# --- Step 3.2: Clean Block 2 (Identification & Job Work Filter) ---
cat("Filtering for job work enterprises...\n")

asuse_id <- asuse_blk_2 %>%
    mutate(
        Enterprise_ID = paste(fsu_serial_no, sample_est_no, sep = "_"),
        NIC_2digit = substr(as.character(major_nic_5dig), 1, 2),
        # CRITICAL: ASUSE district codes appear to map to ASI state codes
        # Based on diagnostics, 'district' contains 2-digit codes like '01', '09', '11'
        # We assume 'district' here actually represents the STATE code in this specific dataset version
        State_Code = as.character(district)
    ) %>%
    filter(
        NIC_2digit %in% c("13", "14"),
        as.character(contract_manuf_service) == "1"
    ) %>%
    select(Enterprise_ID, State_Code, NIC_2digit, Multiplier = mlt)

cat(sprintf("  %d job work enterprises\n", nrow(asuse_id)))
cat("  ASUSE Unique State Codes found:", paste(head(unique(asuse_id$State_Code), 10), collapse = ", "), "...\n")


# --- Step 3.3: Clean Block 4 (Hired Workers) ---
cat("Extracting hired workers (Item 511)...\n")

asuse_workers <- asuse_blk_4 %>%
    mutate(Enterprise_ID = paste(fsu_serial_no, sample_est_no, sep = "_")) %>%
    filter(as.character(item_no) == "511") %>%
    select(Enterprise_ID, Hired_Workers = value_rs)

# --- Step 3.4: Clean Block 5 (Wages) ---
cat("Extracting wages (Item 559)...\n")

asuse_wages <- asuse_blk_5 %>%
    mutate(Enterprise_ID = paste(fsu_serial_no, sample_est_no, sep = "_")) %>%
    filter(as.character(item_no) == "559") %>%
    select(Enterprise_ID, Total_Wages = value_rs)

# In Phase 3.5, replace with:
cat("Merging ASUSE blocks...\n")

# Aggregate by Enterprise_ID to eliminate duplicates
asuse_workers_agg <- asuse_workers %>%
    group_by(Enterprise_ID) %>%
    summarise(Hired_Workers = sum(Hired_Workers, na.rm = TRUE), .groups = "drop")

asuse_wages_agg <- asuse_wages %>%
    group_by(Enterprise_ID) %>%
    summarise(Total_Wages = sum(Total_Wages, na.rm = TRUE), .groups = "drop")

asuse_firm_level <- asuse_id %>%
    inner_join(asuse_workers_agg, by = "Enterprise_ID", relationship = "many-to-one") %>%
    inner_join(asuse_wages_agg, by = "Enterprise_ID", relationship = "many-to-one") %>%
    filter(Hired_Workers > 0, Total_Wages > 0) %>%
    mutate(w_informal_firm = Total_Wages / Hired_Workers)

# --- Step 3.6: Aggregate to STATE-INDUSTRY Level ---
cat("Aggregating to STATE-INDUSTRY level...\n")

asuse_state_industry <- asuse_firm_level %>%
    group_by(State_Code, NIC_2digit) %>%
    summarise(
        w_inf_mean = weighted.mean(w_informal_firm, w = Multiplier, na.rm = TRUE),
        N_informal = n(),
        .groups = "drop"
    )

cat(sprintf("  Created %d state-industry cells\n", nrow(asuse_state_industry)))
cat("\nASUSE State-Industry Summary (Top 10):\n")
print(head(asuse_state_industry %>% arrange(State_Code, NIC_2digit), 10))
cat("\n")

# =============================================================================
# PHASE 4: MERGE AND CREATE FINAL DATASET
# =============================================================================

cat("=== PHASE 4: CREATING MASTER DATASET ===\n")

# --- Step 4.1: Load Enforcement Data ---
cat("Loading enforcement data...\n")

enforcement_path <- "C:/Users/ashwin/Documents/Formal_Informal/Data/External/State_Enforcement.csv"

if (!file.exists(enforcement_path)) {
    cat("  ERROR: File NOT found at:", enforcement_path, "\n")
    stop("Cannot proceed without enforcement data")
}

cat("  File found at:", enforcement_path, "\n")

# Read all columns; the E_s column is named 'E_s (Ratio)' in the CSV
enforcement_raw <- read_csv(enforcement_path,
    show_col_types = FALSE
)

cat("  CSV columns found:", paste(names(enforcement_raw), collapse = ", "), "\n")

# Identify the E_s column (handles both 'E_s' and 'E_s (Ratio)')
es_col <- names(enforcement_raw)[grep("E_s", names(enforcement_raw), ignore.case = TRUE)][1]
cat("  Using E_s column:", es_col, "\n")

enforcement_data <- enforcement_raw %>%
    rename(E_s = all_of(es_col)) %>%
    # CRITICAL FIX: Pad State_Code to 2-digit string to match ASI's a7 format
    mutate(
        State_Code = sprintf("%02d", as.integer(State_Code)),
        E_s = as.numeric(E_s)
    ) %>%
    # Keep only states with valid enforcement data
    filter(!is.na(E_s)) %>%
    select(State_Code, State_Name, E_s)

cat(sprintf("  Loaded enforcement for %d states (with valid E_s values)\n", nrow(enforcement_data)))
cat("  State codes in enforcement data:", paste(sort(enforcement_data$State_Code), collapse = ", "), "\n\n")
print(enforcement_data)


# --- Step 4.2: Merge ASI and ASUSE ---
cat("\nMerging ASI and ASUSE at state-industry level...\n")

master_df <- asi_state_industry %>%
    inner_join(asuse_state_industry, by = c("State_Code", "NIC_2digit")) %>%
    left_join(enforcement_data, by = "State_Code")

cat(sprintf("  Master dataset: %d state-industry observations\n", nrow(master_df)))
cat(sprintf("  Observations WITH enforcement data (E_s not NA): %d\n", sum(!is.na(master_df$E_s))))
cat(sprintf("  Observations WITHOUT enforcement data (E_s = NA): %d\n", sum(is.na(master_df$E_s))))

if (nrow(master_df) == 0) {
    stop(
        "ERROR: No matching state-industry cells between ASI and ASUSE!\n",
        "Check state code mapping between the two datasets."
    )
}

# States in master but missing enforcement
missing_enf <- master_df %>%
    filter(is.na(E_s)) %>%
    distinct(State_Code) %>%
    pull(State_Code)
if (length(missing_enf) > 0) {
    cat("  States missing enforcement (will be dropped from regressions):", paste(missing_enf, collapse = ", "), "\n")
}

# Filter to observations with valid enforcement for regression sample
master_df_reg <- master_df %>% filter(!is.na(E_s))
cat(sprintf("  Regression sample: %d state-industry observations\n\n", nrow(master_df_reg)))

# --- Step 4.3: Calculate Derived Variables (on regression sample) ---
cat("Calculating derived variables...\n")

# First create the log variables on the enforcement-filtered sample
master_df_reg <- master_df_reg %>%
    mutate(
        ln_w_inf = log(w_inf_mean),
        ln_A_formal = log(A_formal),
        ln_Q_out = log(Q_out_mean + 1),
        E_A_interaction = E_s * ln_A_formal
    )

# Manual winsorization function
winsorize_manual <- function(x, lower = 0.01, upper = 0.99) {
    q_lower <- quantile(x, lower, na.rm = TRUE)
    q_upper <- quantile(x, upper, na.rm = TRUE)
    x[x < q_lower] <- q_lower
    x[x > q_upper] <- q_upper
    return(x)
}

# Apply manual winsorization
cat("\nApplying manual winsorization (1% and 99%)...\n")
master_df_reg$ln_w_inf <- winsorize_manual(master_df_reg$ln_w_inf)
master_df_reg$ln_A_formal <- winsorize_manual(master_df_reg$ln_A_formal)
master_df_reg$ln_Q_out <- winsorize_manual(master_df_reg$ln_Q_out)

cat("Winsorization complete\n")

# --- Step 4.4: Summary Statistics ---
cat("\n=== SUMMARY STATISTICS (Regression Sample with Enforcement Data) ===\n")
cat(sprintf(
    "N = %d state-industry observations across %d states\n",
    nrow(master_df_reg), length(unique(master_df_reg$State_Code))
))
cat("\nKey variables:\n")
summary_vars <- master_df_reg %>%
    select(ln_w_inf, ln_A_formal, ln_Q_out, E_s, N_firms, N_informal)
print(summary(summary_vars))

cat("\n\nFull regression dataset:\n")
print(master_df_reg %>%
    select(State_Code, State_Name, NIC_2digit, ln_w_inf, ln_A_formal, ln_Q_out, E_s, N_firms, N_informal))

# =============================================================================
# PHASE 5: REDUCED-FORM REGRESSIONS
# =============================================================================

cat("\n\n=== PHASE 5: REDUCED-FORM REGRESSIONS ===\n\n")

# Model 1: Baseline Productivity Pass-Through
cat("Model 1: Baseline Productivity Pass-Through\n")
mod1 <- feols(ln_w_inf ~ E_s * ln_A_formal | NIC_2digit,
    data = master_df_reg, cluster = ~State_Code
)

# Model 2: Strategic Outsourcing
cat("Model 2: Strategic Outsourcing\n")
mod2 <- feols(ln_Q_out ~ E_s + I(E_s^2) + ln_A_formal | NIC_2digit,
    data = master_df_reg, cluster = ~State_Code
)

# Display results
cat("\n=== REGRESSION RESULTS ===\n\n")
etable(mod1, mod2,
    title = "State-Industry Analysis",
    headers = c("Informal Wages", "Outsourcing")
)

# Save results
cat("\nSaving results to Output/\n")
etable(mod1, mod2, tex = TRUE, file = "Output/state_industry_regressions.tex")
etable(mod1, mod2, file = "Output/state_industry_regressions.csv")

# =============================================================================
# PHASE 6: DIAGNOSTIC CHECKS
# =============================================================================

cat("\n\n=== DIAGNOSTIC CHECKS ===\n\n")

cat("Sample size (regression sample with enforcement):\n")
cat(sprintf("  Total observations: %d\n", nrow(master_df_reg)))
cat(sprintf("  Number of states: %d\n", length(unique(master_df_reg$State_Code))))
cat(sprintf("  Number of industries: %d\n", length(unique(master_df_reg$NIC_2digit))))

cat("\nVariation in key variables:\n")
cat(sprintf("  SD(ln_w_inf): %.3f\n", sd(master_df_reg$ln_w_inf, na.rm = TRUE)))
cat(sprintf("  SD(ln_A_formal): %.3f\n", sd(master_df_reg$ln_A_formal, na.rm = TRUE)))
cat(sprintf("  SD(E_s): %.3f (REAL variation - not a constant!)\n", sd(master_df_reg$E_s, na.rm = TRUE)))

cat("\nCorrelations:\n")
cor_matrix <- cor(master_df_reg %>% select(ln_w_inf, ln_A_formal, E_s, ln_Q_out),
    use = "complete.obs"
)
print(round(cor_matrix, 3))

cat("\n\n=== ANALYSIS COMPLETE ===\n")
cat("\nKey findings documented in Output/ directory\n")
cat(sprintf("Regression sample: %d state-industry observations with valid enforcement data\n", nrow(master_df_reg)))
cat("Full dataset (including states without enforcement):", nrow(master_df), "observations\n")
cat("Recommended: Document sample in methodology section\n")


# After running models, display coefficients
cat("\n=== DETAILED REGRESSION RESULTS ===\n\n")

cat("Model 1: Informal Wages ~ Formal Productivity\n")
cat("--------------------------------------------\n")
summary_mod1 <- summary(mod1)
print(summary_mod1)
cat("\nCoefficient for ln_A_formal:", coef(mod1)["ln_A_formal"], "\n")
cat("Std. Error:", summary_mod1$se["ln_A_formal"], "\n")
cat("P-value:", summary_mod1$coeftable["ln_A_formal", "Pr(>|t|)"], "\n\n")

cat("Model 2: Outsourcing ~ Productivity\n")
cat("-----------------------------------\n")
summary_mod2 <- summary(mod2)
print(summary_mod2)
cat("\nCoefficient for ln_A_formal:", coef(mod2)["ln_A_formal"], "\n")
