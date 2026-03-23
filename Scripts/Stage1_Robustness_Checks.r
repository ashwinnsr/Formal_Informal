################################################################################
# STAGE 2: ROBUSTNESS CHECKS & HETEROGENEITY ANALYSIS
# Dissertation: Formal-Informal Linkages in Textiles & Apparel
#
# Builds on Stage 1 state-industry dataset (47 obs, 26 states)
# Key Stage 1 result: Formal productivity → outsourcing (p=0.043)
#
# This script tests:
#   A. Robustness of the outsourcing result (4 checks)
#   B. Alternative enforcement measure (inspectors per factory)
#   C. Heterogeneity by industry and enforcement level
################################################################################

# =============================================================================
# PHASE 0: SETUP
# =============================================================================

.libPaths(c("R_libs", .libPaths()))

library(tidyverse)
library(haven)
library(fixest)
library(DescTools)

setwd("C:/Users/ashwin/Documents/Formal_Informal")

if (!dir.exists("Output")) dir.create("Output")

cat("================================================================\n")
cat("  STAGE 2: ROBUSTNESS & HETEROGENEITY ANALYSIS\n")
cat("================================================================\n\n")

# =============================================================================
# PHASE 1: REBUILD MASTER DATASET (Identical to Stage 1 pipeline)
# =============================================================================

cat("=== PHASE 1: REBUILDING MASTER DATASET (Stage 1 pipeline) ===\n\n")

# --- 1.1: Load ASI Blocks ---
cat("Loading ASI blocks...\n")
asi_blk_a <- read_sav("ASI_Data/ASI_202324sav/blkA202324.sav")
asi_blk_e <- read_sav("ASI_Data/ASI_202324sav/blkE202324.sav")
asi_blk_f <- read_sav("ASI_Data/ASI_202324sav/blkF202324.sav")
asi_blk_j <- read_sav("ASI_Data/ASI_202324sav/blkJ202324.sav")

# --- 1.2: Clean Block A (Identification) ---
asi_id <- asi_blk_a %>%
    mutate(
        Factory_ID = as.character(a1),
        State_Code = as.character(a7),
        NIC_Code = as.character(a5),
        NIC_2digit = substr(NIC_Code, 1, 2),
        Multiplier = mult
    ) %>%
    filter(NIC_2digit %in% c("13", "14")) %>%
    dplyr::select(Factory_ID, State_Code, NIC_2digit, Multiplier)

cat(sprintf(
    "  %d textile/apparel factories across %d states\n",
    nrow(asi_id), length(unique(asi_id$State_Code))
))

# --- 1.3: Clean Block E (Employment) ---
asi_emp <- asi_blk_e %>%
    mutate(Factory_ID = as.character(AE01)) %>%
    group_by(Factory_ID) %>%
    summarise(Total_Mandays = sum(EI7, na.rm = TRUE), .groups = "drop") %>%
    mutate(L_formal_firm = Total_Mandays / 300)

# --- 1.4: Clean Block F (Outsourcing) ---
asi_outsource <- asi_blk_f %>%
    mutate(
        Factory_ID = as.character(AF01),
        Outsourcing_Cost = ifelse(is.na(F7), 0, F7)
    ) %>%
    dplyr::select(Factory_ID, Outsourcing_Cost)

# --- 1.5: Clean Block J (Output) ---
asi_prod <- asi_blk_j %>%
    mutate(Factory_ID = as.character(AJ01)) %>%
    group_by(Factory_ID) %>%
    summarise(Total_Output = sum(J113, na.rm = TRUE), .groups = "drop")

# --- 1.6: Merge ASI at firm level ---
asi_firm_level <- asi_id %>%
    left_join(asi_emp, by = "Factory_ID") %>%
    left_join(asi_outsource, by = "Factory_ID") %>%
    left_join(asi_prod, by = "Factory_ID") %>%
    replace_na(list(Outsourcing_Cost = 0, Total_Output = 0, L_formal_firm = 0)) %>%
    filter(Total_Output > 0, L_formal_firm > 0) %>%
    mutate(
        Productivity_A = Total_Output / L_formal_firm,
        Outsourcing_Share = Outsourcing_Cost / Total_Output
    )

# --- 1.7: WEIGHTED aggregation to state-industry ---
asi_state_industry <- asi_firm_level %>%
    group_by(State_Code, NIC_2digit) %>%
    summarise(
        A_formal = weighted.mean(Productivity_A, w = Multiplier, na.rm = TRUE),
        Q_out_mean = weighted.mean(Outsourcing_Share, w = Multiplier, na.rm = TRUE),
        L_formal_total = sum(L_formal_firm * Multiplier, na.rm = TRUE),
        N_firms = n(),
        .groups = "drop"
    )

# --- 1.7b: UNWEIGHTED aggregation (for robustness check A2) ---
asi_state_industry_uw <- asi_firm_level %>%
    group_by(State_Code, NIC_2digit) %>%
    summarise(
        A_formal = mean(Productivity_A, na.rm = TRUE),
        Q_out_mean = mean(Outsourcing_Share, na.rm = TRUE),
        L_formal_total = sum(L_formal_firm, na.rm = TRUE),
        N_firms = n(),
        .groups = "drop"
    )

# --- 1.8: Load ASUSE ---
cat("Loading ASUSE blocks...\n")
asuse_blk_2 <- read_sav("ASUSE_Data/ASUSE202324sav/LEVEL - 02(Block 2).sav")
asuse_blk_4 <- read_sav("ASUSE_Data/ASUSE202324sav/LEVEL - 05 (Block 4).sav")
asuse_blk_5 <- read_sav("ASUSE_Data/ASUSE202324sav/LEVEL - 06 (Block 5 - section 5.1 to 5.14).sav")

asuse_id <- asuse_blk_2 %>%
    mutate(
        Enterprise_ID = paste(fsu_serial_no, sample_est_no, sep = "_"),
        NIC_2digit = substr(as.character(major_nic_5dig), 1, 2),
        State_Code = as.character(district)
    ) %>%
    filter(
        NIC_2digit %in% c("13", "14"),
        as.character(contract_manuf_service) == "1"
    ) %>%
    dplyr::select(Enterprise_ID, State_Code, NIC_2digit, Multiplier = mlt)

asuse_workers <- asuse_blk_4 %>%
    mutate(Enterprise_ID = paste(fsu_serial_no, sample_est_no, sep = "_")) %>%
    filter(as.character(item_no) == "511") %>%
    dplyr::select(Enterprise_ID, Hired_Workers = value_rs)

asuse_wages <- asuse_blk_5 %>%
    mutate(Enterprise_ID = paste(fsu_serial_no, sample_est_no, sep = "_")) %>%
    filter(as.character(item_no) == "559") %>%
    dplyr::select(Enterprise_ID, Total_Wages = value_rs)

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

# --- WEIGHTED ASUSE aggregation ---
asuse_state_industry <- asuse_firm_level %>%
    group_by(State_Code, NIC_2digit) %>%
    summarise(
        w_inf_mean = weighted.mean(w_informal_firm, w = Multiplier, na.rm = TRUE),
        N_informal = n(),
        .groups = "drop"
    )

# --- UNWEIGHTED ASUSE aggregation (for robustness A2) ---
asuse_state_industry_uw <- asuse_firm_level %>%
    group_by(State_Code, NIC_2digit) %>%
    summarise(
        w_inf_mean = mean(w_informal_firm, na.rm = TRUE),
        N_informal = n(),
        .groups = "drop"
    )

# --- 1.9: Load enforcement data ---
cat("Loading enforcement data...\n")
enforcement_raw <- read_csv(
    "Data/External/State_Enforcement_2023.csv",
    show_col_types = FALSE
)

es_col <- names(enforcement_raw)[grep("E_s", names(enforcement_raw), ignore.case = TRUE)][1]

enforcement_data <- enforcement_raw %>%
    rename(E_s = all_of(es_col)) %>%
    mutate(
        State_Code = sprintf("%02d", as.integer(State_Code)),
        E_s = as.numeric(E_s),
        # Alternative enforcement: inspectors per factory
        Inspectors = as.numeric(Inspectors),
        Working_Factories = as.numeric(Working_Factories),
        E_s_alt = ifelse(!is.na(Inspectors) & !is.na(Working_Factories) & Working_Factories > 0,
            Inspectors / Working_Factories, NA
        )
    ) %>%
    filter(!is.na(E_s)) %>%
    dplyr::select(State_Code, State_Name, E_s, E_s_alt)

cat(sprintf("  Loaded enforcement for %d states\n", nrow(enforcement_data)))
cat(sprintf(
    "  States with alternative E_s (inspectors/factory): %d\n",
    sum(!is.na(enforcement_data$E_s_alt))
))

# --- 1.10: Build WEIGHTED master dataset ---
master_df <- asi_state_industry %>%
    inner_join(asuse_state_industry, by = c("State_Code", "NIC_2digit")) %>%
    left_join(enforcement_data, by = "State_Code")

master_df_reg <- master_df %>%
    filter(!is.na(E_s)) %>%
    mutate(
        ln_w_inf = log(w_inf_mean),
        ln_A_formal = log(A_formal),
        ln_Q_out = log(Q_out_mean + 1),
        E_A_interaction = E_s * ln_A_formal
    )

# --- 1.11: Build UNWEIGHTED master dataset ---
master_df_uw <- asi_state_industry_uw %>%
    inner_join(asuse_state_industry_uw, by = c("State_Code", "NIC_2digit")) %>%
    left_join(enforcement_data, by = "State_Code")

master_df_uw_reg <- master_df_uw %>%
    filter(!is.na(E_s)) %>%
    mutate(
        ln_w_inf = log(w_inf_mean),
        ln_A_formal = log(A_formal),
        ln_Q_out = log(Q_out_mean + 1),
        E_A_interaction = E_s * ln_A_formal
    )

# --- Winsorization helper ---
winsorize_manual <- function(x, lower = 0.01, upper = 0.99) {
    q_lower <- quantile(x, lower, na.rm = TRUE)
    q_upper <- quantile(x, upper, na.rm = TRUE)
    x[x < q_lower] <- q_lower
    x[x > q_upper] <- q_upper
    return(x)
}

# Apply winsorization to weighted master
master_df_reg$ln_w_inf <- winsorize_manual(master_df_reg$ln_w_inf)
master_df_reg$ln_A_formal <- winsorize_manual(master_df_reg$ln_A_formal)
master_df_reg$ln_Q_out <- winsorize_manual(master_df_reg$ln_Q_out)

# Apply winsorization to unweighted master
master_df_uw_reg$ln_w_inf <- winsorize_manual(master_df_uw_reg$ln_w_inf)
master_df_uw_reg$ln_A_formal <- winsorize_manual(master_df_uw_reg$ln_A_formal)
master_df_uw_reg$ln_Q_out <- winsorize_manual(master_df_uw_reg$ln_Q_out)

cat(sprintf(
    "\n  Weighted master: %d obs across %d states\n",
    nrow(master_df_reg), length(unique(master_df_reg$State_Code))
))
cat(sprintf(
    "  Unweighted master: %d obs across %d states\n",
    nrow(master_df_uw_reg), length(unique(master_df_uw_reg$State_Code))
))


# =============================================================================
# PHASE A: ROBUSTNESS CHECKS
# =============================================================================

cat("\n================================================================\n")
cat("  PHASE A: ROBUSTNESS CHECKS\n")
cat("================================================================\n\n")

# ---- Baseline (Stage 1 replication) ----
cat("--- Baseline (Stage 1 replication) ---\n")
mod1_base <- feols(ln_w_inf ~ E_s * ln_A_formal | NIC_2digit,
    data = master_df_reg, cluster = ~State_Code
)

mod2_base <- feols(ln_Q_out ~ E_s + I(E_s^2) + ln_A_formal | NIC_2digit,
    data = master_df_reg, cluster = ~State_Code
)

cat(sprintf("  Baseline N = %d\n", nobs(mod1_base)))

# ---- A1: Exclude Manipur (E_s = 1.0 outlier) ----
cat("\n--- A1: Exclude Manipur (E_s = 1.0) ---\n")

# Identify Manipur state code
manipur_code <- enforcement_data %>%
    filter(grepl("Manipur", State_Name, ignore.case = TRUE)) %>%
    pull(State_Code)

cat(sprintf("  Manipur State_Code: %s\n", paste(manipur_code, collapse = ", ")))

df_no_manipur <- master_df_reg %>% filter(!(State_Code %in% manipur_code))

mod1_a1 <- feols(ln_w_inf ~ E_s * ln_A_formal | NIC_2digit,
    data = df_no_manipur, cluster = ~State_Code
)

mod2_a1 <- feols(ln_Q_out ~ E_s + I(E_s^2) + ln_A_formal | NIC_2digit,
    data = df_no_manipur, cluster = ~State_Code
)

cat(sprintf("  N after excluding Manipur = %d\n", nobs(mod1_a1)))

# ---- A2: Unweighted regression ----
cat("\n--- A2: Unweighted regression ---\n")

mod1_a2 <- feols(ln_w_inf ~ E_s * ln_A_formal | NIC_2digit,
    data = master_df_uw_reg, cluster = ~State_Code
)

mod2_a2 <- feols(ln_Q_out ~ E_s + I(E_s^2) + ln_A_formal | NIC_2digit,
    data = master_df_uw_reg, cluster = ~State_Code
)

cat(sprintf("  Unweighted N = %d\n", nobs(mod1_a2)))

# ---- A3: Winsorize E_s ----
cat("\n--- A3: Winsorize E_s (5th-95th percentile) ---\n")

df_winsorized_es <- master_df_reg %>%
    mutate(
        E_s_orig = E_s,
        E_s = winsorize_manual(E_s, lower = 0.05, upper = 0.95),
        E_A_interaction = E_s * ln_A_formal
    )

cat(sprintf(
    "  E_s range before: [%.3f, %.3f]\n",
    min(master_df_reg$E_s), max(master_df_reg$E_s)
))
cat(sprintf(
    "  E_s range after:  [%.3f, %.3f]\n",
    min(df_winsorized_es$E_s), max(df_winsorized_es$E_s)
))

mod1_a3 <- feols(ln_w_inf ~ E_s * ln_A_formal | NIC_2digit,
    data = df_winsorized_es, cluster = ~State_Code
)

mod2_a3 <- feols(ln_Q_out ~ E_s + I(E_s^2) + ln_A_formal | NIC_2digit,
    data = df_winsorized_es, cluster = ~State_Code
)

# ---- A4: Quartile dummies for E_s ----
cat("\n--- A4: E_s quartile dummies ---\n")

df_quartiles <- master_df_reg %>%
    mutate(
        E_s_Q = ntile(E_s, 4),
        E_s_Q = factor(E_s_Q, levels = 1:4, labels = c("Q1_low", "Q2", "Q3", "Q4_high"))
    )

cat("  E_s quartile distribution:\n")
print(table(df_quartiles$E_s_Q))

cat("  E_s ranges per quartile:\n")
print(df_quartiles %>%
    group_by(E_s_Q) %>%
    summarise(min_Es = min(E_s), max_Es = max(E_s), n = n(), .groups = "drop"))

mod1_a4 <- feols(ln_w_inf ~ E_s_Q + ln_A_formal | NIC_2digit,
    data = df_quartiles, cluster = ~State_Code
)

mod2_a4 <- feols(ln_Q_out ~ E_s_Q + ln_A_formal | NIC_2digit,
    data = df_quartiles, cluster = ~State_Code
)


# =============================================================================
# PHASE A RESULTS: Combined Robustness Table
# =============================================================================

cat("\n================================================================\n")
cat("  ROBUSTNESS RESULTS: Model 2 (Outsourcing) — All Specifications\n")
cat("================================================================\n\n")

etable(mod2_base, mod2_a1, mod2_a2, mod2_a3, mod2_a4,
    title = "Robustness Checks: Strategic Outsourcing (ln_Q_out)",
    headers = c("Baseline", "No Manipur", "Unweighted", "Winsorized E_s", "Quartile E_s")
)

cat("\n\n--- Robustness Results: Model 1 (Wage Pass-Through) ---\n\n")

etable(mod1_base, mod1_a1, mod1_a2, mod1_a3, mod1_a4,
    title = "Robustness Checks: Wage Pass-Through (ln_w_inf)",
    headers = c("Baseline", "No Manipur", "Unweighted", "Winsorized E_s", "Quartile E_s")
)

# Save robustness tables
etable(mod2_base, mod2_a1, mod2_a2, mod2_a3, mod2_a4,
    title = "Robustness: Outsourcing",
    headers = c("Baseline", "No Manipur", "Unweighted", "Winsor E_s", "Quartile E_s"),
    tex = TRUE, file = "Output/tex/stage2_robustness_outsourcing.tex"
)

etable(mod1_base, mod1_a1, mod1_a2, mod1_a3, mod1_a4,
    title = "Robustness: Wages",
    headers = c("Baseline", "No Manipur", "Unweighted", "Winsor E_s", "Quartile E_s"),
    tex = TRUE, file = "Output/tex/stage2_robustness_wages.tex"
)

# CSV versions
etable(mod2_base, mod2_a1, mod2_a2, mod2_a3, mod2_a4,
    headers = c("Baseline", "No Manipur", "Unweighted", "Winsor E_s", "Quartile E_s"),
    file = "Output/csv/stage2_robustness_outsourcing.csv"
)

etable(mod1_base, mod1_a1, mod1_a2, mod1_a3, mod1_a4,
    headers = c("Baseline", "No Manipur", "Unweighted", "Winsor E_s", "Quartile E_s"),
    file = "Output/csv/stage2_robustness_wages.csv"
)

cat("\n  Saved: Output/tex/stage2_robustness_outsourcing.tex/.csv\n")
cat("  Saved: Output/tex/stage2_robustness_wages.tex/.csv\n")


# =============================================================================
# PHASE B: ALTERNATIVE ENFORCEMENT MEASURE (Inspectors per Factory)
# =============================================================================

cat("\n================================================================\n")
cat("  PHASE B: ALTERNATIVE ENFORCEMENT MEASURE\n")
cat("================================================================\n\n")

# Filter to observations with valid alternative E_s
df_alt_es <- master_df_reg %>%
    filter(!is.na(E_s_alt)) %>%
    mutate(
        ln_E_s_alt = log(E_s_alt + 0.001), # add small constant for log(0) cases
        E_alt_A_interaction = E_s_alt * ln_A_formal
    )

cat(sprintf("  Observations with E_s_alt (inspectors/factory): %d\n", nrow(df_alt_es)))
cat(sprintf("  E_s_alt range: [%.5f, %.5f]\n", min(df_alt_es$E_s_alt), max(df_alt_es$E_s_alt)))
cat(sprintf("  E_s_alt mean: %.5f, SD: %.5f\n", mean(df_alt_es$E_s_alt), sd(df_alt_es$E_s_alt)))

# Model 1 with alternative enforcement
mod1_alt <- feols(ln_w_inf ~ E_s_alt * ln_A_formal | NIC_2digit,
    data = df_alt_es, cluster = ~State_Code
)

# Model 2 with alternative enforcement
mod2_alt <- feols(ln_Q_out ~ E_s_alt + I(E_s_alt^2) + ln_A_formal | NIC_2digit,
    data = df_alt_es, cluster = ~State_Code
)

cat("\n--- Alternative Enforcement Results ---\n\n")
etable(mod1_alt, mod2_alt,
    title = "Alternative Enforcement: Inspectors per Factory",
    headers = c("Wages (E_s_alt)", "Outsourcing (E_s_alt)")
)

# Compare baseline vs alternative side by side
cat("\n--- Baseline E_s vs Alternative E_s_alt (Model 2 comparison) ---\n\n")

# Re-estimate baseline on the same restricted sample for fair comparison
mod2_base_restricted <- feols(ln_Q_out ~ E_s + I(E_s^2) + ln_A_formal | NIC_2digit,
    data = df_alt_es, cluster = ~State_Code
)

etable(mod2_base_restricted, mod2_alt,
    title = "Outsourcing: Inspections/Factory vs Inspectors/Factory",
    headers = c("E_s (Inspections)", "E_s_alt (Inspectors)")
)

# Save alternative enforcement tables
etable(mod1_alt, mod2_alt,
    title = "Alternative Enforcement",
    headers = c("Wages", "Outsourcing"),
    tex = TRUE, file = "Output/tex/stage2_alt_enforcement.tex"
)

etable(mod1_alt, mod2_alt,
    headers = c("Wages", "Outsourcing"),
    file = "Output/csv/stage2_alt_enforcement.csv"
)

cat("\n  Saved: Output/tex/stage2_alt_enforcement.tex/.csv\n")


# =============================================================================
# PHASE C: HETEROGENEITY ANALYSIS
# =============================================================================

cat("\n================================================================\n")
cat("  PHASE C: HETEROGENEITY ANALYSIS\n")
cat("================================================================\n\n")

# ---- C1: By Industry (NIC-13 Textiles vs NIC-14 Apparel) ----
cat("--- C1: Separate regressions by industry ---\n\n")

df_nic13 <- master_df_reg %>% filter(NIC_2digit == "13")
df_nic14 <- master_df_reg %>% filter(NIC_2digit == "14")

cat(sprintf("  NIC-13 (Textiles):  N = %d states\n", nrow(df_nic13)))
cat(sprintf("  NIC-14 (Apparel):   N = %d states\n", nrow(df_nic14)))

# Model 1 by industry (no NIC FE since single industry)
mod1_nic13 <- feols(ln_w_inf ~ E_s * ln_A_formal,
    data = df_nic13, cluster = ~State_Code
)

mod1_nic14 <- feols(ln_w_inf ~ E_s * ln_A_formal,
    data = df_nic14, cluster = ~State_Code
)

# Model 2 by industry
mod2_nic13 <- feols(ln_Q_out ~ E_s + I(E_s^2) + ln_A_formal,
    data = df_nic13, cluster = ~State_Code
)

mod2_nic14 <- feols(ln_Q_out ~ E_s + I(E_s^2) + ln_A_formal,
    data = df_nic14, cluster = ~State_Code
)

cat("\n  Model 1 (Wages) by Industry:\n\n")
etable(mod1_nic13, mod1_nic14,
    title = "Wages: Textiles vs Apparel",
    headers = c("NIC-13 (Textiles)", "NIC-14 (Apparel)")
)

cat("\n  Model 2 (Outsourcing) by Industry:\n\n")
etable(mod2_nic13, mod2_nic14,
    title = "Outsourcing: Textiles vs Apparel",
    headers = c("NIC-13 (Textiles)", "NIC-14 (Apparel)")
)


# ---- C2: By Enforcement Level (Median Split) ----
cat("\n--- C2: High vs Low enforcement states ---\n\n")

# Calculate state-level median (collapse to state level first to avoid
# double-counting states present in both NIC-13 and NIC-14)
state_enforcement <- master_df_reg %>%
    distinct(State_Code, E_s)

median_es <- median(state_enforcement$E_s, na.rm = TRUE)
cat(sprintf("  Median E_s (state level): %.4f\n", median_es))

df_high_es <- master_df_reg %>% filter(E_s >= median_es)
df_low_es <- master_df_reg %>% filter(E_s < median_es)

cat(sprintf(
    "  High enforcement (E_s >= %.4f): N = %d obs, %d states\n",
    median_es, nrow(df_high_es), length(unique(df_high_es$State_Code))
))
cat(sprintf(
    "  Low enforcement  (E_s <  %.4f): N = %d obs, %d states\n",
    median_es, nrow(df_low_es), length(unique(df_low_es$State_Code))
))

# Model 2 by enforcement split (key model since it's the significant one)
mod2_high <- feols(ln_Q_out ~ ln_A_formal | NIC_2digit,
    data = df_high_es, cluster = ~State_Code
)

mod2_low <- feols(ln_Q_out ~ ln_A_formal | NIC_2digit,
    data = df_low_es, cluster = ~State_Code
)

# Model 1 by enforcement split
mod1_high <- feols(ln_w_inf ~ ln_A_formal | NIC_2digit,
    data = df_high_es, cluster = ~State_Code
)

mod1_low <- feols(ln_w_inf ~ ln_A_formal | NIC_2digit,
    data = df_low_es, cluster = ~State_Code
)

cat("\n  Model 2 (Outsourcing) by Enforcement Level:\n\n")
etable(mod2_high, mod2_low,
    title = "Outsourcing: High vs Low Enforcement",
    headers = c("High E_s", "Low E_s")
)

cat("\n  Model 1 (Wages) by Enforcement Level:\n\n")
etable(mod1_high, mod1_low,
    title = "Wages: High vs Low Enforcement",
    headers = c("High E_s", "Low E_s")
)


# Save heterogeneity tables
etable(mod2_nic13, mod2_nic14, mod2_high, mod2_low,
    title = "Heterogeneity: Outsourcing",
    headers = c("Textiles", "Apparel", "High E_s", "Low E_s"),
    tex = TRUE, file = "Output/tex/stage2_heterogeneity_outsourcing.tex"
)

etable(mod1_nic13, mod1_nic14, mod1_high, mod1_low,
    title = "Heterogeneity: Wages",
    headers = c("Textiles", "Apparel", "High E_s", "Low E_s"),
    tex = TRUE, file = "Output/tex/stage2_heterogeneity_wages.tex"
)

etable(mod2_nic13, mod2_nic14, mod2_high, mod2_low,
    headers = c("Textiles", "Apparel", "High E_s", "Low E_s"),
    file = "Output/csv/stage2_heterogeneity_outsourcing.csv"
)

etable(mod1_nic13, mod1_nic14, mod1_high, mod1_low,
    headers = c("Textiles", "Apparel", "High E_s", "Low E_s"),
    file = "Output/csv/stage2_heterogeneity_wages.csv"
)

cat("\n  Saved: Output/tex/stage2_heterogeneity_outsourcing.tex/.csv\n")
cat("  Saved: Output/tex/stage2_heterogeneity_wages.tex/.csv\n")


# =============================================================================
# PHASE D: CONSOLIDATED SUMMARY
# =============================================================================

cat("\n================================================================\n")
cat("  PHASE D: CONSOLIDATED SUMMARY\n")
cat("================================================================\n\n")

cat("KEY QUESTION: Does the Stage 1 outsourcing result (p=0.043) survive?\n\n")

cat(sprintf(
    "%-25s | %8s | %12s | %8s | %s\n",
    "Specification", "N", "Coef(ln_A_f)", "P-value", "Significant?"
))
cat(paste(rep("-", 80), collapse = ""), "\n")

# Helper to extract coefficient info
print_spec <- function(label, mod, var_name = "ln_A_formal") {
    ct <- summary(mod)$coeftable
    if (var_name %in% rownames(ct)) {
        coef_val <- ct[var_name, "Estimate"]
        p_val <- ct[var_name, "Pr(>|t|)"]
        sig <- ifelse(p_val < 0.01, "***",
            ifelse(p_val < 0.05, "**",
                ifelse(p_val < 0.10, "*", "No")
            )
        )
        cat(sprintf(
            "%-25s | %8d | %12.3e | %8.4f | %s\n",
            label, nobs(mod), coef_val, p_val, sig
        ))
    } else {
        cat(sprintf(
            "%-25s | %8d | %12s | %8s | %s\n",
            label, nobs(mod), "N/A", "N/A", "N/A"
        ))
    }
}

cat("\n  Model 2: ln_Q_out (Outsourcing) — Coef on ln_A_formal:\n\n")
print_spec("Baseline (Stage 1)", mod2_base)
print_spec("A1: No Manipur", mod2_a1)
print_spec("A2: Unweighted", mod2_a2)
print_spec("A3: Winsorized E_s", mod2_a3)
print_spec("A4: Quartile E_s", mod2_a4)
print_spec("B: Alt E_s (Inspectors)", mod2_alt)
print_spec("C1a: Textiles only", mod2_nic13)
print_spec("C1b: Apparel only", mod2_nic14)
print_spec("C2a: High enforcement", mod2_high)
print_spec("C2b: Low enforcement", mod2_low)

cat("\n\n  Model 1: ln_w_inf (Wages) — Coef on ln_A_formal:\n\n")
print_spec("Baseline (Stage 1)", mod1_base)
print_spec("A1: No Manipur", mod1_a1)
print_spec("A2: Unweighted", mod1_a2)
print_spec("A3: Winsorized E_s", mod1_a3)
print_spec("A4: Quartile E_s", mod1_a4)
print_spec("B: Alt E_s (Inspectors)", mod1_alt)
print_spec("C1a: Textiles only", mod1_nic13)
print_spec("C1b: Apparel only", mod1_nic14)
print_spec("C2a: High enforcement", mod1_high)
print_spec("C2b: Low enforcement", mod1_low)


cat("\n\n================================================================\n")
cat("  STAGE 2 ANALYSIS COMPLETE\n")
cat("================================================================\n")
cat("\nOutput files created in Output/:\n")
cat("  - stage2_robustness_outsourcing.tex/.csv\n")
cat("  - stage2_robustness_wages.tex/.csv\n")
cat("  - stage2_alt_enforcement.tex/.csv\n")
cat("  - stage2_heterogeneity_outsourcing.tex/.csv\n")
cat("  - stage2_heterogeneity_wages.tex/.csv\n")
cat("\nAll regressions use state-clustered standard errors.\n")
cat("================================================================\n")
