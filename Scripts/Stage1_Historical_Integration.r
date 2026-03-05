################################################################################
# STAGE 1b: HISTORICAL INTEGRATION — NSS 67th (2010-11) & NSS 73rd (2015-16)
#
# Mirrors Stage1_MultiYear_Analysis.r but for historical survey rounds.
# Reads ASI + ASUSE blocks for 2010-11 and 2015-16, creates state × NIC-2digit
# panels, deflates wages to real 2010-11 prices, and combines with the modern
# ASUSE (2021-24) master for long-run surplus distribution analysis.
#
# Data sources:
#   ASI  2010-11 : Data/External/ASI_201011sav/  (blka, blke, blkf, blkJ)
#   ASUSE 2010-11: Data/External/ASUSE201011sav/ (B1, B2, B8, B12 summary)
#   ASI  2015-16 : Data/External/ASI_201516sav/  (Block-A/E/F/J)
#   ASUSE 2015-16: Data/External/ASUSE201516sav/ (B1, B2, B8, B15 summary)
#
# Outputs:
#   Output/csv/historical_panel_df.csv   — 2010-11 & 2015-16 state-industry cells
#   Output/csv/combined_longrun_df.csv   — historical + ASUSE 2021-24 combined
################################################################################

.libPaths(c("R_libs", .libPaths()))
suppressPackageStartupMessages({
    library(tidyverse)
    library(haven)
})

setwd("C:/Users/ashwin/Documents/Formal_Informal")
if (!dir.exists("Output")) dir.create("Output")

cat("================================================================\n")
cat("  STAGE 1b: HISTORICAL INTEGRATION — NSS 67th & 73rd\n")
cat("================================================================\n\n")

# =============================================================================
# HELPERS
# =============================================================================

clean_df <- function(df) {
    df <- zap_labels(df)
    df <- zap_formats(df)
    names(df) <- tolower(names(df))
    as_tibble(df)
}

winsorize_manual <- function(x, lower = 0.01, upper = 0.99) {
    q_l <- quantile(x, lower, na.rm = TRUE)
    q_u <- quantile(x, upper, na.rm = TRUE)
    x[x < q_l] <- q_l
    x[x > q_u] <- q_u
    x
}

TEXTILE_NIC <- c("13", "14")

# =============================================================================
# SECTION 1: CPI DEFLATORS
# =============================================================================
# CPI-IW (Base 2001=100). Deflate all wages to 2010-11 real prices (index=180).
# New series (Base 2012=100) used in ASUSE: linked via factor of 1.95.

cpi_file <- "Data/External/CONSUMER_PRICE_INDEX - Sheet1.csv"
if (!file.exists(cpi_file)) stop("CPI file not found.")

cpi_raw <- read_csv(cpi_file, show_col_types = FALSE) %>%
    mutate(Year = trimws(Year))

BASE_CPI <- cpi_raw %>%
    filter(Year == "2010-11") %>%
    pull(CPI_IW) # = 180
LINK_FACTOR <- 1.95

get_defl <- function(yr, series = "old") {
    row <- cpi_raw %>% filter(Year == yr)
    if (nrow(row) == 0) {
        return(1.0)
    }
    idx <- if (series == "old") row$CPI_IW else row$CPI_IW * LINK_FACTOR
    BASE_CPI / idx
}

defl <- list(
    "2010-11"  = 1.0,
    "2015-16"  = get_defl("2015-16", "old"),
    "2021-22"  = get_defl("2021-22", "new"),
    "2022-23"  = get_defl("2022-23", "new"),
    "2023-24"  = get_defl("2023-24", "new")
)

# =============================================================================
# SECTION 2: ENFORCEMENT DATA
# =============================================================================

load_enforcement <- function(path) {
    if (!file.exists(path)) {
        return(NULL)
    }
    read_csv(path, show_col_types = FALSE) %>%
        rename_with(~ gsub("[[:space:]]", "", .x)) %>%
        rename(E_s = `E_s(Ratio)`) %>%
        mutate(
            State_Code = sprintf("%02d", as.integer(State_Code)),
            E_s = suppressWarnings(as.numeric(E_s))
        ) %>%
        filter(!is.na(E_s)) %>%
        select(any_of(c("State_Code", "State_Name", "E_s")))
}

enf <- list(
    "2010-11" = load_enforcement("Data/External/State-Enforcement_2010.csv"),
    "2015-16" = load_enforcement("Data/External/State_Enforcement_2015.csv")
)

# =============================================================================
# SECTION 3: ASI LOADERS
# =============================================================================

load_asi_2010 <- function() {
    cat("  Loading ASI 2010-11...\n")
    DIR <- "Data/External/ASI_201011sav/"

    a <- clean_df(read_sav(paste0(DIR, "blka201011.sav"))) %>%
        mutate(Factory_ID = as.character(dsl), State_Code = sprintf("%02d", as.integer(statecode)), Multiplier = as.numeric(multilplier), NIC_2digit = substr(as.character(nic5digit), 1, 2)) %>%
        filter(NIC_2digit %in% TEXTILE_NIC)

    e <- clean_df(read_sav(paste0(DIR, "blke201011.sav"))) %>%
        filter(as.numeric(sno) == 1) %>%
        mutate(Factory_ID = as.character(dsl), L_formal_firm = as.numeric(tmanday) / 300, w_formal_firm = ifelse(L_formal_firm > 0, as.numeric(wages) / L_formal_firm, NA))

    f <- clean_df(read_sav(paste0(DIR, "blkf201011.sav"))) %>%
        mutate(Factory_ID = as.character(dsl), Outsourcing_Cost = replace_na(as.numeric(workdoneby), 0))

    j <- clean_df(read_sav(paste0(DIR, "blkJ201011.sav"))) %>%
        mutate(Factory_ID = as.character(dsl)) %>%
        group_by(Factory_ID) %>%
        summarise(Total_Output = sum(as.numeric(exfactvaloutput), na.rm = T), .groups = "drop")

    a %>%
        inner_join(e, by = "Factory_ID") %>%
        inner_join(f, by = "Factory_ID") %>%
        inner_join(j, by = "Factory_ID") %>%
        filter(Total_Output > 0, L_formal_firm > 0) %>%
        mutate(Productivity_A = Total_Output / L_formal_firm, Outsourcing_Share = Outsourcing_Cost / Total_Output) %>%
        group_by(State_Code, NIC_2digit) %>%
        summarise(A_formal = weighted.mean(Productivity_A, w = Multiplier, na.rm = T), w_for_mean = weighted.mean(w_formal_firm, w = Multiplier, na.rm = T), Q_out_mean = weighted.mean(Outsourcing_Share, w = Multiplier, na.rm = T), L_formal_total = sum(L_formal_firm * Multiplier, na.rm = T), N_firms = n(), .groups = "drop") %>%
        mutate(Year = "2010-11")
}

load_asi_2015 <- function() {
    cat("  Loading ASI 2015-16...\n")
    DIR <- "Data/External/ASI_201516sav/"

    a <- clean_df(read_sav(paste0(DIR, "Block-A-201516.sav"))) %>%
        mutate(Factory_ID = as.character(dsl), State_Code = sprintf("%02d", as.integer(state_cd)), Multiplier = as.numeric(multiplier), NIC_2digit = substr(as.character(ind_cd), 1, 2)) %>%
        filter(NIC_2digit %in% TEXTILE_NIC)

    e <- clean_df(read_sav(paste0(DIR, "Block-E-201516.sav"))) %>%
        filter(as.numeric(s_no) %in% c(1, 3)) %>%
        group_by(dsl) %>%
        summarise(tm = sum(as.numeric(mandaysworkedtotal), na.rm = T), wg = sum(as.numeric(wagessalariesrs), na.rm = T), .groups = "drop") %>%
        mutate(Factory_ID = as.character(dsl), L_formal_firm = tm / 300, w_formal_firm = ifelse(L_formal_firm > 0, wg / L_formal_firm, NA))

    f <- clean_df(read_sav(paste0(DIR, "Block-F-201516.sav"))) %>%
        mutate(Factory_ID = as.character(dsl), Outsourcing_Cost = replace_na(as.numeric(workdoneby), 0))

    j <- clean_df(read_sav(paste0(DIR, "Block-J-201516.sav"))) %>%
        mutate(Factory_ID = as.character(dsl)) %>%
        group_by(Factory_ID) %>%
        summarise(Total_Output = sum(as.numeric(ex_factvalqtymanft), na.rm = T), .groups = "drop")

    a %>%
        inner_join(e, by = "Factory_ID") %>%
        inner_join(f, by = "Factory_ID") %>%
        inner_join(j, by = "Factory_ID") %>%
        filter(Total_Output > 0, L_formal_firm > 0) %>%
        mutate(Productivity_A = Total_Output / L_formal_firm, Outsourcing_Share = Outsourcing_Cost / Total_Output) %>%
        group_by(State_Code, NIC_2digit) %>%
        summarise(A_formal = weighted.mean(Productivity_A, w = Multiplier, na.rm = T), w_for_mean = weighted.mean(w_formal_firm, w = Multiplier, na.rm = T), Q_out_mean = weighted.mean(Outsourcing_Share, w = Multiplier, na.rm = T), L_formal_total = sum(L_formal_firm * Multiplier, na.rm = T), N_firms = n(), .groups = "drop") %>%
        mutate(Year = "2015-16")
}

# =============================================================================
# SECTION 4: ASUSE LOADERS (ROBUST VERSION)
# =============================================================================

load_asuse_2010 <- function() {
    cat("  Loading ASUSE 2010-11 (Robust)...\n")
    DIR <- "Data/External/ASUSE201011sav/"

    # Block 1 & 2: ID, NIC, State, Job-work, Weights
    b12 <- clean_df(read_sav(paste0(DIR, "Block-1-Identification of sample enterprise-Records.sav"))) %>%
        select(key_entpr, state, wgt_combined) %>%
        inner_join(clean_df(read_sav(paste0(DIR, "Block-2-particulars of operation and background information of enterprises-records.sav"))) %>%
            select(key_entpr, nic_code = b2_q202, job_work = b2_q239), by = "key_entpr") %>%
        filter(as.numeric(job_work) == 1) %>%
        mutate(NIC_2digit = substr(as.character(nic_code), 1, 2), State_Code = sprintf("%02d", as.integer(state))) %>%
        filter(NIC_2digit %in% TEXTILE_NIC)

    # Block 8: Hired Workers (Item 801)
    b8 <- clean_df(read_sav(paste0(DIR, "Block-8-Employment particulars-records.sav"))) %>%
        filter(as.numeric(b8_c2) == 801) %>%
        group_by(key_entpr) %>%
        summarise(Hired_Workers = sum(as.numeric(b8_c3) + as.numeric(b8_c4), na.rm = T), .groups = "drop")

    # Block 12: Summary GVA (1209) and Comp (1201)
    b12_sum <- clean_df(read_sav(paste0(DIR, "Block-12-factor incomes-records.sav"))) %>%
        filter(as.numeric(b12_c2) %in% c(1201, 1209)) %>%
        pivot_wider(id_cols = key_entpr, names_from = b12_c2, values_from = b12_c3, names_prefix = "item_") %>%
        rename(Total_Wages = item_1201, GVA = item_1209)

    b12 %>%
        left_join(b8, by = "key_entpr") %>%
        left_join(b12_sum, by = "key_entpr") %>%
        replace_na(list(Total_Wages = 0, Hired_Workers = 0)) %>%
        filter(Hired_Workers > 0, Total_Wages > 0) %>%
        mutate(w_informal_firm = Total_Wages / Hired_Workers) %>%
        group_by(State_Code, NIC_2digit) %>%
        summarise(w_inf_mean = weighted.mean(w_informal_firm, w = wgt_combined, na.rm = T), N_informal = n(), .groups = "drop") %>%
        mutate(Year = "2010-11")
}

load_asuse_2015 <- function() {
    cat("  Loading ASUSE 2015-16 (Robust)...\n")
    DIR <- "Data/External/ASUSE201516sav/"
    SEMI <- c("(Semi-Round-1)- ", "(Semi-Round-2)- ")

    load_s <- function(p) {
        b12 <- clean_df(read_sav(paste0(DIR, p, "Block 1.sav"))) %>%
            select(entid, state, mlt) %>%
            inner_join(clean_df(read_sav(paste0(DIR, p, "Block 2.sav"))) %>% select(entid, nic_code = b2_q202, job_work = b2_q239), by = "entid") %>%
            filter(as.numeric(job_work) == 1) %>%
            mutate(NIC_2digit = substr(as.character(nic_code), 1, 2), State_Code = sprintf("%02d", as.integer(state))) %>%
            filter(NIC_2digit %in% TEXTILE_NIC)

        b8 <- clean_df(read_sav(paste0(DIR, gsub("- $", "-", p), "Block 8.sav"))) %>%
            filter(as.numeric(b8_q2) == 801) %>%
            group_by(entid) %>%
            summarise(Hired_Workers = sum(as.numeric(b8_q3) + as.numeric(b8_q4), na.rm = T), .groups = "drop")

        b15 <- clean_df(read_sav(paste0(DIR, gsub("- $", "-", p), "Block 15.sav"))) %>%
            select(entid, Total_Wages = b15_q1502_3, GVA = b15_q1507_3)

        b12 %>%
            left_join(b8, by = "entid") %>%
            left_join(b15, by = "entid") %>%
            mutate(Hired_Workers = as.numeric(Hired_Workers), Total_Wages = as.numeric(Total_Wages)) %>%
            filter(Hired_Workers > 0, Total_Wages > 0) %>%
            mutate(w_informal_firm = Total_Wages / Hired_Workers)
    }

    firm <- bind_rows(lapply(SEMI, load_s))
    firm %>%
        group_by(State_Code, NIC_2digit) %>%
        summarise(w_inf_mean = weighted.mean(w_informal_firm, w = mlt, na.rm = T), N_informal = n(), .groups = "drop") %>%
        mutate(Year = "2015-16")
}

# =============================================================================
# SECTION 5: MERGE & CONSOLIDATE
# =============================================================================

cat("=== Processing Historical Rounds ===\n")
asi_10 <- load_asi_2010()
asuse_10 <- load_asuse_2010()
asi_15 <- load_asi_2015()
asuse_15 <- load_asuse_2015()

merge_y <- function(asi, asuse, label) {
    if (is.null(enf[[label]])) stop("Enforcement not found for ", label)
    asi %>%
        inner_join(asuse, by = c("State_Code", "NIC_2digit", "Year")) %>%
        left_join(enf[[label]], by = "State_Code") %>%
        filter(!is.na(E_s)) %>%
        mutate(w_inf_real = w_inf_mean * defl[[label]], ln_w_inf_real = log(w_inf_real), ln_A_formal = log(A_formal), ln_Q_out = log(Q_out_mean + 1))
}

hist_df <- bind_rows(merge_y(asi_10, asuse_10, "2010-11"), merge_y(asi_15, asuse_15, "2015-16"))
write_csv(hist_df, "Output/csv/historical_panel_df.csv")

# Long-run मास्टर
if (file.exists("Output/csv/multiyear_master_df.csv")) {
    modern <- read_csv("Output/csv/multiyear_master_df.csv", show_col_types = FALSE) %>%
        mutate(
            Year_str = trimws(as.character(Year)),
            defl_val = unlist(defl)[Year_str],
            w_inf_real = w_inf_mean * defl_val,
            ln_w_inf_real = log(w_inf_real)
        )

    common <- c("State_Code", "State_Name", "NIC_2digit", "Year", "E_s", "A_formal", "w_for_mean", "Q_out_mean", "w_inf_real", "N_informal", "N_firms", "L_formal_total")
    longrun <- bind_rows(
        hist_df %>% select(any_of(common)) %>% mutate(NIC_2digit = as.character(NIC_2digit)),
        modern %>% select(any_of(common)) %>% mutate(NIC_2digit = as.character(NIC_2digit))
    ) %>%
        mutate(Year = as.factor(Year), ln_w_inf = log(w_inf_real), ln_A_formal = log(A_formal), ln_Q_out = log(Q_out_mean + 1))

    write_csv(longrun, "Output/csv/combined_longrun_df.csv")
    cat(sprintf("\nDone! Full long-run panel saved to Output/csv/combined_longrun_df.csv (%d rows, %d years)\n", nrow(longrun), length(unique(longrun$Year))))
} else {
    cat(sprintf("\nDone! Historical panel saved: Output/csv/historical_panel_df.csv (%d rows)\n", nrow(hist_df)))
}
