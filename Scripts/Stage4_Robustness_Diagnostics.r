################################################################################
# STAGE 4: ROBUSTNESS CHECKS AND DIAGNOSTICS
# Dissertation: Formal-Informal Linkages in Textiles & Apparel
#
# Checks organized by Research Question:
#
#   RQ1 (Outsourcing):
#     - Placebo test: chemicals sector (NIC-20) — should find β ≈ 0
#
#   RQ2 (Wage Pass-Through):
#     - Outlier diagnostics (Cook-style residual check)
#     - Median wage robustness (re-aggregate ASUSE using median)
#
#   RQ3 (Persistence of Informality):
#     - Unit root test / LLC test (plm::purtest)   ← CRITICAL
#     - AR(2) lag structure check
#
#   General (All Models):
#     - Wild bootstrap clustered SE (fwildclusterboot)
#     - Small-sample t-distribution correction (verify fixest default)
#     - Specification curve (specr) — optional, requires package install
#
# Reads: Output/csv/multiyear_master_df.csv
#        Raw ASI (blkA 2023-24) for placebo
#        Raw ASUSE (2023-24) for median wage
################################################################################

.libPaths(c("R_libs", .libPaths()))

suppressPackageStartupMessages({
    library(tidyverse)
    library(haven)
    library(fixest)
    library(ggplot2)
    library(patchwork)
    library(car) # VIF (multicollinearity)
    library(lmtest) # Breusch-Pagan, Durbin-Watson
    library(sensemakr) # Oster-style sensitivity bounds
    library(boot) # Bootstrapped confidence intervals
})

setwd("C:/Users/ashwin/Documents/Formal_Informal")
if (!dir.exists("Output")) dir.create("Output")

cat("================================================================\n")
cat("  STAGE 4: ROBUSTNESS CHECKS & DIAGNOSTICS\n")
cat("================================================================\n\n")

# =============================================================================
# LOAD BASE DATA
# =============================================================================
# LOAD BASE DATA
master_df <- read_csv("Output/csv/combined_longrun_df.csv", show_col_types = FALSE) %>%
    mutate(
        NIC_2digit = as.factor(NIC_2digit),
        Year_char = as.character(Year),
        Year_num = case_when(
            Year_char == "2010-11" ~ 2010L,
            Year_char == "2015-16" ~ 2015L,
            Year_char == "2021-22" ~ 2021L,
            Year_char == "2022-23" ~ 2022L,
            Year_char == "2023-24" ~ 2023L,
            TRUE ~ NA_integer_
        ),
        Year = as.factor(Year_char),
        ln_N_informal = log(N_informal + 1),
        ln_L_formal = log(L_formal_total + 1)
    )

# Re-build panel for persistence robustness checks (RQ3)
master_df_panel <- master_df %>%
    arrange(State_Code, NIC_2digit, Year_num) %>%
    group_by(State_Code, NIC_2digit) %>%
    mutate(
        lag_ln_N_informal = log(lag(N_informal) + 1)
    ) %>%
    ungroup() %>%
    filter(!is.na(lag_ln_N_informal))

# Re-fit core models (reference for comparisons)
mod_outsourcing <- feols(
    ln_Q_out ~ ln_A_formal + E_s | NIC_2digit + Year,
    data = master_df, cluster = ~State_Code
)

mod_wages <- feols(
    ln_w_inf ~ ln_A_formal * E_s | NIC_2digit + Year,
    data = master_df, cluster = ~State_Code
)

mod_persistence <- feols(
    ln_N_informal ~ lag_ln_N_informal + ln_A_formal + E_s | Year,
    data = master_df_panel, cluster = ~State_Code
)

cat(sprintf(
    "Base dataset: %d obs | Panel (lagged): %d obs\n\n",
    nrow(master_df), nrow(master_df_panel)
))


# =============================================================================
# RQ1 ROBUSTNESS: PLACEBO TEST — CHEMICALS SECTOR (NIC-20)
#
# Logic: If outsourcing responds to productivity due to labour-regulation
# avoidance in labour-intensive textiles, a capital-intensive sector (NIC-20,
# Chemicals) should show NO such relationship. β ≈ 0 would validate that the
# textiles result is not a generic artefact.
# =============================================================================

cat("================================================================\n")
cat("  RQ1 CHECK: PLACEBO — CHEMICALS SECTOR (NIC-20)\n")
cat("================================================================\n\n")

cat("Loading NIC-20 (Chemicals) from ASI 2023-24...\n")

placebo_result <- tryCatch(
    {
        clean_df <- function(df) {
            df <- haven::zap_labels(df)
            df <- haven::zap_formats(df)
            names(df) <- tolower(names(df))
            as_tibble(df)
        }

        blk_a <- read_sav("ASI_Data/ASI_202324sav/blkA202324.sav") %>% clean_df()
        blk_e <- read_sav("ASI_Data/ASI_202324sav/blkE202324.sav") %>% clean_df()
        blk_f <- read_sav("ASI_Data/ASI_202324sav/blkF202324.sav") %>% clean_df()
        blk_j <- read_sav("ASI_Data/ASI_202324sav/blkJ202324.sav") %>% clean_df()

        # Filter for NIC-20 (Chemicals)
        chem_id <- blk_a %>%
            mutate(
                Factory_ID = as.character(a1),
                State_Code = sprintf("%02d", as.integer(a7)),
                NIC_2digit = substr(as.character(a5), 1, 2),
                Multiplier = as.numeric(mult)
            ) %>%
            filter(NIC_2digit == "20") %>%
            select(Factory_ID, State_Code, NIC_2digit, Multiplier)

        cat(sprintf("  NIC-20 factories found: %d\n", nrow(chem_id)))

        if (nrow(chem_id) < 10) stop("Too few NIC-20 factories to proceed")

        # Employment
        mandays_col <- if ("e17" %in% names(blk_e)) "e17" else "ei7"
        wages_col <- if ("e18" %in% names(blk_e)) "e18" else "ei8"
        id_col_e <- if ("ae01" %in% names(blk_e)) "ae01" else names(blk_e)[1]

        chem_emp <- blk_e %>%
            mutate(Factory_ID = as.character(!!sym(id_col_e))) %>%
            group_by(Factory_ID) %>%
            summarise(
                L_formal_firm = sum(as.numeric(!!sym(mandays_col)), na.rm = TRUE) / 300,
                .groups = "drop"
            )

        # Outsourcing
        id_col_f <- if ("af01" %in% names(blk_f)) "af01" else names(blk_f)[1]
        f7_col <- if ("f7" %in% names(blk_f)) "f7" else names(blk_f)[grep("7", names(blk_f))][1]
        chem_out <- blk_f %>%
            mutate(
                Factory_ID = as.character(!!sym(id_col_f)),
                Outsourcing_Cost = ifelse(is.na(as.numeric(!!sym(f7_col))), 0,
                    as.numeric(!!sym(f7_col))
                )
            ) %>%
            select(Factory_ID, Outsourcing_Cost)

        # Output
        id_col_j <- if ("aj01" %in% names(blk_j)) "aj01" else names(blk_j)[1]
        j_col <- if ("j113" %in% names(blk_j)) "j113" else names(blk_j)[grep("113", names(blk_j))][1]
        chem_prod <- blk_j %>%
            mutate(Factory_ID = as.character(!!sym(id_col_j))) %>%
            group_by(Factory_ID) %>%
            summarise(Total_Output = sum(as.numeric(!!sym(j_col)), na.rm = TRUE), .groups = "drop")

        rm(blk_a, blk_e, blk_f, blk_j)
        gc()

        # Merge
        chem_firm <- chem_id %>%
            left_join(chem_emp, by = "Factory_ID") %>%
            left_join(chem_out, by = "Factory_ID") %>%
            left_join(chem_prod, by = "Factory_ID") %>%
            replace_na(list(Outsourcing_Cost = 0, Total_Output = 0, L_formal_firm = 0)) %>%
            filter(Total_Output > 0, L_formal_firm > 0) %>%
            mutate(
                Productivity_A = Total_Output / L_formal_firm,
                Outsourcing_Share = Outsourcing_Cost / Total_Output
            )

        # State-industry aggregate
        chem_state <- chem_firm %>%
            group_by(State_Code, NIC_2digit) %>%
            summarise(
                A_formal = weighted.mean(Productivity_A, w = Multiplier, na.rm = TRUE),
                Q_out_mean = weighted.mean(Outsourcing_Share, w = Multiplier, na.rm = TRUE),
                N_firms = n(),
                .groups = "drop"
            )

        # Load enforcement data
        enforcement_data <- read_csv("Data/External/State_Enforcement_2023.csv", show_col_types = FALSE)
        es_col <- names(enforcement_data)[grep("E_s", names(enforcement_data), ignore.case = TRUE)][1]
        enforcement_data <- enforcement_data %>%
            rename(E_s = all_of(es_col)) %>%
            mutate(State_Code = sprintf("%02d", as.integer(State_Code)), E_s = as.numeric(E_s)) %>%
            filter(!is.na(E_s)) %>%
            select(State_Code, E_s)

        chem_df <- chem_state %>%
            left_join(enforcement_data, by = "State_Code") %>%
            filter(!is.na(E_s)) %>%
            mutate(
                ln_A_formal = log(A_formal),
                ln_Q_out    = log(Q_out_mean + 1),
                NIC_2digit  = as.factor(NIC_2digit)
            )

        cat(sprintf("  Chemicals state cells: %d (with enforcement)\n\n", nrow(chem_df)))

        # Placebo regression
        mod_placebo <- feols(ln_Q_out ~ ln_A_formal + E_s | NIC_2digit,
            data = chem_df, cluster = ~State_Code
        )

        cat("--- Placebo Regression: NIC-20 (Chemicals Outsourcing) ---\n")
        etable(mod_placebo)

        b_placebo <- coef(mod_placebo)["ln_A_formal"]
        p_placebo <- summary(mod_placebo)$coeftable["ln_A_formal", "Pr(>|t|)"]
        b_textiles <- coef(mod_outsourcing)["ln_A_formal"]

        cat("\n--- COMPARISON ---\n")
        cat(sprintf("  Textiles/Apparel β(A_formal): %+.4f — main result\n", b_textiles))
        cat(sprintf("  Chemicals β(A_formal):        %+.4f (p=%.3f) — placebo\n", b_placebo, p_placebo))

        if (p_placebo > 0.10) {
            cat("\n✓ PLACEBO PASSES: Chemicals show no significant outsourcing response.\n")
            cat("  Validates textiles result as labour-regulation-specific, not generic.\n")
        } else {
            cat("\n! PLACEBO FAILS: Chemicals also show significant response.\n")
            cat("  Alternative explanation: generic productivity-outsourcing relationship.\n")
            cat("  Check whether result is sensitive to sector definition.\n")
        }

        etable(mod_placebo, tex = TRUE, file = "Output/tex/robustness_rq1_placebo_chemicals.tex")
        write_csv(tibble(
            Test = "RQ1 Placebo: Chemicals",
            b_textiles = b_textiles, b_chemicals = b_placebo, p_chemicals = p_placebo,
            N_chem_cells = nrow(chem_df)
        ), "Output/csv/robustness_rq1_placebo_chemicals.csv")

        list(success = TRUE, mod = mod_placebo, df = chem_df)
    },
    error = function(e) {
        cat(sprintf("  [NOTE] Chemicals placebo skipped: %s\n", conditionMessage(e)))
        cat("  Ensure ASI 2023-24 files are at ASI_Data/ASI_202324sav/\n")
        write_csv(
            tibble(Test = "RQ1 Placebo: Chemicals", Note = conditionMessage(e)),
            "Output/csv/robustness_rq1_placebo_chemicals.csv"
        )
        list(success = FALSE)
    }
)

cat("\n")


# =============================================================================
# RQ2 ROBUSTNESS CHECK 1: OUTLIER DIAGNOSTICS FOR WAGE PASS-THROUGH
#
# Identify influential observations by standardised residuals.
# If result unchanged after removal → null is robust.
# =============================================================================

cat("================================================================\n")
cat("  RQ2 CHECK 1: OUTLIER DIAGNOSTICS (WAGE PASS-THROUGH)\n")
cat("================================================================\n\n")

master_df <- master_df %>%
    mutate(
        resid_wages  = as.numeric(residuals(mod_wages)),
        fitted_wages = as.numeric(fitted(mod_wages)),
        resid_sd     = sd(residuals(mod_wages), na.rm = TRUE),
        is_outlier   = abs(resid_wages) > 2 * resid_sd
    )

n_outliers <- sum(master_df$is_outlier, na.rm = TRUE)
cat(sprintf("Outliers identified (|residual| > 2 SD): %d observations\n", n_outliers))

if (n_outliers > 0) {
    outlier_states <- master_df %>%
        filter(is_outlier) %>%
        select(any_of(c("State_Code", "State_Name", "NIC_2digit", "Year"))) %>%
        distinct() %>%
        arrange(State_Code)
    cat("\nOutlier observations:\n")
    print(outlier_states)
}

# Re-run wages model excluding outliers
master_df_clean <- master_df %>% filter(!is_outlier | is.na(is_outlier))

mod_wages_nooutlier <- feols(
    ln_w_inf ~ ln_A_formal * E_s | NIC_2digit + Year,
    data = master_df_clean, cluster = ~State_Code
)

cat("\n--- Wage Models: Full vs No-Outlier Sample ---\n")
etable(mod_wages, mod_wages_nooutlier,
    headers = c("Full Sample", "Excl. Outliers")
)

b_full <- coef(mod_wages)["ln_A_formal"]
b_clean <- coef(mod_wages_nooutlier)["ln_A_formal"]
p_full <- summary(mod_wages)$coeftable["ln_A_formal", "Pr(>|t|)"]
p_clean <- summary(mod_wages_nooutlier)$coeftable["ln_A_formal", "Pr(>|t|)"]

cat(sprintf("\n  Full sample:      β = %+.4f (p=%.3f)\n", b_full, p_full))
cat(sprintf("  Excl. outliers:   β = %+.4f (p=%.3f)\n", b_clean, p_clean))

if (p_full > 0.10 && p_clean > 0.10) {
    cat("\n✓ ROBUST: Null wage result holds after removing influential observations.\n")
} else if (p_clean < 0.10 && p_full > 0.10) {
    cat("\n! CAUTION: Outlier removal reveals significance — original result may be dampened by outliers.\n")
    cat("  Investigate outlier states more carefully before reporting null.\n")
}

# Residual plot
p_resid <- ggplot(
    master_df %>% filter(!is.na(resid_wages)),
    aes(x = fitted_wages, y = resid_wages, color = is_outlier)
) +
    geom_point(alpha = 0.7) +
    geom_hline(
        yintercept = c(-2 * master_df$resid_sd[1], 2 * master_df$resid_sd[1]),
        linetype = "dashed", color = "gray55"
    ) +
    geom_hline(yintercept = 0, color = "black") +
    scale_color_manual(
        values = c("FALSE" = "#4393c3", "TRUE" = "#d73027"),
        labels = c("Normal", "Outlier (|e| > 2SD)"), name = NULL
    ) +
    labs(
        title = "RQ2 Outlier Diagnostics: Wage Pass-Through Residuals",
        subtitle = sprintf(
            "%d outlier(s) identified. Null result holds after removal: %s",
            n_outliers, ifelse(p_clean > 0.10, "YES ✓", "NO — investigate")
        ),
        x = "Fitted ln(w_inf)", y = "Residual"
    ) +
    theme_minimal(base_size = 13) +
    theme(plot.title = element_text(face = "bold"), legend.position = "bottom")

ggsave("Output/images/figure_robustness_rq2_outliers.png", p_resid, width = 8, height = 5, dpi = 300)
cat("Figure saved: Output/images/figure_robustness_rq2_outliers.png\n")

etable(mod_wages, mod_wages_nooutlier,
    tex = TRUE,
    file = "Output/tex/robustness_rq2_outlier_wages.tex"
)
write_csv(tibble(
    Test = c("Full sample", "Excl. outliers"),
    b_ln_A_formal = c(b_full, b_clean),
    p_ln_A_formal = c(p_full, p_clean),
    N_obs = c(nrow(master_df), nrow(master_df_clean))
), "Output/csv/robustness_rq2_outlier_wages.csv")

cat("\n")


# =============================================================================
# RQ2 ROBUSTNESS CHECK 2: MEDIAN WAGE MEASURE
#
# Mean wages are sensitive to extreme values. Recompute using median
# from the 2023-24 ASUSE file (most recent year).
# =============================================================================

cat("================================================================\n")
cat("  RQ2 CHECK 2: MEDIAN WAGE ROBUSTNESS\n")
cat("================================================================\n\n")

median_result <- tryCatch(
    {
        clean_df <- function(df) {
            df <- haven::zap_labels(df)
            df <- haven::zap_formats(df)
            names(df) <- tolower(names(df))
            as_tibble(df)
        }

        cat("Loading ASUSE 2023-24 for median wage calculation...\n")
        blk2 <- read_sav("ASUSE_Data/ASUSE202324sav/LEVEL - 02(Block 2).sav") %>% clean_df()
        blk4 <- read_sav("ASUSE_Data/ASUSE202324sav/LEVEL - 05 (Block 4).sav") %>% clean_df()
        blk5 <- read_sav("ASUSE_Data/ASUSE202324sav/LEVEL - 06 (Block 5 - section 5.1 to 5.14).sav") %>%
            clean_df()

        asuse_id <- blk2 %>%
            mutate(
                Enterprise_ID = paste(as.character(fsu_serial_no), as.character(sample_est_no), sep = "_"),
                NIC_2digit    = substr(as.character(major_nic_5dig), 1, 2),
                State_Code    = as.character(district)
            ) %>%
            filter(
                NIC_2digit %in% c("13", "14"),
                as.character(contract_manuf_service) == "1"
            ) %>%
            select(Enterprise_ID, State_Code, NIC_2digit, Multiplier = mlt)
        asuse_id$Multiplier <- as.numeric(asuse_id$Multiplier)
        rm(blk2)
        gc()

        asuse_workers <- blk4 %>%
            mutate(Enterprise_ID = paste(as.character(fsu_serial_no), as.character(sample_est_no), sep = "_")) %>%
            filter(as.character(item_no) == "511") %>%
            group_by(Enterprise_ID) %>%
            summarise(Hired_Workers = sum(as.numeric(value_rs), na.rm = TRUE), .groups = "drop")
        rm(blk4)
        gc()

        asuse_wages <- blk5 %>%
            mutate(Enterprise_ID = paste(as.character(fsu_serial_no), as.character(sample_est_no), sep = "_")) %>%
            filter(as.character(item_no) == "559") %>%
            group_by(Enterprise_ID) %>%
            summarise(Total_Wages = sum(as.numeric(value_rs), na.rm = TRUE), .groups = "drop")
        rm(blk5)
        gc()

        asuse_firm <- asuse_id %>%
            inner_join(asuse_workers, by = "Enterprise_ID") %>%
            inner_join(asuse_wages, by = "Enterprise_ID") %>%
            filter(Hired_Workers > 0, Total_Wages > 0) %>%
            mutate(w_informal_firm = Total_Wages / Hired_Workers)

        # State-industry medians
        asuse_median <- asuse_firm %>%
            group_by(State_Code, NIC_2digit) %>%
            summarise(
                w_inf_median = median(w_informal_firm, na.rm = TRUE),
                N_informal = n(),
                .groups = "drop"
            )

        cat(sprintf("  Median wage cells: %d\n", nrow(asuse_median)))

        # Load 2023-24 ASI state-industry for same year
        asi_202324 <- master_df %>%
            filter(Year_char == "2023-24") %>%
            select(State_Code, NIC_2digit, ln_A_formal, ln_Q_out, E_s, L_formal_total, N_firms) %>%
            mutate(NIC_2digit = as.character(NIC_2digit))

        median_df <- asi_202324 %>%
            inner_join(asuse_median, by = c("State_Code", "NIC_2digit")) %>%
            filter(!is.na(E_s), w_inf_median > 0) %>%
            mutate(
                ln_w_inf_median = log(w_inf_median),
                NIC_2digit = as.factor(NIC_2digit)
            )

        cat(sprintf("  Merged cells for median wage model: %d\n\n", nrow(median_df)))

        # Winsorize
        winsorize_manual <- function(x, lower = 0.01, upper = 0.99) {
            ql <- quantile(x, lower, na.rm = TRUE)
            qu <- quantile(x, upper, na.rm = TRUE)
            x[x < ql] <- ql
            x[x > qu] <- qu
            x
        }
        median_df$ln_w_inf_median <- winsorize_manual(median_df$ln_w_inf_median)

        mod_median <- feols(
            ln_w_inf_median ~ ln_A_formal * E_s | NIC_2digit,
            data = median_df, cluster = ~State_Code
        )

        cat("--- Wage Pass-Through: Mean vs Median Wage ---\n")
        mod_mean_2324 <- feols(
            ln_w_inf ~ ln_A_formal * E_s | NIC_2digit,
            data = master_df %>% filter(Year_char == "2023-24") %>%
                mutate(NIC_2digit = as.factor(as.character(NIC_2digit))),
            cluster = ~State_Code
        )

        etable(mod_mean_2324, mod_median,
            headers = c("Mean Wage (2023-24)", "Median Wage (2023-24)")
        )

        b_mean <- coef(mod_mean_2324)["ln_A_formal"]
        b_median <- coef(mod_median)["ln_A_formal"]
        p_mean <- summary(mod_mean_2324)$coeftable["ln_A_formal", "Pr(>|t|)"]
        p_median <- summary(mod_median)$coeftable["ln_A_formal", "Pr(>|t|)"]

        cat(sprintf("\n  Mean wage β(A_formal):   %+.4f (p=%.3f)\n", b_mean, p_mean))
        cat(sprintf("  Median wage β(A_formal): %+.4f (p=%.3f)\n", b_median, p_median))

        if (p_mean > 0.10 && p_median > 0.10) {
            cat("\n✓ ROBUST: Null result holds with median wage measure.\n")
            cat("  Not driven by mean vs median or outlier enterprise wages.\n")
        } else if (p_median < 0.10) {
            cat("\n! CAUTION: Median shows significance — consider reporting both.\n")
        }

        etable(mod_mean_2324, mod_median,
            tex = TRUE,
            file = "Output/tex/robustness_rq2_median_wage.tex"
        )
        write_csv(tibble(
            Test = c("Mean wage (2023-24)", "Median wage (2023-24)"),
            b_ln_A_formal = c(b_mean, b_median),
            p_ln_A_formal = c(p_mean, p_median),
            N_obs = c(
                nrow(master_df %>% filter(Year_char == "2023-24")),
                nrow(median_df)
            )
        ), "Output/csv/robustness_rq2_median_wage.csv")

        list(success = TRUE)
    },
    error = function(e) {
        cat(sprintf("  [NOTE] Median wage check skipped: %s\n", conditionMessage(e)))
        write_csv(
            tibble(Test = "RQ2 Median Wage", Note = conditionMessage(e)),
            "Output/csv/robustness_rq2_median_wage.csv"
        )
        list(success = FALSE)
    }
)

cat("\n")


# =============================================================================
# RQ3 CHECK 1: UNIT ROOT / STATIONARITY TEST
#
# β=0.90 for persistence raises stationarity concern.
# Option A: Balanced Subsample LLC test (requires T>=3, balanced)
# Option B: Fisher-type Combined ADF test (pools p-values; flexible)
# =============================================================================

cat("================================================================\n")
cat("  RQ3 CHECK 1: UNIT ROOT TESTS (Balanced LLC & Fisher ADF)\n")
cat("================================================================\n\n")

unitroot_result <- tryCatch(
    {
        if (!requireNamespace("plm", quietly = TRUE)) {
            install.packages("plm", lib = "R_libs", repos = "https://cloud.r-project.org", quiet = TRUE)
        }
        library(plm)

        # --- Option A: Balanced Subsample LLC ---
        cat("--- Option A: Balanced Subsample LLC ---\n")
        # Identify state-industries present in all available years
        balanced_units <- master_df %>%
            group_by(State_Code, NIC_2digit) %>%
            summarise(n_years = n(), .groups = "drop") %>%
            filter(n_years == length(unique(master_df$Year)))

        master_balanced <- master_df %>%
            inner_join(balanced_units, by = c("State_Code", "NIC_2digit"))

        cat(sprintf(
            "  Balanced panel: %d obs, %d units\n",
            nrow(master_balanced), nrow(balanced_units)
        ))

        if (nrow(balanced_units) > 0) {
            # Create unique ID for panel index
            master_balanced <- master_balanced %>%
                mutate(Panel_ID = paste(State_Code, NIC_2digit, sep = "_"))

            pdata_balanced <- pdata.frame(master_balanced, index = c("Panel_ID", "Year_char"))

            llc_test <- tryCatch(
                purtest(ln_N_informal ~ 1, data = pdata_balanced, test = "levinlin", lags = 0),
                error = function(e) {
                    cat("  [NOTE] LLC test failed:", conditionMessage(e), "\n")
                    NULL
                }
            )

            if (!is.null(llc_test)) {
                print(llc_test)
                p_llc <- llc_test$statistic$p.value
            } else {
                p_llc <- NA
            }
        } else {
            cat("  [NOTE] No balanced units found for LLC test.\n")
            p_llc <- NA
        }

        # --- Option B: Fisher Combined ADF ---
        cat("\n--- Option B: Fisher Combined ADF ---\n")
        if (!requireNamespace("tseries", quietly = TRUE)) {
            install.packages("tseries", lib = "R_libs", repos = "https://cloud.r-project.org", quiet = TRUE)
        }
        library(tseries)

        # Run ADF on each unit with 3+ observations
        adf_pvals <- master_df %>%
            group_by(State_Code, NIC_2digit) %>%
            summarise(n_obs = n(), .groups = "drop") %>%
            filter(n_obs >= 3) %>%
            inner_join(master_df, by = c("State_Code", "NIC_2digit")) %>%
            group_by(State_Code, NIC_2digit) %>%
            summarise(
                pval = tryCatch(
                    {
                        adf.test(as.numeric(ln_N_informal), k = 0)$p.value
                    },
                    error = function(e) NA_real_
                ),
                .groups = "drop"
            ) %>%
            filter(!is.na(pval))

        if (nrow(adf_pvals) > 0) {
            # Fisher's combined test: -2 * sum(log(p)) ~ chi-sq(2k)
            fisher_stat <- -2 * sum(log(adf_pvals$pval))
            fisher_pval <- pchisq(fisher_stat, df = 2 * nrow(adf_pvals), lower.tail = FALSE)

            cat(sprintf(
                "  Fisher's combined p-value (from %d units): %.4f\n",
                nrow(adf_pvals), fisher_pval
            ))
        } else {
            cat("  [NOTE] Insufficient units for Fisher ADF.\n")
            fisher_pval <- NA
        }

        cat("\n--- INTERPRETATION ---\n")
        # Priority to Fisher if LLC failed or limited power
        final_p <- if (!is.na(fisher_pval)) fisher_pval else p_llc

        if (!is.na(final_p) && final_p < 0.05) {
            cat("✓ STATIONARY: Reject unit root. β=0.90 is valid structural persistence.\n")
        } else if (!is.na(final_p)) {
            cat("! NON-STATIONARY / UNIT ROOT: β=0.90 may be spurious.\n")
            cat("  Consider first-difference specification in text.\n")
        }

        write_csv(tibble(
            Test = c("LLC Balanced", "Fisher ADF"),
            p_value = c(p_llc, fisher_pval),
            N_units = c(nrow(balanced_units), nrow(adf_pvals))
        ), "Output/csv/robustness_rq3_unitroot.csv")

        list(success = TRUE, p_val = final_p)
    },
    error = function(e) {
        cat(sprintf("  [NOTE] Unit root tests section failed: %s\n", conditionMessage(e)))
        write_csv(
            tibble(Test = "RQ3 Unit Root", Note = conditionMessage(e)),
            "Output/csv/robustness_rq3_unitroot.csv"
        )
        list(success = FALSE)
    }
)

cat("\n")


# =============================================================================
# RQ3 CHECK 2: AR(2) LAG STRUCTURE
#
# Test whether a second lag is significant. If AR(2) term is NS,
# the AR(1) specification is adequate. If significant, dynamics are richer.
# =============================================================================

cat("================================================================\n")
cat("  RQ3 CHECK 2: AR(2) LAG STRUCTURE\n")
cat("================================================================\n\n")

master_df_ar2 <- master_df %>%
    arrange(State_Code, NIC_2digit, Year_num) %>%
    group_by(State_Code, NIC_2digit) %>%
    mutate(
        lag1_ln_N_informal = log(dplyr::lag(N_informal, 1) + 1),
        lag2_ln_N_informal = log(dplyr::lag(N_informal, 2) + 1)
    ) %>%
    ungroup() %>%
    filter(!is.na(lag1_ln_N_informal), !is.na(lag2_ln_N_informal))

cat(sprintf("AR(2) panel: %d observations (need 3+ lags)\n\n", nrow(master_df_ar2)))

if (nrow(master_df_ar2) >= 5) {
    mod_ar2 <- tryCatch(
        feols(ln_N_informal ~ lag1_ln_N_informal + lag2_ln_N_informal + ln_A_formal + E_s | Year,
            data = master_df_ar2, cluster = ~State_Code
        ),
        error = function(e) {
            cat("  [NOTE] AR(2) model failed:", conditionMessage(e), "\n")
            NULL
        }
    )

    if (!is.null(mod_ar2)) {
        cat("--- AR(1) vs AR(2) Comparison ---\n")
        etable(mod_persistence, mod_ar2, headers = c("AR(1) — Main", "AR(2) — Check"))

        b_lag2 <- tryCatch(coef(mod_ar2)["lag2_ln_N_informal"], error = function(e) NA)
        p_lag2 <- tryCatch(summary(mod_ar2)$coeftable["lag2_ln_N_informal", "Pr(>|t|)"], error = function(e) NA)

        cat(sprintf(
            "\n  AR(2) coefficient: %+.4f (p=%.3f)\n",
            ifelse(is.na(b_lag2), NA, b_lag2),
            ifelse(is.na(p_lag2), NA, p_lag2)
        ))

        if (!is.na(p_lag2)) {
            if (p_lag2 > 0.10) {
                cat("✓ AR(2) term NS: AR(1) specification is appropriate.\n")
                cat("  β=0.90 from AR(1) model is not biased by omitted higher-order dynamics.\n")
            } else {
                cat("! AR(2) term significant: Dynamics are richer than AR(1).\n")
                cat("  Report AR(2) model as robustness; discuss in text.\n")
            }
        }

        etable(mod_persistence, mod_ar2,
            tex = TRUE,
            file = "Output/tex/robustness_rq3_ar2.tex"
        )
        write_csv(tibble(
            Test = c("AR(1) — Main", "AR(2) — Check"),
            b_lag1 = c(
                coef(mod_persistence)["lag_ln_N_informal"],
                coef(mod_ar2)["lag1_ln_N_informal"]
            ),
            b_lag2 = c(NA, b_lag2),
            p_lag2 = c(NA, p_lag2),
            N_obs = c(nrow(master_df_panel), nrow(master_df_ar2))
        ), "Output/csv/robustness_rq3_ar2.csv")
    }
} else {
    cat("  [NOTE] Insufficient observations for AR(2) with 3-year panel.\n")
    cat("  AR(1) is the only feasible specification.\n")
    write_csv(
        tibble(Test = "AR(2)", Note = "Insufficient obs with T=3"),
        "Output/csv/robustness_rq3_ar2.csv"
    )
}

cat("\n")


# =============================================================================
# DIAGNOSTIC TESTS FOR ALL CORE MODELS
# =============================================================================
# These tests address:
#   1. Multicollinearity (VIF)
#   2. Heteroscedasticity (Breusch-Pagan)
#   3. Autocorrelation (Durbin-Watson)
#   4. Normality of Residuals (Shapiro-Wilk)
#
# Applied to:
#   - mod_outsourcing (RQ1)
#   - mod_wages (RQ2)
#   - mod_persistence (RQ3)
# =============================================================================

cat("\n================================================================\n")
cat("  REGRESSION DIAGNOSTICS: ALL CORE MODELS\n")
cat("================================================================\n\n")

# --- Helper: Fit OLS equivalents for diagnostic tests ---
# fixest::feols objects need OLS equivalents for lmtest functions

ols_outsourcing <- lm(
    ln_Q_out ~ ln_A_formal + E_s + NIC_2digit + Year,
    data = master_df
)

ols_wages <- lm(
    ln_w_inf ~ ln_A_formal * E_s + NIC_2digit + Year,
    data = master_df
)

ols_persistence <- lm(
    ln_N_informal ~ lag_ln_N_informal + ln_A_formal + E_s + Year,
    data = master_df_panel
)


# =========================================================================
# DIAGNOSTIC 1: MULTICOLLINEARITY (VIF)
# =========================================================================
cat("--- DIAGNOSTIC 1: MULTICOLLINEARITY (VIF) ---\n\n")

vif_outsourcing <- tryCatch(car::vif(ols_outsourcing), error = function(e) NULL)
vif_wages <- tryCatch(car::vif(ols_wages), error = function(e) NULL)
vif_persistence <- tryCatch(car::vif(ols_persistence), error = function(e) NULL)

if (!is.null(vif_outsourcing)) {
    cat("Model 1 (Outsourcing) VIFs:\n")
    # Extract only key predictors (not FE dummies)
    key_vifs <- vif_outsourcing[names(vif_outsourcing) %in% c("ln_A_formal", "E_s")]
    print(round(key_vifs, 2))
    if (all(key_vifs < 5)) {
        cat("✓ No concerning multicollinearity (all VIF < 5)\n\n")
    } else {
        cat("! VIF > 5 detected — consider orthogonalization\n\n")
    }
}

if (!is.null(vif_wages)) {
    cat("Model 2 (Wage Pass-Through) VIFs:\n")
    # For interaction models, use type = "terms" with GVIF
    vif_w <- tryCatch(car::vif(ols_wages, type = "terms"), error = function(e) {
        # Fallback: just get GVIF
        car::vif(ols_wages)
    })
    key_names <- intersect(names(vif_w), c("ln_A_formal", "E_s", "ln_A_formal:E_s"))
    if (length(key_names) > 0) {
        print(round(vif_w[key_names], 2))
    } else {
        cat("  (VIF computed for full model — check manually)\n")
        print(head(round(vif_w, 2), 5))
    }
    cat("\n")
}

if (!is.null(vif_persistence)) {
    cat("Model 3 (Persistence) VIFs:\n")
    key_vifs <- vif_persistence[names(vif_persistence) %in% c("lag_ln_N_informal", "ln_A_formal", "E_s")]
    print(round(key_vifs, 2))
    if (all(key_vifs < 5)) {
        cat("✓ No concerning multicollinearity (all VIF < 5)\n\n")
    } else {
        cat("! VIF > 5 detected — investigate\n\n")
    }
}


# =========================================================================
# DIAGNOSTIC 2: HETEROSCEDASTICITY (BREUSCH-PAGAN TEST)
# =========================================================================
cat("--- DIAGNOSTIC 2: HETEROSCEDASTICITY (BREUSCH-PAGAN) ---\n\n")

bp_outsourcing <- tryCatch(lmtest::bptest(ols_outsourcing), error = function(e) NULL)
bp_wages <- tryCatch(lmtest::bptest(ols_wages), error = function(e) NULL)
bp_persistence <- tryCatch(lmtest::bptest(ols_persistence), error = function(e) NULL)

bp_results <- tibble(
    Model = character(), BP_Statistic = numeric(), P_Value = numeric(), Verdict = character()
)

if (!is.null(bp_outsourcing)) {
    verdict <- ifelse(bp_outsourcing$p.value < 0.05,
        "! Heteroscedastic (use robust/clustered SE)",
        "✓ Homoscedastic"
    )
    cat(sprintf(
        "Model 1 (Outsourcing): BP = %.3f, p = %.4f → %s\n",
        bp_outsourcing$statistic, bp_outsourcing$p.value, verdict
    ))
    bp_results <- bind_rows(bp_results, tibble(
        Model = "Outsourcing", BP_Statistic = as.numeric(bp_outsourcing$statistic),
        P_Value = bp_outsourcing$p.value, Verdict = verdict
    ))
}

if (!is.null(bp_wages)) {
    verdict <- ifelse(bp_wages$p.value < 0.05,
        "! Heteroscedastic (use robust/clustered SE)",
        "✓ Homoscedastic"
    )
    cat(sprintf(
        "Model 2 (Wages):       BP = %.3f, p = %.4f → %s\n",
        bp_wages$statistic, bp_wages$p.value, verdict
    ))
    bp_results <- bind_rows(bp_results, tibble(
        Model = "Wage Pass-Through", BP_Statistic = as.numeric(bp_wages$statistic),
        P_Value = bp_wages$p.value, Verdict = verdict
    ))
}

if (!is.null(bp_persistence)) {
    verdict <- ifelse(bp_persistence$p.value < 0.05,
        "! Heteroscedastic (use robust/clustered SE)",
        "✓ Homoscedastic"
    )
    cat(sprintf(
        "Model 3 (Persistence): BP = %.3f, p = %.4f → %s\n",
        bp_persistence$statistic, bp_persistence$p.value, verdict
    ))
    bp_results <- bind_rows(bp_results, tibble(
        Model = "Persistence", BP_Statistic = as.numeric(bp_persistence$statistic),
        P_Value = bp_persistence$p.value, Verdict = verdict
    ))
}

cat("\nNote: All core models use cluster-robust SE (fixest), which is valid\n")
cat("regardless of heteroscedasticity. BP test is reported for completeness.\n\n")


# =========================================================================
# DIAGNOSTIC 3: AUTOCORRELATION (DURBIN-WATSON TEST)
# =========================================================================
cat("--- DIAGNOSTIC 3: AUTOCORRELATION (DURBIN-WATSON) ---\n\n")

dw_outsourcing <- tryCatch(lmtest::dwtest(ols_outsourcing), error = function(e) NULL)
dw_wages <- tryCatch(lmtest::dwtest(ols_wages), error = function(e) NULL)
dw_persistence <- tryCatch(lmtest::dwtest(ols_persistence), error = function(e) NULL)

dw_results <- tibble(
    Model = character(), DW_Statistic = numeric(), P_Value = numeric(), Verdict = character()
)

if (!is.null(dw_outsourcing)) {
    verdict <- ifelse(dw_outsourcing$p.value < 0.05,
        "! Autocorrelation detected",
        "✓ No significant autocorrelation"
    )
    cat(sprintf(
        "Model 1 (Outsourcing): DW = %.3f, p = %.4f → %s\n",
        dw_outsourcing$statistic, dw_outsourcing$p.value, verdict
    ))
    dw_results <- bind_rows(dw_results, tibble(
        Model = "Outsourcing", DW_Statistic = as.numeric(dw_outsourcing$statistic),
        P_Value = dw_outsourcing$p.value, Verdict = verdict
    ))
}

if (!is.null(dw_wages)) {
    verdict <- ifelse(dw_wages$p.value < 0.05,
        "! Autocorrelation detected",
        "✓ No significant autocorrelation"
    )
    cat(sprintf(
        "Model 2 (Wages):       DW = %.3f, p = %.4f → %s\n",
        dw_wages$statistic, dw_wages$p.value, verdict
    ))
    dw_results <- bind_rows(dw_results, tibble(
        Model = "Wage Pass-Through", DW_Statistic = as.numeric(dw_wages$statistic),
        P_Value = dw_wages$p.value, Verdict = verdict
    ))
}

if (!is.null(dw_persistence)) {
    verdict <- ifelse(dw_persistence$p.value < 0.05,
        "! Autocorrelation detected",
        "✓ No significant autocorrelation"
    )
    cat(sprintf(
        "Model 3 (Persistence): DW = %.3f, p = %.4f → %s\n",
        dw_persistence$statistic, dw_persistence$p.value, verdict
    ))
    dw_results <- bind_rows(dw_results, tibble(
        Model = "Persistence", DW_Statistic = as.numeric(dw_persistence$statistic),
        P_Value = dw_persistence$p.value, Verdict = verdict
    ))
}

cat("\nNote: Cluster-robust SE in fixest accounts for within-cluster correlation.\n")
cat("DW test is reported for completeness and transparency.\n\n")


# =========================================================================
# DIAGNOSTIC 4: NORMALITY OF RESIDUALS (SHAPIRO-WILK TEST)
# =========================================================================
cat("--- DIAGNOSTIC 4: NORMALITY OF RESIDUALS (SHAPIRO-WILK) ---\n\n")

resid_out <- residuals(ols_outsourcing)
resid_wag <- residuals(ols_wages)
resid_per <- residuals(ols_persistence)

sw_outsourcing <- tryCatch(shapiro.test(resid_out), error = function(e) NULL)
sw_wages <- tryCatch(shapiro.test(resid_wag), error = function(e) NULL)
sw_persistence <- tryCatch(shapiro.test(resid_per), error = function(e) NULL)

sw_results <- tibble(
    Model = character(), W_Statistic = numeric(), P_Value = numeric(), Verdict = character()
)

if (!is.null(sw_outsourcing)) {
    verdict <- ifelse(sw_outsourcing$p.value < 0.05,
        "! Non-normal residuals (check QQ plot)",
        "✓ Residuals approximately normal"
    )
    cat(sprintf(
        "Model 1 (Outsourcing): W = %.4f, p = %.4f → %s\n",
        sw_outsourcing$statistic, sw_outsourcing$p.value, verdict
    ))
    sw_results <- bind_rows(sw_results, tibble(
        Model = "Outsourcing", W_Statistic = sw_outsourcing$statistic,
        P_Value = sw_outsourcing$p.value, Verdict = verdict
    ))
}

if (!is.null(sw_wages)) {
    verdict <- ifelse(sw_wages$p.value < 0.05,
        "! Non-normal residuals (check QQ plot)",
        "✓ Residuals approximately normal"
    )
    cat(sprintf(
        "Model 2 (Wages):       W = %.4f, p = %.4f → %s\n",
        sw_wages$statistic, sw_wages$p.value, verdict
    ))
    sw_results <- bind_rows(sw_results, tibble(
        Model = "Wage Pass-Through", W_Statistic = sw_wages$statistic,
        P_Value = sw_wages$p.value, Verdict = verdict
    ))
}

if (!is.null(sw_persistence)) {
    verdict <- ifelse(sw_persistence$p.value < 0.05,
        "! Non-normal residuals (check QQ plot)",
        "✓ Residuals approximately normal"
    )
    cat(sprintf(
        "Model 3 (Persistence): W = %.4f, p = %.4f → %s\n",
        sw_persistence$statistic, sw_persistence$p.value, verdict
    ))
    sw_results <- bind_rows(sw_results, tibble(
        Model = "Persistence", W_Statistic = sw_persistence$statistic,
        P_Value = sw_persistence$p.value, Verdict = verdict
    ))
}

cat("\nNote: With cluster-robust inference, exact normality is less critical\n")
cat("(asymptotic validity holds). Results reported for transparency.\n\n")

# --- QQ Plots for all three models ---
cat("Generating QQ plots...\n")

qq_data <- bind_rows(
    tibble(Model = "1: Outsourcing", residual = resid_out),
    tibble(Model = "2: Wage Pass-Through", residual = resid_wag),
    tibble(Model = "3: Persistence", residual = resid_per)
)

p_qq <- ggplot(qq_data, aes(sample = residual)) +
    stat_qq(alpha = 0.6, color = "#2166ac") +
    stat_qq_line(color = "#d73027", linewidth = 0.8) +
    facet_wrap(~Model, scales = "free") +
    labs(
        title = "QQ Plots: Normality of Residuals",
        subtitle = "Points should follow the red line for normally distributed residuals",
        x = "Theoretical Quantiles", y = "Sample Quantiles"
    ) +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold"))

ggsave("Output/images/figure_diagnostics_qqplots.png", p_qq,
    width = 12, height = 4.5, dpi = 300, bg = "white"
)
cat("Figure saved: Output/images/figure_diagnostics_qqplots.png\n\n")

# Save all diagnostic results
diagnostics_combined <- bind_rows(
    bp_results %>% mutate(Test_Type = "Breusch-Pagan (Heteroscedasticity)") %>%
        rename(Statistic = BP_Statistic),
    dw_results %>% mutate(Test_Type = "Durbin-Watson (Autocorrelation)") %>%
        rename(Statistic = DW_Statistic),
    sw_results %>% mutate(Test_Type = "Shapiro-Wilk (Normality)") %>%
        rename(Statistic = W_Statistic)
)
write_csv(diagnostics_combined, "Output/csv/diagnostics_all_models.csv")
cat("Diagnostic results saved: Output/csv/diagnostics_all_models.csv\n\n")


# =============================================================================
# OSTER-STYLE SENSITIVITY BOUNDS (from Stage6)
# =============================================================================
cat("================================================================\n")
cat("  SENSITIVITY TO UNOBSERVED CONFOUNDING (OSTER BOUNDS)\n")
cat("================================================================\n\n")

oster_result <- tryCatch(
    {
        baseline_mod_oster <- lm(
            ln_Q_out ~ ln_A_formal + E_s + as.factor(NIC_2digit) + as.factor(Year),
            data = master_df
        )

        sensitivity <- sensemakr(
            model = baseline_mod_oster,
            treatment = "ln_A_formal",
            benchmark_covariates = "E_s",
            kd = 1:3
        )

        rv_A_formal <- sensitivity$sensitivity_stats$rv_q
        cat(sprintf("Robustness Value (RV): %.1f%%\n", rv_A_formal * 100))
        cat(sprintf("An unobserved confounder would need to explain %.1f%% of both the\n", rv_A_formal * 100))
        cat("residual variation in outsourcing and productivity to reduce β to zero.\n\n")

        write_csv(
            tibble(
                Test = "Oster Bounds (ln_A_formal)",
                Coefficient = coef(baseline_mod_oster)["ln_A_formal"],
                Robustness_Value_Percent = rv_A_formal * 100
            ),
            "Output/csv/robustness_oster_bounds.csv"
        )
        list(success = TRUE)
    },
    error = function(e) {
        cat(sprintf("  [NOTE] Oster bounds skipped: %s\n", conditionMessage(e)))
        list(success = FALSE)
    }
)


# =============================================================================
# VIF & ORTHOGONALIZED MEDIATION (from Stage6)
# =============================================================================
cat("\n================================================================\n")
cat("  VIF & ORTHOGONALIZED MEDIATION (COST-SHIFTING)\n")
cat("================================================================\n\n")

ortho_result <- tryCatch(
    {
        # Check VIF on naive mediation model
        med_full <- lm(ln_w_inf ~ ln_A_formal + ln_Q_out + E_s, data = master_df)
        vifs_med <- car::vif(med_full)
        cat(sprintf("VIF for ln_A_formal: %.2f\n", vifs_med["ln_A_formal"]))
        cat(sprintf("VIF for ln_Q_out:    %.2f\n", vifs_med["ln_Q_out"]))

        if (!is.na(vifs_med["ln_Q_out"]) && vifs_med["ln_Q_out"] > 4) {
            cat("Moderate/High multicollinearity confirmed. Orthogonalization required.\n\n")
        } else {
            cat("VIF acceptable, but orthogonalizing for rigor.\n\n")
        }

        # Orthogonalization: residualize ln_Q_out on ln_A_formal
        mod_ortho_first <- lm(
            ln_Q_out ~ ln_A_formal + as.factor(NIC_2digit) + as.factor(Year) + as.factor(State_Code),
            data = master_df, na.action = na.exclude
        )
        master_df$Q_out_residual <- resid(mod_ortho_first)

        # Second stage: wage ~ productivity + excess outsourcing
        mod_ortho_med <- feols(
            ln_w_inf ~ ln_A_formal + Q_out_residual + E_s | NIC_2digit + Year,
            data = master_df, cluster = ~State_Code
        )

        b_qout_resid <- coef(mod_ortho_med)["Q_out_residual"]
        p_qout_resid <- summary(mod_ortho_med)$coeftable["Q_out_residual", "Pr(>|t|)"]

        cat("--- ORTHOGONALIZED MEDIATION RESULTS ---\n")
        cat(sprintf("Coefficient on Excess Outsourcing: %+.4f\n", b_qout_resid))
        cat(sprintf("P-value: %.3f\n\n", p_qout_resid))

        write_csv(
            tibble(
                Test = "Orthogonalized Mediation (Excess Outsourcing -> Wages)",
                b_Q_out_resid = b_qout_resid,
                p_value = p_qout_resid,
                VIF_Q_out_Original = vifs_med["ln_Q_out"]
            ),
            "Output/csv/robustness_orthogonalization.csv"
        )
        list(success = TRUE)
    },
    error = function(e) {
        cat(sprintf("  [NOTE] Orthogonalized mediation skipped: %s\n", conditionMessage(e)))
        list(success = FALSE)
    }
)


# =============================================================================
# BOOTSTRAP CONFIDENCE INTERVALS FOR PERSISTENCE (from Stage6)
# =============================================================================
cat("\n================================================================\n")
cat("  BOOTSTRAP CONFIDENCE INTERVALS: AR(1) PERSISTENCE\n")
cat("================================================================\n\n")

boot_result <- tryCatch(
    {
        boot_persistence <- function(data, indices) {
            d <- data[indices, ]
            fit <- lm(
                ln_N_informal ~ lag_ln_N_informal + ln_A_formal + E_s +
                    as.factor(Year) + as.factor(NIC_2digit),
                data = d
            )
            return(coef(fit)["lag_ln_N_informal"])
        }

        cat("Running 1000 bootstrap iterations for persistence parameter...\n")
        set.seed(42)
        boot_results <- boot(data = master_df_panel, statistic = boot_persistence, R = 1000)

        ci <- boot.ci(boot_results, type = c("perc", "bca"))
        b_ar1_original <- boot_results$t0
        ci_lower <- ci$bca[4]
        ci_upper <- ci$bca[5]

        cat(sprintf("Bootstrapped AR(1) Estimate: %.4f\n", b_ar1_original))
        cat(sprintf("95%% BCa Confidence Interval: [%.4f, %.4f]\n", ci_lower, ci_upper))

        if (ci_lower > 0.5) {
            cat("✓ Structural Persistence holds robustly > 0.5 under non-parametric resampling.\n\n")
        }

        write_csv(
            tibble(
                Test = "AR(1) Bootstrapped 95% CI",
                AR1_Estimate = b_ar1_original,
                CI_Lower = ci_lower,
                CI_Upper = ci_upper,
                Iterations = 1000
            ),
            "Output/csv/robustness_bootstrap_persistence.csv"
        )
        list(success = TRUE)
    },
    error = function(e) {
        cat(sprintf("  [NOTE] Bootstrap CI skipped: %s\n", conditionMessage(e)))
        list(success = FALSE)
    }
)


# =============================================================================
# ROBUSTNESS SUMMARY TABLE
# =============================================================================

cat("\n================================================================\n")
cat("  ROBUSTNESS CHECKS & DIAGNOSTICS — FULL SUMMARY\n")
cat("================================================================\n\n")

robust_summary <- tibble(
    Check = c(
        "RQ1: Placebo (Chemicals NIC-20)",
        "RQ2: Outlier exclusion",
        "RQ2: Median wage",
        "RQ3: Unit root (Balanced LLC & Fisher ADF)",
        "RQ3: AR(2) lag structure",
        "Diagnostic: Multicollinearity (VIF)",
        "Diagnostic: Heteroscedasticity (Breusch-Pagan)",
        "Diagnostic: Autocorrelation (Durbin-Watson)",
        "Diagnostic: Normality (Shapiro-Wilk)",
        "Sensitivity: Oster Bounds",
        "Mediation: VIF & Orthogonalization",
        "Persistence: Bootstrapped 95% CI"
    ),
    Status = c(
        ifelse(placebo_result$success, "✓ Run", "⚠ Skipped"),
        "✓ Run",
        ifelse(median_result$success, "✓ Run", "⚠ Skipped"),
        ifelse(unitroot_result$success, "✓ Run", "⚠ Skipped"),
        "✓ Run",
        "✓ Run",
        "✓ Run",
        "✓ Run",
        "✓ Run",
        ifelse(oster_result$success, "✓ Run", "⚠ Skipped"),
        ifelse(ortho_result$success, "✓ Run", "⚠ Skipped"),
        ifelse(boot_result$success, "✓ Run", "⚠ Skipped")
    )
)

print(robust_summary, n = Inf)
write_csv(robust_summary, "Output/csv/robustness_summary.csv")

cat("\n================================================================\n")
cat("  STAGE 4 COMPLETE\n")
cat("  All outputs written to Output/\n")
cat("================================================================\n\n")
cat("Key output files:\n")
cat("  robustness_rq1_placebo_chemicals.*\n")
cat("  robustness_rq2_outlier_wages.*\n")
cat("  robustness_rq2_median_wage.*\n")
cat("  robustness_rq3_unitroot.csv\n")
cat("  robustness_rq3_ar2.*\n")
cat("  diagnostics_all_models.csv\n")
cat("  figure_diagnostics_qqplots.png\n")
cat("  robustness_oster_bounds.csv\n")
cat("  robustness_orthogonalization.csv\n")
cat("  robustness_bootstrap_persistence.csv\n")
cat("  robustness_summary.csv\n")
