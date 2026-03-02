################################################################################
# STAGE 3 — APPENDIX TESTS (Tests 4, 5, 7, 8)
# Dissertation: Formal-Informal Linkages in Textiles & Apparel
#
# These exploratory tests examine additional structuralist mechanisms.
# Results are referenced in Appendix C; not part of main RQ1-3 narrative.
#
#   Test 4: Asymmetric Pass-Through   (power asymmetry)
#   Test 5: Firm Size & Exploitation  (monopsony scaling)
#   Test 7: Threshold Effects         (enforcement de-institutionalisation)
#   Test 8: Buyer Concentration       (monopsony power)
#
# Reads: Output/multiyear_master_df.csv
# Writes: Output/appendix_test*.tex/.csv, Output/figure_appendix_*.png
################################################################################

.libPaths(c("R_libs", .libPaths()))

suppressPackageStartupMessages({
    library(tidyverse)
    library(fixest)
    library(splines)
    library(ggplot2)
    library(patchwork)
})

setwd("C:/Users/ashwin/Documents/Formal_Informal")
if (!dir.exists("Output")) dir.create("Output")

cat("================================================================\n")
cat("  APPENDIX TESTS: Additional Structuralist Mechanisms\n")
cat("  (Tests 4, 5, 7, 8 — see main text for Tests 1-3 and 6)\n")
cat("================================================================\n\n")

# =============================================================================
# LOAD DATA
# =============================================================================

master_df <- read_csv("Output/multiyear_master_df.csv", show_col_types = FALSE) %>%
    mutate(
        NIC_2digit = as.factor(NIC_2digit),
        Year_char = as.character(Year),
        Year_num = case_when(
            Year_char == "2021-22" ~ 2021L,
            Year_char == "2022-23" ~ 2022L,
            Year_char == "2023-24" ~ 2023L,
            TRUE ~ NA_integer_
        ),
        Year = as.factor(Year_char),
        ln_N_informal = log(N_informal + 1),
        ln_N_formal = log(N_firms + 1),
        ln_L_formal = log(L_formal_total + 1),
        avg_firm_size = L_formal_total / N_firms,
        ln_firm_size = log(avg_firm_size),
        concentration = 1 / N_firms,
        ln_concentration = log(concentration)
    )

cat(sprintf(
    "Dataset: %d observations, %d states, %d industries, %d years\n\n",
    nrow(master_df),
    length(unique(master_df$State_Code)),
    length(unique(master_df$NIC_2digit)),
    length(unique(master_df$Year))
))


# =============================================================================
# TEST 4: ASYMMETRIC PASS-THROUGH (POWER ASYMMETRY)
# Prediction: β(A rises) ≈ 0, β(A falls) < 0  (Venkatesh "predatory economy")
# =============================================================================

cat("================================================================\n")
cat("  APPENDIX TEST 4: ASYMMETRIC PASS-THROUGH\n")
cat("================================================================\n\n")
cat("[Note] With 3 years of data (~18-36 delta observations after lagging),\n")
cat("this test has limited statistical power. Treat as directional evidence.\n\n")

master_df_panel <- master_df %>%
    arrange(State_Code, NIC_2digit, Year_num) %>%
    group_by(State_Code, NIC_2digit) %>%
    mutate(
        delta_A_formal = ln_A_formal - dplyr::lag(ln_A_formal),
        delta_w_inf    = ln_w_inf - dplyr::lag(ln_w_inf),
        A_increases    = as.integer(delta_A_formal > 0),
        A_decreases    = as.integer(delta_A_formal < 0)
    ) %>%
    ungroup() %>%
    filter(!is.na(delta_A_formal), !is.na(delta_w_inf))

cat(sprintf("First-difference panel: %d observations\n", nrow(master_df_panel)))
cat(sprintf(
    "  Productivity rises: %d | Falls: %d\n\n",
    sum(master_df_panel$A_increases), sum(master_df_panel$A_decreases)
))

mod_sym <- tryCatch(
    feols(delta_w_inf ~ delta_A_formal + E_s | NIC_2digit + Year,
        data = master_df_panel, cluster = ~State_Code
    ),
    error = function(e) {
        cat("  [NOTE] Symmetric model failed\n")
        NULL
    }
)

mod_asymmetric <- tryCatch(
    feols(
        delta_w_inf ~ delta_A_formal:A_increases + delta_A_formal:A_decreases + E_s |
            NIC_2digit + Year,
        data = master_df_panel, cluster = ~State_Code
    ),
    error = function(e) {
        cat("  [NOTE] Asymmetric model failed:", conditionMessage(e), "\n")
        NULL
    }
)

cat("--- Regression Results ---\n")
models_4 <- Filter(Negate(is.null), list("Symmetric" = mod_sym, "Asymmetric" = mod_asymmetric))
if (length(models_4) > 0) do.call(etable, c(models_4, list(headers = names(models_4))))

b_inc <- tryCatch(coef(mod_asymmetric)["delta_A_formal:A_increases"], error = function(e) NA)
b_dec <- tryCatch(coef(mod_asymmetric)["delta_A_formal:A_decreases"], error = function(e) NA)

cat("\n--- INTERPRETATION ---\n")
if (!is.na(b_inc) && !is.na(b_dec)) {
    cat(sprintf("  β(A rises → Δw):  %+.4f\n", b_inc))
    cat(sprintf("  β(A falls → Δw):  %+.4f\n", b_dec))
    if (abs(b_inc) < abs(b_dec) && b_dec < 0) {
        cat("\n→ ASYMMETRIC EXTRACTION: gains not shared; losses passed down\n")
    } else if (abs(b_inc) < 0.05 && abs(b_dec) < 0.05) {
        cat("\n→ FULL MONOPSONY: wages disconnected from productivity\n")
    } else {
        cat("\n→ MIXED / INCONCLUSIVE (likely small-sample attenuation)\n")
    }
} else {
    cat("  [NOTE] Asymmetric model results unavailable.\n")
}

# Export
if (!is.null(mod_asymmetric)) {
    etable(mod_asymmetric, tex = TRUE, file = "Output/appendix_test4_asymmetric.tex")
}
write_csv(
    tibble(
        Test = "Test 4: Asymmetric Pass-Through",
        b_inc = b_inc, b_dec = b_dec,
        N_obs = nrow(master_df_panel),
        Note = "3-year panel; ~2 deltas/cell; low power"
    ),
    "Output/appendix_test4_asymmetric.csv"
)

# Visualization
if (!is.na(b_inc) && !is.na(b_dec)) {
    se_inc <- tryCatch(se(mod_asymmetric)["delta_A_formal:A_increases"], error = function(e) 0)
    se_dec <- tryCatch(se(mod_asymmetric)["delta_A_formal:A_decreases"], error = function(e) 0)

    p_t4 <- ggplot(
        tibble(
            Direction = c("Productivity Rises\n(ΔA > 0)", "Productivity Falls\n(ΔA < 0)"),
            Beta = c(b_inc, b_dec), SE = c(se_inc, se_dec),
            Color = c("Rises", "Falls")
        ),
        aes(x = Direction, y = Beta, fill = Color)
    ) +
        geom_col(width = 0.5, alpha = 0.85) +
        geom_errorbar(aes(ymin = Beta - 1.96 * SE, ymax = Beta + 1.96 * SE), width = 0.15, linewidth = 0.8) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
        scale_fill_manual(values = c("Rises" = "#2166ac", "Falls" = "#d73027"), guide = "none") +
        labs(
            title = "Test 4: Asymmetric Pass-Through (Appendix C)",
            subtitle = "Structuralist prediction: β(rises) ≈ 0, β(falls) < 0",
            x = NULL, y = "Coefficient (Effect on Δln_w_inf)",
            caption = sprintf(
                "N=%d year-over-year observations. Low power with 3-year panel.",
                nrow(master_df_panel)
            )
        ) +
        theme_minimal(base_size = 13) +
        theme(plot.title = element_text(face = "bold"))

    ggsave("Output/figure_appendix_test4_asymmetric.png", p_t4, width = 7, height = 5, dpi = 300)
    cat("Figure saved: Output/figure_appendix_test4_asymmetric.png\n")
}
cat("\n")


# =============================================================================
# TEST 5: FIRM SIZE AND EXPLOITATION INTENSITY
# Prediction: β(size) > 0 in outsourcing; β(size) < 0 in wages (Portes)
# =============================================================================

cat("================================================================\n")
cat("  APPENDIX TEST 5: FIRM SIZE & EXPLOITATION INTENSITY\n")
cat("================================================================\n\n")

master_df_t5 <- master_df %>% filter(is.finite(ln_firm_size), N_firms > 0)
cat(sprintf("Sample: %d observations\n\n", nrow(master_df_t5)))

mod_size_outsourcing <- feols(
    ln_Q_out ~ ln_firm_size * ln_A_formal + E_s | NIC_2digit,
    data = master_df_t5, cluster = ~State_Code
)

mod_size_wages <- feols(
    ln_w_inf ~ ln_firm_size * ln_A_formal + E_s | NIC_2digit,
    data = master_df_t5, cluster = ~State_Code
)

etable(mod_size_outsourcing, mod_size_wages,
    headers = c("ln_Q_out (Outsourcing)", "ln_w_inf (Wages)")
)

b_size_out <- coef(mod_size_outsourcing)["ln_firm_size"]
b_size_wage <- coef(mod_size_wages)["ln_firm_size"]
p_size_out <- summary(mod_size_outsourcing)$coeftable["ln_firm_size", "Pr(>|t|)"]
p_size_wage <- summary(mod_size_wages)$coeftable["ln_firm_size", "Pr(>|t|)"]

cat("\n--- INTERPRETATION ---\n")
cat(sprintf("  β(ln_firm_size) on outsourcing: %+.4f (p=%.3f)\n", b_size_out, p_size_out))
cat(sprintf("  β(ln_firm_size) on wages:       %+.4f (p=%.3f)\n", b_size_wage, p_size_wage))

if (b_size_out > 0 && b_size_wage < 0) {
    cat("\n→ MONOPSONY POWER SCALES WITH FIRM SIZE (supports Portes)\n")
} else {
    cat("\n→ INCONCLUSIVE / MIXED results\n")
}

etable(mod_size_outsourcing, mod_size_wages, tex = TRUE, file = "Output/appendix_test5_firmsize.tex")
write_csv(
    tibble(
        Test = "Test 5: Firm Size",
        b_size_outsourcing = b_size_out, p_size_outsourcing = p_size_out,
        b_size_wages = b_size_wage, p_size_wages = p_size_wage,
        N_obs = nrow(master_df_t5)
    ),
    "Output/appendix_test5_firmsize.csv"
)

p5a <- ggplot(master_df_t5, aes(x = ln_firm_size, y = ln_Q_out)) +
    geom_point(aes(color = as.factor(NIC_2digit)), alpha = 0.6) +
    geom_smooth(method = "lm", se = TRUE, color = "black") +
    scale_color_manual(
        values = c("13" = "#1f77b4", "14" = "#ff7f0e"),
        labels = c("13" = "Textiles", "14" = "Apparel"), name = "Industry"
    ) +
    labs(
        title = "Firm Size → Outsourcing",
        subtitle = sprintf("β = %+.3f (p=%.3f)", b_size_out, p_size_out),
        x = "ln(Avg Firm Size)", y = "ln(Outsourcing Share + 1)"
    ) +
    theme_minimal(base_size = 12)

p5b <- ggplot(master_df_t5, aes(x = ln_firm_size, y = ln_w_inf)) +
    geom_point(aes(color = as.factor(NIC_2digit)), alpha = 0.6) +
    geom_smooth(method = "lm", se = TRUE, color = "black") +
    scale_color_manual(
        values = c("13" = "#1f77b4", "14" = "#ff7f0e"),
        labels = c("13" = "Textiles", "14" = "Apparel"), name = "Industry"
    ) +
    labs(
        title = "Firm Size → Informal Wages",
        subtitle = sprintf("β = %+.3f (p=%.3f)", b_size_wage, p_size_wage),
        x = "ln(Avg Firm Size)", y = "ln(Informal Wage)"
    ) +
    theme_minimal(base_size = 12)

ggsave("Output/figure_appendix_test5_firmsize.png",
    (p5a | p5b) + plot_annotation(title = "Test 5: Firm Size & Exploitation (Appendix C)"),
    width = 11, height = 5, dpi = 300
)
cat("Figure saved: Output/figure_appendix_test5_firmsize.png\n\n")


# =============================================================================
# TEST 7: THRESHOLD EFFECTS IN ENFORCEMENT
# Prediction: β(below threshold) ≈ 0, β(above threshold) > 0
# =============================================================================

cat("================================================================\n")
cat("  APPENDIX TEST 7: THRESHOLD EFFECTS IN ENFORCEMENT\n")
cat("================================================================\n\n")

find_threshold_r2 <- function(data, threshold) {
    data_t <- data %>%
        mutate(
            E_high = as.integer(E_s >= threshold),
            E_low = as.integer(E_s < threshold)
        )
    mod <- tryCatch(
        feols(ln_w_inf ~ ln_A_formal:E_high + ln_A_formal:E_low + E_s | NIC_2digit,
            data = data_t, cluster = ~State_Code
        ),
        error = function(e) NULL
    )
    if (is.null(mod)) {
        return(NA_real_)
    }
    as.numeric(tryCatch(r2(mod)["r2"], error = function(e) NA_real_))
}

thresholds <- seq(0.05, 0.40, by = 0.05)
r2_grid <- sapply(thresholds, function(t) find_threshold_r2(master_df, t))

cat("  Grid search results:\n")
cat(sprintf("  %-10s  %s\n", "Threshold", "R²"))
for (i in seq_along(thresholds)) {
    cat(sprintf("  %-10.2f  %.4f\n", thresholds[i], ifelse(is.na(r2_grid[i]), NA, r2_grid[i])))
}

optimal_idx <- which.max(r2_grid)
optimal_threshold <- thresholds[optimal_idx]
cat(sprintf("\n  Optimal threshold: E_s = %.2f (R² = %.4f)\n\n", optimal_threshold, r2_grid[optimal_idx]))

mod_threshold <- tryCatch(
    feols(
        ln_w_inf ~ ln_A_formal:as.integer(E_s >= optimal_threshold) +
            ln_A_formal:as.integer(E_s < optimal_threshold) + E_s | NIC_2digit,
        data = master_df, cluster = ~State_Code
    ),
    error = function(e) {
        cat("  [NOTE] Threshold model failed\n")
        NULL
    }
)

if (!is.null(mod_threshold)) {
    cat("--- Optimal-Threshold Piecewise Model ---\n")
    etable(mod_threshold)
    etable(mod_threshold, tex = TRUE, file = "Output/appendix_test7_threshold.tex")
}

write_csv(
    tibble(
        Threshold = thresholds, R_squared = r2_grid,
        Is_Optimal = thresholds == optimal_threshold
    ),
    "Output/appendix_test7_threshold.csv"
)

# Visualization: R² grid
p7 <- ggplot(tibble(Threshold = thresholds, R2 = r2_grid), aes(x = Threshold, y = R2)) +
    geom_line(color = "#7b3294", linewidth = 1.1) +
    geom_point(size = 3, color = "#7b3294") +
    geom_point(
        data = tibble(Threshold = optimal_threshold, R2 = r2_grid[optimal_idx]),
        color = "#d73027", size = 5
    ) +
    annotate("text",
        x = optimal_threshold, y = r2_grid[optimal_idx] + 0.005,
        label = sprintf("Optimal\nE_s = %.2f", optimal_threshold),
        color = "#d73027", size = 3.5, hjust = 0.5
    ) +
    labs(
        title = "Test 7: Grid Search for Enforcement Threshold (Appendix C)",
        subtitle = "R² of piecewise model at each candidate threshold",
        x = "E_s threshold", y = "R²"
    ) +
    theme_minimal(base_size = 13) +
    theme(plot.title = element_text(face = "bold"))

ggsave("Output/figure_appendix_test7_threshold.png", p7, width = 7, height = 5, dpi = 300)
cat("Figure saved: Output/figure_appendix_test7_threshold.png\n\n")


# =============================================================================
# TEST 8: BUYER CONCENTRATION AND WAGE SUPPRESSION
# Prediction: β(ln_concentration) < 0  (fewer buyers → lower wages)
# =============================================================================

cat("================================================================\n")
cat("  APPENDIX TEST 8: BUYER CONCENTRATION & WAGE SUPPRESSION\n")
cat("================================================================\n\n")

master_df_t8 <- master_df %>% filter(is.finite(ln_concentration))
cat(sprintf("Sample: %d observations\n\n", nrow(master_df_t8)))

mod_concentration_simple <- feols(
    ln_w_inf ~ ln_concentration + ln_A_formal + E_s | NIC_2digit,
    data = master_df_t8, cluster = ~State_Code
)

mod_concentration <- feols(
    ln_w_inf ~ ln_concentration * ln_A_formal + E_s | NIC_2digit,
    data = master_df_t8, cluster = ~State_Code
)

etable(mod_concentration_simple, mod_concentration,
    headers = c("Additive", "w/ Interaction")
)

b_conc <- coef(mod_concentration_simple)["ln_concentration"]
p_conc <- summary(mod_concentration_simple)$coeftable["ln_concentration", "Pr(>|t|)"]

cat(sprintf("\n--- INTERPRETATION ---\n"))
cat(sprintf("  β(ln_concentration): %+.4f (p=%.3f)\n", b_conc, p_conc))
cat("  [ln_concentration = log(1/N_firms): higher = fewer buyers]\n\n")

if (b_conc < 0 && p_conc < 0.10) {
    cat("→ MONOPSONY CONFIRMED: concentration suppresses wages\n")
} else if (p_conc > 0.10) {
    cat("→ INCONCLUSIVE: 1/N_firms is a crude HHI proxy; null result unsurprising at aggregate level\n")
}

etable(mod_concentration_simple, mod_concentration,
    tex = TRUE, file = "Output/appendix_test8_concentration.tex"
)
write_csv(
    tibble(
        Test = "Test 8: Buyer Concentration",
        b_concentration = b_conc, p_concentration = p_conc, N_obs = nrow(master_df_t8)
    ),
    "Output/appendix_test8_concentration.csv"
)

p8 <- ggplot(master_df_t8, aes(x = ln_concentration, y = ln_w_inf)) +
    geom_point(aes(color = as.factor(NIC_2digit), size = N_informal), alpha = 0.65) +
    geom_smooth(method = "lm", se = TRUE, color = "black") +
    scale_color_manual(
        values = c("13" = "#1f77b4", "14" = "#ff7f0e"),
        labels = c("13" = "Textiles", "14" = "Apparel"), name = "Industry"
    ) +
    scale_size_continuous(name = "N Informal", range = c(2, 8)) +
    labs(
        title = "Test 8: Buyer Concentration & Informal Wages (Appendix C)",
        subtitle = sprintf("β = %+.3f (p=%.3f) | Prediction: β < 0", b_conc, p_conc),
        x = "ln(1/N_firms)  [↑ = fewer buyers]", y = "ln(Informal Wage)",
        caption = sprintf("N=%d. NIC FE, clustered SE.", nrow(master_df_t8))
    ) +
    theme_minimal(base_size = 13) +
    theme(plot.title = element_text(face = "bold"))

ggsave("Output/figure_appendix_test8_concentration.png", p8, width = 9, height = 6, dpi = 300)
cat("Figure saved: Output/figure_appendix_test8_concentration.png\n\n")


# =============================================================================
# APPENDIX SUMMARY TABLE
# =============================================================================

cat("================================================================\n")
cat("  CONSOLIDATED APPENDIX SUMMARY (Tests 4, 5, 7, 8)\n")
cat("================================================================\n\n")

get_b <- function(mod, var) tryCatch(coef(mod)[var], error = function(e) NA_real_)
get_p <- function(mod, var) tryCatch(summary(mod)$coeftable[var, "Pr(>|t|)"], error = function(e) NA_real_)

appendix_summary <- tibble(
    Test = c(
        "4: Asymmetric (β_inc)", "4: Asymmetric (β_dec)",
        "5: Size → Outsourcing", "5: Size → Wages",
        "7: Threshold (above)", "8: Concentration → Wages"
    ),
    Coefficient = c(
        b_inc, b_dec, b_size_out, b_size_wage,
        get_b(mod_threshold, "ln_A_formal:as.integer(E_s >= optimal_threshold)"),
        b_conc
    ),
    P_value = c(
        NA, NA, p_size_out, p_size_wage,
        get_p(mod_threshold, "ln_A_formal:as.integer(E_s >= optimal_threshold)"),
        p_conc
    ),
    Structuralist_Prediction = c(
        "β_inc ≈ 0 (gains not shared)",
        "β_dec < 0 (losses passed down)",
        "β > 0 (bigger firms outsource more)",
        "β < 0 (bigger firms pay less)",
        "β(above) >> β(below) ≈ 0",
        "β < 0 (fewer buyers → lower wages)"
    )
)

print(appendix_summary, n = Inf)
write_csv(appendix_summary, "Output/appendix_summary_tests4578.csv")

cat("\nAll appendix results saved to Output/. For dissertation, reference as Appendix C.\n")
cat("\n================================================================\n")
cat("  APPENDIX TESTS COMPLETE\n")
cat("================================================================\n\n")
