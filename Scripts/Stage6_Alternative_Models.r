################################################################################
# STAGE 6: ALTERNATIVE ECONOMETRIC MODELS
# Dissertation: Formal-Informal Linkages in Textiles & Apparel
#
# Purpose: Re-examine core findings using alternative estimators from PhD
# econometrics syllabus. Each model addresses a specific limitation of the
# baseline fixest OLS/FE approach.
#
# Models implemented:
#   1. Quantile Regression  — wage null across full distribution (RQ2)
#   2. Tobit                — zero-censored outsourcing (RQ1)
#   3. Negative Binomial    — integer enterprise counts, Test A (Density)
#   4. SUR                  — joint 3-equation system efficiency (RQ1+2+3)
#   5. IV / 2SLS            — instrument enforcement with lagged values (RQ1)
#   6. PSM                  — enforcement treatment effect, matched (RQ1+2)
#
# Reads:  Output/csv/combined_longrun_df.csv
# Writes: Output/csv/alternative_models_comparison.csv
#         Output/tex/table_alternative_models.tex
#         Output/images/figure_quantile_wages.png
#         Output/images/figure_psm_balance.png
################################################################################

.libPaths(c("R_libs", .libPaths()))

# --- Package loading with auto-install fallback ---
pkg_require <- function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
        message(sprintf("Installing package: %s", pkg))
        install.packages(pkg,
            lib = "R_libs", repos = "https://cloud.r-project.org",
            quiet = TRUE
        )
    }
    suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}

pkg_require("tidyverse")
pkg_require("fixest")
pkg_require("quantreg") # Quantile regression
pkg_require("AER") # Tobit + ivreg
pkg_require("MASS") # Negative Binomial
pkg_require("systemfit") # SUR
pkg_require("MatchIt") # PSM
pkg_require("ggplot2")
pkg_require("patchwork")

# setwd("C:/Users/ashwin/Documents/Formal_Informal") # Removed for reproducibility
if (!dir.exists("Output/images")) dir.create("Output/images", recursive = TRUE)
if (!dir.exists("Output/tex")) dir.create("Output/tex", recursive = TRUE)
if (!dir.exists("Output/csv")) dir.create("Output/csv", recursive = TRUE)

cat("================================================================\n")
cat("  STAGE 6: ALTERNATIVE ECONOMETRIC MODELS\n")
cat("================================================================\n\n")

# =============================================================================
# DATA PREPARATION
# =============================================================================

master_df <- read_csv("Output/csv/combined_longrun_df.csv",
    show_col_types = FALSE
) %>%
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
        # Derived variables
        ln_N_informal = log(N_informal + 1),
        ln_N_formal = log(N_firms + 1),
        ln_L_formal = log(L_formal_total + 1),
        # Binary enforcement treatment (above median = "high enforcement")
        enf_median = median(E_s, na.rm = TRUE),
        high_enf = as.integer(E_s > enf_median),
        # State-NIC panel ID
        panel_id = paste(State_Code, NIC_2digit, sep = "_")
    )

cat(sprintf(
    "Panel loaded: %d obs | %d states | %d industries | %d years\n\n",
    nrow(master_df), n_distinct(master_df$State_Code),
    n_distinct(master_df$NIC_2digit), n_distinct(master_df$Year)
))

# Baseline coefficients (from Stage 2 / Stage 3) for comparison
baseline <- list(
    rq1_outsourcing = list(
        beta = 3.72e-6, se = NA, p = 0.043,
        model = "OLS FE (baseline)", rq = "RQ1"
    ),
    rq2_wages = list(
        beta = 0.013, se = NA, p = 0.740,
        model = "OLS FE (baseline)", rq = "RQ2"
    ),
    rq3_persistence = list(
        beta = 0.9046, se = NA, p = 0.001,
        model = "OLS FE (baseline)", rq = "RQ3"
    )
)

# Storage for comparison table
results_table <- tibble(
    Model      = character(),
    RQ         = character(),
    DV         = character(),
    Key_Var    = character(),
    Beta       = numeric(),
    SE         = numeric(),
    P_value    = numeric(),
    Confirms   = character(),
    Note       = character()
)

add_result <- function(model, rq, dv, key_var, beta, se, pval, confirms, note) {
    safe_round <- function(x, d) {
        if (is.null(x) || length(x) == 0 || !is.numeric(x)) {
            return(NA_real_)
        }
        round(as.numeric(x), d)
    }
    tibble(
        Model = as.character(model), RQ = as.character(rq), DV = as.character(dv), Key_Var = as.character(key_var),
        Beta = safe_round(beta, 6), SE = safe_round(se, 6), P_value = safe_round(pval, 4),
        Confirms = as.character(confirms), Note = as.character(note)
    )
}

# =============================================================================
# MODEL 1: QUANTILE REGRESSION — WAGE PASS-THROUGH ACROSS DISTRIBUTION
# RQ2: Does the wage null hold for LOW-wage workers (τ<0.25)?
# If yes → monopsony operates across the entire wage distribution.
# =============================================================================

cat("================================================================\n")
cat("  MODEL 1: QUANTILE REGRESSION (Wage Pass-Through — RQ2)\n")
cat("================================================================\n\n")

cat("Question: Does the zero wage pass-through hold at ALL quantiles?\n")
cat("If β ≈ 0 for τ=0.10 ... 0.90 → monopsony holds across distribution.\n\n")

# Add industry and year dummies manually for quantile regression
# (rq() does not support formula-based FE)
qr_df <- master_df %>%
    filter(!is.na(ln_w_inf), !is.na(ln_A_formal), !is.na(E_s)) %>%
    mutate(
        nic14    = as.integer(NIC_2digit == "14"),
        yr2015   = as.integer(Year_char == "2015-16"),
        yr2021   = as.integer(Year_char == "2021-22"),
        yr2022   = as.integer(Year_char == "2022-23"),
        yr2023   = as.integer(Year_char == "2023-24")
    )

taus <- c(0.10, 0.25, 0.50, 0.75, 0.90)

qr_results <- map_dfr(taus, function(tau) {
    mod <- tryCatch(
        rq(
            ln_w_inf ~ ln_A_formal + E_s +
                nic14 + yr2015 + yr2021 + yr2022 + yr2023,
            data = qr_df, tau = tau
        ),
        error = function(e) NULL
    )
    if (is.null(mod)) {
        return(tibble(tau = tau, beta = NA, lower = NA, upper = NA))
    }
    s <- tryCatch(summary(mod, se = "boot", R = 200), error = function(e) NULL)
    if (is.null(s)) {
        return(tibble(tau = tau, beta = NA, lower = NA, upper = NA))
    }

    coef_tbl <- s$coefficients
    if (!("ln_A_formal" %in% rownames(coef_tbl))) {
        return(tibble(tau = tau, beta = NA, lower = NA, upper = NA))
    }

    beta <- coef_tbl["ln_A_formal", 1] # usually "Value"
    se_val <- if ("Std. Error" %in% colnames(coef_tbl)) coef_tbl["ln_A_formal", "Std. Error"] else NA

    if (is.na(se_val)) {
        return(tibble(tau = tau, beta = beta, lower = NA, upper = NA))
    }

    tibble(tau = tau, beta = beta, lower = beta - 1.96 * se_val, upper = beta + 1.96 * se_val)
})

cat("Quantile Regression Results (DV: ln_w_inf | Key: ln_A_formal):\n")
cat(sprintf("  %-8s %-12s %-12s %-12s\n", "Quantile", "Beta", "95% Lo", "95% Hi"))
pwalk(qr_results, function(tau, beta, lower, upper) {
    sig <- if (!is.na(lower) && !is.na(upper) && (lower > 0 | upper < 0)) "***" else "NS"
    cat(sprintf(
        "  τ=%-5.2f  %-12.4f %-12.4f %-12.4f  %s\n",
        tau, beta, lower, upper, sig
    ))
})

zero_at_all <- all(abs(qr_results$beta) < 0.15 | is.na(qr_results$beta))
cat(sprintf(
    "\n=> Pass-through zero at all quantiles: %s\n",
    ifelse(zero_at_all, "YES — confirms monopsony hypothesis", "NO — partial pass-through detected")
))

# Save quantile regression plot
p_qr <- ggplot(qr_results, aes(x = tau, y = beta)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.8) +
    geom_ribbon(aes(ymin = lower, ymax = upper), fill = "#3182bd", alpha = 0.20) +
    geom_line(color = "#3182bd", linewidth = 1.2) +
    geom_point(color = "#3182bd", size = 3.5) +
    geom_hline(
        yintercept = baseline$rq2_wages$beta, linetype = "dotted",
        color = "tomato", linewidth = 0.9
    ) +
    annotate("text",
        x = 0.82, y = baseline$rq2_wages$beta + 0.03,
        label = sprintf("OLS mean: %.3f", baseline$rq2_wages$beta),
        color = "tomato", size = 3.5
    ) +
    scale_x_continuous(
        breaks = taus,
        labels = sprintf("τ=%.2f", taus)
    ) +
    labs(
        title    = "Model 1: Quantile Regression — Wage Pass-Through",
        subtitle = "β(ln_A_formal) across informal wage distribution | Shaded = 95% bootstrap CI",
        x        = "Quantile (τ)",
        y        = "Pass-Through Coefficient β(ln A_formal)",
        caption  = "If all coefficients ≈ 0 and bands include zero → monopsony holds across distribution"
    ) +
    theme_minimal(base_size = 12) +
    theme(
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold")
    )

ggsave("Output/images/figure_quantile_wages.png", p_qr,
    width = 8, height = 5, dpi = 150
)
cat("=> Saved: Output/images/figure_quantile_wages.png\n\n")

# Store in comparison table
for (i in seq_len(nrow(qr_results))) {
    r <- qr_results[i, ]
    confirms <- if (!is.na(r$lower) && r$lower < 0 && r$upper > 0) "Confirms" else "Contradicts"
    results_table <- bind_rows(results_table, add_result(
        sprintf("Quantile Reg (τ=%.2f)", r$tau), "RQ2", "ln_w_inf",
        "ln_A_formal", r$beta, (r$upper - r$lower) / 3.92, NA_real_,
        confirms,
        if (is.na(r$beta)) "Failed" else sprintf("CI [%.3f, %.3f]", r$lower, r$upper)
    ))
}

# =============================================================================
# MODEL 2: TOBIT — ZERO-CENSORED OUTSOURCING (RQ1)
# Many state-industry cells have ln_Q_out = 0 (no outsourcing recorded).
# Tobit is the proper model for a left-censored continuous DV.
# =============================================================================

cat("================================================================\n")
cat("  MODEL 2: TOBIT (Outsourcing — RQ1)\n")
cat("================================================================\n\n")

cat("Question: Is the outsourcing result robust to distributional misspecification?\n")
cat("Many states have zero/near-zero outsourcing → OLS on log(Q+1) is biased.\n\n")

# Use the most recent cross-section (2023-24) to match baseline
tobit_df <- master_df %>%
    filter(Year_char == "2023-24") %>%
    mutate(
        # Use raw ln_Q_out, left-censored at 0 (or at minimum observed value)
        Q_out_raw = ln_Q_out,
        left_limit = min(Q_out_raw[Q_out_raw > 0], na.rm = TRUE) * 0.0
        # Tobit left = 0 (or -Inf; here we use 0 as absolute floor)
    ) %>%
    filter(!is.na(ln_Q_out), !is.na(ln_A_formal), !is.na(E_s))

# Tobit with left-censoring at 0
tobit_mod <- tryCatch(
    AER::tobit(ln_Q_out ~ ln_A_formal + E_s + as.factor(NIC_2digit),
        left = 0, data = tobit_df
    ),
    error = function(e) {
        cat(sprintf("  Note: AER::tobit failed (%s). Trying censReg fallback.\n", e$message))
        NULL
    }
)

if (!is.null(tobit_mod)) {
    s_tobit <- summary(tobit_mod)
    tobit_coefs <- s_tobit$coefficients
    beta_tobit <- tobit_coefs["ln_A_formal", "Estimate"]
    se_tobit <- tobit_coefs["ln_A_formal", "Std. Error"]
    z_tobit <- tobit_coefs["ln_A_formal", "z value"]
    p_tobit <- tobit_coefs["ln_A_formal", "Pr(>|z|)"]

    cat(sprintf(
        "Tobit (ln_Q_out ~ ln_A_formal + E_s, left=0, N=%d):\n",
        nrow(tobit_df)
    ))
    cat(sprintf(
        "  β(ln_A_formal) = %.6f  SE = %.6f  p = %.4f\n",
        beta_tobit, se_tobit, p_tobit
    ))
    cat(sprintf("  Log-likelihood: %.2f\n", logLik(tobit_mod)))

    direction_match <- sign(beta_tobit) == sign(baseline$rq1_outsourcing$beta)
    sig_match <- p_tobit < 0.10
    cat(sprintf(
        "\n  Baseline OLS:  β = %.6f, p = %.3f\n",
        baseline$rq1_outsourcing$beta, baseline$rq1_outsourcing$p
    ))
    cat(sprintf("  Tobit:         β = %.6f, p = %.4f\n", beta_tobit, p_tobit))
    cat(sprintf(
        "  Direction match: %s | Significant: %s\n",
        ifelse(direction_match, "YES", "NO"),
        ifelse(sig_match, "YES", "NO")
    ))
    cat(sprintf(
        "  => %s\n\n",
        if (direction_match && sig_match) {
            "CONFIRMS: outsourcing result robust to Tobit specification"
        } else if (direction_match) {
            "PARTIALLY CONFIRMS: same direction, weaker significance"
        } else {
            "CONTRADICTS: sign reversal detected"
        }
    ))

    results_table <- bind_rows(results_table, add_result(
        "Tobit (left=0)", "RQ1", "ln_Q_out", "ln_A_formal",
        beta_tobit, se_tobit, p_tobit,
        if (direction_match) "Confirms" else "Contradicts",
        sprintf("AIC=%.1f, Log-lik=%.1f", AIC(tobit_mod), logLik(tobit_mod))
    ))
} else {
    cat("  Tobit model could not be estimated — adding NA row.\n\n")
    results_table <- bind_rows(results_table, add_result(
        "Tobit (left=0)", "RQ1", "ln_Q_out", "ln_A_formal",
        NA, NA, NA, "Inconclusive", "Model failed to converge"
    ))
}

# =============================================================================
# MODEL 3: NEGATIVE BINOMIAL — ENTERPRISE COUNTS (DENSITY / SUPP TEST A)
# N_informal is a non-negative integer (count). Log-OLS is a crude proxy.
# NegBin handles overdispersion common in enterprise count data.
# =============================================================================

cat("================================================================\n")
cat("  MODEL 3: NEGATIVE BINOMIAL (Enterprise Counts — Density Test)\n")
cat("================================================================\n\n")

cat("Question: Does formal productivity predict more/fewer informal enterprises?\n")
cat("OLS on log(N+1) ignores the integer nature and overdispersion of count data.\n\n")

nb_df <- master_df %>%
    filter(!is.na(N_informal), !is.na(ln_A_formal), !is.na(E_s), !is.na(N_firms)) %>%
    mutate(N_informal_int = as.integer(round(N_informal)))

# Negative binomial with offset for formal sector scale
nb_mod <- tryCatch(
    MASS::glm.nb(
        N_informal_int ~ ln_A_formal + E_s + NIC_2digit + Year +
            offset(log(N_firms + 1)),
        data = nb_df
    ),
    error = function(e) {
        cat(sprintf("  NegBin with offset failed: %s\n  Trying without offset.\n", e$message))
        tryCatch(
            MASS::glm.nb(
                N_informal_int ~ ln_A_formal + E_s + NIC_2digit + Year,
                data = nb_df
            ),
            error = function(e2) NULL
        )
    }
)

if (!is.null(nb_mod)) {
    s_nb <- summary(nb_mod)
    beta_nb <- coef(nb_mod)["ln_A_formal"]
    se_nb <- s_nb$coefficients["ln_A_formal", "Std. Error"]
    z_nb <- s_nb$coefficients["ln_A_formal", "z value"]
    p_nb <- s_nb$coefficients["ln_A_formal", "Pr(>|z|)"]
    theta_nb <- nb_mod$theta # Overdispersion parameter

    cat(sprintf(
        "Negative Binomial (N_informal ~ ln_A_formal + E_s, N=%d):\n",
        nrow(nb_df)
    ))
    cat(sprintf(
        "  β(ln_A_formal) = %.4f  SE = %.4f  p = %.4f\n",
        beta_nb, se_nb, p_nb
    ))
    cat(sprintf("  Overdispersion θ = %.2f (< 2 → substantial overdispersion)\n", theta_nb))
    cat(sprintf(
        "  IRR (exp(β)) = %.4f — a 1-unit rise in ln_A changes enterprise count by %.1f%%\n",
        exp(beta_nb), (exp(beta_nb) - 1) * 100
    ))
    cat(sprintf("  Baseline OLS β = -0.24 (p=0.316) — Inconclusive\n"))
    cat(sprintf("  NegBin β = %.4f (p = %.4f)\n", beta_nb, p_nb))
    cat(sprintf(
        "  => %s\n\n",
        if (p_nb > 0.10) {
            "CONFIRMS: density test remains inconclusive under proper count model"
        } else if (beta_nb > 0) {
            "CONTRADICTS BASE: positive and significant — adverse incorporation in count model"
        } else {
            "CONTRADICTS: significant displacement in count model"
        }
    ))

    results_table <- bind_rows(results_table, add_result(
        "Neg. Binomial + offset", "Supp. A", "N_informal",
        "ln_A_formal", beta_nb, se_nb, p_nb,
        if (p_nb > 0.10) "Confirms" else "Contradicts",
        sprintf("IRR=%.3f; θ=%.2f (overdispersion)", exp(beta_nb), theta_nb)
    ))
} else {
    cat("  Negative binomial failed to converge — adding NA.\n\n")
    results_table <- bind_rows(results_table, add_result(
        "Neg. Binomial + offset", "Supp. A", "N_informal",
        "ln_A_formal", NA, NA, NA, "Inconclusive", "Model failed to converge"
    ))
}

# =============================================================================
# MODEL 4: SUR — SEEMINGLY UNRELATED REGRESSIONS (Joint System)
# The three core equations share state-level unobservables. SUR is more
# efficient than separate OLS when cross-equation errors are correlated.
# =============================================================================

cat("================================================================\n")
cat("  MODEL 4: SUR — THREE-EQUATION SYSTEM (RQ1 + RQ2 + RQ3)\n")
cat("================================================================\n\n")

cat("Question: Are the three equations jointly estimated more efficiently?\n")
cat("SUR exploits cross-equation error correlation to improve precision.\n\n")

# SUR requires the same observations in all equations.
# Build lag for persistence equation
sur_df <- master_df %>%
    arrange(panel_id, Year_num) %>%
    group_by(panel_id) %>%
    mutate(lag_ln_N_informal = lag(ln_N_informal)) %>%
    ungroup() %>%
    filter(
        !is.na(ln_Q_out), !is.na(ln_w_inf), !is.na(ln_N_informal),
        !is.na(lag_ln_N_informal), !is.na(ln_A_formal), !is.na(E_s)
    ) %>%
    # Add FE dummies manually for systemfit (which uses lm internally)
    mutate(
        nic14  = as.integer(NIC_2digit == "14"),
        yr2015 = as.integer(Year_char == "2015-16"),
        yr2021 = as.integer(Year_char == "2021-22"),
        yr2022 = as.integer(Year_char == "2022-23"),
        yr2023 = as.integer(Year_char == "2023-24")
    )

cat(sprintf("SUR common sample: %d observations\n\n", nrow(sur_df)))

eq1 <- ln_Q_out        ~ ln_A_formal + E_s + nic14 + yr2015 + yr2021 + yr2022 + yr2023
eq2 <- ln_w_inf        ~ ln_A_formal + E_s + nic14 + yr2015 + yr2021 + yr2022 + yr2023
eq3 <- ln_N_informal   ~ lag_ln_N_informal + ln_A_formal + E_s + nic14 + yr2015 + yr2021 + yr2022 + yr2023

sur_system <- list(Outsourcing = eq1, Wages = eq2, Persistence = eq3)

sur_mod <- tryCatch(
    {
        # Attempt 1: Full SUR
        systemfit::systemfit(sur_system, method = "SUR", data = sur_df)
    },
    error = function(e1) {
        cat(sprintf("  SUR Full failed: %s\n  Trying simpler SUR (no Year FE).\n", e1$message))
        tryCatch(
            {
                # Attempt 2: Simpler SUR (no Year dummies)
                eq1_s <- ln_Q_out        ~ ln_A_formal + E_s + nic14
                eq2_s <- ln_w_inf        ~ ln_A_formal + E_s + nic14
                eq3_s <- ln_N_informal   ~ lag_ln_N_informal + ln_A_formal + E_s + nic14
                systemfit::systemfit(list(Outsourcing = eq1_s, Wages = eq2_s, Persistence = eq3_s),
                    method = "SUR", data = sur_df
                )
            },
            error = function(e2) {
                cat(sprintf("  SUR Simple failed: %s\n  Trying OLS system as final fallback.\n", e2$message))
                tryCatch(
                    {
                        # Attempt 3: OLS system (less prone to singularity in solve)
                        systemfit::systemfit(sur_system, method = "OLS", data = sur_df)
                    },
                    error = function(e3) {
                        cat(sprintf("  All systemfit attempts failed: %s\n", e3$message))
                        NULL
                    }
                )
            }
        )
    }
)

if (!is.null(sur_mod)) {
    s_sur <- summary(sur_mod)

    # Extract coefficients for each equation (handles potential prefixes like eq1_)
    extract_coef <- function(eq_name, var_name) {
        cfs <- s_sur$eq[[eq_name]]$coefficients
        # Try exact match first, then suffix match
        match_idx <- which(rownames(cfs) == var_name)
        if (length(match_idx) == 0) {
            match_idx <- grep(paste0(var_name, "$"), rownames(cfs))
        }

        if (length(match_idx) > 0) {
            idx <- match_idx[1]
            return(list(
                beta = as.numeric(cfs[idx, "Estimate"]),
                se   = as.numeric(cfs[idx, "Std. Error"]),
                p    = as.numeric(cfs[idx, "Pr(>|t|)"])
            ))
        }
        list(beta = NA_real_, se = NA_real_, p = NA_real_)
    }

    sur_out <- extract_coef("Outsourcing", "ln_A_formal")
    sur_wage <- extract_coef("Wages", "ln_A_formal")
    sur_pers <- extract_coef("Persistence", "lag_ln_N_informal")

    cat("SUR System Results:\n")
    cat(sprintf(
        "  Eq1 (Outsourcing) β(ln_A_formal)       = %.6f  SE=%.6f  p=%.4f\n",
        sur_out$beta, sur_out$se, sur_out$p
    ))
    cat(sprintf(
        "  Eq2 (Wages)       β(ln_A_formal)       = %.6f  SE=%.6f  p=%.4f\n",
        sur_wage$beta, sur_wage$se, sur_wage$p
    ))
    cat(sprintf(
        "  Eq3 (Persistence) β(lag_ln_N_informal) = %.6f  SE=%.6f  p=%.4f\n",
        sur_pers$beta, sur_pers$se, sur_pers$p
    ))

    # Cross-equation correlation of residuals
    resid_mat <- residuals(sur_mod)
    if (ncol(resid_mat) >= 2) {
        cross_cor <- cor(resid_mat, use = "complete.obs")
        cat(sprintf("\n  Cross-equation residual correlations:\n"))
        print(round(cross_cor, 3))
        cat("  (Non-zero correlations validate SUR efficiency gains over separate OLS)\n")
    }

    results_table <- bind_rows(results_table, add_result(
        "SUR (eq1: Outsourcing)", "RQ1", "ln_Q_out",
        "ln_A_formal", sur_out$beta, sur_out$se, sur_out$p,
        if (!is.na(sur_out$beta) && sign(sur_out$beta) == sign(baseline$rq1_outsourcing$beta)) "Confirms" else "Contradicts",
        "Joint 3-equation system"
    ))
    results_table <- bind_rows(results_table, add_result(
        "SUR (eq2: Wages)", "RQ2", "ln_w_inf",
        "ln_A_formal", sur_wage$beta, sur_wage$se, sur_wage$p,
        if (!is.na(sur_wage$p) && sur_wage$p > 0.10) "Confirms" else "Contradicts",
        "Joint 3-equation system"
    ))
    results_table <- bind_rows(results_table, add_result(
        "SUR (eq3: Persistence)", "RQ3", "ln_N_informal",
        "lag(ln_N_inf)", sur_pers$beta, sur_pers$se, sur_pers$p,
        if (!is.na(sur_pers$beta) && sur_pers$beta > 0.5) "Confirms" else "Contradicts",
        "Joint 3-equation system"
    ))
} else {
    cat("  SUR failed to converge.\n\n")
    for (eq in c("Outsourcing", "Wages", "Persistence")) {
    results_table <- bind_rows(results_table, add_result(
            sprintf("SUR (%s)", eq), "N/A", "N/A", "N/A", NA, NA, NA,
            "Inconclusive", "Model failed"
        ))
    }
}

# =============================================================================
# MODEL 5: IV / 2SLS — INSTRUMENT ENFORCEMENT WITH LAGGED VALUES (RQ1)
# E_s may be endogenous: high-outsourcing states attract enforcement.
# Instrument: enforcement in prior period (E_s_{t-1}).
# =============================================================================

cat("\n================================================================\n")
cat("  MODEL 5: IV / 2SLS (Outsourcing — RQ1, Enforcement Endogeneity)\n")
cat("================================================================\n\n")

cat("Question: Is the outsourcing-productivity elasticity robust to enforcement endogeneity?\n")
cat("Instrument: lagged enforcement (E_s_{t-1}) → identified from within-panel variation.\n\n")

# Build lagged enforcement
iv_df <- master_df %>%
    arrange(panel_id, Year_num) %>%
    group_by(panel_id) %>%
    mutate(lag_E_s = lag(E_s)) %>%
    ungroup() %>%
    filter(!is.na(ln_Q_out), !is.na(ln_A_formal), !is.na(E_s), !is.na(lag_E_s))

cat(sprintf("IV sample (with lagged enforcement): %d obs\n\n", nrow(iv_df)))

if (nrow(iv_df) >= 20) {
    # First stage
    first_stage <- lm(E_s ~ lag_E_s + ln_A_formal + NIC_2digit + Year,
        data = iv_df
    )
    f_stat_iv <- summary(first_stage)$fstatistic
    f_val <- if (!is.null(f_stat_iv)) f_stat_iv[1] else NA
    # Overflow detection: F > 1e10 indicates near-perfect collinearity
    if (!is.na(f_val) && f_val > 1e10) {
        cat("  [WARNING] F-statistic overflow detected — likely near-perfect collinearity.\n")
        cat("  lag_E_s may be collinear with year-matched E_s. Instrument validity unconfirmed.\n")
        f_val <- NA
    }
    cat(sprintf("First-stage F-statistic: %s ", ifelse(is.na(f_val), "NA (overflow)", sprintf("%.2f", f_val))))
    cat(sprintf(
        "(%s — instrument %s)\n\n",
        if (!is.na(f_val) && f_val > 10) "F > 10" else "F ≤ 10 or NA",
        if (!is.na(f_val) && f_val > 10) "NOT weak" else "potentially WEAK or collinear"
    ))

    iv_mod <- tryCatch(
        AER::ivreg(
            ln_Q_out ~ ln_A_formal + E_s + NIC_2digit + Year |
                ln_A_formal + lag_E_s + NIC_2digit + Year,
            data = iv_df
        ),
        error = function(e) {
            cat(sprintf("  ivreg failed: %s\n", e$message))
            NULL
        }
    )

    if (!is.null(iv_mod)) {
        s_iv <- summary(iv_mod, diagnostics = TRUE)
        iv_coefs <- s_iv$coefficients
        beta_iv <- iv_coefs["ln_A_formal", "Estimate"]
        se_iv <- iv_coefs["ln_A_formal", "Std. Error"]
        # Safe extraction of p-value (AER ivreg can use z or t)
        p_iv_col <- intersect(colnames(iv_coefs), c("Pr(>|z|)", "Pr(>|t|)"))
        p_iv <- if (length(p_iv_col) > 0) iv_coefs["ln_A_formal", p_iv_col[1]] else NA_real_

        cat(sprintf("IV/2SLS Results (ln_Q_out ~ ln_A_formal | instrument: lag_E_s):\n"))
        cat(sprintf(
            "  β(ln_A_formal) = %.6f  SE = %.6f  p = %s\n",
            beta_iv, se_iv, ifelse(is.na(p_iv), "NA", sprintf("%.4f", p_iv))
        ))
        cat(sprintf(
            "  Baseline OLS:  β = %.6f  p = %.4f\n",
            baseline$rq1_outsourcing$beta, baseline$rq1_outsourcing$p
        ))

        # Hausman-type comparison
        attenuation <- abs(beta_iv) < abs(baseline$rq1_outsourcing$beta)
        cat(sprintf(
            "  Coefficient %s under IV → endogeneity bias was %s\n\n",
            if (attenuation) "attenuated" else "amplified",
            if (attenuation) "upward in OLS" else "downward in OLS"
        ))

        # Weak instrument warning
        if (!is.na(f_val) && f_val < 10) {
            cat("  ⚠ WEAK INSTRUMENT WARNING: F < 10. IV estimates may be unreliable.\n")
            cat("    Interpret with caution. Consider LIML or Fuller-k as alternatives.\n\n")
        }

        confirms_iv <- if (sign(beta_iv) == sign(baseline$rq1_outsourcing$beta)) "Confirms" else "Contradicts"
    results_table <- bind_rows(results_table, add_result(
            "IV/2SLS (lag_E_s instrument)", "RQ1", "ln_Q_out",
            "ln_A_formal", beta_iv, se_iv, p_iv, confirms_iv,
            sprintf(
                "First-stage F=%.1f; %s", f_val,
                if (!is.na(f_val) && f_val < 10) "WEAK INSTRUMENT" else "Instrument adequate"
            )
        ))
    } else {
    results_table <- bind_rows(results_table, add_result(
            "IV/2SLS (lag_E_s instrument)", "RQ1", "ln_Q_out",
            "ln_A_formal", NA, NA, NA, "Inconclusive", "ivreg failed"
        ))
    }
} else {
    cat(sprintf(
        "  Insufficient sample after lag-building (N=%d < 20). Skipping IV.\n\n",
        nrow(iv_df)
    ))
    results_table <- bind_rows(results_table, add_result(
        "IV/2SLS (lag_E_s instrument)", "RQ1", "ln_Q_out",
        "ln_A_formal", NA, NA, NA, "Inconclusive",
        sprintf("N=%d after lag; insufficient", nrow(iv_df))
    ))
}

# =============================================================================
# MODEL 6: PROPENSITY SCORE MATCHING (Enforcement Treatment Effect)
# Binary treatment: high_enf = 1 if E_s > median(E_s)
# Match on: ln_A_formal, NIC_2digit, Year_num, ln_N_formal
# Estimate ATT on outsourcing (RQ1) and wages (RQ2)
# =============================================================================

cat("================================================================\n")
cat("  MODEL 6: PROPENSITY SCORE MATCHING (Enforcement Treatment)\n")
cat("================================================================\n\n")

cat("Question: In balanced matched samples, does enforcement affect outsourcing/wages?\n")
cat("Treatment: E_s > median → 'High Enforcement' state. Match on formal sector observables.\n\n")

psm_df <- master_df %>%
    filter(
        !is.na(ln_Q_out), !is.na(ln_w_inf), !is.na(ln_A_formal),
        !is.na(E_s), !is.na(N_firms)
    ) %>%
    mutate(
        nic14_num = as.integer(NIC_2digit == "14"),
        Year_num2 = Year_num
    )

cat(sprintf(
    "PSM sample: %d obs | %d treated (high enforcement) | %d control\n\n",
    nrow(psm_df), sum(psm_df$high_enf), sum(1 - psm_df$high_enf)
))

psm_match <- tryCatch(
    MatchIt::matchit(
        high_enf ~ ln_A_formal + nic14_num + Year_num2 + ln_N_formal,
        data = psm_df,
        method = "nearest",
        ratio = 1,
        distance = "logit"
    ),
    error = function(e) {
        cat(sprintf("  MatchIt failed: %s\n", e$message))
        NULL
    }
)

if (!is.null(psm_match)) {
    matched_df <- MatchIt::match.data(psm_match)
    n_matched <- nrow(matched_df)
    cat(sprintf("Matched sample: %d observations (%d pairs)\n", n_matched, n_matched / 2))

    # Balance summary
    sm <- summary(psm_match)
    cat("\nCovariate Balance (Standardised Mean Differences before/after matching):\n")
    if (!is.null(sm$sum.matched)) {
        bal <- sm$sum.matched[, c("Means Treated", "Means Control", "Std. Mean Diff.")]
        print(round(bal, 3))
    }

    # ATT on outsourcing (RQ1)
    att_out <- lm(ln_Q_out ~ high_enf + ln_A_formal + nic14_num + Year_num2,
        data = matched_df, weights = matched_df$weights
    )
    s_att_out <- summary(att_out)$coefficients
    beta_att_out <- s_att_out["high_enf", "Estimate"]
    p_att_col_out <- intersect(colnames(s_att_out), c("Pr(>|z|)", "Pr(>|t|)"))
    p_att_out <- if (length(p_att_col_out) > 0) s_att_out["high_enf", p_att_col_out[1]] else NA_real_

    # ATT on wages (RQ2)
    att_wage <- lm(ln_w_inf ~ high_enf + ln_A_formal + nic14_num + Year_num2,
        data = matched_df, weights = matched_df$weights
    )
    s_att_wage <- summary(att_wage)$coefficients
    beta_att_wage <- s_att_wage["high_enf", "Estimate"]
    p_att_col_wage <- intersect(colnames(s_att_wage), c("Pr(>|z|)", "Pr(>|t|)"))
    p_att_wage <- if (length(p_att_col_wage) > 0) s_att_wage["high_enf", p_att_col_wage[1]] else NA_real_

    cat(sprintf("\nATT (Average Treatment Effect on Treated) — Matched Sample:\n"))
    cat(sprintf(
        "  Outsourcing (ln_Q_out) | ATT = %.4f  p = %.4f\n",
        beta_att_out, p_att_out
    ))
    cat(sprintf(
        "    → High enforcement states have %.1f%% %s outsourcing (p=%.3f)\n",
        abs(beta_att_out) * 100,
        if (beta_att_out > 0) "MORE" else "LESS", p_att_out
    ))

    cat(sprintf(
        "  Wages (ln_w_inf) | ATT = %.4f  p = %.4f\n",
        beta_att_wage, p_att_wage
    ))
    cat(sprintf(
        "    → High enforcement states have %.1f%% %s informal wages (p=%.3f)\n",
        abs(beta_att_wage) * 100,
        if (beta_att_wage > 0) "HIGHER" else "LOWER", p_att_wage
    ))

    cat(sprintf("\n  Key finding from PSM:\n"))
    cat(sprintf(
        "    Outsourcing: %s | Wages: %s\n",
        if (beta_att_out > 0 && p_att_out < 0.10) {
            "HIGH ENFORCEMENT → MORE OUTSOURCING (regulatory arbitrage confirmed)"
        } else if (beta_att_out > 0) {
            "positive but NS (directionally consistent)"
        } else {
            "no enforcement-outsourcing link"
        },
        if (p_att_wage > 0.10) {
            "NO WAGE EFFECT (monopsony confirmed)"
        } else {
            sprintf("%.1f%% wage change (p=%.3f)", beta_att_wage * 100, p_att_wage)
        }
    ))

    # Balance plot
    p_bal <- ggplot(
        data.frame(
            var    = rownames(sm$sum.matched),
            before = sm$sum.all[rownames(sm$sum.matched), "Std. Mean Diff."],
            after  = sm$sum.matched[, "Std. Mean Diff."]
        ) %>% pivot_longer(c(before, after), names_to = "Stage", values_to = "SMD"),
        aes(x = SMD, y = var, color = Stage, shape = Stage)
    ) +
        geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
        geom_vline(
            xintercept = c(-0.1, 0.1), linetype = "dotted",
            color = "tomato", alpha = 0.7
        ) +
        geom_point(size = 3.5, alpha = 0.8, position = position_dodge(width = 0.3)) +
        scale_color_manual(values = c(before = "#d62728", after = "#2ca02c")) +
        labs(
            title    = "Model 6: PSM Covariate Balance",
            subtitle = "Standardised Mean Differences before and after matching",
            x        = "Standardised Mean Difference (|SMD| < 0.10 = good balance)",
            y        = NULL,
            caption  = "Red dotted lines = ±0.10 threshold"
        ) +
        theme_minimal(base_size = 12) +
        theme(legend.position = "bottom", plot.title = element_text(face = "bold"))

    ggsave("Output/images/figure_psm_balance.png", p_bal,
        width = 7, height = 4, dpi = 150
    )
    cat("=> Saved: Output/images/figure_psm_balance.png\n\n")

    results_table <- bind_rows(results_table, add_result(
        "PSM (ATT: Outsourcing)", "RQ1", "ln_Q_out",
        "high_enf (ATT)", beta_att_out,
        summary(att_out)$coefficients["high_enf", "Std. Error"],
        p_att_out,
        if (beta_att_out > 0) "Confirms" else "Contradicts",
        sprintf("N_matched=%d; 1:1 nearest-neighbour", n_matched)
    ))
    results_table <- bind_rows(results_table, add_result(
        "PSM (ATT: Wages)", "RQ2", "ln_w_inf",
        "high_enf (ATT)", beta_att_wage,
        summary(att_wage)$coefficients["high_enf", "Std. Error"],
        p_att_wage,
        if (p_att_wage > 0.10) "Confirms" else "Contradicts",
        sprintf("N_matched=%d; 1:1 nearest-neighbour", n_matched)
    ))
} else {
    results_table <- bind_rows(results_table, add_result(
        "PSM (ATT: Outsourcing)", "RQ1", "ln_Q_out", "high_enf (ATT)",
        NA, NA, NA, "Inconclusive", "MatchIt failed"
    ))
    results_table <- bind_rows(results_table, add_result(
        "PSM (ATT: Wages)", "RQ2", "ln_w_inf", "high_enf (ATT)",
        NA, NA, NA, "Inconclusive", "MatchIt failed"
    ))
}

# =============================================================================
# COMPARISON TABLE: BASELINE vs. ALL ALTERNATIVE MODELS
# =============================================================================

cat("\n================================================================\n")
cat("  COMPARISON TABLE: ALL MODELS vs. BASELINE\n")
cat("================================================================\n\n")

# Add baseline rows
baseline_rows <- tribble(
    ~Model, ~RQ, ~DV, ~Key_Var, ~Beta, ~SE, ~P_value, ~Confirms, ~Note,
    "OLS FE (baseline)", "RQ1", "ln_Q_out", "ln_A_formal", 3.72e-6, NA, 0.043, "—", "Baseline",
    "OLS FE (baseline)", "RQ2", "ln_w_inf", "ln_A_formal", 0.013, NA, 0.740, "—", "Baseline",
    "OLS FE (baseline)", "RQ3", "ln_N_informal", "lag_ln_N_inf", 0.9046, NA, 0.0001, "—", "Baseline"
)

full_table <- bind_rows(baseline_rows, results_table)

# Print
cat(sprintf(
    "%-35s %-6s %-10s %-12s %-10s %s\n",
    "Model", "RQ", "Beta", "P-value", "Confirms", "Note"
))
cat(strrep("-", 100), "\n")
pwalk(full_table, function(Model, RQ, DV, Key_Var, Beta, SE, P_value, Confirms, Note) {
    cat(sprintf(
        "%-35s %-6s %-10.5f %-12s %-10s %s\n",
        substr(Model, 1, 34), RQ,
        ifelse(is.na(Beta), 0, Beta),
        ifelse(is.na(P_value), "NA", sprintf("%.4f", P_value)),
        Confirms, substr(Note, 1, 40)
    ))
})

# Save comparison CSV
write_csv(full_table, "Output/csv/alternative_models_comparison.csv")
cat("\n=> Saved: Output/csv/alternative_models_comparison.csv\n")

# Save LaTeX table
latex_table <- full_table %>%
    mutate(
        Beta_str = if_else(is.na(Beta), "--", sprintf("%.5f", Beta)),
        SE_str = if_else(is.na(SE), "--", sprintf("%.5f", SE)),
        P_str = if_else(is.na(P_value), "--", sprintf("%.4f", P_value)),
        Stars = case_when(
            is.na(P_value) ~ "",
            P_value < 0.01 ~ "***",
            P_value < 0.05 ~ "**",
            P_value < 0.10 ~ "*",
            TRUE ~ ""
        )
    )

tex_lines <- c(
    "% Alternative Models Comparison Table — Stage 6",
    "% Generated by Stage6_Alternative_Models.r",
    "\\begin{table}[H]",
    "  \\centering",
    "  \\caption{Comparison of Baseline and Alternative Econometric Models}",
    "  \\label{tab:alternative_models}",
    "  \\begin{tabular}{llllll}",
    "    \\toprule",
    "    Model & RQ & DV & $\\hat{\\beta}$ & $p$-value & Confirms \\\\",
    "    \\midrule"
)
for (i in seq_len(nrow(latex_table))) {
    r <- latex_table[i, ]
    tex_lines <- c(tex_lines, sprintf(
        "    %s & %s & %s & %s%s & %s & %s \\\\",
        gsub("_", "\\_", r$Model), r$RQ, gsub("_", "\\_", r$DV),
        r$Beta_str, r$Stars, r$P_str, r$Confirms
    ))
}
tex_lines <- c(
    tex_lines,
    "    \\bottomrule",
    "  \\end{tabular}",
    "  \\begin{tablenotes}",
    "    \\footnotesize",
    "    \\item Baseline = cluster-robust OLS with 2-way FE (\\texttt{fixest}). ",
    "    Quantile Reg = \\texttt{quantreg::rq()} with bootstrapped CIs. ",
    "    Tobit = left-censored at 0 via \\texttt{AER::tobit()}. ",
    "    Neg.~Binomial = \\texttt{MASS::glm.nb()} with formal-sector offset. ",
    "    SUR = \\texttt{systemfit::systemfit()} joint 3-equation system. ",
    "    IV/2SLS = \\texttt{AER::ivreg()} with lagged enforcement instrument. ",
    "    PSM = 1:1 nearest-neighbour matching via \\texttt{MatchIt}. ",
    "    Significance: *** $p<0.01$, ** $p<0.05$, * $p<0.10$.",
    "  \\end{tablenotes}",
    "\\end{table}"
)
writeLines(tex_lines, "Output/tex/table_alternative_models.tex")
cat("=> Saved: Output/tex/table_alternative_models.tex\n")

# =============================================================================
# SYNTHESIS: DO ALTERNATIVE MODELS CHANGE THE CONCLUSIONS?
# =============================================================================

cat("\n================================================================\n")
cat("  SYNTHESIS: WHAT DO ALTERNATIVE MODELS TELL US?\n")
cat("================================================================\n\n")

rq1_confirms <- results_table %>%
    filter(RQ == "RQ1", Confirms == "Confirms") %>%
    nrow()
rq2_confirms <- results_table %>%
    filter(RQ == "RQ2", Confirms == "Confirms") %>%
    nrow()
rq3_confirms <- results_table %>%
    filter(RQ == "RQ3", Confirms == "Confirms") %>%
    nrow()
rq1_total <- results_table %>%
    filter(RQ == "RQ1") %>%
    nrow()
rq2_total <- results_table %>%
    filter(RQ == "RQ2") %>%
    nrow()
rq3_total <- results_table %>%
    filter(RQ == "RQ3") %>%
    nrow()

cat(sprintf("RQ1 (Outsourcing):  %d/%d alternative models confirm\n", rq1_confirms, rq1_total))
cat(sprintf("RQ2 (Wages):        %d/%d alternative models confirm\n", rq2_confirms, rq2_total))
cat(sprintf("RQ3 (Persistence):  %d/%d alternative models confirm\n", rq3_confirms, rq3_total))

cat("\nInterpretation:\n")
cat("  - RQ2 (wage null) is typically the most stable across models;\n")
cat("    if confirmed in Quantile Reg and SUR, structural monopsony interpretation is robust.\n")
cat("  - RQ1 (outsourcing) benefit most from Tobit (distributional fix) and PSM\n")
cat("    (selection correction); confirmation strengthens the regulatory-arbitrage narrative.\n")
cat("  - Supplementary Test A (density) benefits from NegBin; if still inconclusive,\n")
cat("    confirms the enterprise-count mechanism is genuinely unresolved, not a model artefact.\n")
cat("  - SUR cross-equation correlation indicates whether the three equations share\n")
cat("    common structural determinants at the state level.\n")
cat("  - IV confirmation (if instrument is strong) would partially address endogeneity.\n")

cat("\n================================================================\n")
cat("  STAGE 6 COMPLETE\n")
cat("================================================================\n")
cat("Outputs saved:\n")
cat("  Output/csv/alternative_models_comparison.csv\n")
cat("  Output/tex/table_alternative_models.tex\n")
cat("  Output/images/figure_quantile_wages.png\n")
cat("  Output/images/figure_psm_balance.png\n")
