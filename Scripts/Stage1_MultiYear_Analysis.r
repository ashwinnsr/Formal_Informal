################################################################################
# MULTI-YEAR ANALYSIS: MODEL 1 SIGNIFICANCE TEST
# Dissertation: Formal-Informal Linkages in Textiles & Apparel
#
# Pools ASI + ASUSE data across 3 years (2021-22, 2022-23, 2023-24)
# to test whether Model 1 (ln_w_inf ~ E_s * ln_A_formal) yields
# significant results with increased sample size.
################################################################################

# =============================================================================
# PHASE 1: SETUP
# =============================================================================

.libPaths(c("R_libs", .libPaths()))

library(tidyverse)
library(haven)
library(fixest)
library(DescTools)

setwd("C:/Users/ashwin/Documents/Formal_Informal")

if (!dir.exists("Output")) dir.create("Output")

cat("================================================================\n")
cat("  MULTI-YEAR ANALYSIS: MODEL 1 SIGNIFICANCE TEST\n")
cat("  Years: 2021-22, 2022-23, 2023-24\n")
cat("================================================================\n\n")


# =============================================================================
# PHASE 2: DEFINE YEAR-SPECIFIC FILE PATHS
# =============================================================================

asi_paths <- list(
    "2021-22" = list(
        blkA = "ASI_Data/ASI_202122sav/blkA202122.sav",
        blkE = "ASI_Data/ASI_202122sav/blkE202122.sav",
        blkF = "ASI_Data/ASI_202122sav/blkF202122.sav",
        blkJ = "ASI_Data/ASI_202122sav/blkJ202122.sav"
    ),
    "2022-23" = list(
        blkA = "ASI_Data/ASI_202223sav/blkA202223.sav",
        blkE = "ASI_Data/ASI_202223sav/blkE202223.sav",
        blkF = "ASI_Data/ASI_202223sav/blkF202223.sav",
        blkJ = "ASI_Data/ASI_202223sav/blkJ202223.sav"
    ),
    "2023-24" = list(
        blkA = "ASI_Data/ASI_202324sav/blkA202324.sav",
        blkE = "ASI_Data/ASI_202324sav/blkE202324.sav",
        blkF = "ASI_Data/ASI_202324sav/blkF202324.sav",
        blkJ = "ASI_Data/ASI_202324sav/blkJ202324.sav"
    )
)

asuse_paths <- list(
    "2021-22" = list(
        blk2 = "ASUSE_Data/ASUSE202122sav/LEVEL - 02(Block 2).sav",
        blk4 = "ASUSE_Data/ASUSE202122sav/LEVEL - 05 (Block 4).sav",
        blk5 = "ASUSE_Data/ASUSE202122sav/LEVEL - 06 (Block 5 - section 5.1 to 5.13).sav",
        format = "sav"
    ),
    "2022-23" = list(
        blk2 = "ASUSE_Data/ASUSE202223sav/LEVEL - 02(Block 2).xpt",
        blk4 = "ASUSE_Data/ASUSE202223sav/LEVEL - 05 (Block 4).xpt",
        blk5 = "ASUSE_Data/ASUSE202223sav/LEVEL - 06 (Block 5 - section 5_1 to 5_14).xpt",
        format = "xpt"
    ),
    "2023-24" = list(
        blk2 = "ASUSE_Data/ASUSE202324sav/LEVEL - 02(Block 2).sav",
        blk4 = "ASUSE_Data/ASUSE202324sav/LEVEL - 05 (Block 4).sav",
        blk5 = "ASUSE_Data/ASUSE202324sav/LEVEL - 06 (Block 5 - section 5.1 to 5.14).sav",
        format = "sav"
    )
)


# =============================================================================
# PHASE 3: DATA LOADING FUNCTIONS
# =============================================================================

winsorize_manual <- function(x, lower = 0.01, upper = 0.99) {
    q_lower <- quantile(x, lower, na.rm = TRUE)
    q_upper <- quantile(x, upper, na.rm = TRUE)
    x[x < q_lower] <- q_lower
    x[x > q_upper] <- q_upper
    return(x)
}

clean_df <- function(df) {
    df <- zap_labels(df)
    df <- zap_formats(df)
    names(df) <- tolower(names(df))
    return(as_tibble(df))
}

load_asi_year <- function(year, paths) {
    cat(sprintf("\n  --- Loading ASI %s ---\n", year))

    cat("    Reading Blocks...\n")
    blk_a <- read_sav(paths$blkA) %>% clean_df()
    blk_e <- read_sav(paths$blkE) %>% clean_df()
    blk_f <- read_sav(paths$blkF) %>% clean_df()
    blk_j <- read_sav(paths$blkJ) %>% clean_df()

    mandays_col <- if ("e17" %in% names(blk_e)) "e17" else "ei7"

    asi_id <- blk_a %>%
        mutate(
            Factory_ID = as.character(a1),
            State_Code = as.character(a7),
            NIC_Code = as.character(a5),
            NIC_2digit = substr(NIC_Code, 1, 2),
            Multiplier = as.numeric(mult)
        ) %>%
        filter(NIC_2digit %in% c("13", "14")) %>%
        select(Factory_ID, State_Code, NIC_2digit, Multiplier)

    wages_col <- if ("e18" %in% names(blk_e)) "e18" else "ei8"
    ae_id_col <- if ("ae01" %in% names(blk_e)) "ae01" else names(blk_e)[1]
    asi_emp <- blk_e %>%
        mutate(Factory_ID = as.character(!!sym(ae_id_col))) %>%
        group_by(Factory_ID) %>%
        summarise(
            Total_Mandays = sum(as.numeric(!!sym(mandays_col)), na.rm = TRUE),
            Total_Formal_Wages = sum(as.numeric(!!sym(wages_col)), na.rm = TRUE),
            .groups = "drop"
        ) %>%
        mutate(
            L_formal_firm = Total_Mandays / 300,
            w_formal_firm = ifelse(L_formal_firm > 0, Total_Formal_Wages / L_formal_firm, NA)
        )

    af_id_col <- if ("af01" %in% names(blk_f)) "af01" else names(blk_f)[1]
    f_val_col <- if ("f7" %in% names(blk_f)) "f7" else names(blk_f)[grep("7", names(blk_f))][1]
    asi_outsource <- blk_f %>%
        mutate(
            Factory_ID = as.character(!!sym(af_id_col)),
            Outsourcing_Cost = ifelse(is.na(as.numeric(!!sym(f_val_col))), 0, as.numeric(!!sym(f_val_col)))
        ) %>%
        select(Factory_ID, Outsourcing_Cost)

    aj_id_col <- if ("aj01" %in% names(blk_j)) "aj01" else names(blk_j)[1]
    j_val_col <- if ("j113" %in% names(blk_j)) "j113" else names(blk_j)[grep("113", names(blk_j))][1]
    asi_prod <- blk_j %>%
        mutate(Factory_ID = as.character(!!sym(aj_id_col))) %>%
        group_by(Factory_ID) %>%
        summarise(Total_Output = sum(as.numeric(!!sym(j_val_col)), na.rm = TRUE), .groups = "drop")

    rm(blk_a, blk_e, blk_f, blk_j)
    gc()

    asi_firm <- asi_id %>%
        left_join(asi_emp, by = "Factory_ID") %>%
        left_join(asi_outsource, by = "Factory_ID") %>%
        left_join(asi_prod, by = "Factory_ID") %>%
        replace_na(list(Outsourcing_Cost = 0, Total_Output = 0, L_formal_firm = 0)) %>%
        filter(Total_Output > 0, L_formal_firm > 0) %>%
        mutate(
            Productivity_A = Total_Output / L_formal_firm,
            Outsourcing_Share = Outsourcing_Cost / Total_Output
        )

    asi_state_ind <- asi_firm %>%
        group_by(State_Code, NIC_2digit) %>%
        summarise(
            A_formal = weighted.mean(Productivity_A, w = Multiplier, na.rm = TRUE),
            w_for_mean = weighted.mean(w_formal_firm, w = Multiplier, na.rm = TRUE),
            Q_out_mean = weighted.mean(Outsourcing_Share, w = Multiplier, na.rm = TRUE),
            L_formal_total = sum(L_formal_firm * Multiplier, na.rm = TRUE),
            N_firms = n(),
            .groups = "drop"
        ) %>%
        mutate(Year = year)

    cat(sprintf("    Created %d state-industry cells for ASI %s\n", nrow(asi_state_ind), year))
    return(asi_state_ind)
}

load_asuse_year <- function(year, paths) {
    cat(sprintf("\n  --- Loading ASUSE %s ---\n", year))

    read_fn <- if (paths$format == "xpt") read_xpt else read_sav

    # Block 2
    cat("    Processing Block 2 Identification...\n")
    blk2 <- read_fn(paths$blk2) %>% clean_df()

    # Final Mapping Determination
    if (year == "2023-24") {
        c_fsu <- "fsu_serial_no"
        c_est <- "sample_est_no"
        c_nic <- "major_nic_5dig"
        c_job <- "contract_manuf_service"
        c_mlt <- "mlt"
        c_item <- "item_no"
        c_v4 <- "value_rs"
        c_v5 <- "value_rs"
        c_item5 <- "item_no"
    } else {
        c_fsu <- "fsu"
        c_est <- "b1q5"
        c_job <- "b2239"
        c_item <- "b4q2"
        c_v4 <- "b4q3"
        c_v5 <- "b5q4"
        c_item5 <- "b5q2"
        # Robust NIC selection for coding variations
        c_nic <- if ("b2202_b" %in% names(blk2)) "b2202_b" else "b2202b"
        c_mlt <- if ("mult" %in% names(blk2)) "mult" else "mlt"
    }

    asuse_id <- blk2 %>%
        mutate(
            Enterprise_ID = paste(as.character(!!sym(c_fsu)), as.character(!!sym(c_est)), sep = "_"),
            NIC_2digit = substr(as.character(!!sym(c_nic)), 1, 2),
            State_Code = as.character(district)
        ) %>%
        filter(NIC_2digit %in% c("13", "14"), as.character(!!sym(c_job)) == "1") %>%
        select(Enterprise_ID, State_Code, NIC_2digit, Multiplier = !!sym(c_mlt))
    asuse_id$Multiplier <- as.numeric(asuse_id$Multiplier)
    rm(blk2)
    gc()

    # Block 4
    cat("    Processing Block 4 Workers...\n")
    blk4 <- read_fn(paths$blk4) %>% clean_df()
    asuse_workers <- blk4 %>%
        mutate(Enterprise_ID = paste(as.character(!!sym(c_fsu)), as.character(!!sym(c_est)), sep = "_")) %>%
        filter(as.character(!!sym(c_item)) == "511") %>%
        group_by(Enterprise_ID) %>%
        summarise(Hired_Workers = sum(as.numeric(!!sym(c_v4)), na.rm = TRUE), .groups = "drop")
    rm(blk4)
    gc()

    # Block 5
    cat("    Processing Block 5 Wages...\n")
    blk5 <- read_fn(paths$blk5) %>% clean_df()
    asuse_wages <- blk5 %>%
        mutate(Enterprise_ID = paste(as.character(!!sym(c_fsu)), as.character(!!sym(c_est)), sep = "_")) %>%
        filter(as.character(!!sym(c_item5)) == "559") %>%
        group_by(Enterprise_ID) %>%
        summarise(Total_Wages = sum(as.numeric(!!sym(c_v5)), na.rm = TRUE), .groups = "drop")
    rm(blk5)
    gc()

    # Merge
    asuse_firm <- asuse_id %>%
        inner_join(asuse_workers, by = "Enterprise_ID") %>%
        inner_join(asuse_wages, by = "Enterprise_ID") %>%
        filter(Hired_Workers > 0, Total_Wages > 0) %>%
        mutate(w_informal_firm = Total_Wages / Hired_Workers)

    asuse_state_ind <- asuse_firm %>%
        group_by(State_Code, NIC_2digit) %>%
        summarise(
            w_inf_mean = weighted.mean(w_informal_firm, w = Multiplier, na.rm = TRUE),
            N_informal = n(),
            .groups = "drop"
        ) %>%
        mutate(Year = year)

    cat(sprintf("    Created %d state-industry cells for ASUSE %s\n", nrow(asuse_state_ind), year))
    return(asuse_state_ind)
}


# =============================================================================
# PHASE 4: LOAD ALL YEARS
# =============================================================================

cat("\nPHASE 4: LOADING DATA FOR ALL YEARS\n")
years <- c("2021-22", "2022-23", "2023-24")

asi_all_years <- bind_rows(lapply(years, function(y) load_asi_year(y, asi_paths[[y]])))
asuse_all_years <- bind_rows(lapply(years, function(y) load_asuse_year(y, asuse_paths[[y]])))

cat(sprintf("\nTotal ASI cells: %d, Total ASUSE cells: %d\n", nrow(asi_all_years), nrow(asuse_all_years)))


# =============================================================================
# PHASE 5: MERGE AND CREATE MASTER DATASET
# =============================================================================

cat("\nPHASE 5: CREATING MASTER DATASET\n")

enforcement_data <- read_csv("Data/External/State_Enforcement_2023.csv", show_col_types = FALSE) %>%
    rename(E_s = names(.)[grep("E_s", names(.), ignore.case = TRUE)][1]) %>%
    mutate(State_Code = sprintf("%02d", as.integer(State_Code)), E_s = as.numeric(E_s)) %>%
    filter(!is.na(E_s)) %>%
    select(State_Code, State_Name, E_s)

master_df <- asi_all_years %>%
    inner_join(asuse_all_years, by = c("State_Code", "NIC_2digit", "Year")) %>%
    left_join(enforcement_data, by = "State_Code") %>%
    filter(!is.na(E_s)) %>%
    mutate(
        ln_w_inf = log(w_inf_mean),
        ln_A_formal = log(A_formal),
        ln_Q_out = log(Q_out_mean + 1),
        Year = as.factor(Year)
    )

master_df$ln_w_inf <- winsorize_manual(master_df$ln_w_inf)
master_df$ln_A_formal <- winsorize_manual(master_df$ln_A_formal)
master_df$ln_Q_out <- winsorize_manual(master_df$ln_Q_out)

write_csv(master_df, "Output/csv/multiyear_master_df.csv")
cat(sprintf("\nSaved integrated master dataset with formalized wages to Output/csv/multiyear_master_df.csv\n"))

# =============================================================================
# PHASE 7+: ANALYSIS & OUTPUT
# =============================================================================

cat("\nPHASE 7-12: RUNNING REGRESSIONS & SAVING OUTPUT\n")

# Model 1 Pooled
mod1_pooled <- feols(ln_w_inf ~ E_s * ln_A_formal | NIC_2digit + Year, data = master_df, cluster = ~State_Code)
print(summary(mod1_pooled))

# Model 2 Pooled
mod2_pooled <- feols(ln_Q_out ~ E_s + I(E_s^2) + ln_A_formal | NIC_2digit + Year, data = master_df, cluster = ~State_Code)
print(summary(mod2_pooled))

# Year-by-year for Model 1
cat("\n--- Year-by-Year Model 1 ---\n")
year_models <- list()
for (y in levels(master_df$Year)) {
    df_y <- master_df %>% filter(Year == y)
    if (nrow(df_y) >= 5) {
        year_models[[y]] <- feols(ln_w_inf ~ E_s * ln_A_formal | NIC_2digit, data = df_y, cluster = ~State_Code)
        cat(sprintf("\nYear %s:\n", y))
        print(summary(year_models[[y]]))
    }
}

# Save
etable(mod1_pooled, tex = TRUE, file = "Output/tex/multiyear_model1_pooled.tex")
etable(mod1_pooled, file = "Output/csv/multiyear_model1_pooled.csv")
if (length(year_models) > 0) {
    do.call(etable, c(year_models, list(tex = TRUE, file = "Output/tex/multiyear_model1_yearly.tex")))
}
etable(mod2_pooled, tex = TRUE, file = "Output/tex/multiyear_model2_pooled.tex")

cat("\nResults saved to Output/ directory.\n")
# =============================================================================
# PHASE 13: VISUALIZATIONS
# =============================================================================

cat("\nPHASE 13: GENERATING FIGURES\n")

# Figure 1: Model 1 - Wage Pass-through by Enforcement
# We split enforcement into High/Low for visualization
median_es <- median(master_df$E_s, na.rm = TRUE)
master_df <- master_df %>%
    mutate(Enforcement_Level = ifelse(E_s >= median_es, "High Enforcement", "Low Enforcement"))

p1 <- ggplot(master_df, aes(x = ln_A_formal, y = ln_w_inf, color = Enforcement_Level)) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm", se = TRUE) +
    labs(
        title = "Model 1: Formal Productivity and Informal Wages",
        subtitle = "Interaction with State Enforcement Intensity",
        x = "ln(Formal Productivity)",
        y = "ln(Informal Wage)",
        color = "Enforcement"
    ) +
    theme_minimal() +
    scale_color_manual(values = c("High Enforcement" = "#2c7bb6", "Low Enforcement" = "#d7191c"))

ggsave("Output/images/figure1_wage_passthrough.png", p1, width = 8, height = 6, dpi = 300)

# Figure 2: Model 2 - Outsourcing Intensity and Productivity
p2 <- ggplot(master_df, aes(x = ln_A_formal, y = ln_Q_out)) +
    geom_point(alpha = 0.5, color = "#7b3294") +
    geom_smooth(method = "lm", color = "black") +
    labs(
        title = "Model 2: Strategic Outsourcing",
        subtitle = "Formal Productivity vs Outsourcing Intensity",
        x = "ln(Formal Productivity)",
        y = "ln(Outsourcing Share + 1)"
    ) +
    theme_minimal()

ggsave("Output/images/figure2_outsourcing_intensity.png", p2, width = 8, height = 6, dpi = 300)

# Figure 3: Enforcement Distribution
p3 <- ggplot(enforcement_data, aes(x = reorder(State_Name, E_s), y = E_s)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    coord_flip() +
    labs(
        title = "State Labor Enforcement Intensity (E_s)",
        x = "State",
        y = "Enforcement Index"
    ) +
    theme_minimal()

ggsave("Output/images/figure3_enforcement_map.png", p3, width = 8, height = 8, dpi = 300)

cat("Figures saved to Output/ directory.\n")
cat("\nANALYSIS COMPLETE.\n")
