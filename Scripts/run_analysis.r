################################################################################
# MAIN ANALYSIS RUNNER
# Dissertation: Formal-Informal Linkages in Textiles & Apparel
#
# Pipeline: 7 steps in execution order
#
# NOTE: Stage1_StateIndustry_Analysis.r and Stage1_MultiYear_Analysis.r
#       are retained in the Scripts/ folder for reference but are NOT
#       included here. The Historical Integration script (Step 1) creates
#       the full long-run panel (2010-2024) used by all downstream stages.
################################################################################

cat("================================================================\n")
cat("   STARTING FULL ANALYSIS PIPELINE\n")
cat("================================================================\n\n")

# Step 1: Historical Integration — Full dataset creation (2010-2024)
#   Creates: Output/csv/combined_longrun_df.csv
cat("\n--- STEP 1: Historical Integration (NSS 67 & 73 + ASUSE 2021-24) ---\n")
source("Scripts/Stage1_Historical_Integration.r")

# Step 2: Robustness Checks for RQ1 outsourcing result
#   Creates: Output/csv/robustness_*.csv, Output/tex/robustness_*.tex
cat("\n--- STEP 2: Robustness Checks (Outsourcing & Wage Models) ---\n")
source("Scripts/Stage1_Robustness_Checks.r")

# Step 3: Political Economy Tests (Monopsony, Density, Cost-Shifting)
#   Creates: Output/csv/political_economy_summary.csv, Output/images/figure_*.png
cat("\n--- STEP 3: Political Economy Tests (3 Mechanisms) ---\n")
source("Scripts/Stage2_Political_Economy.r")

# Step 4: Persistence of Informality — AR(1) test (RQ3)
#   Creates: Output/tex/stage3_persistence_models.tex, Output/images/figure_test6_*.png
cat("\n--- STEP 4: Persistence Test (RQ3 — Structuralist) ---\n")
source("Scripts/Stage3_Structuralist_Tests.r")

# Step 5: Supplementary Appendix Tests (Tests 4, 5, 7, 8)
#   Creates: Output/tex/appendix_test*.tex, Output/images/figure_appendix_*.png
cat("\n--- STEP 5: Appendix Tests (Supplementary) ---\n")
source("Scripts/Stage3_Appendix_Tests.r")

# Step 6: Full Robustness & Diagnostics Suite
#   Includes: Placebo, Outlier, Median wage, Unit root, AR(2),
#             VIF, Breusch-Pagan, Durbin-Watson, Shapiro-Wilk,
#             Oster bounds, Orthogonalized mediation, Bootstrap CI
#   Creates: Output/csv/diagnostics_all_models.csv, Output/images/figure_diagnostics_*.png
cat("\n--- STEP 6: Robustness Diagnostics (All Models) ---\n")
source("Scripts/Stage4_Robustness_Diagnostics.r")

# Step 7: Policy Counterfactual Simulation (Monte Carlo)
#   Creates: Output/images/figure_policy_sim_*.png, Output/tex/table_policy_simulation_v4.tex
cat("\n--- STEP 7: Policy Simulation (Monte Carlo) ---\n")
source("Scripts/Stage5_Policy_Simulation.r")

cat("\n================================================================\n")
cat("   FULL ANALYSIS PIPELINE COMPLETE\n")
cat("================================================================\n")
