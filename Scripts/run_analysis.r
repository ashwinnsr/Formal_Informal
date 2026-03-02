################################################################################
# MAIN ANALYSIS RUNNER
# Dissertation: Formal-Informal Linkages in Textiles & Apparel
################################################################################

cat("================================================================\n")
cat("   STARTING FULL ANALYSIS PIPELINE\n")
cat("================================================================\n\n")

# 1. Stage 1: State-Industry Analysis (Baseline)
cat("\n--- STEP 1: Running Stage 1 State-Industry Analysis ---\n")
source("Scripts/Stage1_StateIndustry_Analysis.r")

# 2. Stage 1: Multi-Year Analysis
cat("\n--- STEP 2: Running Stage 1 Multi-Year Analysis ---\n")
source("Scripts/Stage1_MultiYear_Analysis.r")

# 3. Stage 1: Robustness Checks
cat("\n--- STEP 3: Running Stage 1 Robustness Checks ---\n")
source("Scripts/Stage1_Robustness_Checks.r")

# 4. Stage 2: Political Economy Analysis
cat("\n--- STEP 4: Running Stage 2 Political Economy Analysis ---\n")
source("Scripts/Stage2_Political_Economy.r")

# 5. Stage 3: Persistence of Informality (RQ3 primary finding)
cat("\n--- STEP 5: Running Stage 3 — Persistence (RQ3) ---\n")
source("Scripts/Stage3_Structuralist_Tests.r")

# 6. Stage 3 Appendix: Exploratory structuralist tests (for Appendix C)
cat("\n--- STEP 6: Running Stage 3 Appendix Tests (Tests 4, 5, 7, 8) ---\n")
source("Scripts/Stage3_Appendix_Tests.r")

# 7. Stage 4: Robustness checks and diagnostics (all RQs)
cat("\n--- STEP 7: Running Stage 4 Robustness Diagnostics ---\n")
source("Scripts/Stage4_Robustness_Diagnostics.r")

# 8. Stage 5: Policy Counterfactual Simulation
cat("\n--- STEP 8: Running Stage 5 Policy Simulation ---\n")
source("Scripts/Stage5_Policy_Simulation.r")

cat("\n================================================================\n")
cat("   FULL ANALYSIS PIPELINE COMPLETE\n")
cat("================================================================\n")
