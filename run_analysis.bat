@echo off
"C:\Program Files\R\R-4.5.1\bin\Rscript.exe" "C:\Users\ashwin\Documents\Formal_Informal\Scripts\run_analysis.r" > "C:\Users\ashwin\Documents\Formal_Informal\analysis_log.txt" 2>&1
echo Exit: %ERRORLEVEL%
