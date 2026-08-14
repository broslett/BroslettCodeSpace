@echo off
REM Run a saved pipeline:  run-pipeline.bat <name> [options]
cd /d "%~dp0"
Rscript run_pipeline.R %*
