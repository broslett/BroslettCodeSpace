@echo off
REM Launch the Data Pipeline Builder. Binds to 127.0.0.1 only -- see SECURITY.md.
cd /d "%~dp0"
where Rscript >nul 2>nul || (echo Rscript not found. Install R 4.1 or newer, and add it to PATH. & exit /b 1)
if "%DATAPIPE_PORT%"=="" set DATAPIPE_PORT=8080
echo Starting on http://127.0.0.1:%DATAPIPE_PORT%  (Ctrl-C to stop)
Rscript app.R
