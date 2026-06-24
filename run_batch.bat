@echo off
cd /d "%~dp0"

if /I "%1"=="runjob" goto RUNJOB

schtasks /delete /tn "BatchTask" /f >nul 2>&1
schtasks /create /tn "BatchTask" /tr "\"%~dp0run_batch.bat\" runjob" /sc minute /mo 3 /f
goto END

:RUNJOB
bin\BATCH-UPDATE.EXE >> batch_log.txt 2>&1

:END