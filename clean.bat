@echo off
echo Cleaning up src directory and subdirectories...

:: Elimina file .bak
for /r "src" %%f in (*.bak) do if exist "%%f" del /f /q "%%f"
echo   Deleted .bak files

:: Elimina binari (.exe, .dll, .so)
for /r "src" %%f in (*.exe *.dll *.so) do if exist "%%f" del /f /q "%%f"
echo   Deleted .exe, .dll, .so files

:: Elimina file .lrs (disabilitato)
:: for /r "src" %%f in (*.lrs) do if exist "%%f" del /f /q "%%f"

:: Elimina risorse (.res)
for /r "src" %%f in (*.res) do if exist "%%f" del /f /q "%%f"
echo   Deleted .res files

echo Cleanup completed successfully!