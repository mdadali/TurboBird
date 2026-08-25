Write-Host "Cleaning up src directory..." -ForegroundColor Cyan

# Delete all .bak files (backups)
Get-ChildItem -Path "src" -Recurse -Filter "*.bak" -File -ErrorAction SilentlyContinue | Remove-Item -Force
Write-Host "  Deleted .bak files" -ForegroundColor Green

# Delete compiled binaries (.exe, .dll, .so)
# Nota: 'src\*' e ' -Depth 100' servono a far funzionare l'Include in tutte le sottocartelle
Get-ChildItem -Path "src\*" -Recurse -File -Include "*.exe", "*.dll", "*.so" -ErrorAction SilentlyContinue | Remove-Item -Force
Write-Host "  Deleted .exe, .dll, .so files" -ForegroundColor Green

# Delete all .lrs files (can be regenerated on build)
# Get-ChildItem -Path "src" -Recurse -Filter "*.lrs" -File -ErrorAction SilentlyContinue | Remove-Item -Force
# Write-Host "  Deleted .lrs files" -ForegroundColor Green

# Delete all .res files (compiled resources)
Get-ChildItem -Path "src" -Recurse -Filter "*.res" -File -ErrorAction SilentlyContinue | Remove-Item -Force
Write-Host "  Deleted .res files" -ForegroundColor Green

Write-Host "Cleanup completed successfully!" -ForegroundColor Green