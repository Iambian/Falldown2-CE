@echo off
del /Q obj\* >nul 2>&1
tools\spasm64 -E src\game.asm obj\game.bin
tools\zx7 obj\game.bin obj\game.zx7
tools\spasm64 -E src\main.asm bin\FALLDOWN.8xp






