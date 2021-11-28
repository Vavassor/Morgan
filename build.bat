@echo.
@echo Deleting build files
@echo ====================
@del main.o
@del Morgan.nes
@del Morgan.map.txt
@del Morgan.labels.txt
@del Morgan.nes.ram.nl
@del Morgan.nes.0.nl
@del Morgan.nes.1.nl
@del Morgan.nes.dbg
@echo.
@echo Compiling
@echo ====================
"C:\cc65\bin\ca65" -g -o main.o main.s
@echo.
@echo Linking
@echo ====================
"C:\cc65\bin\ld65" -o Morgan.nes -C main.cfg -m Morgan.map.txt -Ln Morgan.labels.txt --dbgfile Morgan.nes.dbg main.o
@IF ERRORLEVEL 1 GOTO failure
@echo.
@echo Build succeeded!
@pause
@GOTO endbuild
:failure
@echo.
@echo Build failed!
@pause
:endbuild