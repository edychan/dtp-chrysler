@ECHO OFF
set obj=\dtp\lib
set lib=\dtp\lib
cd..\obj
BLINKER @BRAC
copy dtp.exe \dtp\chrysler\dtp\dtp.exe
DEL dtp.EXE
cd..\prg

