@echo off

call "%VS100COMNTOOLS%vsvars32.bat"

C:
chdir d:\tools\cygwin\bin

REM wmic path win32_networkadapter where index=14 call disable

d:\tools\cygwin\bin\bash --rcfile d:\tools\MTT-HLRS\mtt-files\.mttrc

REM wmic path win32_networkadapter where index=14 call enable
