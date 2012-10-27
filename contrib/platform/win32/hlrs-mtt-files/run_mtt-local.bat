@echo off

call "%VS100COMNTOOLS%vsvars32.bat"

C:
chdir C:\Users\hpcfan\Documents\tools\cygwin\bin

wmic path win32_networkadapter where index=14 call disable

bash --rcfile ~/Documents/tools/MTT-HLRS/mtt-files/.mttrc-local

wmic path win32_networkadapter where index=14 call enable
