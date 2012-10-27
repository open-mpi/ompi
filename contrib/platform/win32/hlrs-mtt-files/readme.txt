These folder contains files for running MTT on a Wiindows machine at HLRS.

To use it, one needs Cygwin+Perl and MTT source files. One point is that Cygwin has to be set with Visual Studio (add "call "%VS80COMNTOOLS%vsvars32.bat" >NUL:" to cygwin.bat).

run_mtt.bat and run_mtt-local.bat are the main batch file to start the test, one for submitting results to MTT server, one for run locally.

.mttrc and .mttrc-local includes corresponding settings for cygwin and different tests.

The .sh files are shell scripts that managing the open mpi versions and temp/output folders.

The .ini files are the MTT config files.