#!/usr/bin/perl
use Time::HiRes qw( gettimeofday );
($sec, $microsec) = gettimeofday;
$cmd = "mpirun -n " . @ARGV[0] . " ./ziatest " . @ARGV[1] . " " . $sec . " " . $microsec;
system($cmd);
