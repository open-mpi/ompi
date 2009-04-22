#!/usr/bin/perl
use Time::HiRes qw( gettimeofday );
($sec, $microsec) = gettimeofday;
$cmd = "mpirun -npernode " . @ARGV[0] . " ./ziatest " . @ARGV[0] . " " . $sec . " " . $microsec;
system($cmd);
