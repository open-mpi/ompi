#!/usr/bin/perl
use Time::HiRes qw( gettimeofday );
($sec, $microsec) = gettimeofday;
$cmd = "mpirun -npernode " . @ARGV[0] . " ./ziatest " . " $sec " . $microsec;
system($cmd);
