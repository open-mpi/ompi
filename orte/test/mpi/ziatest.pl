#!/usr/bin/perl
push(@INC, $ENV{HOME}."/my-perl-mods");
require Time::HiRes;
($s, $usec) = Time::HiRes::gettimeofday();
$cmd = "mpirun -npernode " . @ARGV[0] . " ./ziatest " . " $s " . $usec;
system($cmd);
