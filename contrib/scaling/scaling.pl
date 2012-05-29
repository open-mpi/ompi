#!/usr/bin/env perl
#
# Copyright (c) 2012      Los Alamos National Security, Inc.
#                         All rights reserved.

use strict;

# globals
my $showme_arg = 0;
my $num_nodes = 0;
my $my_arg;

my @tests = qw(/bin/true ./orte_no_op ./mpi_no_op ./mpi_barrier);

# Cannot use the usual GetOpts library as the user might
# be passing -options to us! So have to
# parse the options ourselves to look for help and showme
my $i = 0;
foreach $my_arg (@ARGV) {
    if ($my_arg eq "-h" ||
        $my_arg eq "--h" ||
        $my_arg eq "-help" ||
        $my_arg eq "--help") {
        print "Options:
  --showme                      Show the actual commands without executing them
  --nodes                       Number of nodes to run the test across
  --help | -h                   This help list\n";
        exit;
    } elsif ($my_arg eq "-showme" ||
               $my_arg eq "--showme") {
        $showme_arg = 1;
    } elsif ($my_arg eq "-nodes" ||
             $my_arg eq "--nodes") {
        $num_nodes = @ARGV[$i+1];
    }
    $i++;
}

my $n = 1;
my $cmd;

my $test;
foreach $test (@tests) {
    if (-e $test) {
        $n = 1;
        while ($n <= $num_nodes) {
            $cmd = "time mpirun -npernode 1 -max-vm-size " . $n . " $test";
            print $cmd . "\n";
            if (0 == $showme_arg) {
                system $cmd;
                print "\n";
            }
            $n = 2 * $n;
        }
        if ($n != (2 * $num_nodes)) {
            $cmd = "time mpirun -npernode 1 $test";
            if (1 == $showme_arg) {
                print $cmd . "\n";
            } else {
                system $cmd;
                print "\n";
            }
        }
        print "\n\n";
    } else {
        print "Test " . $test . " was not found - test skipped\n\n";
    }
}
