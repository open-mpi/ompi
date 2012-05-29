#!/usr/bin/env perl
#
# Copyright (c) 2012      Los Alamos National Security, Inc.
#                         All rights reserved.

use strict;

# globals
my $showme_arg = 0;
my $num_nodes = 0;
my $my_arg;
my $reps = 1;

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
  --reps                        Number of times to run each test (for statistics)
  --help | -h                   This help list\n";
        exit;
    } elsif ($my_arg eq "-showme" ||
               $my_arg eq "--showme") {
        $showme_arg = 1;
    } elsif ($my_arg eq "-nodes" ||
             $my_arg eq "--nodes") {
        $num_nodes = @ARGV[$i+1];
    } elsif ($my_arg eq "--reps" ||
             $my_arg eq "-reps") {
        $reps = @ARGV[$i+1];
    }
    $i++;
}

print "REPS " . $reps . "\n";
my $n = 1;
my $cmd;

my $test;
my $output;
my @lines;
my $line;
my @results;
my $res;
my $toggle;
print "\n--------------------------------------------------\n";
foreach $test (@tests) {
    if (-e $test) {
        # pre-position the executable
        $cmd = "mpirun -npernode 1 $test 2>&1";
        system($cmd);
        $n = 1;
        while ($n <= $num_nodes) {
            $cmd = "time mpirun -npernode 1 -max-vm-size " . $n . " $test 2>&1";
            print $cmd . "\n";
            if (0 == $showme_arg) {
                for (1..$reps) {
                    $toggle = 1;
                    $output = `$cmd`;
                    @lines = split(/\n/, $output);
                    foreach $line (@lines) {
                        if (0 <= index($line, "user") ||
                            0 <= index($line, "sys") ||
                            0 <= index($line, "real") ||
                            0 <= index($line, "elapsed")) {
                            @results = split(/\s+/,$line);
                            foreach $res (@results) {
                                print $res;
                                if (0 == $toggle) {
                                    print " ";
                                    $toggle = 1;
                                } else {
                                    print "    ";
                                    $toggle = 0;
                                }
                            }
                            print "\n";
                        }
                    }
                }
            }
            $n = 2 * $n;
        }
        if ($n != (2 * $num_nodes)) {
            $cmd = "time mpirun -npernode 1 $test 2>&1";
            print $cmd . "\n";
            if (0 == $showme_arg) {
                for (1..$reps) {
                    $output = `$cmd`;
                    $output =~ s/(.+)\n.*/$1/;
                    @results = split(/\s+/,$output);
                    print $results[0] . "    " . $results[1] . "    " . $results[2] . "\n";
                }
            }
        }
        print "\n--------------------------------------------------\n";
    } else {
        print "Test " . $test . " was not found - test skipped\n";
        print "\n--------------------------------------------------\n";
    }
}
