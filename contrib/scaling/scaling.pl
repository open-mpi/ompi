#!/usr/bin/env perl
#
# Copyright (c) 2012      Los Alamos National Security, Inc.
#                         All rights reserved.
# Copyright (c) 2015      Intel, Inc. All rights reserved.

use strict;
use Getopt::Long;

# globals
my $num_nodes = 2;
my $my_arg;
my $reps = 1;
my $usedvm = 0;
my $usesrun = 0;
my $usempirun = 0;
my $runall = 0;

my @tests = qw(/bin/true ./orte_no_op ./mpi_no_op ./mpi_no_op);
my @options = ("", "", "", "-mca mpi_add_procs_cutoff 0 -mca pmix_base_async_modex 1");
my @starters = qw(mpirun orte-submit srun orterun);
my @starteroptions = ("-npernode 1 --novm",
                      "--hnp file:dvm_uri -pernode",
                      "--distribution=cyclic",
                      "-npernode 1 --novm");

# Set to true if the script should merely print the cmds
# it would run, but don't run them
my $SHOWME = 0;
# Set to true to suppress most informational messages.
my $QUIET = 0;
# Set to true if we just want to see the help message
my $HELP = 0;

GetOptions(
    "help" => \$HELP,
    "quiet" => \$QUIET,
    "showme" => \$SHOWME,
    "reps=s" => \$reps,
    "dvm" => \$usedvm,
    "srun" => \$usesrun,
    "mpirun" => \$usempirun,
    "all" => \$runall,
) or die "unable to parse options, stopped";

if ($HELP) {
    print <<EOT;
$0 [options]

--help | -h          This help message
--quiet | -q         Only output critical messages to stdout
--showme             Show the actual commands without executing them
--reps               Number of times to run each test (for statistics)
--mpirun             Use only mpirun (or its equivalent orterun)
--dvm                Use only orte-dvm to execute the test
--srun               Use only srun to execute the test
--all                Use all available start commands [default]
EOT
    exit(0);
}

my $n = 1;
my $cmd;
my $starter;
my $test;
my $output;
my @lines;
my $line;
my @results;
my $res;
my $toggle;
my $idx;
my $option;
my $havedvm = 0;

# see which starters are available
my @path = split(":", $ENV{PATH});
my $exists = 0;
$idx=0;
while ($idx <= $#starters) {
    $starter = $starters[$idx];
    $exists = 0;
    foreach my $path (@path) {
        if ( -x "$path/$starter") {
            $exists = 1;
            last;
        }
    }
    unless ($exists) {
        # remove this one from the list
        splice @starters, $idx, 1;
        splice @starteroptions, $idx, 1;
        # adjust the index
        $idx = $idx - 1;
    } elsif ($usedvm && $starter ne "orte-submit") {
        # remove this one from the list
        splice @starters, $idx, 1;
        splice @starteroptions, $idx, 1;
        # adjust the index
        $idx = $idx - 1;
    } elsif ($usesrun && $starter ne "srun") {
        # remove this one from the list
        splice @starters, $idx, 1;
        splice @starteroptions, $idx, 1;
        # adjust the index
        $idx = $idx - 1;
    } elsif ($usempirun && (($starter ne "mpirun") && ($starter ne "orterun"))) {
        # remove this one from the list
        splice @starters, $idx, 1;
        splice @starteroptions, $idx, 1;
        # adjust the index
        $idx = $idx - 1;
    }
    $idx = $idx + 1;
}

# if both mpirun and orterun are present, then
# we don't need to run both as they are just
# symlinks to each other
$exists = 0;
foreach my $path (@path) {
    if ( -x "$path/mpirun") {
        $idx=0;
        foreach $starter (@starters) {
            if ($starter eq "orterun") {
                splice @starters, $idx, 1;
                splice @starteroptions, $idx, 1;
                last;
            }
            $idx = $idx + 1;
        }
        if ($exists) {
            last;
        }
    }
}

# bozo check
if (scalar @starters == 0) {
    print "No available starters\n";
    exit;
}

# if we are going to use the dvm, then we
# need to start it
if (-e "dvm_uri") {
    system("rm -f dvm_uri");
}
foreach $starter (@starters) {
    if ($starter eq "orte-submit") {
        $cmd = "orte-dvm --report-uri dvm_uri 2>&1 &";
        print $cmd . "\n";
        if (!$SHOWME) {
            system($cmd);
            # wait for the rendezvous file to appear
            while (! -e "dvm_uri") {
                sleep(1);
            }
            $havedvm = 1;
        }
    }
}

# determine the number of nodes - doesn't
# matter which starter we use
$cmd = $starters[0] . " " . $starteroptions[0] . " hostname";
$output = `$cmd`;
@lines = split(/\n/, $output);
$num_nodes = $#lines + 1;


print "\n--------------------------------------------------\n";

my $index = 0;
foreach $starter (@starters) {
    my $testnum = 0;
    foreach $test (@tests) {
        $option = $options[$testnum];
        if (-e $test) {
            if (!$SHOWME) {
                # pre-position the executable
                $cmd = $starter . $starteroptions[$index] . " $test 2>&1";
                system($cmd);
            }
            $n = 1;
            while ($n <= $num_nodes) {
                $cmd = "time " . $starter . " " . $starteroptions[$index] . " -np $n $option $test 2>&1";
                print $cmd . "\n";
                if (!$SHOWME) {
                    for (1..$reps) {
                        $toggle = 1;
                        $output = `$cmd`;
                        print $output . "\n";
                        @lines = split(/\n/, $output);
                        foreach $line (@lines) {
                            if (0 <= index($line, "user") ||
                                0 <= index($line, "sys") ||
                                0 <= index($line, "real") ||
                                0 <= index($line, "elapsed")) {
                                $idx = 0;
                                @results = split(/\s+/,$line, 4);
                                foreach $res (@results) {
                                    if ($idx < 3) {
                                        print $res;
                                        if (0 == $toggle) {
                                            print " ";
                                            $toggle = 1;
                                        } else {
                                            print "    ";
                                            $toggle = 0;
                                        }
                                    }
                                    $idx = $idx + 1;
                                }
                                print "\n";
                            }
                        }
                    }
                    print "\n";
                }
                $n = 2 * $n;
            }
            if ($n < $num_nodes) {
                $cmd = "time " . $starter . " " . $starteroptions[$index] . " $option $test 2>&1";
                print $cmd . "\n";
                if (!$SHOWME) {
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
        $testnum = $testnum + 1;
    }
    $index = $index + 1;
}

if ($havedvm) {
    if (!$SHOWME) {
        $cmd = "orte-submit --hnp file:dvm_uri --terminate";
        system($cmd);
    }
    if (-e "dvm_uri") {
        system("rm -f dvm_uri");
    }
}
