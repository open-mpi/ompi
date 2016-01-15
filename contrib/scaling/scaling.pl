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
my $useaprun = 0;
my $useaprun = 0;
my $myapp;
my $runall = 0;
my $rawoutput = 0;
my $myresults;
my @csvrow;

my @tests = qw(/bin/true ./orte_no_op ./mpi_no_op ./mpi_no_op);
my @options = ("", "", "", "-mca mpi_add_procs_cutoff 0 -mca pmix_base_async_modex 1");
my @starters = qw(mpirun orte-submit srun aprun orterun);
my @starteroptions = ("-npernode 1 --novm",
                      "--hnp file:dvm_uri -pernode",
                      "--distribution=cyclic",
                      "-N 1",
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
    "aprun" => \$useaprun,
    "mpirun" => \$usempirun,
    "myapp=s" => \$myapp,
    "all" => \$runall,
    "results=s" => \$myresults,
    "rawout" => \$rawoutput,
) or die "unable to parse options, stopped";

if ($HELP) {
    print <<EOT;
$0 [options]

--help | -h          This help message
--quiet | -q         Only output critical messages to stdout
--showme             Show the actual commands without executing them
--reps=s             Number of times to run each test (for statistics)
--mpirun             Use only mpirun (or its equivalent orterun)
--dvm                Use only orte-dvm to execute the test
--srun               Use only srun (if available) to execute the test
--arpun              Use only aprun (if available) to execute the test
--myapp=s            In addition to the standard tests, run this specific application (including any args)
--all                Use all available start commands [default]
--results=file       File where results are to stored in comma-separated value format
--rawout             Provide raw timing output to the file
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
    } elsif ($useaprun && $starter ne "aprun") {
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

# if they gave us an app, add it to the list of tests
if ($myapp) {
    push @tests, $myapp;
}

if ($myresults) {
    # open the results file
    open FILE, ">$myresults" || die "file could not be opened";
}

# determine the number of nodes - doesn't
# matter which starter we use
$cmd = $starters[0] . " " . $starteroptions[0] . " hostname";
print "CMD: $cmd\n";
$output = `$cmd`;
print "$output\n";
@lines = split(/\n/, $output);
$num_nodes = $#lines + 1;

# collect the complete list of starters
my $mystarters;
$idx=1;
$mystarters = $starters[0];
while ($idx < $#starters) {
    $mystarters = $mystarters . "," . $starters[$idx];
    $idx = $idx + 1;
}

# get the local date and time
my ($sec,$min,$hour,$day,$month,$yr19,@rest) =   localtime(time);

# start by printing out the resulting configuration
print "\n--------------------------------------------------\n";
print "\nTest configuration:\n";
print "\tDate:\t" . "$day-".++$month. "-".($yr19+1900) . " " . sprintf("%02d",$hour).":".sprintf("%02d",$min).":".sprintf("%02d",$sec) . "\n";;
print "\tNum nodes:\t" . $num_nodes . "\n";
print "\tStarters:\t" . $mystarters . "\n";
print "\n--------------------------------------------------\n";

# and tag the output file as well
if ($myresults) {
    print FILE "Test configuration:\n";
    print FILE "Date:\t" . "$day-".++$month. "-".($yr19+1900) . " " . sprintf("%02d",$hour).":".sprintf("%02d",$min).":".sprintf("%02d",$sec) . "\n";;
    print FILE "Num nodes:\t" . $num_nodes . "\n";
    print FILE "Starters:\t" . $mystarters . "\n";
}

my $index = 0;

sub runcmd()
{
    for (1..$reps) {
        $output = `$cmd`;
        if ($myresults && $rawoutput) {
            print FILE $n . " " . $output . "\n";
        }
        @lines = split(/\n/, $output);
        foreach $line (@lines) {
            if (0 <= index($line, "real") ||
                0 <= index($line, "elapsed")) {
                # we know that at least one item of interest is
                # in this line, so let's look for it - start
                # by getting rid of any leading whitespace
                $line =~ s/^\s+//;
                @results = split (/ +/,$line);
                $idx = 0;
                foreach $res (@results) {
                    # we are only interested in the real or elapsed time
                    my $strloc = index($res, "real");
                    if (0 <= $strloc) {
                        # some systems put the number in front of
                        # this word, and some append the word to
                        # the number - consider both cases
                        if (0 == $strloc) {
                            if (0 == $idx) {
                                # it must be in the next location
                                push @csvrow,$results[1];
                            } else {
                                # it must be in the prior location
                                push @csvrow,$results[$idx-1];
                            }
                        } else {
                            # take the portion of the string up to the tag
                            push @csvrow,substr($res, 0, $strloc);
                        }
                    } else {
                        $strloc = index($res, "elapsed");
                        if (0 <= $strloc) {
                            # some systems put the number in front of
                            # this word, and some append the word to
                            # the number - consider both cases
                            if (0 == $strloc) {
                                if (0 == $idx) {
                                    # it must be in the next location
                                    push @csvrow,$results[1];
                                } else {
                                    # it must be in the prior location
                                    push @csvrow,$results[$idx-1];
                                }
                            } else {
                                # take the portion of the string up to the tag
                                push @csvrow,substr($res, 0, $strloc);
                            }
                        }
                    }
                    $idx = $idx + 1;
                }
            }
        }
    }
    # we have now completed all the reps, so log the results
    if ($myresults) {
        my $myout;
        my $mycnt=0;
        while ($mycnt <= $#csvrow) {
            if (0 == $mycnt) {
                $myout = $csvrow[$mycnt];
            } else {
                $myout = $myout . "," . $csvrow[$mycnt];
            }
            $mycnt = $mycnt + 1;
        }
        print FILE "$myout\n";
        # clear the output
        @csvrow = ();
    }
    print "\n";
}

foreach $starter (@starters) {
    # if we are going to use the dvm, then we
    if ($starter eq "orte-submit") {
        # need to start it
        if (-e "dvm_uri") {
            system("rm -f dvm_uri");
        }
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

    if ($myresults) {
        print FILE "\n\n$starter\n\n";
    }
    my $testnum = 0;
    foreach $test (@tests) {
        $option = $options[$testnum];
        if (-e $test) {
            if ($myresults) {
                print FILE "#nodes,$test\n";
            }
            if (!$SHOWME) {
                # pre-position the executable
                $cmd = $starter . $starteroptions[$index] . " $test 2>&1";
                system($cmd);
            }
            $n = 1;
            while ($n <= $num_nodes) {
                push @csvrow,$n;
                $cmd = "time " . $starter . " " . $starteroptions[$index] . " -n $n $option $test 2>&1";
                print $cmd . "\n";
                if (!$SHOWME) {
                    runcmd();
                }
                $n = 2 * $n;
            }
            if (0 != $num_nodes & $n) {
                $cmd = "time " . $starter . " " . $starteroptions[$index] . " $option $test 2>&1";
                print $cmd . "\n";
                if (!$SHOWME) {
                    runcmd();
                }
            }
            print "\n--------------------------------------------------\n";
        } else {
            print "Test " . $test . " was not found - test skipped\n";
            print "\n--------------------------------------------------\n";
        }
        $testnum = $testnum + 1;
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
    $index = $index + 1;
}

if ($myresults) {
    close(FILE);
}

