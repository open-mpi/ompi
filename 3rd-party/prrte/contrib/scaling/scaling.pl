#!/usr/bin/env perl
#
# Copyright (c) 2012      Los Alamos National Security, Inc.
#                         All rights reserved.
# Copyright (c) 2015-2016 Intel, Inc. All rights reserved.
# Copyright (c) 2017-2018 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.


use strict;
use Getopt::Long;

# globals
my $num_nodes = 2;
my $my_arg;
my $reps = 5;
my $usedvm = 0;
my $usesrun = 0;
my $usempirun = 0;
my $useaprun = 0;
my $useaprun = 0;
my $myapp;
my $runall = 1;
my $rawoutput = 0;
my $myresults = "myresults";
my $ppn = 1;
my $npmin = 1;
my @csvrow;
my $multiplier = 1;

my @tests = qw(/bin/true ./prte_no_op ./mpi_no_op ./mpi_no_op ./mpi_no_op);
my @options = ("", "", "", "-mca mpi_add_procs_cutoff 0 -mca pmix_base_async_modex 1 -mca pmix_base_collect_data 0", "-mca mpi_add_procs_cutoff 0 -mca pmix_base_async_modex 1 -mca async_mpi_init 1 -mca async_mpi_finalize 1 -mca pmix_base_collect_data 0");
my @starterlist = qw(mpirun prun srun aprun);
my @starteroptionlist = (" --novm --timeout 600",
                         " --system-server-only",
                         " --distribution=cyclic",
                         "");

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
    "results=s" => \$myresults,
    "rawout" => \$rawoutput,
    "ppn=s" => \$ppn,
    "multiplier=s" => \$multiplier,
    "npmin=s" => \$npmin,
) or die "unable to parse options, stopped";

if ($HELP) {
    print "$0 [options]

--help | -h          This help message
--quiet | -q         Only output critical messages to stdout
--showme             Show the actual commands without executing them
--reps=s             Number of times to run each test (for statistics)
--mpirun             Use mpirun (or its equivalent prterun)
--dvm                Use prte-dvm to execute the test
--srun               Use srun (if available) to execute the test
--arpun              Use aprun (if available) to execute the test
--myapp=s            In addition to the standard tests, run this specific application (including any args)
--results=file       File where results are to be stored in comma-separated value format
--rawout             Provide raw timing output to the file
--ppn=n              Run n procs/node
--multiplier=n       Run n daemons/node (only for DVM and mpirun)
--npmin=n            Minimal number of nodes
";
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
my @starters;
my @starteroptions;
my $pid;

# if they explicitly requested specific starters, then
# only use those
if ($useaprun || $usempirun || $usesrun || $usedvm) {
    $runall = 0
}

# if they didn't specify something, then set all starters to requested
if ($runall) {
    $useaprun = 1;
    $usempirun = 1;
    $usesrun = 1;
    $usedvm = 1;
}

# see which starters are available
my @path = split(":", $ENV{PATH});
my $exists = 0;
my $opt;
$idx=0;
foreach $starter (@starterlist) {
    $exists = 0;
    foreach my $path (@path) {
        if ( -x "$path/$starter") {
            $exists = 1;
            last;
        }
    }
    if ($exists) {
        if ($usedvm && $starter eq "prun") {
            push @starters, $starter;
            $opt = $starteroptionlist[$idx] . " --npernode " . $ppn;
            push @starteroptions, $opt;
        } elsif ($usempirun && $starter eq "mpirun") {
            push @starters, $starter;
            $opt = $starteroptionlist[$idx] . " --npernode " . $ppn;
            if ($multiplier gt 1) {
                $opt = $opt . " --mca rtc ^hwloc --mca ras_base_multiplier " . $multiplier;
            }
            push @starteroptions, $opt;
        } elsif ($useaprun && $starter eq "aprun") {
            push @starters, $starter;
            $opt = $starteroptionlist[$idx] . " -N " . $ppn;
            push @starteroptions, $opt;
        } elsif ($usesrun && $starter eq "srun") {
            push @starters, $starter;
            $opt = $starteroptionlist[$idx] . " --ntasks-per-node " . $ppn;
            push @starteroptions, $opt;
        }
    }
    $idx = $idx + 1;
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
$cmd = "mpirun --pernode hostname";
$output = `$cmd`;
@lines = split(/\n/, $output);
$num_nodes = $#lines + 1;

# get the local date and time
my ($sec,$min,$hour,$day,$month,$yr19,@rest) =   localtime(time);

my $pstarts = join(", ", @starters);
# start by printing out the resulting configuration
print "\n--------------------------------------------------\n";
print "\nTest configuration:\n";
print "\tDate:\t" . "$day-".++$month. "-".($yr19+1900) . " " . sprintf("%02d",$hour).":".sprintf("%02d",$min).":".sprintf("%02d",$sec) . "\n";;
print "\tNum nodes:\t" . $num_nodes . "\n";
print "\tStarters:\t" . $pstarts . "\n";
print "\n--------------------------------------------------\n";

# and tag the output file as well
if ($myresults) {
    print FILE "Test configuration:\n";
    print FILE "Date:\t" . "$day-".++$month. "-".($yr19+1900) . " " . sprintf("%02d",$hour).":".sprintf("%02d",$min).":".sprintf("%02d",$sec) . "\n";;
    print FILE "Num nodes:\t" . $num_nodes . "\n";
    print FILE "Starters:\t" . $pstarts . "\n";
}

my $index = 0;

sub runcmd()
{
    my $rc;
    for (1..$reps) {
        $output = `$cmd`;
        # Check the error code of the command; if the error code is alright
        # just add a 0 in front of the number to neutraly mark the success;
        # If the code is not correct, add a ! in front of the number to mark
        # it invalid.
        if($? != 0) {
            $rc = "0";
        }
        else {
            $rc = "!";
        }
        if ($myresults && $rawoutput) {
            print FILE $n . " " . $output . " $rc\n";
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
                                push @csvrow,join $rc,$results[1];
                            } else {
                                # it must be in the prior location
                                push @csvrow,join $rc,$results[$idx-1];
                            }
                        } else {
                            # take the portion of the string up to the tag
                            push @csvrow,join $rc,substr($res, 0, $strloc);
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
                                    push @csvrow,join $rc,$results[1];
                                } else {
                                    # it must be in the prior location
                                    push @csvrow,join $rc,$results[$idx-1];
                                }
                            } else {
                                # take the portion of the string up to the tag
                                push @csvrow,join $rc,substr($res, 0, $strloc);
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
    my $dvmout;
    print "STARTER: $starter\n";
    # if we are going to use the dvm, then we
    if ($starter eq "prun") {
        my $dvm = "prte-dvm --system-server";
        if ($multiplier gt 1) {
            $dvm = $dvm . " --mca rtc ^hwloc --mca ras_base_multiplier " . $multiplier;
        }
        # need to start it
        print "##DVM: Launching $dvm\n";
        if ($myresults) {
            print FILE "\n\n$dvm\n";
        }
        if (!$SHOWME) {
            $havedvm = open($dvmout, $dvm."|") or die "##DVM: Spawn error $!\n";
            print "##DVM: pid=$havedvm\n";
            # Wait that the dvm reports that it is ready
            my $waitready = <$dvmout>;
            if($waitready =~ /DVM ready/i) {
                print "##DVM: $waitready\n";
            }
            else {
                die "##DVM: error: $waitready\n";
            }
        }
    } else {
        if ($myresults) {
            print FILE "\n\n";
        }
    }

    if ($myresults) {
        print FILE "$starter $starteroptions[$index]\n\n";
    }
    my $testnum = 0;
    foreach $test (@tests) {
        $option = $options[$testnum];
        if ($starter eq "aprun") {
            $option =~ s/-mca\s+(\S+)\s+(\S+)/-e OMPI_MCA_$1=$2/g;
        }
        if ($starter eq "srun") {
            $option =~ s/-mca\s+(\S+)\s+(\S+)\s*/OMPI_MCA_$1=$2,/g;
            $option =~ s/\s*(OMPI_MCA\S+)/ --export=$1ALL/g;
        }
        if (-e $test) {
            if ($myresults) {
                print FILE "#nodes,$test,$option\n";
            }
            if (!$SHOWME) {
                # pre-position the executable
                $cmd = $starter . $starteroptions[$index] . " $test 2>&1";
                my $error;
                $error = `$cmd`;
                if (0 != $error) {
                    if ($myresults) {
                        print FILE "Command $cmd returned error $error\n";
                        $testnum = $testnum + 1;
                        next;
                    }
                }
            }
            $n = $npmin;
            while ($n <= $num_nodes) {
                push @csvrow,$n;
                if ($starter eq "prun" or $starter eq "mpirun" or $starter eq "aprun") {
                    my $np = $n * $ppn;
                    $cmd = "time " . $starter . " " . $starteroptions[$index] . " $option -n $np $test 2>&1";
                } else {
                    $cmd = "time " . $starter . " " . $starteroptions[$index] . " $option -N $n $test 2>&1";
                }
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
        if ($starter eq "srun" or $starter eq "aprun") {
            if ($testnum ge 3) {
                last;
            }
        }
    }
    if ($havedvm) {
        if (!$SHOWME) {
            $cmd = "prun --system-server-only --terminate";
            system($cmd);
            waitpid($havedvm, 0);
        }
        $havedvm = 0;
    }
    $index = $index + 1;
}

if ($myresults) {
    close(FILE);
}
