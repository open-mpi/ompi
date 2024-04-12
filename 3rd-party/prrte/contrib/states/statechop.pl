#!/usr/bin/env perl
# Fri Jun 05 2020  15:06:24PM EDT   Thomas Naughton <naughtont@ornl.gov>
#
# Process logs from PRRTE
# with '--prtemca prrte_state_base_verbose 1'
# enabled and the TJN timing patch applied
#
#  Input: Logfile from a testrun (`mpirun ... a.out >& LOG`)
# Output: Column oriented output with semicolon (`;`) separator
#
# Usage:
#   prterun --prtemca prrte_state_base_verbose 1 -np 1 ./a.out >& LOG
#   ./statechop.pl LOG > STATE_LOG.txt
#
####
use strict;

my $file = $ARGV[0];

if (! -f "$file") {
    print "ERROR: Failed to read input file '$file'\n";
    exit(1);
}

print "# File: $file\n";
open(FH, "<$file") or die "Error: failed to open '$file'\n";
my @data = <FH>;
chomp(@data);
close(FH);

print "# Node;  Job;  Timestamp;  JobState\n";
foreach my $line (@data) {
    next unless $line =~ /.* STATE .*/;
    next unless $line =~ /.* JOB .*/;

    # TJN: ignore the activating (reaching) msgs for now
    next if     $line =~ /.* ACTIVATING .*/;

    my ($a, $b) = split("STATE", $line);


    #a=([node0:31036] [[9239,0],0] [1585072455.946404] ACTIVATE JOB NULL )
    my ($node, $job, $timestamp, $other) = split(/ /, $a);

    my ($b1, $b2) = split(" AT ", $b);
    $b1 =~ s/^ //;

    #print " a=($a)       b=($b1)\n";
    print " $node;  $job;  $timestamp;  $b1\n";
}
