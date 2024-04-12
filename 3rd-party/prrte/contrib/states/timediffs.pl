#!/usr/bin/env perl
# Fri Jun 05 2020  15:06:24PM EDT   Thomas Naughton <naughtont@ornl.gov>
#
# Compare and show time differences based on log output when running
# with '--prtemca prrte_state_base_verbose 1'
#
#  Input: List of time stamps (first line is considered start/first)
# Output: Time offsets for each step (relative to the first and predecessor)
#
# Usage:
#   ./timediffs.pl TIME_LOG
#    --or--
#   cat TIME_LOG | ./timediffs.pl -
#
# Example:
#    # Ignore comments, and
#    # and get only the HNP (0th jobID "[9239,0]")
#  ./statechop.pl logfile-ex1.txt\
#     | grep -v '^#' \
#     | grep '9239,0],0' \
#     > TIME_LOG
#
#  ./timediffs.pl TIME_LOG
#
#  --or--
#    # Avoid logfile and use STDIN
#  ./statechop.pl logfile-ex1.txt \
#     | grep -v '^#' \
#     | grep '9239,0],0' \
#     | ./timediffs.pl -
#
# ChangeLog:
#  - v0.2 add support to show event name with timing info
#  - v0.1 Initial script
####
use strict;

my $VERSION="v0.2";

# DEBUG (show JobID) (set to 1 to enable)
my $DBG_SHOW_JOBID=0;

# Trim whitespace from front/rear
sub trim($)
{
    my $str = shift;
    $str =~ s/^\s*//;  # Trim leading space
    $str =~ s/\s*$//;  # Trim trailing space
    return ($str);
}

####
# Process line of input into separate record fields,
# returning array with two values: [0]time, [1]evtname, [2]jobid
#
# Example input:
#  [batch4:39389];  [[8240,0],0];  [1585138484.185501];  PENDING ALLOCATION
#
# Which is: Node;   JobID;         TimeStamp;            EventName
####
sub get_evt_info($)
{
    my $str = shift;

    my @fields = split(/;/, $str);
    my @arr;

    $arr[0] = get_value($fields[2]);
    $arr[1] = trim($fields[3]);
    if ($DBG_SHOW_JOBID) {
        $arr[2] = trim($fields[1]);
    } else {
        $arr[2] = "";
    }

    return (@arr);
}

sub get_value($)
{
    my $val = shift;

    $val = trim($val);
    $val =~ s/^\[//;   # Trim leading bracket
    $val =~ s/\]$//;   # Trim trailing bracket

    return ($val);
}

sub get_value_diff($$)
{
    my $val  = shift;   # event time
    my $base = shift;   # base  time (to compare against)

    my $diff = 0.0;

    $diff = $base - $val;

    return ($diff);
}


#
# MAIN
#

my $file = $ARGV[0];
print "DBG: FILE: $file\n";

if ("$file" =~ /^-$/) {
    # Read from stdin
    #print "DBG: GET STDIN\n";
} elsif (! -f "$file") {
    print "ERROR: Failed to read input file '$file'\n";
    exit(1);
}

my @data;
my @input;

if ("$file" =~ /^-$/) {
    # Read from stdin
    #print "# File: STDIN\n";
    @input= <STDIN>;
    chomp(@input);
} else {
    #print "# File: $file\n";
    open(FH, "<$file") or die "Error: failed to open '$file'\n";
    @input = <FH>;
    chomp(@input);
    close(FH);
}


# Skip all comments from input
foreach my $i (@input) {
    next if $i =~ /^\s*#/;
    push @data, $i;
}

#     Node             JobID             TimeStamp          EventName
#  [batch4:39389];  [[8240,0],0];  [1585138484.185501];  PENDING ALLOCATION
my $base = shift(@data);  # Get first line of data and treat as "base" time
my ($base_time, $base_evtname, $jobid) = get_evt_info($base);

print "####################################################\n";
print "#  EvtTime -- actual event time\n";
print "# BaseDiff -- difference from initial  base  time\n";
print "#  RelDiff -- difference from previous event time\n";
print "####################################################\n";
print "# EvtTime    \t      BaseDiff   \t  RelDiff  \t  EvtName\n";
print "#------------        ------------     ------------       -------------\n";
printf " %0.6f \t    n/a \t   n/a  \t  %s \t  %s\n", $base_time, $base_evtname, $jobid;

my $prev_time = $base_time;

foreach my $line (@data) {

    my ($evt_time, $evt_name, $jobid) = get_evt_info($line);
    my $evt_diff = get_value_diff($evt_time, $base_time);

    my $relative_diff = get_value_diff($evt_time, $prev_time);
    $prev_time    = $evt_time;

    printf " %0.6f \t  %0.6f \t %0.6f  \t  %s \t  %s\n", $evt_time, $evt_diff, $relative_diff, $evt_name, $jobid;
}
