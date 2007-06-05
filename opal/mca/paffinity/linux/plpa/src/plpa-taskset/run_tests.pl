#!/usr/bin/env perl

use strict;

open(FILE, "test_cases.txt") or die "Can't open test_cases.txt file";
my $test_num = 1;
while (<FILE>) {
    chomp;
    /(\S+)\s+(\S+)/m;
    my $in = $1;
    my $expected_out = $2;
    if ($in ne "") {
        my $stdout = `./plpa_taskset -tc $in`;
        chomp $stdout;
        $stdout =~ /(.+?)\n(.+)/;
        my $real_out = $2;
        print "Test $test_num: ";
        if ($real_out ne $expected_out) {
            print "FAILED\n  In: $in\n  Out: $real_out\n  Expected: $expected_out\n";
        } else {
            print "PASSED\n";
        }
        ++$test_num;
    }
}
close(FILE);
