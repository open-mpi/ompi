#!/usr/bin/env perl

# Make a simple data file of a argv[0]-specified size (understand "k",
# "m", and "g" suffixes) of a repeating pattern.

use strict;

my $size_arg = $ARGV[0];
$size_arg =~ m/^(\d+)/;
my $size = $1;
$size_arg =~ m/([mkg])$/i;
my $size_unit = lc($1);

if ($size_unit eq "k") {
    $size *= 1024;
} elsif ($size_unit eq "m") {
    $size *= 1048576;
} elsif ($size_unit eq "g") {
    $size *= 1073741824;
}
print "Generating size $size\n";

my $file = "data-" . lc($size_arg);
unlink($file);

my $line = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ!@#$%^&*()_+=-;";
my $line_len = length($line);
$line .= $line;

open(FILE, ">$file") || die "Can't open file $file";
my $count = 0;
my $line_count = 0;
while ($count < $size) {
    my $offset = $line_count % $line_len;
    my $num_to_print =
        ($size - $count < $line_len) ? $size - $count : $line_len;
    if ($num_to_print > 0) {
        my $printable = substr($line, $offset, $num_to_print - 1);
        print FILE $printable . "\n";
        $count += $num_to_print;
    }
    ++$line_count;
}
close(FILE);

exit(0);
