#!/usr/bin/perl
#
# $HEADER$
#

if (scalar(@ARGV) != 1) {
    print "Usage: generate_file_list <diff file>\n";
    exit(3);
}

$file_name = @ARGV[0];
open(FILE,"$file_name") || print "File count not be opened\n";
open(TEMP,"> file_list") || print "Could not open file for writing\n";
while (<FILE>) {
    if (/Index/) {
        s/^Index:\s*//g;
        print TEMP;
    }
}
close(TEMP);
close($file_name);

