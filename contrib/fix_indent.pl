#!/usr/bin/perl
#To keep brian happy

if (scalar(@ARGV) != 1) {
    print "We need a source tree path\n";
    exit(3);
}

$source_path = @ARGV[0];

open(HEADERS, "find $source_path -name *.h |");
while(<HEADERS>) {
    open(TEMP, ">temp.txt");
    $file_name = $_;
    print $file_name;
    open(FILE, "$file_name");
    while(<FILE>) {
        s/^(#)([\s|\t]*)(\w)/$1$3/;
        print TEMP;
    }
    close(TEMP);
    close(FILE);
    system("mv temp.txt $file_name");
}
close(HEADERS);

open(SOURCES, "find $source_path -name *.c |");
while(<SOURCES>) {
    open(TEMP, ">temp.txt");
    $file_name = $_;
    print $file_name;
    open(FILE, "$file_name");
    while(<FILE>) {
        s/^(#)([\s|\t]*)(\w)/$1$3/;
        print TEMP;
    }
    close(TEMP);
    close(FILE);
    system("mv temp.txt $file_name");
}
close(SOURCES);
