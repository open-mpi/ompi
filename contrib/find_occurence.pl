#!/usr/bin/perl

if (scalar(@ARGV) != 2) {
    print "Usage: #find_occurence <string> <source-path>\n";
    exit(3);
}

$search_string = @ARGV[0];
$source_path = @ARGV[1];

open (SOURCE_FILES, "find $source_path -name *.c |") || print "could not open the pipe\n";
while (<SOURCE_FILES>) {
    
    #open the file and delete the occurence
    
    $file_name = $_;
    
    open (FILE, "$file_name") || print "Could not open $file_name for reading\n";
    
    while (<FILE>) {
        if (/$search_string/) {
            print $file_name;
        }
    }
    close(FILE);
}

close(SOURCE_FILES);
