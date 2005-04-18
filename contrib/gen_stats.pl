#!/usr/bin/perl

# Currently, we implement only the directory option
# The following features have been requested for
#
# 1. Get the name of the directory and produce a 
#    statistic for all the files which have been touched
#    in all the subdirectories starting from that 
#    directory
#
# 2. Since gcov spits out a statistic for all the header
#    files included by all the source files, we need to 
#    aggregate them somehow into a single file. This might
#    have to be done manually by going through all the lines
#    which have been executed in that file since a header 
#    file may have multiple inclusions meaning that they
#    might have multiple .gcov files in different directories
#
# 3. Support command line parsing of arguments
#
# 4. Support giving a listing of all the files which have
#    been covered less than a certain percentage, say for
#    eg., all files which have been covered less that 5%
#

use strict;
my $ret = open(DIRFILES, "< ./dir_list");
if ($ret < 0) {
      print "ERROR: could not open directory listing\n";
      exit(3);
}

system("rm -f coverage_stats.txt zero_coverage.txt touched_files.txt untouched_files.txt");

open (COVERAGE_STATS, "> coverage_stats.txt");
open (ZERO_COVERAGE, "> zero_coverage.txt");

print COVERAGE_STATS "#Index                             Filename                          Directory                 Usage(%)\n";
print COVERAGE_STATS "#======================================================================================================\n";
print ZERO_COVERAGE "#Index                             Filename                          Directory                 Usage(%)\n";
print ZERO_COVERAGE "#======================================================================================================\n";

close(COVERAGE_STATS);
close(ZERO_COVERAGE);

my $k = 0;
my $l = 0;

while(<DIRFILES>) {
    chomp();
    my $c_files = `find $_ -name \"*.c\"`;
    my $cc_files = `find $_ -name \"*.cc\"`;
    $cc_files =~ s/\.cc//g;
    $c_files =~ s/\.c//g;
    $c_files = $c_files . $cc_files;
    my @C_FILES = split(/\n/, $c_files);
    
    my $da_files = `find $_ -name \"*.da\"`;
    $da_files =~ s/\.da//g;
    my @DA_FILES = split(/\n/, $da_files);

    open (TEMP1, "> temp1");
    open (TEMP2, "> temp2");
    print TEMP1 $c_files;
    print TEMP2 $da_files;
    close(TEMP1);
    close(TEMP2);

    # Now do the manual diff 
    open(TEMP1, "< temp1");
    open(UNTOUCHED_FILES, ">> untouched_files.txt");
    open(TOUCHED_FILES, ">> touched_files.txt");

    while(<TEMP1>) {
        my $c_file = $_;
        my $found = 0;
        open(TEMP2, "< temp2");
        while(<TEMP2>) {
            if ($c_file eq $_) {
                $found = 1;
            }
        }
        close(TEMP2);
        if ($found == 0) {
            print UNTOUCHED_FILES $c_file;
        } else {
            chomp($c_file);
            $c_file = $c_file . ".c\n";
           print TOUCHED_FILES $c_file;
        }
    }
    close(UNTOUCHED_FILES);
    close(TOUCHED_FILES);
    close(TEMP1);
    system("rm temp1 temp2");
}
close(DIRFILES);
    
#Now to print the stats of all the files which are touched
system("sort touched_files.txt -o temp; uniq temp touched_files.txt");
system("sort untouched_files.txt -o temp; uniq temp untouched_files.txt");

open (TOUCHED_FILES, "< touched_files.txt"); 
open (COVERAGE_STATS, ">> coverage_stats.txt");
open (ZERO_COVERAGE, ">> zero_coverage.txt");

while (<TOUCHED_FILES>) {
    #generate the gcov file for this particular file
    #1. Get the directory name and filename seperately
    #2. Invoke gcov on the file
    #3. Print the statistic onto a file
    
    chomp();
    my $full_name = $_;
    my $file_name = `basename $full_name`;
    my $dir_name = `dirname $full_name`;
    chomp($dir_name);
    chomp($file_name);

    open(RESULT, "cd $dir_name; gcov $file_name 2> /dev/null |");
    while (<RESULT>) {
        if (/Creating/) {} 
        else  {
            #Now we are doing the right line. Search for this file
            if (/$file_name/) {
                s/^([0-9]+\.[0-9]+\%)\.*/$1/;
                my $val = $1; 
                $k++;
                my $print_string = sprintf("%4d   %40s %40s       %3.2f\n", $k, $file_name, $dir_name, $val);
                if ($val == 0.00) { 
                    $l++;
                    my $zero_string = sprintf("%4d   %40s %40s       %3.2f\n", $l, $file_name, $dir_name, $val);
                    print ZERO_COVERAGE $zero_string;
                }
                print COVERAGE_STATS $print_string;
            }
        } 
    }
    close(RESULT);
}
close(TOUCHED_FILES);
close(COVERAGE_STATS);
close(ZERO_COVERAGE);
