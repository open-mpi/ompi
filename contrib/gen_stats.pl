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
use Switch;
use File::Basename;

#global variables
my $index = 0;
my $num_args = scalar(@ARGV);
my $percentage = 0.0;
my $ret1 = 0;
my $ret2 = 0;
my $dir_list_given = 0;
my $req_list_given = 0;

$ret1 = open(DIR_FILE, "dir_list.txt");
$ret2 = open(REQ_FILE, "req_list.txt");

if ($ret1 < 0 || $ret2 < 0) {
    print "ERROR: opening dir_list.txt or req_list.txt\n";
    exit(3);
}

# check the error condition
if (0 == ($num_args % 2)) {
} else {
    print "ERROR: the number of arguments has to be even\n";
    exit(3);
}

# process the arguments
while($num_args > 0) {
    switch ($ARGV[$index]) {
        case "-d" { 
           print DIR_FILE $ARGV[$index+1];
           $index += 2;
           $num_args -= 2;   
           $dir_list_given = 1;
        }
        case "-f" { 
           my $filename = `find . -name $ARGV[$index+1]`;
           print REQ_FILE $filename;
           $index += 2;
           $num_args -= 2;   
           $req_list_given = 1;
        }
        case "-p" { 
           $percentage = $ARGV[$index];
           $index += 2;
           $num_args -= 2;   
        }
        else { 
            print "ERROR: Incorrect command line option\n";
            exit(3);
        }
    }
}

close(DIR_FILE);
close(REQ_FILE);

# when nothing is specified, everything defaults to just "src"
if (0 == $dir_list_given && 0 == $req_list_given) {
    print "WARN: No directory or file has been requested\ndefaulting to src directory\n";
    system("echo src > dir_list.txt");
    $dir_list_given = 1;
}

#Now, we have both the requested file list and the directory list
#So simply call the procedure generate_stats with the required arguments
if (1 == $dir_list_given) {
    get_file_list("./dir_list.txt",
                  "touched_files.txt",
                  "untouched_files.txt");
                  
    generate_stats("touched_files.txt", # file_list
                   "coverage_stats.txt",# generic coverage numbers
                   "percent_stats.txt", # files below a certain %
                   $percentage,         # percentage below which we report
                   1);                  # 1 to report
}
               
if (1 == $req_list_given) {
    generate_stats("req_list.txt", # file_list
                   "req_stats.txt",# generic coverage
                   "",             # no percentage coverage required
                   0,              # not required
                   0);             # 0 to not report percentages
}

sub get_file_list {
    my ($input, $touched, $untouched) = @_;
    my $ret = open(DIRFILES, "< $input");
    if ($ret < 0) {
        print "ERROR: could not open directory listing\n";
        exit(3);
    }
 
    while(<DIRFILES>) {
        chomp();
        my $c_files = `find $_ -name \"*.c\"`;
        my $cc_files = `find $_ -name \"*.cc\"`;

        $cc_files =~ s/\.cc//g;
        $c_files =~ s/\.c//g;
        $c_files = $c_files . $cc_files;
        my @C_FILES = split(/\n/, $c_files);
        
        my $da_files = `find $_ -name \"*.da\" -o -name \"*.gcda\"`;
        $da_files =~ s/\.gcda//g;
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
        open(UNTOUCHED_FILES, ">> $untouched");
        open(TOUCHED_FILES, ">> $touched");
 
        while(<TEMP1>) {
            my $c_file = $_;
            my $found = 0;
            my $file_name;
            my $dir_name;
            my $file_ext;
            my $search_file;
            ($file_name, $dir_name, $file_ext) = fileparse($c_file, ('\.c') );

            # For our libtool build, every .gcda and .gcno file lands in the .libs dir.
            $search_file = $dir_name . ".libs/" . $file_name;

            open(TEMP2, "< temp2");
            while(<TEMP2>) {
                if ($search_file eq $_) {
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
    system("sort $touched -o temp; uniq temp $touched");
    system("sort $untouched -o temp; uniq temp $untouched");
}
    

# This is the function which generates the statistics and dumps it out
# to a file. Details are pretty straightforward at this point
sub generate_stats {
    my ($input_file, $coverage_file, $percent_file, $percentage, $calculate) = @_;
    my $k = 0;
    my $l = 0;

    open (INPUT, "< $input_file"); 
    open (COVERAGE, "> $coverage_file");
    if ($calculate == 1) {
        open (PERCENT, "> percent_coverage.txt");
    }

    print COVERAGE "#Index                             Directory                          Filename                 Usage(%)\n";
    print COVERAGE "#======================================================================================================\n";
    
    if ($calculate == 1) {
        print PERCENT "#Index                             Directory                          Filename                 Usage(%)\n";
        print PERCENT "#======================================================================================================\n";
    }

    my $average = 0.0;
    my $num_files = `wc -l touched_files.txt`;

    print "num_files:", $num_files;

    while (<INPUT>) {
        #generate the gcov file for this particular file
        #1. Get the directory name and filename seperately
        #2. Invoke gcov on the file
        #3. Print the statistic onto a file
    
        chomp();
        my $full_name = $_;
        my $dir_name;
        my $file_name;
        my $file_ext;
        my $file_gcda;
        my $found_file;
        ($file_name, $dir_name, $file_ext) = fileparse($full_name, ('\.c') );
        $file_gcda = $file_name . ".gcda";

        open(RESULT, "cd $dir_name; gcov $file_gcda -o .libs 2> /dev/null | ");
        while (<RESULT>) {
            if (/Creating/) { $found_file = 0; } 
            else  {
                # print "check: ", $_;
                # Do not check including the file_extension; might be .c or .cc or .C
                if (/^File '$file_name/) {
                    # print "Found File:\n", $_;
                    $found_file = 1;
                }

                #Now we are doing the right line. Search for this file
                if (/^Lines/ && $found_file == 1) {
                    # print "Found Lines:\n", $_;
                    s/([\s,0-9]*\.[0-9]+\%)\.*/$1/;
                    my $val = $1; 
                    $average += $val;
                    $k++;
                    my $print_string = sprintf("%4d   %40s %40s       %3.2f\n", $k, $dir_name, $file_name, $val);
                    if ($calculate == 1) {
                        if ($val <= $percentage) { 
                            $l++;
                            my $zero_string = sprintf("%4d   %40s %40s       %3.2f\n", $l, $dir_name, $file_name, $val);
                            print PERCENT $zero_string;
                        }
                    }
                    print COVERAGE $print_string;

                    # Need to detect the next round
                    $found_file = 0;
                }
            } 
        }
        close(RESULT);
    }
    if ($num_files != 0){
        print COVERAGE "==============================================================\n";
        print COVERAGE "Average coverage was: ", $average/$num_files, "  \n";
        print COVERAGE "==============================================================\n";
    }
    close(INPUT);
    close(COVERAGE);
    if ($calculate == 1) {
        close(PERCENT);
    }
}
