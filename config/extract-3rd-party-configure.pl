#!/usr/bin/env perl
#
# Copyright (c) 2021      IBM Corporation.  All rights reserved.
# Copyright (c) 2021 Cisco Systems.  All rights reserved.
#
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#
# A script used to extract the AC_ARG_WITH/AC_ARG_ENABLE strings from 3rd
# party libraries into a format that can be included in a higher level
# configure.ac.
#

use strict;

use Cwd;
use File::Basename;
use File::Find;
use Data::Dumper;
use Getopt::Long;

my $quiet_arg = 0;
my $debug_arg = 0;
my $help_arg = 0;
my $list_only = 0;
my $package_dir;
my $only_filename; # for debugging
my $ompi_exclude;
my $project_name;

my $package_configure;
my $package_config_dir;

my %skip_files = ();
my %skip_options = ();

# Default exclude file for all 3rd party packages
my @exclude_config_files = ("3rd-party/exclude-config.ini");
my @mca_configure_locations = ();

##############################################################################
# Command line parameters
my $OPTION_ALL_MARKER = "--";

my $ok = Getopt::Long::GetOptions("quiet|q" => \$quiet_arg,
                                  "debug|d" => \$debug_arg,
                                  "help|h" => \$help_arg,
                                  "list|l" => \$list_only,
                                  "package|p=s" => \$package_dir,
                                  "project|n=s" => \$project_name,
                                  "file|f=s" => \$only_filename,
                                  "exclude|e=s" => \$ompi_exclude,
    );

if (!$ok || $help_arg) {
    print "Invalid command line argument.\n\n"
        if (!$ok);
    print "Options:
  --project | -n                Project name (Required)
  --exclude | -e                Additional exclude file
  --package | -p                Full path to the 3rd party library (Required)
  --list | -l                   List options not in m4 syntax, but in exclude file syntax
  --quiet | -q                  Do not display normal verbose output like Warnings
  --debug | -d                  Output lots of debug information
  --help | -h                   This help list
\n";
    exit($ok ? 0 : 1);
}

if (! defined $package_dir && ! defined $only_filename) {
    print "Error: Supply a package directory\n";
    exit(1);
}

if (defined $package_dir) {
    $package_configure = $package_dir . "/configure.ac";
    if (!-e $package_configure) {
        print "Error: Configure binary not found at: $package_configure\n";
        exit(1);
    }
    $package_config_dir = $package_dir . "/config";
    if (!-d $package_config_dir) {
        undef($package_config_dir);
    }

    # Well known locations to look for additional .m4 files (related to MCA components)
    push(@mca_configure_locations, "$package_dir/src/mca");
    push(@mca_configure_locations, "$package_dir/ompi/mca");
    push(@mca_configure_locations, "$package_dir/oshmem/mca");
    push(@mca_configure_locations, "$package_dir/opal/mca");
}

if (! defined $project_name) {
    print "Error: Must supply a project name\n";
    exit(1);
}
my $project_name_uc = uc($project_name);
my $project_name_lc = lc($project_name);

if (defined $ompi_exclude) {
    push(@exclude_config_files, $ompi_exclude);
}

#---------------------------------------------------------------------------
sub process_m4($)
{
    my $filename = shift(@_);
    my $line;
    my $char;
    my $kw;
    my $option_name = "";
    my $option_name_help_str = "";

    # Read all config files looking for the keywords
    # Note that we only want the first two arguments
    #  AC_ARG_ENABLE (feature, help-string, [action-if-given], [action-if-not-given])
    #  AC_ARG_WITH (package, help-string, [action-if-given], [action-if-not-given])
    my @conf_keywords = ("AC_ARG_ENABLE", "AC_ARG_WITH");

    # stack with function parameters: '(', ')'
    my @the_stack = ();
    # push(@the_stack) / pop(@the_stack) / scalar(@the_stack)
    my $arg_num = 0;
    my $line_num = 0;

    open(FH, '<', $filename) or die $!;
    while($line = <FH>) {
        my $process_this_line = 0;

        chomp($line);
        $line_num += 1;

        # If the stack is empty, then we are looking for a keyword
        if( scalar(@the_stack) <= 0 ) {
            foreach $kw (@conf_keywords) {
                # If this is starting a new configure option, then grab it's option name
                if( $line =~ /$kw\(\s*\[?\s*([-\w\d\$]*)\s*\]?/ ) {
                    $option_name = $1;

                    # Skip anything excluded
                    if( exists($skip_options{$option_name}) && 
                        ($skip_options{$option_name} eq $OPTION_ALL_MARKER || lc($skip_options{$option_name}) eq $project_name_lc) ) {
                        if( !$list_only && !$quiet_arg) {
                            print "# Warning: Excluded: $option_name\n";
                            print "# " . "-"x40 . " Above from $filename:$line_num\n\n";
                        }
                        $process_this_line = 0;
                        last;
                    }

                    # Skip any option that contains a variable substitution
                    if ( $option_name =~ /\$/ ) {
                        if( !$list_only && !$quiet_arg) {
                            print "# Warning: Skipped (embedded variable): $option_name\n";
                            print "# " . "-"x40 . " Above from $filename:$line_num\n\n";
                        }
                        $process_this_line = 0;
                        last;
                    }

                    # If just listing then there is no need to further process the _ARG_
                    if( $list_only ) {
                        print "# $project_name : $kw : From $filename:$line_num\n";
                        print "OPTION: $option_name\n";
                        $process_this_line = 0;
                        last;
                    } else {
                        $process_this_line = 1;
                    }

                    $arg_num = 0;
                }
            }
        }
        # If the stack is not empty, then keep processing lines
        else {
            $process_this_line = 1;
        }

        # Process the line character-by-character to extract only the
        # first two arguments. Note that the '(' ')' mark the boundary
        # to the function. The '[' ']' are optional bounds on the argument
        # string - so they might not be there. If they are not there then
        # we need to look for the ',' to separate the arguments. If they
        # are there then we need to look for the ',' after ']'.
        if( 0 != $process_this_line ) {
            if( $debug_arg ) {
                print "LINE: $line\n";
                print "----: ";
            }
            foreach $char (split(//, $line)) {
                # start of [ ] block
                if( $char eq "[" ) {
                    push(@the_stack, $char);
                }
                # end of [ ] block
                elsif( $char eq "]" ) {
                    pop(@the_stack);
                }
                # start of ( ) function, as long as we are not in a []
                elsif( $char eq "(" ) {
                    # Track it unless we are in a [
                    if(scalar(@the_stack) == 0 || $the_stack[-1] ne "[" ) {
                        push(@the_stack, $char);
                    }
                }
                # end of ( ) function, as long as we are not in a []
                elsif( $char eq ")" ) {
                    # Track it unless we are in a [
                    if($the_stack[-1] ne "[" ) {
                        pop(@the_stack);
                    }
                }
                # Argument separator
                # We only care about counting arguments for the outermost
                # function (e.g., AC_ARG_ENABLE), not the innermost
                # function (e.g., AS_HELP_STRING) - which we take all of.
                # We know we are in the outermost because the '(' will be
                # only thing on the stack.
                elsif( scalar(@the_stack) == 1 && $char eq "," ) {
                    $arg_num += 1;
                    # If we just finished the 2nd argument of the outer function
                    # Then we want to stop so we do not pick up the actions.
                    if( $arg_num == 2 && scalar(@the_stack) == 1 ) {
                        print ")";
                        $process_this_line = 0;
                        @the_stack = ();

                        if( $debug_arg ) {
                            print "\nStop After 2nd Argument";
                        }
                        last;
                    }
                }
                if( 0 != $process_this_line ) {
                    print $char;
                }
            }

            if( $debug_arg ) {
                print "||-- @the_stack --||\n";
            } else {
                print "\n";
            }

            # When we are all done with this function make a note of
            # which file we extracted it from, just in case we need
            # to track it down later.
            if( scalar(@the_stack) <= 0 ) {
                print "# " . "-"x40 . " Above from $filename:$line_num\n\n";
            }
        }
    }
    close(FH);

    # Sanity check that the parsing was balanced.
    if( scalar(@the_stack) > 0 ) {
        print "Error: Parsing failed. Unbalanced brackets and/or parenthesis\n";
        print "Stack: \"@the_stack\"\n";
        print "File : $filename\n";
        exit(2);
    }
}

#---------------------------------------------------------------------------
# Look for excluded options

@exclude_config_files = sort(@exclude_config_files);
foreach $ompi_exclude (@exclude_config_files) {
    if (-f "$ompi_exclude") {
        my $line;

        if( $debug_arg ) {
            print "Processing Exclude File: $ompi_exclude\n";
        }
        open(FH, '<', "$ompi_exclude") or die $!;
        while($line = <FH>) {
            if( $line =~ /^\s*#/ || $line =~ /^\s*$/) {
                next;
            }
            chomp($line);

            # Files to exclude
            if( $line =~ /^\s*FILE\s*:\s*/ ) {
                $line =~ s/^\s*FILE\s*:\s*//;
                $line =~ s/^\s*|\s*$//g;
                $skip_files{$line} = 1;
            }
            # Options to exclude regardless of project
            elsif( $line =~ /^\s*OPTION\s*:\s*/ ) {
                $line =~ s/^\s*OPTION\s*:\s*//;
                $line =~ s/^\s*|\s*$//g;
                $skip_options{$line} = $OPTION_ALL_MARKER;
            }
            # Options to exclude for a specific project
            elsif( $line =~ /^\s*OPTION\s*\((\w+)\):\s*/ ) {
                my $proj = $1;
                $line =~ s/^\s*OPTION\s*\(\w+\):\s*//;
                $line =~ s/^\s*|\s*$//g;
                $skip_options{$line} = $proj;
            }
        }
        close(FH);
    }
}
if( $debug_arg ) {
    print "---------------- Excluded files\n";
    foreach my $key (keys(%skip_files)) {
        print "FILE: $key\n";
    }

    print "---------------- Excluded options\n";
    foreach my $key (keys(%skip_options)) {
        print "OPTION (".$skip_options{$key}."): $key\n";
    }

    print "----------------\n\n";
}

#---------------------------------------------------------------------------
my $filename;
my @all_files = ();
my $dir;

# Process only a single file, useful for debugging
if( defined $only_filename ) {
    process_m4($only_filename);
    exit(0);
}

# Generate the file list, then sort it for consistent processing of the options
if (defined $package_configure ) {
    push(@all_files, $package_configure);
}

if (defined $package_config_dir) {
    # First look in the $package_config_dir for .m4 files
    opendir(DIR, $package_config_dir) or die "Error: Failed to open the directory: $package_config_dir";
    while($dir = readdir(DIR)) {
        $filename = $package_config_dir . "/" . $dir;
        if( -f $filename && $filename =~ /\.m4$/ ) {
            if( ! exists($skip_files{$dir}) && ! exists($skip_files{$filename}) ) {
                if( $debug_arg ) {
                    print "# DEBUG: Processing: $filename\n";
                }
                push(@all_files, $filename);
            } elsif( $debug_arg ) {
                print "# DEBUG: SKIP: $filename\n";
            }
        }
    }
    closedir(DIR);

    # Look for .m4 files in well known locations (related to MCA components)
    @mca_configure_locations = sort(@mca_configure_locations);
    foreach $dir (@mca_configure_locations) {
        if (-d "$dir") {
            my @mca_configure_files;
            @mca_configure_files = `find $dir -name '*.m4' -type f`;
            foreach $filename (@mca_configure_files) {
                chomp($filename);
                if( ! exists($skip_files{$filename}) ) {
                    if( $debug_arg ) {
                        print "# DEBUG: Processing: $filename\n";
                    }
                    push(@all_files, $filename);
                } elsif( $debug_arg ) {
                    print "# DEBUG: SKIP: $filename\n";
                }
            }
        }
    }
}

@all_files = sort(@all_files);

#---------------------------------------------------------------------------
# Actually process the files and generate the result to stdout

if( $list_only ) {
print <<EOL;
# -----
# This file is auto-generated during autogen.pl
# -----
EOL
;
} else {
print <<EOL;
# -----
# This file is auto-generated during autogen.pl
# -----

AC_DEFUN([OMPI_${project_name_uc}_ADD_ARGS],[

EOL
;
}
print "\n# " . "-"x40 . "\n\n";

foreach $filename (@all_files) {
    if( $filename =~ /auto-extracted-\w+-configure-args.m4$/ ) {
        if( $debug_arg ) {
            print "SKIP: $filename\n";
        }
    } else {
        process_m4($filename);
    }
}

if( !$list_only ) {
print <<EOL;

])
EOL
}
#---------------------------------------------------------------------------
exit(0);
