#!/usr/bin/env perl
#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

use strict;
use Data::Dumper;
use LWP::UserAgent;

my $base_url = "http://www.open-mpi.org/~jsquyres/openmpi";
my $platform_file = "platforms.php";
my $ptl_file = "ptls.php";
my $pcm_file = "pcms.php";
my $submit_uri = "submit.php";

my $ua = LWP::UserAgent->new({ env_proxy => 1 });
$ua->agent("Open MPI test reporter/1.0");

#----------------------------------------------------------------------------

sub download {
    my ($url, $file) = @_;

    # Delete previous and download the new

    unlink($file);
    my $uri = "$url/$file";
    my $req = HTTP::Request->new(GET => $uri);
    my $res = $ua->request($req);
    if ($res->is_success()) {
        open(FILE, ">$file") || 
            die("ERROR: Could not write to $file");
        print FILE $res->content;
        close(FILE);
    } else {
        print "Error retrieving URL: $uri\n";
        die $res->message;
    }
    
    # Find the fields and prompts in the config files

    my @prompts;
    my @fields;
    open CONFIG, $file || die "Can't open $file";
    my $i = 0;
    while (<CONFIG>) {
        my $line = $_;
        chomp($line);
        if ($line =~ /^#PROMPTS/) {
            @prompts = split(/:/, substr($line, 9));
            ++$i;
        } elsif ($line =~ /^#FIELDS/) {
            @fields = split(/:/, substr($line, 8));
            ++$i;
        }

        last
            if ($i >= 2);
    }
    close CONFIG;

    die "Could not find list of prompts in file $file"
        if ($#prompts < 0);

    # Parse the rest of the config file into a hash

    my $output;
    open CONFIG, $file || die "Can't open downloaded $file";
    while (<CONFIG>) {
        my $line = $_;
        chomp($line);
        if (length($line) > 0 && 
            ! ($line =~ /^[ \t]+$/) &&
            ! ($line =~ /^[ \t]*#/)) {
            my @vals = split(/:/, $line);

            # There are definitely better ways to do this, but I can't
            # quite figure out the exact right syntax, so I'm just
            # gonna punt for now.  :-)

            if ($#vals == 0) {
                $output->{$vals[0]} = "";
            } elsif ($#vals == 1) {
                $output->{$vals[0]}->{$vals[1]} = "";
            } elsif ($#vals == 2) {
                $output->{$vals[0]}->{$vals[1]}->{$vals[2]} = "";
            } elsif ($#vals == 3) {
                $output->{$vals[0]}->{$vals[1]}->{$vals[2]}->{$vals[3]} = ""
            } elsif ($#vals == 4) {
                $output->{$vals[0]}->{$vals[1]}->{$vals[2]}->{$vals[3]}->{$vals[4]} = "";
            } elsif ($#vals == 5) {
                $output->{$vals[0]}->{$vals[1]}->{$vals[2]}->{$vals[3]}->{$vals[4]}->{$vals[5]} = "";
            } elsif ($#vals == 6) {
                $output->{$vals[0]}->{$vals[1]}->{$vals[2]}->{$vals[3]}->{$vals[4]}->{$vals[5]}->{$vals[6]} = "";
            } else {
                die "Tell Jeff to fix the syntax parser ($#vals fields found)";
            }
        }
    }
    close CONFIG;

    # Return it

    (\@prompts, \@fields, $output);
}

#----------------------------------------------------------------------------

# Descend through the hash until we have a unique configuration

sub do_menu {
    my ($prompts, $options) = @_;

    my $i;
    my $level = $options;
    my @config;

    for ($i = 0; $i <= $#$prompts; ++$i) {
        while (1) {
            my @keys = sort keys (%$level);

            if ($#keys == 0) {
                print "Assuming: $$prompts[$i] = $keys[0]\n\n";
                push(@config, $keys[0]);
                $level = $level->{$keys[0]};
            } else {
                for (my $j = 0; $j <= $#keys; ++$j) {
                    printf("%d. %s\n", $j + 1, $keys[$j]);
                }
                print "\n$$prompts[$i] (1-" . ($#keys + 1) . "): ";
                
                my $input = <STDIN>;
                print "\n";
                chomp($input);
                if ($input < 1 || $input > $#keys + 1) {
                    print "Please enter a valid selection\n";
                    next;
                }
            
                push(@config, $keys[$input - 1]);
                $level = $level->{$keys[$input - 1]};
            }

            last;
        }
    }

    # Return it

    \@config;
}

#----------------------------------------------------------------------------

sub do_question {
    my ($q, $default, $check_func) = @_;

    my $input;
    while (1) {
        print "$q ";
        $input = <STDIN>;
        chomp($input);
        if (length($input) == 0) {
            $input = $default;
        }
        if (&$check_func($input)) {
            last;
        }
    }

    $input;
}

#----------------------------------------------------------------------------

sub do_yn {
    my ($q, $default) = @_;
    my $input = do_question($q, $default, 
                sub { my ($line) = @_; lc($line) eq "y" || lc($line) eq "n"; });
    lc($input);
}

#----------------------------------------------------------------------------

sub do_int {
    my ($q, $default) = @_;
    do_question($q, $default, 
                sub { my ($line) = @_; $line >= 0; });
}

#----------------------------------------------------------------------------

sub do_compile {
    my $compile = do_yn("Did Open MPI compile successfully (Y/n)?", "y");
    my $warnings = do_yn("Were there compiler warnings (Y/n)?", "y");
    my $link = do_yn("Were you able to link MPI applications (Y/n)?", "y");

    # Submit the results

    "type=compile&compile=$compile&warnings=$warnings&link=$link";
}

#----------------------------------------------------------------------------

sub do_mpi {
    my $intel_passed = 0;
    my $intel_failed = 0;
    my $mpich_passed = 0;
    my $mpich_failed = 0;
    my $ibm_passed = 0;
    my $ibm_failed = 0;

    our $ptl_prompts;
    our $ptls;

    my $ptl = do_menu($ptl_prompts, $ptls);

    $intel_passed = do_int("How many Intel tests PASSED? (0 = not run)", -1);
    $intel_failed = do_int("How many Intel tests failed?", -1)
        if ($intel_passed > 0);
    $mpich_passed = do_int("How many MPICH tests PASSED? (0 = not run)", -1);
    $mpich_failed = do_int("How many MPICH tests failed?", -1)
        if ($mpich_passed > 0);
    $ibm_passed = do_int("How many IBM tests PASSED? (0 = not run)", -1);
    $ibm_failed = do_int("How many IBM tests failed?", -1)
        if ($ibm_passed > 0);

    # Submit the results

    "type=mpi&ptl=$$ptl[0]&intel_passed=$intel_passed&intel_failed=$intel_failed&mpich_passed=$mpich_passed&mpich_failed=$mpich_failed&ibm_passed=$ibm_passed&ibm_failed=$ibm_failed";
}

#----------------------------------------------------------------------------

sub do_rte {
    our $pcm_prompts;
    our $pcms;

    my $pcm = do_menu($pcm_prompts, $pcms);

    my $hello = do_yn("Was MPI hello world run successfully (Y/n)", "y");
    my $abort = do_yn("Was MPI abort world run successfully (Y/n)", "y");
    my $spawn = do_yn("Was MPI spawn run successfully (Y/n)", "y");

    # Submit the results

    "type=rte&pcm=$$pcm[0]&hello=$hello&abort=$abort&spawn=$spawn";
}

#----------------------------------------------------------------------------

# Download the files

our ($platform_prompts, $platform_fields, $platforms) =
    download($base_url, $platform_file);
our ($ptl_prompts, $ptl_fields, $ptls) = download($base_url, $ptl_file);
our ($pcm_prompts, $pcm_fields, $pcms) = download($base_url, $pcm_file);

# Traverse the platform choices

my $platform = do_menu($platform_prompts, $platforms);

# Common information for all test categories

my $svn_r = do_int("What is the SVN revision number you are testing?", "-1");
print "\n";
open HOST, "hostname|";
my $hostname = <HOST>;
chomp($hostname);
close(HOST);
my ($username) = getpwuid($<);

print("-----------------------------------------------------\n\n");

print "SVN revision: $svn_r
Hostname: $hostname
Username: $username
Configuration: " . join("/", @$platform) . "\n\n";

# Compile, PTL, or PCM?

print("-----------------------------------------------------\n\n");
my $type = {
    "Compile" => "",
    "MPI" => "",
    "RTE" => "",
};
my $type_prompts = [
    "What kind of test are you reporting?"
];
my $test = do_menu($type_prompts, $type);

# Process

my $result;

if ($$test[0] eq "Compile") {
    $result = do_compile();
} elsif ($$test[0] eq "MPI") {
    $result = do_mpi();
} elsif ($$test[0] eq "RTE") {
    $result = do_rte();
} else {
    die "Unknown test report type!";
}
$result .= "&svn=$svn_r";
$result .= "&hostname=$hostname";
$result .= "&username=$username";
for (my $i = 0; $i <= $#$platform_fields; ++$i) {
    $result .= "&platform_" . $$platform_fields[$i] . "=" . $$platform[$i];
}

# Send back

my $url = "$base_url/$submit_uri?$result";
my $req = HTTP::Request->new(POST => $url);
$req->header(User_Agent => "OMPI test_submit");
$req->header(Content_Type => "text/html");
my $res = $ua->request($req);
if ($res->is_success()) {
    my $content = $res->content;
    if ($content && $content =~ /SUCCESS/) {
        print "Results submitted successfully.  Thanks!\n";
    } else {
        print "FAILED SUBMIT!\n\n";
        print "Response: " . $res->content . "\n";
    }
} else {
    print "FAILED SUBMIT!\n\n";
    print $res->message;
}

# All done

exit(0);
