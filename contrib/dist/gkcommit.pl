#!/usr/bin/env perl
#
# Copyright (c) 2010 Cisco Systems, Inc.  All rights reserved.
#
# Helper script for gatekeepers: it marshals together a GK-worthy SVN
# commit message containing the CMR number(s) being closed and SVN
# commit log messages for the SVN r numbers referenced.
#

use strict;

use locale ':not_characters';
# Respect the locale (LANG, LC_CTYPE, etc.) specified in the environment in
# which this script is run when performing file input and output.  Necessary to
# ensure proper transcoding when grabbing log messages from SVN and then
# writing them back out again.
use open ':locale';

use Getopt::Long;
use XML::Parser;
use Data::Dumper;
use Cwd;
use LWP;
use File::Temp qw/ :POSIX /;

my $base_trac_url = "https://svn.open-mpi.org/trac/ompi/ticket/%d?format=csv";
my $base_svn_url = "https://svn.open-mpi.org/svn/ompi/trunk";

###########################################################################

# Command line parsing
my @cmr_arg;
my @r_arg;
my $svn_up_arg = 1;
my $help_arg = 0;
my $dry_run_arg = 0;

&Getopt::Long::Configure("bundling");
my $ok = Getopt::Long::GetOptions("cmr|c=s" => \@cmr_arg,
                                  "r|r=s" => \@r_arg,
                                  "svn-up|s!" => \$svn_up_arg,
                                  "dry-run|d!" => \$dry_run_arg,
                                  "help|h!" => \$help_arg);

if (!$ok || $help_arg) {
    print "$0 [--cmr=<list>|-c <list>] [--r=<list>|-r <list>] 
     [--[no-]svn-up|-s] [--dry-run|-d]

<list> is a comma-delimited list of integers.

If --dry-run is specified, 'svn up' and 'svn commit' will not be executed.

If --cmr is not specified on the command line, you will be prompted
interactively.  Ditto for --r.

--no-svn-up inhibits running \"svn up\" before doing the commit.\n";
    exit(0);
}

print "DRY RUN: no svn state-changing commands will be run\n"
    if ($dry_run_arg);

# Parse the -cmr argument
my @cmrs;
if (@cmr_arg) {
    foreach my $cmr (@cmr_arg) {
        foreach my $c (split(/,/, $cmr)) {
            push(@cmrs, $c);
        }
    }
}

# Parse the -r argument
my @rs;
if (@r_arg) {
    foreach my $r (@r_arg) {
        foreach my $rr (split(/,/, $r)) {
            $rr =~ s/^r?//;
            push(@rs, $rr);
        }
    }
}

###########################################################################

# Make sure we're in an SVN tree
die "Not in a SVN tree"
    if (! -d ".svn");

###########################################################################

# If we didn't get cmrs on the command line, prompt for them
if (!@cmrs) {
    print "Enter a list of CMRs closed by this commit.\n";
    while (1) {
        print "\nList so far: ";
        if ($#cmrs >= 0) {
            print "#" . join(', #', @cmrs) . "\n";
        } else {
            print "<none>\n";
        }
        
        print "CMR number (-1 to exit)? ";
        my $cmrs = <STDIN>;
        chomp($cmrs);
        last
            if (-1 == $cmrs);

        foreach my $cmr (split(/[\s,]+/, $cmrs)) {
            if ($cmr > 0) {
                push(@cmrs, $cmr);
            } else {
                print "Invalid CMR number ($cmr); must be greater than 0 (ignored).\n";
            }
        }
    }
    print("\n");
}
if ($#cmrs < 0) {
    print "Must supply at least one CMR that is closed by this commit\n";
    exit(1);
}

# If we didn't get r numbers on the command line, prompt for them
if (!@rs) {
    print "Enter a list of SVN r numbers included in this commit.\n";
    while (1) {
        print "\nList so far: ";
        if ($#rs >= 0) {
            print "r" . join(', r', @rs) . "\n";
        } else {
            print "<none>\n";
        }
        
        print "SVN r number (-1 to exit)? ";
        my $rs = <STDIN>;
        chomp($rs);
        last
            if (-1 == $rs);

        foreach my $r (split(/[\s,]+/, $rs)) {
            $r =~ s/^r//;
            if ($r > 0) {
                push(@rs, $r);
            } else {
                print "Invalid SVN r number ($r); must be greater than 0 (ignored).\n";
            }
        }
    }
    print("\n");
}

###########################################################################

# Prettyprint
print "\nFinal list of CMRs closed by this commit: #" .
    join(', #', @cmrs) . "\n";
print "\nFinal list of SVN r number closed by this commit: ";
if ($#rs >= 0) {
    print "r" . join(', r', @rs) . "\n";
} else {
    print "<none>\n";
}

###########################################################################

# Retrieve subject lines for Trac CMRs
print "Retrieving Trac CMR summaries...\n";
my $cmr_summaries;
foreach my $cmr (@cmrs) {
    my $url = sprintf($base_trac_url, $cmr);

    # Recent (as of 3 Aug 2011) versions of LWP in Macports seem to
    # have broken SSL certificate verification.  The IU CA is in my
    # Mac system keychain (and has been there for quite a long time),
    # but after a recent ports update, LWP fails the SSL certificate
    # verification.  Fine.  So we'll just turn it off, per
    # http://search.cpan.org/~gaas/libwww-perl-6.02/lib/LWP/UserAgent.pm.
    my $ua = LWP::UserAgent->new(env_proxy => 0, 
                                 ssl_opts => { verify_hostname => 0 });

    # @#$@!$# LWP proxying for https *does not work*.  So don't set
    # $ua->proxy() for it.  Instead, rely on $ENV{https_proxy} being
    # set whenever we process requests that require SSL proxying,
    # because that is obeyed deep down in the innards underneath LWP.
    $ua->agent("gkcommit");

    my $res = $ua->get($url);
    if (!$res->is_success()) {
        print("Failed to download Trac ticket #" . $cmr . "\n");
        print $res->status_line . "\n";
        exit(1);
    }
    my @lines = split('\n', $res->content);
    my @fields = split(',', $lines[0]);

    # The summary field may have a "," in it, so do the parsing with
    # care.  If it does, the value will be enclosed in quotes ("foo").
    # If not, there will be no quotes.
    my $summary = $lines[1];
    $summary =~ s/^.*?,//;
    if (substr($summary, 0, 1) eq '"') {
        # Quotes are escaped in the string with double quotes
        my $marker="===GK-COMMIT-DOUBLE-QUOTE===";
        $summary =~ s/\"\"/$marker/g;
        $summary =~ m/^\"(.+?)\",/;
        $summary = $1;
        $summary =~ s/$marker/\"/g;
    } else {
        $summary =~ s/(.+?),.+/\1/;
    }
    $cmr_summaries->{$cmr} = $summary;
}

# If we have r numbers to parse, get the SVN logs and parse them into
# a data structure
my $logentries;
my $logentry;
my $chars;
if ($#rs >= 0) {
    print "Retrieving SVN log messages...\n";
    my $cmd = "svn log $base_svn_url --xml ";
    foreach my $r (@rs) {
        $cmd .= "-r $r ";
    }
    print "Running: $cmd\n";
    my $xml;
    open(CMD, "$cmd|");
    $xml .= $_
        while (<CMD>);
    close(CMD);

    my $x = new XML::Parser(Style => 'Subs',
                            Handlers => { Char => \&my_char });

    $x->parse($xml);
}

# Run "svn up" just to get the tree consistent
if ($dry_run_arg) {
    print "DRY RUN: skipping 'svn up' step\n";
} elsif ($svn_up_arg) {
    print "Running 'svn up'...\n";
    system("svn up");
} else {
    print "Skipping 'svn up' step\n";
}

###########################################################################

# Create a SVN commit message for the gatekeeper
my $commit_file = File::Temp::tempnam(Cwd::cwd(), "gkcommit");
open(FILE, ">$commit_file") ||
    die "Can't open temp file";
foreach my $cmr (@cmrs) {
    print FILE "Fixes #$cmr: $cmr_summaries->{$cmr}\n";
}
print FILE "\n";

# If we have r numbers, print them.  Use a special line to make the
# pre-commit hook ignore all of these messages (i.e., so that it
# doesn't try to close some ticket twice, or something like that).
print FILE "---svn-pre-commit-ignore-below---\n\n"
    if ($#rs >= 0);
foreach my $r (@rs) {
    print FILE "r$r [[BR]]
$logentries->{$r}->{msg}\n\n";
}

# Now add all the files that changes so that the gk can examine them
print "Running 'svn status'...\n";
open(SVN, "svn status|") ||
    die "Can't open svn status";
print FILE "--This line, and those below, will be ignored--

****************************************************************************
   GATEKEEPER: If you wish to abort this commit, delete all content from
   this file, save the file, and then quit your editor as normal.  The
   gkcommit script will see the 0-byte file and not perform the commit.
****************************************************************************

";

while (<SVN>) {
    print FILE $_;
}
close(SVN);
close(FILE);

# Now allow the gk to edit the file
if ($dry_run_arg) {
    # Dry run -- just show what would have happened.
    print "DRY RUN: skipping edit of this commit message:
----------------------------------------------------------------------------\n";
    my $pager = "more";
    $pager = $ENV{PAGER}
        if ($ENV{PAGER});
    system("$pager $commit_file");
    print("----------------------------------------------------------------------------\n");
} else {
    # Let the GK edit the file
    if ($ENV{SVN_EDITOR}) {
        system("$ENV{SVN_EDITOR} $commit_file");
    } elsif ($ENV{EDITOR}) {
        system("$ENV{EDITOR} $commit_file");
    } else {
        system("vi $commit_file");
    }
    if (! -f $commit_file) {
        print "Commit file no longer exists!  Aborting.\n";
        exit(1);
    }

    # Ensure that the file is >0 bytes long
    if (-z $commit_file) {
        print "ABORT: Commit file is 0 bytes long.  Nothing committed.\n";
        exit(1);
    }
}

# Finally, run the commit
my $cmd = "svn commit --file $commit_file " . join(' ', @ARGV);
if ($dry_run_arg) {
    print "DRY RUN: skipping '$cmd' step\n";
} else {
    print "Running: $cmd\n";
    if (0 == system($cmd)) {
        unlink($commit_file);
        print "\nRunning final 'svn up' to get a stable svnversion\n";
        system("svn up");
        exit(0);
    } else {
        print "Error during SVN commit!\n";
        print "GK commit message left in: $commit_file\n";
        exit(1);
    }
}

###########################################################################
###########################################################################
###########################################################################
# Helper functions
###########################################################################
###########################################################################
###########################################################################

# Called for the first logentry tag in the XML parsing
sub logentry {
    # The beginning logentry tag has arugments of the form:
    # ($expat, 'logentry', attr1, val1, attr2, val2, ...);
    shift(@_);
    shift(@_);
    while (@_) {
        my $attr = shift(@_);
        my $val = shift(@_);
        $logentry->{$attr} = $val;
    }
}

# Called for the last logentry tag in the XML parsing
sub logentry_ {
    $logentries->{$logentry->{revision}} = $logentry;
    $logentry = undef;
}

# Called for the last anchor tag in the XML parsing
sub author_ {
    chomp($chars);
    $chars =~ s/^\n*//;
    $logentry->{author} = $chars;
    $chars = '';
}

# Called for the last date tag in the XML parsing
sub date_ {
    chomp($chars);
    $chars =~ s/^\n*//;
    $logentry->{date} = $chars;
    $chars = '';
}

# Called for the last revision tag in the XML parsing
sub revision_ {
    chomp($chars);
    $chars =~ s/^\n*//;
    $logentry->{revision} = $chars;
    $chars = '';
}

# Called for the last msg tag in the XML parsing
sub msg_ {
    chomp($chars);
    $chars =~ s/^\n*//;
    $logentry->{msg} = $chars;
    $chars = '';
}

# Called for general character data in XML parsing
sub my_char {
    my ($expat, $tmp) = @_;
    $chars .= $tmp;
}
