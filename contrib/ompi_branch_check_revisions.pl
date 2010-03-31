#!/usr/bin/env perl
#
# Copyright (c) 2010      Oak Ridge National Labs.  All rights reserved.
#
# Script to check which revisions on Trunk have not yet been merged into
# a branch.
#

use strict;
### use warnings;

use Getopt::Long;
use XML::Parser;
use SVN::Client;
use Cwd;
use LWP;
use File::Temp qw/ :POSIX /;

my $base_trac_url = "https://svn.open-mpi.org/trac/ompi/";
my $base_svn_url = "https://svn.open-mpi.org/svn/ompi/";

###########################################################################

# Command line parsing
my $branch_arg = "v1.5";
my $url_arg;
my $output_arg = "html";
my $html=1;
my $width_arg = 90;
my $start_rev_arg = 0;
my $notes_file_arg = 0;
my $debug_arg = 0;
my $help_arg = 0;

my $trunk_url;
my $branch_url;
my $svn_ctx_trunk;
my $svn_ctx_branch;

my $notes_file;
my $notes_file_fh;
my $branch_rev;
my $pool = SVN::Pool->new_default;

my $cmd;
my $cmd_output;
my $xml;
my %logentries;
my %logentries_branch;
my %logentries_trunk;
my $logentry;
my $chars;
my $i;
my $x;

my @revs;
my @revs_svn;
my @revs_eligible;
my @revs_merged;
my $rev;
my $line;

my %count;


&Getopt::Long::Configure("bundling");
my $ok = Getopt::Long::GetOptions("branch|b=s" => \$branch_arg,
                                  "url|u=s" => \$url_arg,
                                  "output|o=s" => \$output_arg,
                                  "width|w=s" => \$width_arg,
                                  "start-rev|s=s" => \$start_rev_arg,
                                  "notes-file|n=s" => \$notes_file_arg,
                                  "debug|d!" => \$debug_arg,
                                  "help|h!" => \$help_arg);

if (!$ok || $help_arg) {
    print "
Usage: $0 [--help|-h] [--branch|-b=BRANCH] [--url|-u=URL]
          [--output|-o=OUTPUT TYPE] [--width|-w=WIDTH] [--start-rev|-s=REV]
          [--merge-file|-m=FILE] [--revision-file|-r=FILE]

Outputs a HTML or a comma-separated file, listing outstanding r-numbers.
May use an input file per branch, describing notes such as pending CMRs,
dependencies or reasons, why a revision should not be moved.

-b   Branch version, which is appended to default URL [default:$branch_arg]
-u   URL to compare to [default: https://svn..../ompi/branches/$branch_arg]
-o   Output Type, possible: txt, html [default:$output_arg]
-w   Width of commit msg. that will be cut at [default:$width_arg]
-s   Start with revision number [default:Creation of branch]
-n   Notes file, comma separated revision, notes [FUTURE: default:svn update of $0-$branch_arg.txt]
-d   Debug output
-h   This help

When parsing the revision file notes (see option -n), the following marks will be done
  MAILED           Author has been made aware of filing a CMR
  CMR:BRANCH[:NUM] There exists a CMR (with link to TRACS)
  MERGED           The Revision will not show up in the output
The markings are in increasing order, i.e. if MERGED is part of notes the line is not shown.
\n";

    exit(0);
}

###########################################################################

# Make sure we're in an SVN tree
die "Not in a SVN tree"
    if (! -d ".svn");

$trunk_url = $base_svn_url . "/trunk";
$svn_ctx_trunk = new SVN::Client( url => $trunk_url );
die "SVN URL $trunk_url is wrong"
    if (! $svn_ctx_trunk );

if ($url_arg) {
    $branch_url = $url_arg;
} else {
    $branch_url = $base_svn_url . "/branches/" . $branch_arg;
}
$svn_ctx_branch = new SVN::Client( url => $branch_url );
die "SVN URL $branch_url is wrong, specify with [--url|-u=URL]"
    if (! $svn_ctx_branch );

print "branch_url: $branch_url\n"
    if ($debug_arg);

# XXX For now (as long as ompi_branch_check_revisions-v1.5 is not on the branch, hard-code the file:
$notes_file = "ompi_branch_check_revisions-v1.5.txt";
# if ($notes_file_arg) {
#     $notes_file = $notes_file_arg;
# } else {
#     $notes_file = $branch_url . "/contrib/ompi_branch_check_revisions-" . $branch_arg . ".txt";
#     $branch_rev = $svn_ctx_branch->update ( $notes_file, "HEAD", 0, $pool );
# }

open $notes_file_fh, '<', $notes_file or die "Cannot not open notes-file $notes_file [--notes-file|-n]";

chomp ($output_arg);
if (lc($output_arg) eq "text") {
    $html = 0;
} elsif (lc($output_arg) eq "html") {
    $html = 1;
} else {
    die "Output type wrong [--output|-o=OUTPUT TYPE]"
}

###########################################################################

# First grep the eligible revisions
$cmd = "svn mergeinfo --show-revs eligible $trunk_url $branch_url";

print "Running: $cmd\n"
    if ($debug_arg);

open (CMD, "$cmd|");
$cmd_output .= $_
    while (<CMD>);
close(CMD);

@revs = split('\n', $cmd_output);
foreach $rev (@revs) {
    $rev =~ s/^r?([0-9]+)/$1/;
    push @revs_svn, $rev;
}

if ($debug_arg) {
    print "Number of svn-eligible revs: $#revs_svn\n";
    $i = 0;
    foreach $rev (@revs_svn) {
        print "i:$i rev:$rev\n";
        $i += 1;
    }
}

###########################################################################
# Then check, whether these revisions are already merged (parse log messages of branch!)
$cmd = "svn log --stop-on-copy --xml $branch_url";

open (CMD, "$cmd|");
$xml .= $_
    while (<CMD>);
close(CMD);

# Somehow, I cannot set another Start/End-handler, that would assign
# to my specific %hash (e.g. one end-handler for %logentries_branch another one for trunk)
# So just copy the hashes after the parse.
$x = new XML::Parser ( Style => 'Subs',
                       Handlers => {
                            Char => \&my_char_handler
                       });
%logentries = ();
$x->parse ($xml);
%logentries_branch = %logentries;


# Search all revisions stored in the hash and grok their msg for From patterns.
# Delete those revisions form the above eligible revisions.
foreach $rev (keys %logentries_branch) {
    # Scan each line in the commit msg.
    foreach $line (split('\n', $logentries_branch{$rev}->{msg})) {
        # Lines matching a specific merge "> From rXXXX:" or "(Import rXXX)"?
        if ($line =~ /^\>? *From *r?([0-9]+):?/) {
            $line =~ s/^\>? *From *r?([0-9]+):?/$1/g;
            push @revs_merged, $line;
        }
    }
}

if ($debug_arg) {
    print "Number of parsed merged revs: $#revs_merged\n";
    $i = 0;
    foreach $rev (@revs_merged) {
        print "i:$i parsed merged rev:$rev\n";
        $i += 1;
    }
}


# Count each revision from BOTH arrays (the svn-eligible and already merged) into a hash
foreach $rev (@revs_svn, @revs_merged) { $count{$rev}++ }
# Copy all revisions from the svn-eligible, whose count is only ONE
foreach $rev (@revs_svn) {
    push @revs_eligible, $rev
        if ($count{$rev} == 1);
}

if ($debug_arg) {
    print "Number of eligible revs: $#revs_eligible\n";
    $i = 0;
    foreach $rev (@revs_eligible) {
        print "i:$i eligible rev:$rev\n";
        $i += 1;
    }
}

###########################################################################
# svn log all the required revisions -- we collect ALL -- it is quicker and
# actually saver, as we don't fill the cmd-line up.
$cmd = "svn log --xml --revision $revs_eligible[0]:HEAD $base_svn_url ";

$xml = ();
open (CMD, "$cmd|");
$xml .= $_
    while (<CMD>);
close(CMD);

$x = new XML::Parser ( Style => 'Subs',
                       Handlers => {
                           Char => \&my_char_handler
                       });
%logentries = ();
$x->parse ($xml);
%logentries_trunk = %logentries;

###########################################################################
# Read in the notes_file and put the notes into the HASH!!!
while (<$notes_file_fh>) {
    chomp;
    my ($rev, $note) = split /,/;
    if ($rev =~ /^#/) {
        next;
    }
    $rev =~ s/^r?([0-9]+)/$1/;

    # Replace the #CMR with a proper link.
    if ($html) {
        $note =~ s%#([0-9]+)%\<A HREF=\"$base_trac_url/ticket/$1\"\>#$1\<\/A\>%g;
    }

    print "rev:$rev After CMR replacement: note:$note\n"
        if ($debug_arg);

    $logentries_trunk{$rev}->{note} = $note;
}
close $notes_file_fh;


###########################################################################
# Output the HTML (or text)
if ($html) {
    my $sec; my $min; my $hour; my $mday; my $mon; my $year; my $wday; my $yday; my $isdst;
   ($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst)=localtime(time);

    my $string = sprintf("Revisions eligible to merge into branch $branch_arg as of %4d-%02d-%02d %02d:%02d:%02d",
                         $year+1900, $mon+1, $mday, $hour, $min, $sec);
    print "<HTML><HEAD><TITLE>$string</TITLE></HEAD>\n";
    print "<BODY><H2>$string</H2>\n";
    print "<TABLE BORDER=1 BGCOLOR=#FFFFFF><TR BGCOLOR=#CCCCCC><TH>Revision</TH><TH>Author</TH><TH>Commit Msg.</TH><TH>Notes</TH></TR>\n";
}

# Now loop over the available revisions and output
$i = 0;
foreach $rev (@revs_eligible) {
    my @lines = split('\n', $logentries_trunk{$rev}->{msg});
    my $bgcolor;
    my $msg = substr ($lines[0], 0, $width_arg);

    $bgcolor = ();
    if ($logentries_trunk{$rev}->{note}) {
        my $note = lc($logentries_trunk{$rev}->{note});

        ### print "Note:$note";
        # Set color accordingly, possibly sed the TRAC-CMR number by a URL to trac
        # Mark MAILED as Yellow
        if (-1 != index $note, "mailed") {
            $bgcolor="#FFFF00";
        }
        # Mark FIX as Red
        elsif (-1 != index $note, "fix") {
            $bgcolor="#FF0000";
        }
        # Mark CMR as green -- but only if for this branch
        elsif (-1 != index $note, "cmr:$branch_arg") {
            $bgcolor="#00FF00";
        }
        # Continue upon MERGED!!! This means this was not detected!
        # (investigate why svn merge / svn mergeinfo does not work)
        elsif (-1 != index $note, "merged") {
            print "\nWARNING: rev:$rev is marked as merged but shows up in `svn mergeinfo`, please check note (for discard) or whether this script is in error.\n\n"
                if ($debug_arg);
            next;
        }

    }
    # If color has not been set, yet...
    if (! $bgcolor) {
        if ($i % 2 == 0) {
            $bgcolor="#EEEEEE";
        } else {
            $bgcolor="#FFFFFF";
        }
    }
    if ($html) {
        print "<TR BGCOLOR=$bgcolor><TD><A HREF=\"$base_trac_url/changeset/$rev\">r$rev</A></TD><TD>$logentries_trunk{$rev}->{author}</TD><TD>$msg</TD><TD>$logentries_trunk{$rev}->{note}</TD></TR>\n";
    } else {
        printf "%d. REV:%6d AUTHOR:%10s MSG:%s",
               $i, $rev, $logentries_trunk{$rev}->{author}, $msg;
        if ($logentries_trunk{$rev}->{note}) {
            printf "   NOTE:%s\n",
                   $logentries_trunk{$rev}->{note};
        } else {
            printf "\n";
        }
    }
    $i += 1;
}

if ($html) {
    print "</TABLE></BODY></HTML>\n";
}


###########################################################################
# All the Handlers functions
###########################################################################

# Called for the first logentry tag in the XML parsing
sub logentry {
    # The beginning logentry tag has arguments of the form:
    # ($expat, 'logentry', attr1, val1, attr2, val2, ...);
    # print "CALL logentry\n";
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
    # print "CALL logentry_\n";
    # print "logentry:$logentry->{revision}\n";
    %logentries->{$logentry->{revision}} = $logentry;
    $logentry = undef;
}

# Called for the last anchor tag in the XML parsing
sub author_ {
    # print "CALL author_\n";
    chomp($chars);
    $chars =~ s/^\n*//;
    $logentry->{author} = $chars;
    $chars = '';
}

# Called for the last date tag in the XML parsing
sub date_ {
    # print "CALL date_\n";
    chomp($chars);
    $chars =~ s/^\n*//;
    $logentry->{date} = $chars;
    $chars = '';
}

# Called for the last revision tag in the XML parsing
sub revision_ {
    # print "CALL revision_\n";
    chomp($chars);
    $chars =~ s/^\n*//;
    $logentry->{revision} = $chars;
    $chars = '';
}

# Called for the last msg tag in the XML parsing
sub msg_ {
    # print "CALL msg_\n";
    chomp($chars);
    $chars =~ s/^\n*//;
    $logentry->{msg} = $chars;
    $chars = '';
}

# Called for general character data in XML parsing
sub my_char_handler {
    my ($expat, $tmp) = @_;
    $chars .= $tmp;
}
