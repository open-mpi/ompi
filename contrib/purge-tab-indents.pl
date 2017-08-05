#!/usr/bin/perl -w

use strict;
use Cwd;
use File::Basename;
use Text::Tabs;
use Getopt::Long;

# Set to true if the script should merely check for up-to-date copyrights.
# Will exit with status 111 if there are out of date copyrights which this
# script can correct.
my $CHECK_ONLY = 0;
# used by $CHECK_ONLY logic for bookeeping
my $would_replace = 0;

# Set to true to suppress most informational messages.  Only out of date files
# will be printed.
my $QUIET = 0;

# Set to true if we just want to see the help message
my $HELP = 0;

# Set to true if we want to strip blank lines from all files
my $ALL = 0;

GetOptions(
    "help" => \$HELP,
    "quiet" => \$QUIET,
    "check-only" => \$CHECK_ONLY,
    "all" => \$ALL,
) or die "unable to parse options, stopped";

if ($HELP) {
    print <<EOT;
$0 [options]

--help | -h          This help message
--quiet | -q         Only output critical messages to stdout
--check-only         exit(111) if there are modified files with trailing blank lines
--all                Strip trailing blank lines from all fines, even those not modified
EOT
    exit(0);
}

#-------------------------------------------------------------------------------
# predeclare sub for print-like syntax
sub quiet_print {
    unless ($QUIET) {
        print @_;
    }
}

#-------------------------------------------------------------------------------

my @exts = qw(.h .c);

# Find the top-level OMPI source tree dir
my $start = cwd();
my $top = $start;
while (! -f "$top/HACKING") {
    chdir("..");
    $top = cwd();
    die "Can't find top-level directory"
        if ($top eq "/");
}
chdir($start);

my @files = find_modified_files();

if ($#files < 0) {
    exit(0);
}

my $fh;

# Examine each of the files and purge any tabs
foreach my $f (@files) {
    my ($name, $dir, $ext) = fileparse($f, @exts);

    if ($ext !~ /h/ && $ext !~ /c/) {
        next;
    }

    quiet_print "FILE: $f\n";

    my ($dev,$ino,$mode,$nlink,$uid,$gid,$rdev,$size,
        $atime,$mtime,$ctime,$blksize,$blocks)
           = stat($f);

    open(FILE, "$f");
    my @lines_with_tabs = <FILE>;
    close(FILE);

    my @expanded_lines = expand(@lines_with_tabs);
    open(TEMP, ">temp.txt");
    print TEMP @expanded_lines;
    close(TEMP);
    system("mv temp.txt $f");
    chmod($mode, $f);
}

# Returns a list of file names (relative to pwd) which the VCS considers to be modified.
sub find_modified_files {
    my @files = ();

    # Number of path entries to remove from ${top}-relative paths.
    # (--show-cdup either returns the empty string or sequence of "../"
    # entries, always ending in a "/")
    my $n_strip = scalar(split(m!/!, scalar(`git rev-parse --show-cdup`))) - 1;

    # "." restricts scope, but does not get us relative path names
    my $cmd = "git status -z --porcelain --untracked-files=no .";
    my $lines = `$cmd`;

    # From git-status(1):
    # X          Y     Meaning
    # -------------------------------------------------
    #           [MD]   not updated
    # M        [ MD]   updated in index
    # A        [ MD]   added to index
    # D         [ M]   deleted from index
    # R        [ MD]   renamed in index
    # C        [ MD]   copied in index
    # [MARC]           index and work tree matches
    # [ MARC]     M    work tree changed since index
    # [ MARC]     D    deleted in work tree
    # -------------------------------------------------
    # D           D    unmerged, both deleted
    # A           U    unmerged, added by us
    # U           D    unmerged, deleted by them
    # U           A    unmerged, added by them
    # D           U    unmerged, deleted by us
    # A           A    unmerged, both added
    # U           U    unmerged, both modified
    # -------------------------------------------------
    # ?           ?    untracked
    # -------------------------------------------------
    my $s1 = "";
    my $s2 = "";
    my $fullname = "";
    foreach my $line (split /\x{00}/, $lines) {
        my $keep = 0;
        unless (($s1, $s2, $fullname) = $line =~ m/^(.)(.) (.*)$/) {
            next;
        }

        if ($ALL) {
            $keep = 1;
        } else {
            # ignore all merge cases
            next if ($s1 eq "D" and $s2 eq "D");
            next if ($s1 eq "A" and $s2 eq "A");
            next if ($s1 eq "U" or $s2 eq "U");

            # only update for actually added/modified cases, no copies,
            # renames, etc.
            $keep = 1 if ($s1 eq "M" or $s2 eq "M");
            $keep = 1 if ($s1 eq "A");
        }

        if ($keep) {
            my $relname = $fullname;
            $relname =~ s!^([^/]*/){$n_strip}!!g;

            push @files, $relname
                if (-f $relname);
        }
    }
    return @files;
}

exit 0;
