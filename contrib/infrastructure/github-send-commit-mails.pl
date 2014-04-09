#!/usr/bin/env perl

use strict;

use Cwd;

die "Must specify location of source git repo"
    if ($#ARGV < 0);

sub doit {
    my ($cmd, $repo) = @_;

    my $rc;
    my $outfile = "/tmp/github-send-email-tmp.$$";
    unlink($outfile);
    $rc = system("$cmd >$outfile 2>&1");
    if (0 != $rc) {
        print "Command failed:

Command: $cmd
Repo:    $repo
Output:\n";
        open(IN, $outfile);
        print $_
            while (<IN>);
        close(IN);
        die "Aborting";
    }
    unlink($outfile);
}

foreach my $src_repo (@ARGV) {
    die "Specified location of source git repo is invalid"
        if (! -d $src_repo);
    chdir($src_repo);
    die "Could not chdir to $src_repo"
        if (getcwd() != $src_repo);

    doit("/u/mpiteam/git/local/bin/git fetch", $src_repo);
    doit("/u/mpiteam/git/local/bin/git push email", $src_repo);
}

