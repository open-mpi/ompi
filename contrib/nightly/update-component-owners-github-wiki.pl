#!/usr/bin/env perl
#
# Copyright (c) 2015 Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
#

use strict;
use warnings;

my $ompi_github_url = "https://github.com/open-mpi/ompi.git";
my $ompi_wiki_url = "git\@github.com:open-mpi/ompi.wiki.git";

my $tmpdir;

#------------------------------------------------------------------

sub cleanup {
    chdir("/");
    system("rm -rf $tmpdir")
        if (defined($tmpdir));
}

sub mydie {
    cleanup();

    die @_;
}

#------------------------------------------------------------------

$tmpdir = "/tmp/ompi-github-owners.$$";
system("rm -rf $tmpdir");
mkdir($tmpdir);
mydie "Couldn't make $tmpdir"
    if (! -d $tmpdir);

#------------------------------------------------------------------

chdir($tmpdir);
my $ret = system("git clone $ompi_github_url");
mydie "Could not clone $ompi_github_url"
    if ($ret != 0);

# Run the check-owner.pl script
chdir("ompi");
$ret = system("./contrib/check-owner.pl");

#------------------------------------------------------------------

# Write new ComponentOwners.md file with each of the *.md files found
opendir(my $dh, ".") ||
    mydie "Can't opendir .";
my @md_files = grep { /\.md$/ } readdir($dh);
closedir($dh);

open(OUT, ">ComponentOwners.md") ||
    mydie "Can't write to ComponentOwners.md";
print OUT "Owners of each component in the Open MPI source tree,
listed by project.\n";
foreach my $md (@md_files) {
    $md =~ m/([a-z]+)_mca/;
    my $project = $1;
    open(IN, $md) ||
        mydie "Can't read $md file";
    my $contents;
    $contents .= $_
        while (<IN>);
    close(IN);

    print OUT "
## $project layer

$contents";
}
close(OUT);

#------------------------------------------------------------------

# Checkout the OMPI wiki, update with the new ComponentOwners.md
chdir($tmpdir);
$ret = system("git clone $ompi_wiki_url");
mydie "Could not clone $ompi_wiki_url"
    if ($ret != 0);

chdir("ompi.wiki");
system("cp $tmpdir/ompi/ComponentOwners.md .");
$ret = system("git commit ComponentOwners.md -m 'Automated update of ComponentOwners'");
$ret = $ret >> 8;
if ($ret == 1) {
    print "--> No change, nothing to push\n";
} else {
    $ret = system("git push");
    mydie "Could not git push"
        if ($ret != 0);
}

cleanup();
exit(0);
