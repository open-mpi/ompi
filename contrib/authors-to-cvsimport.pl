#!/usr/bin/env perl
#
# Copyright (c) 2013 Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
#
# Feed the AUTHORS file to this script either on stdin or as an argument to the
# script.  It will emit a cvsimport-formatted file which is suitable for
# feeding to 'git-svn' or other tools.

use strict;
use warnings;

my $EMAIL_DOMAIN = 'open-mpi-git-mirror.example.com';
my $line;

# discard preamble
do {
    $line = <>;
} until ($line =~ m/^Username\s+Name\s+Affiliation/);

unless (($line = <>) =~ m/^(-+ )(-+ )(-+)$/) {
    die "expected properly formatted table header, stopped";
}
my $login_len = length($1);
my $name_len  = length($2);
my $affil_len = length($3);

while ($line = <>) {
    chomp($line);
    last if ($line =~ m/^\s*$/);

    # 4th arg consumes the substring from $line as we go.
    my $login = substr($line, 0, $login_len, "");
    my $name  = substr($line, 0, $name_len, "");
    my $affil = substr($line, 0, $affil_len, "");

    # strip trailing whitespace
    $login =~ s/\s*$//;
    $name =~ s/\s*$//;
    $affil =~ s/\s*$//;

    printf("%s = %s <%s\@$EMAIL_DOMAIN>\n", $login, $name, $login);
}

printf("%s = %s <%s\@$EMAIL_DOMAIN>\n", '(no author)', 'Unknown', 'unknown');

