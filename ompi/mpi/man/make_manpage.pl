#!/usr/bin/env perl
#
# Copyright (c) 2015      Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# Copyright (c) 2015      Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
#
# Subroutine to generate a bunch of Fortran declarations and symbols
#

use strict;

use Getopt::Long;

my $package_name;
my $package_version;
my $ompi_date;
my $opal_date;
my $orte_date;
my $cxx = '1';
my $fortran = '1';
my $f08 = '1';
my $input;
my $output;
my $help_arg = 0;

&Getopt::Long::Configure("bundling");
my $ok = Getopt::Long::GetOptions("package-name=s" => \$package_name,
                                  "package-version=s" => \$package_version,
                                  "ompi-date=s" => \$ompi_date,
                                  "opal-date=s" => \$opal_date,
                                  "orte-date=s" => \$orte_date,
                                  "cxx!" => \$cxx,
                                  "fortran!" => \$fortran,
                                  "f08!" => \$f08,
                                  "input=s" => \$input,
                                  "output=s" => \$output);

if ($help_arg || !$ok ||
    !defined($input) ||
    !defined($output) ||
    !defined($package_name) ||
    !defined($package_version) ||
    !defined($ompi_date) ||
    !defined($opal_date) ||
    !defined($orte_date)) {
    print "Usage: $0 --package-name=<package name> --package-version=<package version> --ompi-date=<ompi date> --opal-date=<opal date> --orte-date=<orte date> --input=<input file> --output=<output file> [--nocxx] [ --nofortran] [--nof08]\n";
    exit(1 - $ok);
}

open(FILE, $input) ||
    die "Can't open $input";
my $file;
$file .= $_
    while(<FILE>);
close(FILE);

$file =~ s/#PACKAGE_NAME#/$package_name/g;
$file =~ s/#PACKAGE_VERSION#/$package_version/g;
$file =~ s/#OMPI_DATE#/$ompi_date/g;
$file =~ s/#OPAL_DATE#/$opal_date/g;
$file =~ s/#ORTE_DATE#/$orte_date/g;

if ($cxx == 0) {
    $file =~ s/\n\.SH C\+\+ Syntax.+?\n\.SH/\n\.SH/s;
}

if ($fortran == 0) {
    $file =~ s/\n\.SH Fortran Syntax.+?\n\.SH/\n\.SH/s;
}

if ($f08 == 0) {
    $file =~ s/\n\.SH Fortran 2008 Syntax.+?\n\.SH/\n\.SH/s;
}

open(FILE, ">$output") ||
    die "Can't open $output";
print FILE $file;
close(FILE);

exit(0);

