#!/usr/bin/env perl
#
# $HEADER$
#
# Look for public symbols in Open MPI libraries and components that
# are "bad"
#

use strict;
use Data::Dumper;

my $prefix = $ARGV[0];
if (! $prefix) {
    die "Must supply the prefix to an Open MPI installation";
}

# Filenames of libraries to look through

my @lib_prefixes = ("libmpi", "libmca" );
my @lib_suffixes = ('\.so', '\.a');

# Filenames of components to look through

my @comp_prefixes = ("mca_");
my @comp_suffices = (".so");

# Acceptable public symbol prefixes

my @acceptable = ("ompi_" , "mpi_", "MPI_", "OMPI_", "MPI::", "PMPI_", "PMPI::", "mca_", "lt_" );

# Troll through the library directory

my $libdir = "$prefix/lib";
if (! -d $libdir) {
    die "libdir does not exist: $libdir";
}
opendir(LIB, $libdir) || die "Unable to open libdir: $libdir";
my @dir_files = readdir(LIB);
closedir(LIB);

# Ok, not efficient.  Sue me.  :-)
# Find all the matching files

my @found_files;
foreach my $prefix (@lib_prefixes) {
    foreach my $suffix (@lib_suffixes) {
        foreach my $file (@dir_files) {
            if ($file =~ /^$prefix.*$suffix$/) {
                push(@found_files, $file);
            }
        }
    }
}

# Run nm on each of those files looking for global symbols with "bad"
# names

my @found_symbols;
foreach my $file (@found_files) {
    open NM, "nm -l -C $libdir/$file|";
    while (<NM>) {
        chomp;
        my ($bogus1, $scope, $symbol, $location) = split(/[ \t]+/, $_);
        if ($scope =~ /[A-Z]/ && 
            $scope !~ /[UVW]/ &&
            $symbol !~ /^_/) {
            push(@found_symbols, {
                file => $file,
                symbol => $symbol,
                scope => $scope,
                location => $location,
            });
        }
    }
}

# Now list all the bad ones

my $ok;
foreach my $symbol (@found_symbols) {
    $ok = 0;
    foreach my $prefix (@acceptable) {
        if ($symbol->{symbol} =~ /^$prefix/) {
            $ok = 1;
        }
    }
    if (! $ok) {
        print "$symbol->{file}: $symbol->{symbol}\n";
        if ($symbol->{location}) {
            print " --> $symbol->{location}\n";
        }
    }
}
