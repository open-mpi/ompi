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
# Look for public symbols in Open MPI libraries and components that
# are "bad"
#

use strict;
use Getopt::Long;

# Filenames of libraries to look through

my @lib_prefixes = ("libmpi", "libmca" );
my @lib_suffixes = ('\.so', '\.a');

# Filenames of components to look through

my @comp_prefixes = ("mca_");
my @comp_suffixes = (".so");

# Acceptable public symbol prefixes

my @lib_acceptable = ("ompi_" , "mpi_", "MPI_", "OMPI_", "MPI::", "PMPI_", "PMPI::", "mca_", "lt_", "orte_", "epoll", "opal_", "MPIR_",
# Portland Group compiler adds a bunch of these
                      "pghpf_",
);
my @comp_acceptable = ("mca_", "ompi_", "orte_", "opal_",
# The latest incarnation of ROMIO adds these
                      "MPIO_", "MPIOI_", "ADIO_", "ADIOI_", "MPIU_",
);

# Subject line for e-mail

my $subject = "Open MPI Illegal symbol report";

#--------------------------------------------------------------------------

# Troll through a list of directories looking for libraries

sub find_libs {
    return find_files(\@_, \@lib_prefixes, \@lib_suffixes);
}

#--------------------------------------------------------------------------

# Troll through a list of directories looking for components

sub find_comps {
    return find_files(\@_, \@comp_prefixes, \@comp_suffixes);
}

#--------------------------------------------------------------------------

sub find_files {
    my ($dirs, $prefixes, $suffixes) = @_;

    my @found_files;
    foreach my $dir (@$dirs) {
        if (! -d $dir) {
            die "filedir does not exist: $dir";
        }
        opendir(DIR, $dir) || die "Unable to open dir: $dir";
        my @files = readdir(DIR);
        closedir(DIR);

        # Ok, not efficient.  Sue me.  :-)
        # Find all the matching files

        foreach my $prefix (@$prefixes) {
            foreach my $suffix (@$suffixes) {
                foreach my $file (@files) {
                    if ($file =~ /^$prefix.*$suffix$/) {
                        push(@found_files, "$dir/$file");
                    }
                }
            }
        }
    }

    \@found_files;
}

#--------------------------------------------------------------------------

# Troll through a list of library files and look for "bad" symbol
# names

sub check_libs {
    return check_files(\@_, \@lib_acceptable);
}

#--------------------------------------------------------------------------

# Troll through a list of component files and look for "bad" symbol
# names

sub check_comps {
    return check_files(\@_, \@comp_acceptable);
}


#--------------------------------------------------------------------------

sub check_files {
    my ($files, $acceptable)  = @_;
    my $bad_symbols;

    my $ok;
    foreach my $file (@$files) {
        open NM, "nm -l -C $file|";
        while (<NM>) {
            chomp;
            my ($bogus1, $scope, $symbol, $location) = split(/[ \t]+/, $_);

            # Only look for symbols that are a) global [i.e.,
            # uppercase scope], b) not U, V, or W

            if ($scope =~ /[A-Z]/ && 
                $scope !~ /[UVW]/ &&
                $symbol !~ /^_/) {

                $ok = 0;
                foreach my $prefix (@$acceptable) {
                    if ($symbol =~ /^$prefix/) {
                        $ok = 1;
                        last;
                    }
                }
                if (!$ok) {
                    my $line_num;
                    if (!$location) {
                        $location = "Unknown source file";
                    }

                    if ($location =~ /src\//) {
                        $location =~ s/.+?\/(src\/)/$1/;
                    }
                    if ($location =~ /:/) {
                        $line_num = $location;
                        $line_num =~ s/.+:([0-9]+)/$1/;
                        $location =~ s/(.+):.+/$1/;
                    }
                    push(@{$bad_symbols->{$file}->{$location}}, {
                        symbol => $symbol,
                        line => $line_num,
                        scope => $scope,
                    });
                }
            }
        }
    }

    \$bad_symbols;
}

#--------------------------------------------------------------------------

# find a program from a list and load it into the target variable
sub find_program {
    my @names = @_;

    # loop through the list and save the first one that we find
    my $i = 0;
    while ($i <= $#names) {
        my $ret = system("which $names[$i] 2>&1 >/dev/null");
        my $status = $ret >> 8;
        if ($status == 0) {
            return $names[$i];
        }
        ++$i;
    }
    return undef;
}

#--------------------------------------------------------------------------

# Did we find anything?

sub mail_symbols {
    my ($bad, $mail) = @_;

    foreach my $file (sort keys(%{$$bad})) {
        print $mail "File: $file\n";
        foreach my $location (sort keys(%{$$bad->{$file}})) {
            print $mail "  Source: $location\n";
            my $array = $$bad->{$file}->{$location};
            foreach my $symbol (@$array) {
                if ($symbol->{line}) {
                    print $mail "  --> Line $symbol->{line}: $symbol->{symbol}\n";
                } else {
                    print $mail "  --> $symbol->{symbol}\n";
                }
            }
            
        }
        print $mail "\n";
    }
}

#--------------------------------------------------------------------------

#
# main
#

my $mail;

my @libdir_arg;
my @lib_arg;
my @compdir_arg;
my @comp_arg;
my @prefix_arg;
my $email_arg;
my $delete_arg;

# parse the command line
&Getopt::Long::Configure("bundling", "require_order");
my $ok = Getopt::Long::GetOptions("libdir|l=s" => \@libdir_arg,
                                  "lib=s" => \@lib_arg,
                                  "compdir|c=s" => \@compdir_arg,
                                  "comp=s" => \@comp_arg,
                                  "prefix|p=s" => \@prefix_arg,
                                  "email|e=s" => \$email_arg,
                                  "delete" => \$delete_arg,
                                  );

# Check args

if (!$email_arg) {
    die "Must have an e-mail argument: specify --email <address>";
}
if ($#libdir_arg < 0 && $#lib_arg < 0 &&
    $#compdir_arg < 0 && $#comp_arg < 0 &&
    $#prefix_arg < 0) {
    die "Nothing to do!";
}

# Find a mail program

$mail = find_program(qw(Mail mailx mail));
die "Could not find mail program; aborting in despair\n"
    if (!defined($mail));

# Look for libraries

my @libs;
if ($#prefix_arg >= 0) {
    foreach my $prefix (@prefix_arg) {
        push(@libdir_arg, "$prefix/lib")
            if (-d "$prefix/lib");
    }
}
if ($#libdir_arg >= 0) {
    my $found = find_libs(@libdir_arg);
    push(@libs, @$found);
}
foreach my $dir (@lib_arg) {
    push(@libs, $dir)
        if (-f $dir);
}

my $bad_libsymbols = check_libs(@libs);

# Look for components

my @comps;
if ($#prefix_arg >= 0) {
    foreach my $prefix (@prefix_arg) {
        push(@compdir_arg, "$prefix/lib/openmpi")
            if (-d "$prefix/lib/openmpi");
    }
}
if ($#compdir_arg >= 0) {
    my $found = find_comps(@compdir_arg);
    push(@comps, @$found);
}

foreach my $dir (@comp_arg) {
    push(@comps, $dir)
        if (-f $dir);
}

my $bad_compsymbols = check_comps(@comps);

if ($$bad_compsymbols || $$bad_libsymbols) {

    open MAIL, "|$mail -s \"$subject\" \"$email_arg\"" ||
        die "Could ot open pipe to output e-mail\n";
    print MAIL "Found global symbols with missing or illegal prefixes\n\n";

    if ($$bad_compsymbols) {
        mail_symbols($bad_compsymbols, *MAIL{IO});
    }
    if ($$bad_libsymbols) {
        mail_symbols($bad_libsymbols, *MAIL{IO});
    }
   
    print MAIL "\nYour friendly server,\nCyrador\n";
    close MAIL;
}

# If --delete was given, remove all the dirs and files listed on the
# command line -- ignore any errors in case subdirectories were given.

if ($delete_arg) {
    foreach my $file (@lib_arg) {
        system("rm -rf $file >/dev/null 2>/dev/null");
    }
    foreach my $file (@comp_arg) {
        system("rm -rf $file >/dev/null 2>/dev/null");
    }
    foreach my $file (@libdir_arg) {
        system("rm -rf $file >/dev/null 2>/dev/null");
    }
    foreach my $file (@compdir_arg) {
        system("rm -rf $file >/dev/null 2>/dev/null");
    }
    foreach my $file (@prefix_arg) {
        system("rm -rf $file >/dev/null 2>/dev/null");
    }
}

# All done

exit(0);
