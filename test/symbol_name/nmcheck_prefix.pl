#!/usr/bin/env perl

# Copyright (c) 2017      IBM Corporation. All rights reserved.

sub main {
    if (!$ENV{MYBASE}) {
        print "Test expects env var MYBASE set to base dir\n";
        print "(where ompi/ opal/ orte/ test/ etc live)\n";
        print "And optionally OMPI_LIBMPI_NAME should be set\n";
        print "if MPI is configured with some name other than\n";
        print "\"mpi\" for that.\n";
        exit -1;
    }

# env var MYBASE should be the top dir where ompi/ opal/ orte/ test/ etc live.
# env var OMPI_LIBMPI_NAME should be @OMPI_LIBMPI_NAME@ from automake
#
# Most likely the libs we want to check are
#     ompi/.libs/lib<mpi>.so
#     ompi/mpi/fortran/mpif-h/.libs/lib<mpi>_mpifh.so
#     ompi/mpi/fortran/use-mpi-tkr/.libs/lib<mpi>_usempi.so
#     orte/.libs/libopen-rte.so
#     opal/.libs/libopen-pal.so
# but I hate to assume those are the locations, so I'll use 'find' and
# test whatever ".so"s are found.

    @libs = ();
    $mpi = "mpi";
    if ($ENV{OMPI_LIBMPI_NAME}) {
        $mpi = $ENV{OMPI_LIBMPI_NAME};
    }
    for $name (
        "lib${mpi}.so",
        "lib${mpi}_mpifh.so",
        "lib${mpi}_usempi.so",
        "libopen-rte.so",
        "libopen-pal.so" )
    {
        for $loc (split(/\n/, `find $ENV{MYBASE} -name $name`)) {
            if ($loc !~ /openmpi-gitclone/) {
                push @libs, $loc;
            }
        }
    }

    @mca_symbols = lookup_mca_symbols();

    print "Checking for bad symbol names in the main libs:\n";
    $isbad = 0;
    for $lib (@libs) {
        print "checking $lib\n";
        check_lib_for_bad_exports($lib);
    }
    if ($isbad) { exit(-1); }
}

# Find libraries with names of the form  libmca_coll_basic.a etc
# and presume those to be MCAs that are being built into libmpi.so etc
# rather than the usual case where it becomes mca_coll_basic.so.
#
# When name pollution occurs in an MCA .so we don't care about it.
# When it's an MCA built into libmpi.so we care a little, but aren't
# going to make this testcase fail over it.
sub lookup_mca_symbols {
    my @list;
    my $lib;

    @list = ();
    for $lib (split(/\n/, `find $ENV{MYBASE} -name libmca_[a-zA-Z0-9_-]*\\.a`))
    {
        if ($lib !~ /openmpi-gitclone/) {
            print "NOTE: found static $lib\n";
            push @list, get_nm($lib, 'all');
        }
    }

    return @list;
}

sub check_lib_for_bad_exports {
    my $lib = $_[0];
    my @symbols;
    my $s;

    @symbols = get_nm($lib, 'all');

    # grep to get rid of symbol prefixes that are considered acceptable,
    # leaving behind anything bad:
    @symbols = grep(!/^ompi_/i, @symbols);
    @symbols = grep(!/^opal_/i, @symbols);
    @symbols = grep(!/^orte_/i, @symbols);
    @symbols = grep(!/^orted_/i, @symbols);
    @symbols = grep(!/^oshmem_/i, @symbols);
    @symbols = grep(!/^mpi_/i, @symbols);
    @symbols = grep(!/^pmpi_/i, @symbols);
    @symbols = grep(!/^pmix_/i, @symbols);
    @symbols = grep(!/^pmix2x_/i, @symbols);
    @symbols = grep(!/^PMI_/i, @symbols);
    @symbols = grep(!/^PMI2_/i, @symbols);
    @symbols = grep(!/^MPIR_/, @symbols);
    @symbols = grep(!/^MPIX_/, @symbols);
    @symbols = grep(!/^mpidbg_dll_locations$/, @symbols);
    @symbols = grep(!/^mpimsgq_dll_locations$/, @symbols);
    @symbols = grep(!/^ompit_/i, @symbols);
    @symbols = grep(!/^ADIO_/i, @symbols);
    @symbols = grep(!/^ADIOI_/i, @symbols);
    @symbols = grep(!/^MPIO_/i, @symbols);
    @symbols = grep(!/^MPIOI_/i, @symbols);
    @symbols = grep(!/^MPIU_/i, @symbols);
    @symbols = grep(!/^NBC_/i, @symbols);
    @symbols = grep(!/^mca_/, @symbols);

    @symbols = grep(!/^_fini$/, @symbols);
    @symbols = grep(!/^_init$/, @symbols);
    @symbols = grep(!/^_edata$/, @symbols);
    @symbols = grep(!/^_end$/, @symbols);
    @symbols = grep(!/^__bss_start$/, @symbols);
    @symbols = grep(!/^__malloc_initialize_hook$/, @symbols);

    # The symbols can now be split into two groups: fatal and warning.
    # The warnings will be for symbols that appear to be from MCAs that
    # this build has placed into a main lib, but which would normally
    # be segregated into some mca_*.so
    # for the fatal ones.
    @warning_symbols = @fatal_symbols = ();
    if (scalar(@mca_symbols) > 0) {
        %whash = ();
        for $s (@mca_symbols) {
            $whash{$s} = 1;
        }
        for $s (@symbols) {
            if ($whash{$s}) {
                push @warning_symbols, $s;
            } else {
                push @fatal_symbols, $s;
            }
        }
    } else {
        @fatal_symbols = @symbols;
    }

    for $s (@fatal_symbols) {
        print "    [error]   $s\n";
        $isbad = 1;
    }
    for $s (@warning_symbols) {
        print "    [warning] $s \n";
    }
}

# get_nm /path/to/some/libfoo.so <func|wfunc|all>

sub get_nm {
    my $lib = $_[0];
    my $mode = $_[1];
    my $pattern;
    my $cmd;
    my @tmp;

    $pattern = " [TWBCDVR] ";
    if ($mode eq 'func') { $pattern = " [T] "; }
    if ($mode eq 'wfunc') { $pattern = " [W] "; }

    $cmd = "nm $lib";
    $cmd = "$cmd | grep \"$pattern\"";
    $cmd = "$cmd | sed -e 's/  *\$//' -e 's/.* //'";

    @tmp = split(/\n/, qx#$cmd#);
    @tmp = sort(@tmp);

    return(@tmp);
}

main();
