#!/usr/bin/perl

# Copyright (c) 2019-2020 IBM Corporation. All rights reserved.
# $COPYRIGHT$

# This script parses mpi.h to stdout of the form
# int MPI_Send(void *, int, MPI_Datatype, int, int, MPI_Comm);
# Only parsing out the MPI_* symbols (skip over PMPI).

# args:
#   1. /path/to/mpi.h.in
# which is expected to contain lines of the form
# OMPI_DECLSPEC  int MPI_Send(const void *buf, int count,
#                           MPI_Datatype datatype, int dest,
#                           int tag, MPI_Comm comm);
#   2. /path/to/cppoutput

$mpih = $ARGV[0];
$cppoutput = $ARGV[1];
if ( ! -e $mpih || ! -e $cppoutput ) {
    print "Error, input file [$mpih] not found\n";
    exit(-1);
}

# Remove all variable names, leaving only recognized types behind,
# possibly with const in front of them
sub fixline {
    my $string = $_[0];
    my $rvtype;
    my $mpifunc;
    my @args;
    my $arg;
    my @modargs;
    my $type;
    my $stars;

    if ($string !~ /^\s*([A-Za-z0-9_]+)\s+(MPI_[A-Za-z0-9_]+)\s*\(\s*(.*)\s*\).*/) {
        print "ERROR, unrecognized syntax:\n";
        print "$string\n";
        exit(-1);
    }
    $rvtype = $1;
    $mpifunc = $2;
    @args = split(/\s*,\s*/, $3);

    @modargs = ();
    for $arg (@args) {
        $const = 0;
        if ($arg =~ s/^const\s+//) {
            $const = 1;
        }
        # check for things like
        #   int *name[]
        # then
        #   int *
        if ($arg =~ /^([A-Za-z0-9_]+)\s+([ *]*)[A-Za-z0-9_]+\s*([ \[\]0-9]*)/) {
            $type = $1;
            $stars = "$2$3";
            $stars =~ s/\s//g;
        }
        elsif ($arg =~ /^([A-Za-z0-9_]+)\s*([ *]*)/) {
            $type = $1;
            $stars = $2;
            $stars =~ s/\s//g;
        }
        else {
            print "ERROR, unrecognized arg [$arg]\n";
            exit(-1);
        }
        if ("$stars" ne "") {
            $stars = " $stars";
        }
        if (!$const) {
            push(@modargs, "$type$stars");
        } else {
            push(@modargs, "const $type$stars");
        }

# This is a long list of acceptable values for $type, but it helps
# ensure the parsing isn't broken.
        if (   $type ne "int"
            && $type ne "void"
            && $type ne "char"
            && $type ne "MPI_Comm"
            && $type ne "MPI_Datatype"
            && $type ne "MPI_Aint"
            && $type ne "MPI_Op"
            && $type ne "MPI_Win"
            && $type ne "MPI_Request"
            && $type ne "MPI_Info"
            && $type ne "MPI_Errhandler"
            && $type ne "MPI_Handler_function"
            && $type !~ /MPI_[A-Za-z]*_errhandler_function/
            && $type !~ /MPI_[A-Za-z]*_copy_attr_function/
            && $type !~ /MPI_[A-Za-z]*_delete_attr_function/
            && $type ne "MPI_Group"
            && $type ne "MPI_Fint"
            && $type ne "MPI_File"
            && $type ne "MPI_Offset"
            && $type ne "MPI_Status"
            && $type ne "MPI_Count"
            && $type !~ /MPI_Grequest_[a-z]*_function/
            && $type ne "MPI_Message"
            && $type ne "MPI_Copy_function"
            && $type ne "MPI_Delete_function"
            && $type ne "MPI_User_function"
            && $type !~ /MPI_Datarep_[a-z]*_function/
            && $type ne "MPI_T_enum"
            && $type ne "MPI_T_cvar_handle"
            && $type ne "MPI_T_pvar_session"
            && $type ne "MPI_T_pvar_handle"
            )
        {
            print "ERROR, unrecognized type $type\n";
            exit(-1);
        }
    }

    # ("MPI_Aint", "int", "void", "char", "MPI_Status", "MPI_Datatype", "MPI_Request", "MPI_Comm")

    $string = "$rvtype $mpifunc(" .
        join(", ", @modargs) . ");";

    return $string;
}

# This gets a little convoluted, because we need cpp output to identify which functions are
# really being declared in mpi.h. But we don't want to cpp output for reading the actual
# declarations (for ex MPI_Comm_c2f's return gets morphed to ompi_fortran_integer_t when
# the proper MPI declaration would be the un-cpp'ed MPI_Fint).

%use_function = ();
open $fh, "< $cppoutput";
while ($line = <$fh>) {
    chomp $line;
    if ($line =~ /^\s+[A-Za-z0-9_]*\s+(MPI_[A-Za-z0-9_]*) *\(/) {
        $use_function{$1} = 1;
    }
}

open $fh, "< $mpih";
while ($line = <$fh>) {
    chomp $line;
    if ($line =~ /^OMPI_DECLSPEC\s+[A-Za-z0-9_]*\s+(MPI_[A-Za-z0-9_]*) *\(/) {
        $func = $1;
        $line =~ s/^OMPI_DECLSPEC\s+//;
        while (!($line =~ /\)/)) {
            $more = <$fh>;
            chomp $more;
            $line = "$line $more";
        }

        if ($line =~ /MPI_Pcontrol/) {
# I don't think a va_list can be passed through to the next level in
# general.. that's why things like vprintf exist.
            $line =~ s/, \.\.\.//;
        }
        $line = fixline($line);
        if ($use_function{$func}) {
            print "$line\n";
        }
    }
}
close($fh);
