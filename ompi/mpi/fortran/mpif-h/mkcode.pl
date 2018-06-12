#!/usr/bin/perl

# Expects input in flist to be a bunch of C functions like
# int MPI_Send(void*, int, MPI_Datatype, int, int, MPI_Comm);

# Print to files, declaring, defining and seting function
# pointers for ompi_fptr_MPI_Send etc:
#   - constructed_fptr_declarations.h
#   - constructed_fptr_definitions.h
#   - constructed_fptr_assignments_MPI.h
#   - constructed_fptr_assignments_PMPI.h

sub main {
    $file = "flist";
    open $fh, "< $file";

    $file = "constructed_fptr_declarations.h";
    open $fh1, "> $file";
    $file = "constructed_fptr_definitions.h";
    open $fh2, "> $file";
    $file = "constructed_fptr_assignments_MPI.h";
    open $fh3, "> $file";
    $file = "constructed_fptr_assignments_PMPI.h";
    open $fh4, "> $file";

# Start constructed_fptr_declarations.h with a
# #define OMPI_FORTRAN_FPTR(fn) ompi_fptr_ ## fn
# that the various send_f.c files will use
# This way a configure-time option could be made to disable this feature completely.

    print $fh1 <<"EODATA";
// At some point the below might become something configurable
#define OMPI_FORTRAN_USE_FPTR 1

#if OMPI_FORTRAN_USE_FPTR
#define OMPI_FORTRAN_FPTR(fn) ompi_fptr_ ## fn
#else
#define OMPI_FORTRAN_FPTR(fn) P ## fn
#endif

EODATA

    while ($line = <$fh>) {
        chomp $line;
        $oline = $line;

        $ret_type = $line;
        $ret_type =~ s/ .*$//;
        $line =~ s/[^ ]* *//; # line should be sitting at "MPI_Foo..." now
        $func_name = $line;
        $func_name =~ s/\(.*//;
        $line =~ s/[^(]*\(*//; # line should be sitting at "type, type, ..." now
        @type_list = ();
        while (!($line =~ /^\)/)) {
            $type = $line;
            $type =~ s/^\s*//;
            $type =~ s/\s*([,)])/$1/;
            $type =~ s/\s*([A-Za-z_\[\]0-9 ]* *\**)[,)].*/$1/;
            $type =~ s/  */ /g;
            $type =~ s/ \*/\*/g;
            if ($type ne "void") {
                @type_list = (@type_list, $type);
            }
            $line =~ s/^[^,\)]*//;
            $line =~ s/^,//;
        }
  
# From @type_list, construct @var_list
        @var_list = ();
        @fvar_list = ();
        $fnstring = 0; # count number of char* that need ints on the end
        @fstring_indexes = ();
        for $i (0 .. $#type_list) {
            $arg = $type_list[$i];
            @var_list = (@var_list, "v$i");
        }

# definitions of the function pointer arrays
        $argdefs = defstring(\@type_list, \@var_list);
        print $fh1 "extern $ret_type (*ompi_fptr_${func_name})($argdefs);\n";
        print $fh2 "$ret_type (*ompi_fptr_${func_name})($argdefs);\n";

# assignments for the function pointer arrays

        print $fh3 "ompi_fptr_${func_name} = &${func_name};\n";
        print $fh4 "ompi_fptr_${func_name} = &P${func_name};\n";
    }
    close($fh1);
    close($fh2);
    close($fh3);
    close($fh4);
    close($fh);

    system("touch constructed_code_done.h");
}

# Given a type "int" and a var name "v1" output "int v1".
# Also handle uglier cases like type "int []" to output "int v1[]"
# and "const char*" to output "const char* v1"
sub vardecl {
    my $type = $_[0];
    my $vname = $_[1];
    my $decl;
    if ($type =~ /\[/) {
        $decl = $type;
        $decl =~ s/\[/$vname\[/;
    } else {
        $decl = "$type $vname";
    }
    return $decl;
}

# input @type_list and @var_list
# output string like "int v1, int v2, char* v3"
sub defstring {
    my @type_list = @{$_[0]};
    my @var_list = @{$_[1]};
    my $argdefs;
    my $i;
    my $def;
    my @outlist;

    @outlist = ();
    for ($i=0; $i<=$#type_list; ++$i) {
        $def = vardecl($type_list[$i], $var_list[$i]);
        push(@outlist, $def);
    }
    $argdefs = join(", ", @outlist);
    if ($argdefs eq "") {
        $argdefs = "void";
    }
    return $argdefs;
}

main();
