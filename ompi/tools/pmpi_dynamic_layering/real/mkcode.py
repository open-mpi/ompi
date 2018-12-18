#!/usr/bin/env python3

# Copyright (c) 2019-2020 IBM Corporation. All rights reserved.
# $COPYRIGHT$

# The data we want from the MPI standard amounts to
# int MPI_Send(void*, int, MPI_Datatype, int, int, MPI_Comm)
# for example for each C function.

# Print to files
# 1. declarations for fptr_MPI_Send function pointer arrays
#    -> constructed_wrapper_variable_defs.h
# 2. assignments of function pointers for element [i] of each ftpr array
#    -> constructed_wrapper_variable_assignments.h
# 3. Wrappers for all the C MPI_Send etc functions
#    -> constructed_wrapper_cfunctions.h
# 4. Wrappers for all the F mpi_send etc functions
#    -> constructed_wrapper_ffunctions.h

import re
import sys
import json

def main():
    with open('mpi_calls.json') as fh:
        mpi_call = json.load(fh)

    fh1 = open("constructed_wrapper_variable_defs.h", 'w')
    fh2 = open("constructed_wrapper_variable_assignments.h", 'w')
    fh3 = open("constructed_wrapper_cfunctions.h", 'w')
    fh4 = open("constructed_wrapper_ffunctions.h", 'w')

    for cfunc in dict.keys(mpi_call):
# mpi_calls.json doesn't include callbacks or predefined functions
        if (re.match(".*_c2f08$", cfunc)):
            continue
        if (re.match(".*_f082c$", cfunc)):
            continue
        if (re.match(".*_f2f08$", cfunc)):
            continue
        if (re.match(".*_f082f$", cfunc)):
            continue
# And skip things that are allowed to be macros that won't work with PMPI
        if (cfunc == "MPI_Wtime" or
            cfunc == "MPI_Wtick" or
            cfunc == "MPI_Aint_add" or
            cfunc == "MPI_Aint_diff"
        ):
            continue

        print(f"processing {cfunc}")

        ret_type = mpi_call[cfunc]['returnType']
        fret_type = "void";
        if (ret_type == 'double'):
            fret_type = 'double'

        func_name = cfunc
        ffunc_name = cfunc.lower()
        type_list = []
        var_list = []
        ftype_list = []
        fvar_list = []
        for param in mpi_call[cfunc]['parameters']:
# Examples of suppress settings:
#   c_parameter : every function's ierr
#   f90_parameter : argc/argv in MPI_Init
            skip_c = param['suppress_c']
            skip_f = param['suppress_f90']

            decl = param['whole_c']

            varname = param['name']
            m = re.match(f'(.*[^\s]+)\s*{varname}\s*([\[\]0-9]*)$', decl);
            if (m):
                typename = m.group(1) + ' ' + m.group(2)
                ftypename = "void*"
            elif (decl == "" and param["is_fortran_ierr"]):
                ftypename = "void*"
            else:
                print(f"*** error, {decl} not formatted as expected")
                continue

#           if (param["kind"] == 'FUNCTION'):
#               ftypename = "voidfuncptr"

            if (not skip_c):
                type_list.append(typename)
                var_list.append(varname)
            if (not skip_f):
                ftype_list.append(ftypename)
                fvar_list.append(varname)

# In the list of types we ended up with, see how many char* there are, each
# will get an "int len_<var>" added to the end of the list
        for typename,varname in zip(ftype_list, fvar_list):
            m = re.match(r'.* char\s*\*.*', ' '+typename)
            if (m):
                ftype_list.append('int')
                fvar_list.append('len_' + varname)

# ---- generate:
# definitions of the function pointer arrays
# Incoming MPI_Send -> define fptrs MPI_Send and PMPI_Send
# Also define fortran fptrs.
        argdefs = defstring(type_list, var_list)
        fargdefs = defstring(ftype_list, fvar_list)
        fh1.write(f"static {ret_type} (*(fptr_{func_name}[MAX_LEVELS]))({argdefs});\n")
        fh1.write(f"static {fret_type} (*(fptr_{ffunc_name}[MAX_LEVELS]))({fargdefs});\n")

# ---- generate:
# assignments for the function pointer arrays
# The extra "if" on every dlsym is for libs like "libmywrap.so" that have
# an "ldd" that includes "libmpi.so.1".  A dlsym on such libs doesn't only
# give MPI_Foo out of libmywrap.so, it actually looks down into libmpi.so.1
# etc to find MPI_Foo, we want to specifically decline those MPI_Foo's.
        fh2.write(f"*(void**)(&fptr_{func_name}[i]) = " +
            f"lookup_fp(i, 0, \"{func_name}\");\n")
        fh2.write(f"*(void**)(&fptr_{ffunc_name}[i]) = " +
            f"lookup_fp(i, 1, \"{ffunc_name}\");\n")
 
        if (func_name != 'MPI_Pcontrol'):
# define the functions
            printfunc(fh3, "C", "m", ret_type, f"{func_name}",
                f"fptr_{func_name}", type_list, var_list)
            printfunc(fh3, "C", "p", ret_type, f"P{func_name}",
                f"fptr_{func_name}", type_list, var_list)
# and fortran
            printfunc(fh4, "F", "m", fret_type, f"{ffunc_name}",
                f"fptr_{ffunc_name}", ftype_list, fvar_list);
            printfunc(fh4, "F", "m", fret_type, f"{ffunc_name}_",
                f"fptr_{ffunc_name}", ftype_list, fvar_list);
            printfunc(fh4, "F", "m", fret_type, f"{ffunc_name}__",
                f"fptr_{ffunc_name}", ftype_list, fvar_list);
            printfunc(fh4, "F", "p", fret_type, f"p{ffunc_name}",
                f"fptr_{ffunc_name}", ftype_list, fvar_list);
            printfunc(fh4, "F", "p", fret_type, f"p{ffunc_name}_",
                f"fptr_{ffunc_name}", ftype_list, fvar_list);
            printfunc(fh4, "F", "p", fret_type, f"p{ffunc_name}__",
                f"fptr_{ffunc_name}", ftype_list, fvar_list);
#           upcase = ffunc_name.upper();
#           printfunc(fh4, "F", "m", fret_type, f"{upcase}",
#               f"fptr_{ffunc_name}", ftype_list, fvar_list);
#           printfunc(fh4, "F", "m", fret_type, f"{upcase}_",
#               f"fptr_{ffunc_name}", ftype_list, fvar_list);
#           printfunc(fh4, "F", "m", fret_type, f"{upcase}__",
#               f"fptr_{ffunc_name}", ftype_list, fvar_list);
#           printfunc(fh4, "F", "p", fret_type, f"P{upcase}",
#               f"fptr_{ffunc_name}", ftype_list, fvar_list);
#           printfunc(fh4, "F", "p", fret_type, f"P{upcase}_",
#               f"fptr_{ffunc_name}", ftype_list, fvar_list);
#           printfunc(fh4, "F", "p", fret_type, f"P{upcase}__",
#               f"fptr_{ffunc_name}", ftype_list, fvar_list);

    fh1.close()
    fh2.close()
    fh3.close()
    fh4.close()

#   system("touch constructed_wrapper_done.h");
# end main

# Given a type "int" and a var name "v1" output "int v1".
# Also handle uglier cases like type "int []" to output "int v1[]"
# and "const char*" to output "const char* v1"
def vardecl(tname, vname):
    if (re.match(r".*\[", tname)):
        decl = tname
        decl = re.sub(r"\[", f"{vname}[", decl, count=1)
    else:
        decl = f"{tname} {vname}"

    return(decl)

# input @type_list and @var_list
# output string like "int v1, int v2, char* v3"
def defstring(type_list, var_list):
    args = []
    for t,v in zip(type_list, var_list):
        args.append(vardecl(t, v))

    if (args == []):
        return "void"

    return ", ".join(args)

def printfunc(fh, lang, is_m_or_p, ret_type, func_name, fptrname, type_list, var_list):
# lang is C / F
# is_m_or_p is m / p for (mpi / pmpi)

    argdefs = defstring(type_list, var_list)
    argvars = ", ".join(var_list)

    if (re.match("^P*MPI_Pcontrol$", func_name)):
        argdefs = f"{argdefs}, ..."

    if (is_m_or_p == "m"):
        nextlev_code = "nextlev = entrylev;" ;
    else:
        nextlev_code = "nextlev = entrylev + 1;" ;

    if (lang == "C"):
        rvdecl = f"{ret_type} rv;";
        rvequals = "rv = ";
        returnrv = "return(rv);";
    else:
        rvdecl = "";
        rvequals = "";
        returnrv = "return;";
        if (ret_type != "void"):
            rvdecl = f"{ret_type} rv;";
            rvequals = "rv = ";
            returnrv = "return(rv);";

    fh.write(f"""\
{ret_type} {func_name}({argdefs});

{ret_type}
{func_name}({argdefs}) {{
    {rvdecl}
    int entrylev, nextlev;
    int *calldepth;
    INIT_STUFF
#ifndef _WIN32
    calldepth = (int*) pthread_getspecific(depthkey);
#else
    calldepth = (int*) TlsGetValue(depthkey);
#endif
    entrylev = *calldepth;
    {nextlev_code}
    if (nextlev >= nwrapper_levels) {{ --nextlev; }}
    while (! {fptrname}[nextlev] && nextlev<nwrapper_levels) {{ ++nextlev; }}
    if (nextlev >= nwrapper_levels) {{
        printf("Fatal Error: unable to find symbol at level>=%d for %s\\n",
            entrylev, "{func_name}");
        exit(1);
    }}
    *calldepth = nextlev;
    {rvequals}{fptrname}[nextlev]({argvars});
    *calldepth = entrylev;
    {returnrv}
}}
""")

main()
