dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
dnl                         University Research and Technology
dnl                         Corporation.  All rights reserved.
dnl Copyright (c) 2004-2005 The University of Tennessee and The University
dnl                         of Tennessee Research Foundation.  All rights
dnl                         reserved.
dnl Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
dnl                         University of Stuttgart.  All rights reserved.
dnl Copyright (c) 2004-2005 The Regents of the University of California.
dnl                         All rights reserved.
dnl $COPYRIGHT$
dnl 
dnl Additional copyrights may follow
dnl 
dnl $HEADER$
dnl

define(OMPI_MAKE_STRIPPED_FLAGS,[

# Process a set of flags and remove all debugging and optimization
# flags

s_arg="$1"
s_result=
for s_word in $s_arg; do
    case $s_word in
    -g)    ;;
    +K0)   ;;
    +K1)   ;;
    +K2)   ;;
    +K3)   ;;
    +K4)   ;;
    +K5)   ;;
    -O)    ;;
    -O0)   ;;
    -O1)   ;;
    -O2)   ;;
    -O3)   ;;
    -O4)   ;;
    -O5)   ;;
    -O6)   ;;
    -O7)   ;;
    -O8)   ;;
    -O9)   ;;
    -xO)   ;;
    -xO0)  ;;
    -xO1)  ;;
    -xO2)  ;;
    -xO3)  ;;
    -xO4)  ;;
    -xO5)  ;;
    -xO6)  ;;
    -xO7)  ;;
    -xO8)  ;;
    -xO9)  ;;
    -fast) ;;
    *)     s_result="$s_result $s_word"
    esac
done

# Clean up

unset s_word s_arg])
