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

AC_DEFUN([OMPI_CHECK_OPTFLAGS],[

# Modularize this setup so that sub-configure.in scripts can use this
# same setup code.

##################################
# Optimization flags
##################################

# If the user did not specify optimization flags, add some (the value
# from $OPTFLAGS)

co_arg="$1"
co_found=0
for co_word in $co_arg; do
    case $co_word in
    -g)    co_found=1 ;;
    -g1)   co_found=1 ;;
    -g2)   co_found=1 ;;
    -g3)   co_found=1 ;;
    +K0)   co_found=1 ;;
    +K1)   co_found=1 ;;
    +K2)   co_found=1 ;;
    +K3)   co_found=1 ;;
    +K4)   co_found=1 ;;
    +K5)   co_found=1 ;;
    -O)    co_found=1 ;;
    -O0)   co_found=1 ;;
    -O1)   co_found=1 ;;
    -O2)   co_found=1 ;;
    -O3)   co_found=1 ;;
    -O4)   co_found=1 ;;
    -O5)   co_found=1 ;; 
    -O6)   co_found=1 ;;
    -O7)   co_found=1 ;;
    -O8)   co_found=1 ;;
    -O9)   co_found=1 ;;
    -xO)   co_found=1 ;;
    -xO0)  co_found=1 ;;
    -xO1)  co_found=1 ;;
    -xO2)  co_found=1 ;;
    -xO3)  co_found=1 ;;
    -xO4)  co_found=1 ;;
    -xO5)  co_found=1 ;; 
    -xO6)  co_found=1 ;;
    -xO7)  co_found=1 ;;
    -xO8)  co_found=1 ;;
    -xO9)  co_found=1 ;;
    -fast) co_found=1 ;;
    esac
done

if test "$co_found" = "0"; then
    co_result="$OPTFLAGS $co_arg"
else
    co_result="$co_arg"
fi

# Clean up

unset co_found co_word co_arg
])
