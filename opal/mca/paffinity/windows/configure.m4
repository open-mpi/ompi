# -*- shell-script -*-
#
# Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
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

# MCA_paffinity_windows_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_paffinity_windows_CONFIG],[
    # check for GetProcessAffinityMask, which is defined only for some
    # flavors of Windows. We should first check that the function is defined,
    # and then check for it's presence in the kernel32 library.
    AC_MSG_CHECKING(for working GetProcessAffinityMask)
    AC_TRY_RUN( [#include <windows.h>
int main( int argc, char** argv ) {
    DWORD aff, mask;
    GetProcessAffinityMask( NULL, &aff, &mask );
    return 0; }],
        [AC_MSG_RESULT([yes])
         $1],
        [AC_MSG_RESULT([no])
         $2],
        [AC_COMPILE_IFELSE([#include <windows.h>
int main( int argc, char** argv ) {
    DWORD aff, mask;
    GetProcessAffinityMask( NULL, &aff, &mask );
    return 0; }],
        [AC_MSG_RESULT([yes])
         $1],
        [AC_MSG_RESULT([no])
         $2])])
])dnl
