# -*- shell-script -*-
#
# Copyright (c) 2004-2007 The Trustees of the University of Tennessee.
#                         All rights reserved.
# Copyright (c) 2010      Cisco Systems, Inc. All rights reserved
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

AC_DEFUN([MCA_opal_if_windows_COMPILE_MODE], [
    AC_MSG_CHECKING([for MCA component $2:$3 compile mode])
    $4="static"
    AC_MSG_RESULT([$$4])
])


# MCA_if_windows_CONFIG(action-if-can-compile, 
#                        [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_opal_if_windows_CONFIG],[
    AC_CONFIG_FILES([opal/mca/if/windows/Makefile])

    # check for RegOpenKeyEx allowing access to the Windows
    # registry. We should first check that the function is defined,
    # and then check for it's presence in the kernel32 library.
    AC_MSG_CHECKING([for working RegOpenKeyEx])
    AC_TRY_RUN( [#include <windows.h>
int main( int argc, char** argv ) {
    RegOpenKeyEx( HKEY_CURRENT_USER, "Software\\Open MPI", 0, KEY_READ, NULL);
    return 0; }],
        [AC_MSG_RESULT([yes])
         $1],
        [AC_MSG_RESULT([no])
         $2],
        [AC_COMPILE_IFELSE([#include <windows.h>
int main( int argc, char** argv ) {
    RegOpenKeyEx( HKEY_CURRENT_USER, "Software\\Open MPI", 0, KEY_READ, NULL);
    return 0; }],
        [AC_MSG_RESULT([yes])
         $1],
        [AC_MSG_RESULT([no])
         $2])])
])dnl

