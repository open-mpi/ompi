dnl -*- autoconf -*-
dnl
dnl Copyright (c) 2022      Amazon.com, Inc. or its affiliates.  All Rights reserved.
dnl Copyright (c) 2024      Nanook Consulting  All rights reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$


dnl OAC_LINKER_STATIC_CHECK: Check if a linker or compiler flag will force
dnl                          static linking
dnl
dnl 1 -> action if static linking
dnl 2 -> action if not static linking
AC_DEFUN([OAC_LINKER_STATIC_CHECK], [
OAC_VAR_SCOPE_PUSH([oac_linker_arg])
AC_CACHE_CHECK([if static link flag supplied],
    [oac_cv_linker_found_static_linker_flag],
    [oac_cv_linker_found_static_linker_flag="no"
     for oac_linker_arg in ${CFLAGS} ${LDFLAGS} ; do
         AS_IF([test "${oac_linker_arg}" = "-static" -o \
                 "${oac_linker_arg}" = "--static" -o \
                 "${oac_linker_arg}" = "-Bstatic" -o \
                 "${oac_linker_arg}" = "-Wl,-static" -o \
                 "${oac_linker_arg}" = "-Wl,--static" -o \
                 "${oac_linker_arg}" = "-Wl,-Bstatic"],
               [oac_cv_linker_found_static_linker_flag="yes"])
     done])
AS_IF([test "${oac_cv_linker_found_static_linker_flag}" = "yes"], [$1], [$2])
OAC_VAR_SCOPE_POP
])
