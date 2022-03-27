# -*- shell-script -*-
#
# Copyright (c) 2018      DataDirect Networks. All rights reserved.
# Copyright (c) 2022      Amazon.com, Inc. or its affiliates.  All Rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# MCA_fs_ime_CONFIG(action-if-can-compile,
#                        [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_ompi_fs_ime_CONFIG],[
    AC_CONFIG_FILES([ompi/mca/fs/ime/Makefile])

    OMPI_CHECK_IME([fs_ime],
                    [fs_ime_happy="yes"],
                    [fs_ime_happy="no"])

    OPAL_SUMMARY_ADD([OMPIO File Systems], [DDN Infinite Memory Engine], [], [$fs_ime_happy])
    AS_IF([test "$fs_ime_happy" = "yes"],
          [$1],
          [$2])

    # substitute in the things needed to build ime
    AC_SUBST([fs_ime_CPPFLAGS])
    AC_SUBST([fs_ime_LDFLAGS])
    AC_SUBST([fs_ime_LIBS])
])dnl
