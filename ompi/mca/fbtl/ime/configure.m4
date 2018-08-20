# -*- shell-script -*-
#
# Copyright (c) 2018      DataDirect Networks. All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# MCA_fbtl_ime_CONFIG(action-if-can-compile,
#                        [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_ompi_fbtl_ime_CONFIG],[
    AC_CONFIG_FILES([ompi/mca/fbtl/ime/Makefile])

    OMPI_CHECK_IME([fbtl_ime],
                    [fbtl_ime_happy="yes"],
                    [fbtl_ime_happy="no"])

    AS_IF([test "$fbtl_ime_happy" = "yes"],
          [$1],
          [$2])

    # substitute in the things needed to build ime
    AC_SUBST([fbtl_ime_CPPFLAGS])
    AC_SUBST([fbtl_ime_LDFLAGS])
    AC_SUBST([fbtl_ime_LIBS])
])dnl
