# -*- shell-script -*-
#
# Copyright (c) 2026      Amazon.com, Inc. or its affiliates.  All Rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
# SPDX-License-Identifier: BSD-3-Clause-Open-MPI
#

# MCA_opal_memory_kdreg2_CONFIG(action-if-can-compile,
#                               [action-if-cant-compile])
# ------------------------------------------------
# --with-kdreg2=DIR names the directory that holds kdreg2.h, as with
# Libfabric's option of the same name.  Without DIR, look for
# kdreg2.h and then linux/kdreg2.h in the default include path.
AC_DEFUN([MCA_opal_memory_kdreg2_CONFIG],[
    AC_ARG_WITH([kdreg2],
        [AS_HELP_STRING([--with-kdreg2(=DIR)],
                        [Build the kdreg2 memory hooks component.  DIR is the directory that contains kdreg2.h.  Error if kdreg2 support was requested and kdreg2.h cannot be found.])])

    opal_memory_kdreg2_happy=no
    memory_kdreg2_CPPFLAGS=

    AS_IF([test "$with_kdreg2" != "no"],
          [opal_memory_kdreg2_save_CPPFLAGS=$CPPFLAGS
           AS_IF([test -n "$with_kdreg2" && test "$with_kdreg2" != "yes"],
                 [memory_kdreg2_CPPFLAGS="-I$with_kdreg2"
                  CPPFLAGS="$CPPFLAGS $memory_kdreg2_CPPFLAGS"])
           AC_CHECK_HEADERS([kdreg2.h linux/kdreg2.h], [opal_memory_kdreg2_happy=yes; break])
           CPPFLAGS=$opal_memory_kdreg2_save_CPPFLAGS])

    AS_IF([test "$opal_memory_kdreg2_happy" = "no" && test -n "$with_kdreg2" && test "$with_kdreg2" != "no"],
          [AC_MSG_ERROR([kdreg2 support requested but kdreg2.h was not found])])

    AC_SUBST([memory_kdreg2_CPPFLAGS])

    AS_IF([test "$opal_memory_kdreg2_happy" = "yes"], [$1], [$2])

    AC_CONFIG_FILES([opal/mca/memory/kdreg2/Makefile])
])
