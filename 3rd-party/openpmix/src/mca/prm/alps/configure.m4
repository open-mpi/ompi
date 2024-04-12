# -*- shell-script -*-
#
# Copyright (c) 2020      Intel, Inc.  All rights reserved.
# Copyright (c) 2023-2024 Nanook Consulting  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# MCA_pmix_prm_alps_CONFIG([action-if-can-compile],
#                          [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_pmix_prm_alps_CONFIG], [
    AC_CONFIG_FILES([src/mca/prm/alps/Makefile])

    PMIX_CHECK_ALPS([prm_alps], [prm_alps_good=yes], [prm_alps_good=no])

    PMIX_SUMMARY_ADD([Resource Managers], [Cray Alps], [], [$prm_alps_good])

    AS_IF([test "$prm_alps_good" = "yes"],
          [$1],
          [$2])

    AC_SUBST([prm_alps_CPPFLAGS])
    AC_SUBST([prm_alps_LDFLAGS])
    AC_SUBST([prm_alps_LIBS])
])dnl
