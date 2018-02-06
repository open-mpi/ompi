# Copyright © 2009-2011 Cisco Systems, Inc.  All rights reserved.
# Copyright © 2013 Inria.  All rights reserved.
# See COPYING in top-level directory.

AC_INIT([hwloc-embedded-test], [1.0], [http://www.open-mpi.org/projects/hwloc/], [hwloc-embedded-test])
AC_PREREQ([2.63])
AC_CONFIG_AUX_DIR([./config])
AC_CONFIG_MACRO_DIR([./config])
AC_CANONICAL_TARGET
AM_INIT_AUTOMAKE([1.10 dist-bzip2 foreign subdir-objects tar-ustar -Wall -Werror])
m4_ifdef([AM_SILENT_RULES], [AM_SILENT_RULES([yes])])

AC_LANG([C])

AC_PROG_CC
AM_PROG_CC_C_O

cat <<EOF

=== Configuring hwloc (embedded mode)
EOF

HWLOC_SET_SYMBOL_PREFIX([mytest_])
HWLOC_SETUP_CORE([hwloc-tree], [hwloc_setup=happy], [hwloc_setup=unhappy])
AS_IF([test "$hwloc_setup" = "unhappy"],
      [AC_MSG_ERROR([Cannot build hwloc core])])
HWLOC_DO_AM_CONDITIONALS

cat <<EOF
=== Done configuring hwloc (embedded mode)

EOF

m4_ifdef([AM_PROG_AR], [AM_PROG_AR])

AM_ENABLE_SHARED
AM_DISABLE_STATIC
AM_PROG_LIBTOOL

AC_CONFIG_FILES([Makefile])
AC_OUTPUT
