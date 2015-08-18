dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
dnl                         University Research and Technology
dnl                         Corporation.  All rights reserved.
dnl Copyright (c) 2004-2005 The University of Tennessee and The University
dnl                         of Tennessee Research Foundation.  All rights
dnl                         reserved.
dnl Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
dnl                         University of Stuttgart.  All rights reserved.
dnl Copyright (c) 2004-2005 The Regents of the University of California.
dnl                         All rights reserved.
dnl Copyright (c) 2006-2010 Cisco Systems, Inc.  All rights reserved.
dnl Copyright (c) 2007      Sun Microsystems, Inc.  All rights reserved.
dnl Copyright (c) 2009      IBM Corporation.  All rights reserved.
dnl Copyright (c) 2009-2013 Los Alamos National Security, LLC.  All rights
dnl                         reserved.
dnl Copyright (c) 2009      Oak Ridge National Labs.  All rights reserved.
dnl
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl


AC_DEFUN([ORTE_CONFIGURE_OPTIONS],[
opal_show_subtitle "ORTE Configuration options"

#
# Do we want orterun's --prefix behavior to be enabled by default?
#
AC_MSG_CHECKING([if want orterun "--prefix" behavior to be enabled by default])
AC_ARG_ENABLE([orterun-prefix-by-default],
    [AC_HELP_STRING([--enable-orterun-prefix-by-default],
        [Make "orterun ..." behave exactly the same as "orterun --prefix \$prefix" (where \$prefix is the value given to --prefix in configure)])])
AC_ARG_ENABLE([mpirun-prefix-by-default],
    [AC_HELP_STRING([--enable-mpirun-prefix-by-default],
        [Synonym for --enable-orterun-prefix-by-default])])
if test "$enable_orterun_prefix_by_default" = ""; then
    enable_orterun_prefix_by_default=$enable_mpirun_prefix_by_default
fi
if test "$enable_orterun_prefix_by_default" = "yes"; then
    AC_MSG_RESULT([yes])
    orte_want_orterun_prefix_by_default=1
else
    AC_MSG_RESULT([no])
    orte_want_orterun_prefix_by_default=0
fi
AC_DEFINE_UNQUOTED([ORTE_WANT_ORTERUN_PREFIX_BY_DEFAULT],
                   [$orte_want_orterun_prefix_by_default],
                   [Whether we want orterun to effect "--prefix $prefix" by default])


AC_MSG_CHECKING([if want orte static ports])
AC_ARG_ENABLE([orte-static-ports],
              [AC_HELP_STRING([--enable-orte-static-ports],
	        [Enable orte static ports for tcp oob. (default: enabled)])])
if test "$enable_orte_static_ports" = "no"; then
    AC_MSG_RESULT([no])
    orte_enable_static_ports=0
else
    AC_MSG_RESULT([yes])
    orte_enable_static_ports=1
fi
AC_DEFINE_UNQUOTED([ORTE_ENABLE_STATIC_PORTS],
                   [$orte_enable_static_ports],
		   [Whether we want static ports enabled])

])dnl
