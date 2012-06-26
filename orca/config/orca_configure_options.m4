dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2012      Oak Ridge National Labs.  All rights reserved.
dnl
dnl $COPYRIGHT$
dnl 
dnl Additional copyrights may follow
dnl 
dnl $HEADER$
dnl


AC_DEFUN([ORCA_CONFIGURE_OPTIONS],[
ompi_show_subtitle "ORCA Configuration options"

#
# ORTE Check
#
# Please do 'try' not to use the orca_direct_orte_* Flags as they bypass
# much of the orca layer. These flags are only to be used outside of the
# ORCA layer by the various tools that are defined out of ORTE, but
# need direct access to ORTE.
# Note: A better way to handle the tools is to allow them to specify their
#       own configure.m4 similar to the MCA components.
#       This is a bit easier, but less elegant.
#
ORCA_CHECK_ORTE([orca_direct_orte],
    [orca_orte_happy="yes"],
    [orca_orte_happy="no"])

AC_MSG_CHECKING([if want ORTE supported ORCA])
AS_IF([test "$orca_orte_happy" = "yes"],
    [AC_SUBST([orca_direct_orte_CFLAGS])
     AC_SUBST([orca_direct_orte_CPPFLAGS])
     AC_SUBST([orca_direct_orte_LDFLAGS])
     AC_SUBST([orca_direct_orte_LIBS])
     ORCA_WITH_ORTE_SUPPORT=1
     AC_MSG_RESULT([yes])],
    [ORCA_WITH_ORTE_SUPPORT=0
     AC_MSG_RESULT([no])])

AC_DEFINE_UNQUOTED([ORCA_WITH_ORTE_SUPPORT],
                   [$ORCA_WITH_ORTE_SUPPORT],
                   [ORCA built for ORTE])
AM_CONDITIONAL(ORCA_WITH_ORTE_SUPPORT, test "$ORCA_WITH_ORTE_SUPPORT" = "1")

#
# Check if we have -full- ORTE support
# see orte_configure_options.m4 for --without-rte-support
# Note: With ORCA, this option may not be needed in the future.
#
ORCA_WITH_FULL_ORTE_SUPPORT=0
if test "$ORCA_WITH_ORTE_SUPPORT" = "1"; then
    AC_MSG_CHECKING([if have full ORTE support])
    if test -z "$with_rte_support" -a ! "$with_rte_support" = "no"; then
        ORCA_WITH_FULL_ORTE_SUPPORT=1
        AC_MSG_RESULT([yes])
    else
        AC_MSG_RESULT([no])
    fi
fi

AC_DEFINE_UNQUOTED([ORCA_WITH_FULL_ORTE_SUPPORT],
    [$ORCA_WITH_FULL_ORTE_SUPPORT],
    [ORCA built with full ORTE or not])
AM_CONDITIONAL(ORCA_WITH_FULL_ORTE_SUPPORT, test "$ORCA_WITH_FULL_ORTE_SUPPORT" = "1")

#AC_MSG_ERROR([Stopping. Happy "$orca_check_orte_happy"])

])dnl
