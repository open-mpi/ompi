dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2012      Los Alamos National Security, Inc.  All rights reserved.
dnl $COPYRIGHT$
dnl 
dnl Additional copyrights may follow
dnl 
dnl $HEADER$
dnl

# This macro is necessary to get the title to be displayed first.  :-)
AC_DEFUN([ORTE_SETUP_HADOOP_BANNER],[
    ompi_show_subtitle "HADOOP class libraries" 
])

# ORTE_SETUP_HADOOP()
# ----------------
# Do everything required to setup the HADOOP libraries.  Safe to AC_REQUIRE
# this macro.
AC_DEFUN([ORTE_SETUP_HADOOP],[
    AC_REQUIRE([ORTE_SETUP_HADOOP_BANNER])

    AC_MSG_CHECKING([if want Hadoop support])
    AC_ARG_ENABLE(hadoop,
        AC_HELP_STRING([--enable-hadoop],
                       [Enable Hadoop support - path to Hadoop taken from environment]))

    # Collect the jars
    ORTE_HAVE_HADOOP_SERIES_1=0
    ORTE_HAVE_HADOOP_SERIES_2=0

    # Only build the Hadoop support if requested
    if test "$enable_hadoop" = "yes"; then
        AC_MSG_RESULT([yes])
        orte_enable_hadoop_support=0
        # if this is Hadoop 2.x, we will find a share/hadoop/common
        # directory under the location given in the environ
        AC_MSG_CHECKING([for Hadoop 2.0 commons directory])
        AS_IF([test "x$HADOOP_COMMON_HOME" != "x" -a -d "$HADOOP_COMMON_HOME/share/hadoop/common"],
              [AC_MSG_RESULT([found])
               orte_enable_hadoop_support=1
               ORTE_HAVE_HADOOP_SERIES_2=1],
              [AC_MSG_RESULT([not found])
               # check instead for Hadoop 1.0.2
               AC_MSG_CHECKING([for Hadoop 1.0.2])
               AS_IF([test "x$HADOOP_HOME" != "x" -a -f "$HADOOP_HOME/hadoop-core-1.0.2.jar"],
                     [AC_MSG_RESULT([found])
                      orte_enable_hadoop_support=1
                      ORTE_HAVE_HADOOP_SERIES_1=1],
                     [AC_MSG_RESULT([not found])
                         AC_MSG_WARN([HADOOP support requested but supported version not found])
                         AC_MSG_ERROR([Cannot continue])])])
    else
        AC_MSG_RESULT([no])
        orte_enable_hadoop_support=0
    fi

    AC_SUBST([ORTE_HAVE_HADOOP_SERIES_1])

    AC_DEFINE_UNQUOTED([ORTE_WANT_HADOOP_SUPPORT], [$orte_enable_hadoop_support],
                       [do we want hadoop support])
    AM_CONDITIONAL(ORTE_WANT_HADOOP_SUPPORT, test "$orte_enable_hadoop_support" = "1")

    AM_CONDITIONAL(ORTE_HAVE_HADOOP_SERIES1, test "$ORTE_HAVE_HADOOP_SERIES_1" = "1")
    AC_DEFINE_UNQUOTED([ORTE_HAVE_HADOOP_SERIES1], [$ORTE_HAVE_HADOOP_SERIES_1],
                       [do we have MRV1])

    AM_CONDITIONAL(ORTE_HAVE_HADOOP_SERIES2, test "$ORTE_HAVE_HADOOP_SERIES_2" = "1")
    AC_DEFINE_UNQUOTED([ORTE_HAVE_HADOOP_SERIES2], [$ORTE_HAVE_HADOOP_SERIES_2],
                       [do we have MRV2])
])
