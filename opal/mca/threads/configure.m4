dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
dnl                         University Research and Technology
dnl                         Corporation.  All rights reserved.
dnl Copyright (c) 2004-2005 The University of Tennessee and The University
dnl                         of Tennessee Research Foundation.  All rights
dnl                         reserved.
dnl Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
dnl                         University of Stuttgart.  All rights reserved.
dnl Copyright (c) 2004-2005 The Regents of the University of California.
dnl                         All rights reserved.
dnl Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl

dnl we only want one :)
m4_define(MCA_opal_threads_CONFIGURE_MODE, STOP_AT_FIRST)

AC_DEFINE_UNQUOTED([OPAL_ENABLE_MULTI_THREADS], [1],
	[Whether we should enable thread support within the OPAL code base])
AC_DEFUN([MCA_opal_threads_CONFIG],[
        threads_base_include=

        # All components look at this value
        AC_ARG_WITH([threads],
            [AC_HELP_STRING([--with-threads=TYPE],
                        [Build high resolution threads component TYPE])])

        # first, compile all the components
        MCA_CONFIGURE_FRAMEWORK($1, $2, 1)

        # someone should have set this...
        if test "$threads_base_include" = "" ; then
            threads_base_include="base/threads_base_null.h"
        fi

        if test "$mutex_base_include" = "" ; then
#            mutex_base_include="base/mutex_base_null.h"
            mutex_base_include="pthreads/mutex_unix.h"
        fi
        AC_DEFINE_UNQUOTED([MCA_threads_IMPLEMENTATION_HEADER],
                           ["opal/mca/threads/$threads_base_include"],
                           [Header to include for threads implementation])

        AC_DEFINE_UNQUOTED([MCA_mutex_IMPLEMENTATION_HEADER],
                           ["opal/mca/threads/$mutex_base_include"],
                           [Header to include for mutex implementation])
])


