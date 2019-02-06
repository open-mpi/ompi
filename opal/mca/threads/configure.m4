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
dnl Copyright (c) 2019      Sandia National Laboratories.  All rights reserved.
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
        # All components look at this value
        AC_ARG_WITH([threads],
            [AC_HELP_STRING([--with-threads=TYPE],
                        [Build high resolution threads component TYPE])],
                        [],
                        [with_threads=pthreads])

        thread_type=$with_threads

        # first, compile all the components
        MCA_CONFIGURE_FRAMEWORK($1, $2, 1)

        threads_base_include="${thread_type}/threads_${thread_type}_threads.h"
        mutex_base_include="${thread_type}/threads_${thread_type}_mutex.h"
        tsd_base_include="${thread_type}/threads_${thread_type}_tsd.h"
        wait_sync_base_include="${thread_type}/threads_${thread_type}_wait_sync.h"

        AC_DEFINE_UNQUOTED([MCA_threads_IMPLEMENTATION_HEADER],
                           ["opal/mca/threads/$threads_base_include"],
                           [Header to include for threads implementation])

        AC_DEFINE_UNQUOTED([MCA_mutex_IMPLEMENTATION_HEADER],
                           ["opal/mca/threads/$mutex_base_include"],
                           [Header to include for mutex implementation])

        AC_DEFINE_UNQUOTED([MCA_tsd_IMPLEMENTATION_HEADER],
                           ["opal/mca/threads/$tsd_base_include"],
                           [Header to include for tsd implementation])

        AC_DEFINE_UNQUOTED([MCA_wait_sync_IMPLEMENTATION_HEADER],
                           ["opal/mca/threads/$wait_sync_base_include"],
                           [Header to include for wait_sync implementation])
])
