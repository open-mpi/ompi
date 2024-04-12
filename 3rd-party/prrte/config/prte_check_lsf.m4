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
dnl Copyright (c) 2007-2020 Cisco Systems, Inc.  All rights reserved
dnl Copyright (c) 2015      Research Organization for Information Science
dnl                         and Technology (RIST). All rights reserved.
dnl Copyright (c) 2016      Los Alamos National Security, LLC. All rights
dnl                         reserved.
dnl Copyright (c) 2017-2021 IBM Corporation.  All rights reserved.
dnl Copyright (c) 2017-2020 Intel, Inc.  All rights reserved.
dnl Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl

# check for lsf
# PRTE_CHECK_LSF(prefix, [action-if-found], [action-if-not-found])
# --------------------------------------------------------
AC_DEFUN([PRTE_CHECK_LSF],[
       AC_ARG_WITH([lsf],
               [AS_HELP_STRING([--with-lsf(=DIR)],
                       [Build LSF support])])
       AC_ARG_WITH([lsf-libdir],
               [AS_HELP_STRING([--with-lsf-libdir=DIR],
                       [Search for LSF libraries in DIR])])

       AS_IF([test "$with_lsf" != "no"],[
          # If no directories were specified, look for LSF_LIBDIR,
          # LSF_INCLUDEDIR, and/or LSF_ENVDIR.
          AS_IF([test "${with_lsf}" = "yes" -o -z "${with_lsf}"],
                [AS_IF([test ! -z "$LSF_ENVDIR" && test -z "$LSF_INCLUDEDIR" && test -f "$LSF_ENVDIR/lsf.conf"],
                       [LSF_INCLUDEDIR=`egrep ^LSF_INCLUDEDIR= $LSF_ENVDIR/lsf.conf | cut -d= -f2-`])
                 AS_IF([test ! -z "$LSF_INCLUDEDIR"],
                       [with_lsf_incdir=`dirname $LSF_INCLUDEDIR`
                        AC_MSG_NOTICE([Setting LSF includedir to ${with_lsf_incdir}])])

                 AS_IF([test -z "${with_lsf_libdir}"],
                       [AS_IF([test ! -z "$LSF_ENVDIR" && test -z "$LSF_LIBDIR" && test -f "$LSF_ENVDIR/lsf.conf"],
                              [LSF_LIBDIR=`egrep ^LSF_LIBDIR= $LSF_ENVDIR/lsf.conf | cut -d= -f2-`])
                        AS_IF([test ! -z "$LSF_LIBDIR"],
                              [with_lsf_libdir=$LSF_LIBDIR
                               AC_MSG_NOTICE([Setting LSF libdir to ${with_lsf_libdir}])])])])

          prte_check_lsf_$1_save_CPPFLAGS="$CPPFLAGS"
          prte_check_lsf_$1_save_LDFLAGS="$LDFLAGS"
          prte_check_lsf_$1_save_LIBS="$LIBS"

          prte_check_lsf_happy="yes"

          # liblsf requires yp_all, yp_get_default_domain, and ypprot_err
          # on Linux, Solaris, NEC, and Sony NEWSs these are found in libnsl
          # on AIX it should be in libbsd
          # on HP-UX it should be in libBSD
          # on IRIX < 6 it should be in libsun (IRIX 6 and later it is in libc)
          # on RHEL: libnsl, libnsl2 AND libnsl2-devel are required to link libnsl to get yp_all.
          AS_IF([test "$prte_check_lsf_happy" = "yes"],
                [PRTE_SEARCH_LIBS_COMPONENT([yp_all_nsl], [yp_all], [nsl bsd BSD sun],
                              [prte_check_lsf_happy="yes"],
                              [AC_MSG_WARN([[Could not find yp_all. Please see https://github.com/openpmix/prrte/wiki/Building-LSF-support for more details.]])
                              prte_check_lsf_happy="no"])])

          # liblsf requires shm_open, shm_unlink, which are in librt
          AS_IF([test "$prte_check_lsf_happy" = "yes"],
                [PRTE_SEARCH_LIBS_COMPONENT([shm_open_rt], [shm_open], [rt],
                              [prte_check_lsf_happy="yes"],
                              [prte_check_lsf_happy="no"])])

          AS_IF([test "$prte_check_lsf_happy" = "yes"],
                [OAC_CHECK_PACKAGE([lsf],
                                   [ls_info_lsf],
                                   [lsf/lsf.h],
                                   [lsf $yp_all_nsl_LIBS $shm_open_rt_LIBS],
                                   [ls_info],
                                   [prte_check_lsf_happy="yes"],
                                   [prte_check_lsf_happy="no"])])

          # test function of liblsb LSF package
          AS_IF([test "$prte_check_lsf_happy" = "yes"],
                [with_lsfbatch=${with_lsf}
                 with_lsfbatch_incdir=${with_lsf_incdir}
                 with_lsfbatch_libdir=${with_lsf_libdir}
                 OAC_CHECK_PACKAGE([lsfbatch],
                                   [prte_check_lsf],
                                   [lsf/lsbatch.h],
                                   [bat $ls_info_lsf_LIBS],
                                   [lsb_launch],
                                   [prte_check_lsf_happy="yes"],
                                   [prte_check_lsf_happy="no"])])

          # Some versions of LSF ship with a libevent.so in their library path.
          # This is _not_ a copy of Libevent, but something specific to their project.
          # The Open MPI components should not need to link against LSF's libevent.so
          # However, the presence of it in the linker search path can cause a problem
          # if there is a system installed Libevent and Open MPI chooses the 'external'
          # event component prior to this stage.
          #
          # Add a check here to see if we are in a scenario where the two are conflicting.
          # In which case the earlier checks for successful compile of an LSF program will
          # have failed with messages like:
          #   lib64/libevent_pthreads.so: undefined reference to `evthread_set_condition_callbacks'
          #   lib64/libevent_pthreads.so: undefined reference to `event_mm_malloc_'
          #   lib64/libevent_pthreads.so: undefined reference to `event_mm_free_'
          #   lib64/libevent_pthreads.so: undefined reference to `evthread_set_id_callback'
          #   lib64/libevent_pthreads.so: undefined reference to `evthread_set_lock_callbacks'
          # Because it picked up -levent from LSF, but -levent_pthreads from Libevent.
          #
          # So look for a function that libevent_pthreads is looking for from libevent.so.
          # If it does appears then we have the correct libevent.so, otherwise then we picked
          # up the LSF version and a conflict has been detected.
          # If the external libevent component used 'event_core' instead of 'event'
          prte_check_lsf_event_conflict=na
          # Split libs into an array, see if -levent is in that list
          prte_check_lsf_libevent_present=`echo "$LIBS" | awk '{split([$]0, a, " "); {for (k in a) {if (a[[k]] == "-levent") {print a[[k]]}}}}' | wc -l | tr -d '[[:space:]]'`
          # (1) LSF check must have failed above. We need to know why...
          AS_IF([test "$prte_check_lsf_happy" = "no"],
                [# (2) If there is a -levent in the $LIBS then that might be the problem
                 AS_IF([test "$prte_check_lsf_libevent_present" != "0"],
                       [AS_IF([test "$prte_check_lsf_libdir" = "" ],
                              [],
                              [LDFLAGS="$LDFLAGS -L$prte_check_lsf_libdir"])
                        # Note that we do not want to set LIBS here to include -llsf since
                        # the check is not for an LSF library, but for the conflict with
                        # LDFLAGS.
                        # (3) Check to see if the -levent is from Libevent (check for a symbol it has)
                        AC_CHECK_LIB([event], [evthread_set_condition_callbacks],
                                     [AC_MSG_CHECKING([for libevent conflict])
                                      AC_MSG_RESULT([No conflict found. The correct libevent.so was linked.])
                                      prte_check_lsf_event_conflict=no],
                                     [# (4) The libevent.so is not from Libevent. Warn the user.
                                      AC_MSG_CHECKING([for libevent conflict])
                                      AC_MSG_RESULT([Conflict found. Detected a libevent.so that is not from Libevent.])
                                      prte_check_lsf_event_conflict=yes])
                       ],
                       [AC_MSG_CHECKING([for libevent conflict])
                        AC_MSG_RESULT([No conflict found. -levent is not being explicitly used.])
                        prte_check_lsf_event_conflict=na])],
                [AC_MSG_CHECKING([for libevent conflict])
                 AC_MSG_RESULT([No conflict found. LSF checks passed.])
                 prte_check_lsf_event_conflict=na])

          AS_IF([test "$prte_check_lsf_event_conflict" = "yes"],
                [AC_MSG_WARN([===================================================================])
                 AC_MSG_WARN([===================================================================])
                ])

          CPPFLAGS="$prte_check_lsf_$1_save_CPPFLAGS"
          LDFLAGS="$prte_check_lsf_$1_save_LDFLAGS"
          LIBS="$prte_check_lsf_$1_save_LIBS"

       ], [prte_check_lsf_happy="no (disabled by user)"])

       PRTE_SUMMARY_ADD([Resource Managers], [LSF], [], [$prte_check_lsf_happy])

    AS_IF([test "$prte_check_lsf_happy" = "yes"],
          [$1_LIBS="[$]$1_LIBS $prte_check_lsf_LIBS"
           $1_LDFLAGS="[$]$1_LDFLAGS $prte_check_lsf_LDFLAGS"
           $1_CPPFLAGS="[$]$1_CPPFLAGS $prte_check_lsf_CPPFLAGS"
           # add the LSF libraries to static builds as they are required
           $2],
          [AS_IF([test ! -z "$with_lsf" && test "$with_lsf" != "no"],
                 [AC_MSG_WARN([LSF support requested (via --with-lsf) but not found.])
                  AC_MSG_ERROR([Aborting.])])
           $3])
])
