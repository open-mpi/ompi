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
dnl Copyright (c) 2007-2016 Cisco Systems, Inc.  All rights reserved.
dnl Copyright (c) 2015      Research Organization for Information Science
dnl                         and Technology (RIST). All rights reserved.
dnl Copyright (c) 2016      Los Alamos National Security, LLC. All rights
dnl                         reserved.
dnl Copyright (c) 2017-2021 IBM Corporation.  All rights reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl

# check for lsf
# ORTE_CHECK_LSF(prefix, [action-if-found], [action-if-not-found])
# --------------------------------------------------------
AC_DEFUN([ORTE_CHECK_LSF],[
    AS_IF([test -z "$orte_check_lsf_happy"],[
       AC_ARG_WITH([lsf],
               [AC_HELP_STRING([--with-lsf(=DIR)],
                       [Build LSF support])])
       OPAL_CHECK_WITHDIR([lsf], [$with_lsf], [include/lsf/lsbatch.h])
       AC_ARG_WITH([lsf-libdir],
               [AC_HELP_STRING([--with-lsf-libdir=DIR],
                       [Search for LSF libraries in DIR])])
       OPAL_CHECK_WITHDIR([lsf-libdir], [$with_lsf_libdir], [libbat.*])

       AS_IF([test "$with_lsf" != "no"],[
          # Defaults
          orte_check_lsf_dir_msg="compiler default"
          orte_check_lsf_libdir_msg="linker default"

          # Save directory names if supplied
          AS_IF([test ! -z "$with_lsf" && test "$with_lsf" != "yes"],
                    [orte_check_lsf_dir="$with_lsf"
                     orte_check_lsf_dir_msg="$orte_check_lsf_dir (from --with-lsf)"])
          AS_IF([test ! -z "$with_lsf_libdir" && test "$with_lsf_libdir" != "yes"],
                    [orte_check_lsf_libdir="$with_lsf_libdir"
                     orte_check_lsf_libdir_msg="$orte_check_lsf_libdir (from --with-lsf-libdir)"])

          # If no directories were specified, look for LSF_LIBDIR,
          # LSF_INCLUDEDIR, and/or LSF_ENVDIR.
          AS_IF([test -z "$orte_check_lsf_dir" && test -z "$orte_check_lsf_libdir"],
                    [AS_IF([test ! -z "$LSF_ENVDIR" && test -z "$LSF_LIBDIR" && test -f "$LSF_ENVDIR/lsf.conf"],
                           [LSF_LIBDIR=`egrep ^LSF_LIBDIR= $LSF_ENVDIR/lsf.conf | cut -d= -f2-`])
                     AS_IF([test ! -z "$LSF_ENVDIR" && test -z "$LSF_INCLUDEDIR" && test -f "$LSF_ENVDIR/lsf.conf"],
                           [LSF_INCLUDEDIR=`egrep ^LSF_INCLUDEDIR= $LSF_ENVDIR/lsf.conf | cut -d= -f2-`])
                     AS_IF([test ! -z "$LSF_LIBDIR"],
                           [orte_check_lsf_libdir=$LSF_LIBDIR
                            orte_check_lsf_libdir_msg="$LSF_LIBDIR (from \$LSF_LIBDIR)"])
                     AS_IF([test ! -z "$LSF_INCLUDEDIR"],
                           [orte_check_lsf_dir=`dirname $LSF_INCLUDEDIR`
                            orte_check_lsf_dir_msg="$orte_check_lsf_dir (from \$LSF_INCLUDEDIR)"])])

          AS_IF([test "$with_lsf" = "no"],
                    [orte_check_lsf_happy="no"],
                    [orte_check_lsf_happy="yes"])

          orte_check_lsf_$1_save_CPPFLAGS="$CPPFLAGS"
          orte_check_lsf_$1_save_LDFLAGS="$LDFLAGS"
          orte_check_lsf_$1_save_LIBS="$LIBS"

          # liblsf requires yp_all, yp_get_default_domain, and ypprot_err
          # on Linux, Solaris, NEC, and Sony NEWSs these are found in libnsl
          # on AIX it should be in libbsd
          # on HP-UX it should be in libBSD
          # on IRIX < 6 it should be in libsun (IRIX 6 and later it is in libc)
          AS_IF([test "$orte_check_lsf_happy" = "yes"],
                [OPAL_SEARCH_LIBS_COMPONENT([yp_all_nsl], [yp_all], [nsl bsd BSD sun],
                              [orte_check_lsf_happy="yes"],
                              [orte_check_lsf_happy="no"])])

          # liblsf requires shm_open, shm_unlink, which are in librt
          AS_IF([test "$orte_check_lsf_happy" = "yes"],
                [OPAL_SEARCH_LIBS_COMPONENT([shm_open_rt], [shm_open], [rt],
                              [orte_check_lsf_happy="yes"],
                              [orte_check_lsf_happy="no"])])

          # liblsb requires liblsf - using ls_info as a test for liblsf presence
          AS_IF([test "$orte_check_lsf_happy" = "yes"],
                [OPAL_CHECK_PACKAGE([ls_info_lsf],
                           [lsf/lsf.h],
                           [lsf],
                           [ls_info],
                           [$yp_all_nsl_LIBS $shm_open_rt_LIBS],
                           [$orte_check_lsf_dir],
                           [$orte_check_lsf_libdir],
                           [orte_check_lsf_happy="yes"],
                           [orte_check_lsf_happy="no"])])

          # test function of liblsb LSF package
          AS_IF([test "$orte_check_lsf_happy" = "yes"],
                    [AC_MSG_CHECKING([for LSF dir])
                     AC_MSG_RESULT([$orte_check_lsf_dir_msg])
                     AC_MSG_CHECKING([for LSF library dir])
                     AC_MSG_RESULT([$orte_check_lsf_libdir_msg])
                     OPAL_CHECK_PACKAGE([orte_check_lsf],
                        [lsf/lsbatch.h],
                        [bat],
                        [lsb_launch],
                        [$ls_info_lsf_LIBS $yp_all_nsl_LIBS $shm_open_rt_LIBS],
                        [$orte_check_lsf_dir],
                        [$orte_check_lsf_libdir],
                        [orte_check_lsf_happy="yes"],
                        [orte_check_lsf_happy="no"])])


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
          orte_check_lsf_event_conflict=na
          # Split libs into an array, see if -levent is in that list
          orte_check_lsf_libevent_present=`echo "$LIBS" | awk '{split([$]0, a, " "); {for (k in a) {if (a[[k]] == "-levent") {print a[[k]]}}}}' | wc -l | tr -d '[[:space:]]'`
          # (1) LSF check must have failed above. We need to know why...
          AS_IF([test "$orte_check_lsf_happy" = "no"],
                [# (2) If there is a -levent in the $LIBS then that might be the problem
                 AS_IF([test "$opal_event_external_support" = "yes" && test "$orte_check_lsf_libevent_present" != "0"],
                       [AS_IF([test "$orte_check_lsf_libdir" = "" ],
                              [],
                              [LDFLAGS="$LDFLAGS -L$orte_check_lsf_libdir"])
                        # Note that we do not want to set LIBS here to include -llsf since
                        # the check is not for an LSF library, but for the conflict with
                        # LDFLAGS.
                        # (3) Check to see if the -levent is from Libevent (check for a symbol it has)
                        AC_CHECK_LIB([event], [evthread_set_condition_callbacks],
                                     [AC_MSG_CHECKING([for libevent conflict])
                                      AC_MSG_RESULT([No. The correct libevent.so was linked.])
                                      orte_check_lsf_event_conflict=no],
                                     [# (4) The libevent.so is not from Libevent. Warn the user.
                                      AC_MSG_CHECKING([for libevent conflict])
                                      AC_MSG_RESULT([Yes. Detected a libevent.so that is not from Libevent.])
                                      orte_check_lsf_event_conflict=yes])
                       ],
                       [AC_MSG_CHECKING([for libevent conflict])
                        AC_MSG_RESULT([No. Internal Libevent or libevent_core is being used.])
                        orte_check_lsf_event_conflict=na])],
                [AC_MSG_CHECKING([for libevent conflict])
                 AC_MSG_RESULT([No. LSF checks passed.])
                 orte_check_lsf_event_conflict=na])

          AS_IF([test "$orte_check_lsf_event_conflict" = "yes"],
                [AC_MSG_WARN([===================================================================])
                 AC_MSG_WARN([Conflicting libevent.so libraries detected on the system.])
                 AC_MSG_WARN([])
                 AC_MSG_WARN([A system-installed Libevent library was detected and the Open MPI])
                 AC_MSG_WARN([build system chose to use the 'external' component expecting to])
                 AC_MSG_WARN([link against the Libevent in the linker search path.])
                 AC_MSG_WARN([If LSF is present on the system and in the default search path then])
                 AC_MSG_WARN([it _may be_ the source of the conflict.])
                 AC_MSG_WARN([LSF provides a libevent.so that is not from Libevent in its])
                 AC_MSG_WARN([library path. At this point the linker is attempting to resolve])
                 AC_MSG_WARN([Libevent symbols using the LSF library because of the lack of])
                 AC_MSG_WARN([an explicit linker path pointing to the system-installed Libevent.])
                 AC_MSG_WARN([])
                 AC_MSG_WARN([To resolve this issue either (A) explicitly pass the Libevent])
                 AC_MSG_WARN([library path on the configure line (--with-libevent-libdir), or])
                 AC_MSG_WARN([(B) use the internal libevent by requesting it from configure ])
                 AC_MSG_WARN([with the --with-libevent=internal option.])
                 AC_MSG_WARN([===================================================================])
                ])

          CPPFLAGS="$orte_check_lsf_$1_save_CPPFLAGS"
          LDFLAGS="$orte_check_lsf_$1_save_LDFLAGS"
          LIBS="$orte_check_lsf_$1_save_LIBS"

       ],[orte_check_lsf_happy=no])

       OPAL_SUMMARY_ADD([[Resource Managers]],[[LSF]],[$1],[$orte_check_lsf_happy])
    ])

    AS_IF([test "$orte_check_lsf_happy" = "yes"],
          [$1_LIBS="[$]$1_LIBS $orte_check_lsf_LIBS"
           $1_LDFLAGS="[$]$1_LDFLAGS $orte_check_lsf_LDFLAGS"
           $1_CPPFLAGS="[$]$1_CPPFLAGS $orte_check_lsf_CPPFLAGS"
           # add the LSF libraries to static builds as they are required
           $1_WRAPPER_EXTRA_LDFLAGS=[$]$1_LDFLAGS
           $1_WRAPPER_EXTRA_LIBS=[$]$1_LIBS
           $2],
          [AS_IF([test ! -z "$with_lsf" && test "$with_lsf" != "no"],
                 [AC_MSG_WARN([LSF support requested (via --with-lsf) but not found.])
                  AC_MSG_ERROR([Aborting.])])
           $3])
])
