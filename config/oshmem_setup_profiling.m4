# -*- shell-script -*-
#
# Copyright (c) 2014 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2014      Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

AC_DEFUN([OSHMEM_SETUP_PROFILING],[
    ompi_show_subtitle "OpenSHMEM profiling"

    AC_MSG_CHECKING([if pshmem will be enabled])
    AS_IF([test "$enable_oshmem" != "no" && \
           test "$enable_oshmem_profile" != "no"],
          [AS_IF([test "$OPAL_C_HAVE_WEAK_SYMBOLS" -eq 1 && \
                  test "$OPAL_C_HAVE_MACRO_WEAK_SYMBOLS" -eq 1],
                 [ # We want OSHMEM and we have all the required weak
                   # symbol support
                  oshmem_profiling_support=1
                  AC_MSG_RESULT([yes (weak symbols supported)])],
                 [ # We want OSHMEM, but we do NOT have all the
                   # required weak symbol support
                  oshmem_profiling_support=0
                  AC_MSG_RESULT([no (weak symbols not supported)])
                  AS_IF([test "$OPAL_C_HAVE_WEAK_SYMBOLS" -eq 0],
                        [AC_MSG_WARN([Weak symbols not supported by compiler])])
                  AS_IF([test "$OPAL_C_HAVE_MACRO_WEAK_SYMBOLS" -eq 0],
                       [AC_MSG_WARN([Macro weak symbols not supported by compiler])])
                  AC_MSG_WARN([OpenSHMEM profiling is disabled.])

                  AS_IF([test "$enable_oshmem_profile" = "yes" && \
                         test "$oshmem_profiling_support" -eq 0],
                        [AC_MSG_WARN([OpenSHMEM profiling requested but cannot be enabled])
                         AC_MSG_ERROR([Cannot continue])])
                 ])
          ],[
           # We do not want OSHMEM, so disable profiling
           oshmem_profiling_support=0
           AC_MSG_RESULT([no (OpenSHMEM disabled)])
          ])

    AM_CONDITIONAL([OSHMEM_PROFILING], [test $oshmem_profiling_support -eq 1])
]) dnl
