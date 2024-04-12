dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2020      Intel, Inc.  All rights reserved.
dnl Copyright (c) 2020      Cisco Systems, Inc.  All rights reserved
dnl Copyright (c) 2022-2024 Nanook Consulting  All rights reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl

# See if there is support for ptrace options required for
# "stop-on-exec" behavior.

AC_DEFUN([PRTE_CHECK_PTRACE],[

    PRTE_VAR_SCOPE_PUSH(prte_have_ptrace_traceme prte_have_ptrace_detach prte_have_ptrace_header prte_have_ptrace prte_want_stop_on_exec prte_traceme_cmd prte_detach_cmd prte_ptrace_linux_sig prte_ptrace_CFLAGS_save)

    prte_have_ptrace_traceme=no
    prte_have_ptrace_detach=no
    prte_traceme_cmd=
    prte_detach_cmd=

    AC_CHECK_HEADER([sys/ptrace.h],
                    [prte_have_ptrace_header=1
                     # must manually define the header protection since check_header doesn't do it
                     AC_DEFINE_UNQUOTED([HAVE_SYS_PTRACE_H], [1], [Whether or not we have the ptrace header])],
                    [prte_have_ptrace_header=0])

    AC_CHECK_FUNC([ptrace],
                  [prte_have_ptrace=yes],
                  [prte_have_ptrace=no])

    if test "$prte_have_ptrace_header" = "1" && test "$prte_have_ptrace" = "yes"; then
        AC_MSG_CHECKING([PTRACE_TRACEME])
        AC_EGREP_CPP([yes],
                     [#include <sys/ptrace.h>
                      #ifdef PTRACE_TRACEME
                        yes
                      #endif
                     ],
                     [AC_MSG_RESULT(yes)
                      prte_have_ptrace_traceme=yes
                      prte_traceme_cmd=PTRACE_TRACEME],
                     [AC_MSG_RESULT(no)
                      AC_MSG_CHECKING([PT_TRACE_ME])
                      AC_EGREP_CPP([yes],
                                   [#include <sys/ptrace.h>
                                    #ifdef PT_TRACE_ME
                                      yes
                                    #endif
                                   ],
                                   [AC_MSG_RESULT(yes)
                                    prte_have_ptrace_traceme=yes
                                    prte_traceme_cmd=PT_TRACE_ME],
                                   [AC_MSG_RESULT(no)
                                    prte_have_ptrace_traceme=no])
                     ])

        AC_MSG_CHECKING([PTRACE_DETACH])
        AC_EGREP_CPP([yes],
                     [#include <sys/ptrace.h>
                      #ifdef PTRACE_DETACH
                        yes
                      #endif
                     ],
                     [AC_MSG_RESULT(yes)
                      prte_have_ptrace_detach=yes
                      prte_detach_cmd=PTRACE_DETACH],
                     [AC_MSG_RESULT(no)
                      AC_MSG_CHECKING(PT_DETACH)
                      AC_EGREP_CPP([yes],
                                   [#include <sys/ptrace.h>
                                    #ifdef PT_DETACH
                                      yes
                                    #endif
                                   ],
                                   [AC_MSG_RESULT(yes)
                                    prte_have_ptrace_detach=yes
                                    prte_detach_cmd=PT_DETACH],
                                   [AC_MSG_RESULT(no)
                                    prte_have_ptrace_detach=no])
                     ])

        AC_MSG_CHECKING([Linux ptrace function signature])
        AC_LANG_PUSH(C)
        # must have -Werror set here
        prte_ptrace_CFLAGS_save=$CFLAGS
        CFLAGS="$CFLAGS -Werror"
        AC_COMPILE_IFELSE(
            [AC_LANG_PROGRAM(
                [[#include "sys/ptrace.h"]],
                [[long (*ptr)(enum __ptrace_request request, pid_t pid, void *addr, void *data);]
                 [ptr = ptrace;]])
            ],[
                AC_MSG_RESULT([yes])
                prte_ptrace_linux_sig=1
            ],[
                AC_MSG_RESULT([no])
                prte_ptrace_linux_sig=0
            ])
        AC_LANG_POP(C)
        CFLAGS=$prte_ptrace_CFLAGS_save

    fi

    AC_MSG_CHECKING(ptrace stop-on-exec will be supported)
    AS_IF([test "$prte_have_ptrace_traceme" = "yes" && test "$prte_have_ptrace_detach" = "yes"],
          [AC_MSG_RESULT(yes)
           prte_want_stop_on_exec=1],
          [AC_MSG_RESULT(no)
           prte_want_stop_on_exec=0])

    AC_DEFINE_UNQUOTED([PRTE_HAVE_LINUX_PTRACE], [$prte_ptrace_linux_sig], [Does ptrace have the Linux signature])
    AC_DEFINE_UNQUOTED([PRTE_HAVE_STOP_ON_EXEC], [$prte_want_stop_on_exec], [Whether or not we have stop-on-exec support])
    AC_DEFINE_UNQUOTED([PRTE_TRACEME], [$prte_traceme_cmd], [Command for declaring that process expects to be traced by parent])
    AC_DEFINE_UNQUOTED([PRTE_DETACH], [$prte_detach_cmd], [Command to detach from process being traced])

    PRTE_VAR_SCOPE_POP
])
