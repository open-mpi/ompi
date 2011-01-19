dnl
dnl PAC_RUN_LOG mimics _AC_RUN_LOG which is autoconf internal routine.
dnl We also make sure PAC_RUN_LOG can be used in AS_IF, so the last
dnl test command should have terminating ]), i.e. without newline before ]).
dnl
AC_DEFUN([PAC_RUNLOG],[
{ AS_ECHO(["$as_me:$LINENO: $1"]) >&AS_MESSAGE_LOG_FD
  (eval $1) 2>&AS_MESSAGE_LOG_FD
  ac_status=$?
  AS_ECHO(["$as_me:$LINENO: \$? = $ac_status"]) >&AS_MESSAGE_LOG_FD
  test $ac_status = 0; }])
dnl
dnl PAC_COMMAND_IFELSE is written to replace AC_TRY_EVAL with added logging
dnl to config.log, i.e. AC_TRY_EVAL does not log anything to config.log.
dnl If autoconf provides AC_COMMAND_IFELSE or AC_EVAL_IFELSE,
dnl AC_COMMAND_IFELSE dnl should be replaced by the official autoconf macros.
dnl
dnl PAC_COMMAND_IFELSE(COMMMAND,[ACTION-IF-RUN-OK],[ACTION-IF-RUN-FAIL])
dnl
AC_DEFUN([PAC_COMMAND_IFELSE],[
dnl Should use _AC_DO_TOKENS but use AC_RUN_LOG instead
dnl because _AC_XX is autoconf's undocumented macro.
AS_IF([PAC_RUNLOG([$1])],[
    $2
],[
    AS_ECHO(["$as_me: program exited with status $ac_status"]) >&AS_MESSAGE_LOG_FD
    m4_ifvaln([$3],[
        (exit $ac_status)
        $3
    ])
])
])
dnl
dnl
dnl
AC_DEFUN([PAC_EVAL_IFELSE],[
dnl Should use _AC_DO_TOKENS but use AC_RUN_LOG instead
dnl because _AC_XX is autoconf's undocumented macro.
AS_IF([PAC_RUNLOG([$$1])],[
    $2
],[
    AS_ECHO(["$as_me: program exited with status $ac_status"]) >&AS_MESSAGE_LOG_FD
    m4_ifvaln([$3],[
        (exit $ac_status)
        $3
    ])
])
])
dnl
dnl
dnl
AC_DEFUN([PAC_RUNLOG_IFELSE],[
dnl pac_TESTLOG is the internal temporary logfile for this macro.
pac_TESTLOG="pac_test.log"
rm -f $pac_TESTLOG
PAC_COMMAND_IFELSE([$1 > $pac_TESTLOG],[
    ifelse([$2],[],[],[$2])
],[
    AS_ECHO(["*** $1 :"]) >&AS_MESSAGE_LOG_FD
    cat $pac_TESTLOG >&AS_MESSAGE_LOG_FD
    ifelse([$3],[],[],[$3])
])
rm -f $pac_TESTLOG
])
