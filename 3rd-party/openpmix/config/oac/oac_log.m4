dnl -*- autoconf -*-
dnl
dnl Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
dnl                         University Research and Technology
dnl                         Corporation.  All rights reserved.
dnl Copyright (c) 2004-2018 The University of Tennessee and The University
dnl                         of Tennessee Research Foundation.  All rights
dnl                         reserved.
dnl Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
dnl                         University of Stuttgart.  All rights reserved.
dnl Copyright (c) 2004-2005 The Regents of the University of California.
dnl                         All rights reserved.
dnl Copyright (c) 2007      Sun Microsystems, Inc.  All rights reserved.
dnl Copyright (c) 2009      Oak Ridge National Labs.  All rights reserved.
dnl Copyright (c) 2009-2020 Cisco Systems, Inc.  All rights reserved.
dnl Copyright (c) 2014      Intel, Inc. All rights reserved.
dnl Copyright (c) 2015-2017 Research Organization for Information Science
dnl                         and Technology (RIST). All rights reserved.
dnl Copyright (c) 2021-2022 Amazon.com, Inc. or its affiliates.  All Rights reserved.
dnl
dnl Copyright (c) 2024      Nanook Consulting  All rights reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$


dnl OAC_LOG_MSG: Log message in config.log, including prefix
dnl              giving line number
dnl
dnl 1 -> the message to log
AC_DEFUN([OAC_LOG_MSG],
[AS_ECHO(["configure:__oline__: $1"]) >&AS_MESSAGE_LOG_FD])dnl


dnl OAC_LOG_MSG_NOPREFIX: Log message in config.log, with no prefix
dnl
dnl 1 -> the message to log
AC_DEFUN([OAC_LOG_MSG_NOPREFIX],
[AS_ECHO([$1]) >&AS_MESSAGE_LOG_FD])dnl


dnl OAC_LOG_FILE: Dump the specified file into config.log
dnl
dnl 1 -> filename of file to dump into config.log
AC_DEFUN([OAC_LOG_FILE],
[AS_IF([test -n "$1" && test -f "$1"], [cat $1 >&AS_MESSAGE_LOG_FD])])dnl


dnl OAC_LOG_COMMAND: Run command, logging output, and checking status
dnl
dnl 1 -> command to execute
dnl 2 -> action if successful
dnl 3 -> action if if fail
AC_DEFUN([OAC_LOG_COMMAND],[
OAC_LOG_MSG([$1])
$1 1>&AS_MESSAGE_LOG_FD 2>&1
oac_log_command_status=$?
OAC_LOG_MSG([\$? = $oac_log_command_status])
AS_IF([test $oac_log_command_status -eq 0],
      [$2], [$3])
AS_UNSET([oac_log_command_status])])
