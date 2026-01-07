# -*- shell-script -*-
#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2010-2016 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2016      Los Alamos National Security, LLC. All rights
#                         reserved.
# Copyright (c) 2022      Amazon.com, Inc. or its affiliates.  All Rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# MCA_btl_tcp_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_opal_btl_tcp_CONFIG],[
    AC_CONFIG_FILES([opal/mca/btl/tcp/Makefile])

    # check for sockaddr_in (a good sign we have TCP)
    AC_CHECK_TYPES([struct sockaddr_in],
                   [opal_btl_tcp_happy=yes
		    $1],
                   [opal_btl_tcp_happy=no
		    $2],
                   [AC_INCLUDES_DEFAULT
#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif
		   ])
    AC_CHECK_HEADERS([sys/ucred.h sys/socket.h])
    AC_CHECK_DECLS([getpeereid],
                   [AC_DEFINE(HAVE_GETPEEREID, 1, [Define to 1 if you have the `getpeereid' function.])],
                    ,[
#if HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#if HAVE_UNISTD_H
#include <unistd.h>
#endif
#if HAVE_SYS_SOCKET_H
#include <sys/socket.h>
#endif
    ])
    AC_MSG_CHECKING([for SO_PEERCRED support])
    AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
#define _GNU_SOURCE
#if HAVE_SYS_UCRED_H
#include <sys/ucred.h>
#endif
#if HAVE_SYS_SOCKET_H
#include <sys/socket.h>
#endif
#if HAVE_UNISTD_H
#include <unistd.h>
#endif
 
/* Test for the existence of SO_PEERCRED and the struct ucred */
#ifndef SO_PEERCRED
    #error "SO_PEERCRED not defined"
#endif
 
int main() {
    truct ucred cred;
    socklen_t len = sizeof(cred);
    (void)cred;
    (void)len;
    return 0;
}
    ]])], [
        AC_MSG_RESULT([yes])
        AC_DEFINE([HAVE_SO_PEERCRED], [1], [Define to 1 if SO_PEERCRED is supported.])
    ], [
        AC_MSG_RESULT([no])
    ])
 
    AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
#if HAVE_SYS_UCRED_H
#include <sys/ucred.h>
#endif
#include <sys/socket.h>
#include <unistd.h>
 
/* Test for the existence of LOCAL_PEERCRED and the struct ucred */
#ifndef LOCAL_PEERCRED
    #error "LOCAL_PEERCRED not defined"
#endif
 
int main() {
    struct ucred cred;
    socklen_t len = sizeof(cred);
    (void)cred;
    (void)len;
    return 0;
}
    ]])], [
        AC_MSG_RESULT([yes])
        AC_DEFINE([HAVE_LOCAL_PEERCRED], [1], [Define to 1 if LOCAL_PEERCRED is supported.])
    ], [
        AC_MSG_RESULT([no])
    ])
    OPAL_SUMMARY_ADD([Transports], [TCP], [], [$opal_btl_tcp_happy])
])dnl
