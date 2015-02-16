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
# Copyright (c) 2009-2010 Cisco Systems, Inc. All rights reserved.
# Copyright (c) 2014      Intel, Inc. All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# MCA_notifier_smtp_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_orte_notifier_smtp_CONFIG], [
    AC_CONFIG_FILES([orte/mca/notifier/smtp/Makefile])

    AC_CHECK_TYPES( [include/libesmtp.h],
                    [libesmtp*],
                    [libesmtp.h],
                    [esmtp],
                    [smtp_create_session],
                    [],
                    [orte_notifier_want_smtp=1],
                    [orte_notifier_want_smtp=0])

    AS_IF([test "$orte_notifier_want_smtp" = 1],
          [$1],
          [$2])
])dnl
