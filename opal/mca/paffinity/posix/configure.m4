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
# Copyright (c) 2007-2008 Cisco, Inc. All rights reserved.
# Copyright (c) 2008      Sun Microsystems, Inc. All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# MCA_paffinity_posix_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_paffinity_posix_CONFIG],[
    OMPI_VAR_SCOPE_PUSH([paff_posix_happy])
    # Check to see if we have <unistd.h>
    AC_CHECK_HEADER([unistd.h], [paff_posix_happy=yes], [paff_posix_happy=no])

    # Check to see if we have _SC_NPROCESSORS_ONLN
    AS_IF([test "$paff_posix_happy" = "yes"],
          [AC_MSG_CHECKING([for _SC_NPROCESSORS_ONLN])
           AS_IF([test "$OMPI_HAVE__SC_NPROCESSORS_ONLN" = "1"],
                 [paff_posix_happy=yes], [paff_posix_happy=no])
           AC_MSG_RESULT([(cached) $paff_posix_happy])])

    AS_IF([test "$paff_posix_happy" = "yes"], [$1], [$2])
    OMPI_VAR_SCOPE_POP
])dnl

