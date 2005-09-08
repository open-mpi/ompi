# -*- shell-script -*-
#
# Copyright (c) 2004-2005 The Trustees of Indiana University.
#                         All rights reserved.
# Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
#                         All rights reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# MCA_pls_poe_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------

AC_DEFUN([MCA_pls_poe_CONFIG],[
    AC_ARG_WITH([poe], 
                [AC_HELP_STRING([--with-poe=DIR], 
                               [directory where POE was installed])])

    AS_IF([test ! -z "$with_poe" -a "$with_poe" = "no"], [$2], [
        AS_IF([test -z "$with_poe" -o "$with_poe" = "yes"], 
              [AC_PATH_PROG(OMPI_POE,poe)], 
              [AC_PATH_PROG(OMPI_POE,poe,,$with_poe)]
        )
        AS_IF([test ! -z "$OMPI_POE"], [  
              AC_DEFINE_UNQUOTED([OMPI_HAVE_POE], [1], [Whether we have POE support or not])
              AC_DEFINE_UNQUOTED([OMPI_POE], ["$OMPI_POE"], [POE execution path])
              AC_PATH_PROG(LSLPP,lslpp)
              AC_MSG_CHECKING([POE Version])
              AS_IF([test ! -z "$LSLPP"], [  
                  OMPI_POE_VERSION=`$LSLPP -q -c -L ppe.poe | cut -f 3 -d ':'`
                  AC_MSG_RESULT($OMPI_POE_VERSION)
                  AC_DEFINE_UNQUOTED([OMPI_POE_VERSION], ["$OMPI_POE_VERSION"], [POE Version])
              ], [
                  AC_MSG_RESULT([Unknown])
              ])
              [$1]
        ], [$2] )
    ])
])
