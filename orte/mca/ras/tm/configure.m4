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

AC_DEFUN([MCA_ras_tm_CONFIG],[
    OMPI_CHECK_TM([ras_tm], [ras_tm_good=1], [ras_tm_good=0])
         
    AS_IF([test "$ras_tm_good" = "0"], [$2],
          [ras_tm_WRAPPER_EXTRA_LDFLAGS="$ras_tm_LDFLAGS"
           ras_tm_WRAPPER_EXTRA_LIBS="$ras_tm_LIBS"
           $1])

    AC_SUBST([ras_tm_CPPFLAGS])
    AC_SUBST([ras_tm_LDFLAGS])
    AC_SUBST([ras_tm_LIBS])
])dnl
