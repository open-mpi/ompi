# -*- shell-script -*-
#
# Copyright (c) 2004-2008 The Trustees of the University of Tennessee.
#                         All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# MCA_pml_dr_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_pml_dr_CONFIG],[
    # Dont compile DR if threading is enabled but there is no
    # support for 64 bits atomics.
    AS_IF([test $OPAL_ASM_SUPPORT_64BIT -eq 1],
        [$1],
        [AS_IF([test $OPAL_ENABLE_PROGRESS_THREADS -eq 1 -o $OPAL_ENABLE_MPI_THREADS -eq 1],
               [$2],
               [$1])
        ])
])dnl
