dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
dnl                         University Research and Technology
dnl                         Corporation.  All rights reserved.
dnl Copyright (c) 2004-2005 The University of Tennessee and The University
dnl                         of Tennessee Research Foundation.  All rights
dnl                         reserved.
dnl Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
dnl                         University of Stuttgart.  All rights reserved.
dnl Copyright (c) 2004-2005 The Regents of the University of California.
dnl                         All rights reserved.
dnl $COPYRIGHT$
dnl 
dnl Additional copyrights may follow
dnl 
dnl $HEADER$
dnl

dnl we only want one :)
m4_define(MCA_memory_CONFIGURE_MODE, STOP_AT_FIRST)

AC_DEFUN([MCA_memory_CONFIG],[
        memory_base_found=0

        # first, compile all the components
        MCA_CONFIGURE_FRAMEWORK($1, $2)

        AC_DEFINE_UNQUOTED([OMPI_MEMORY_HAVE_COMPONENT], [$memory_base_found],
            [Whether any opal memory mca components were found])
])
