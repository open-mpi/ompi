# -*- shell-script -*-
#
# Copyright (c) 2010 Cisco Systems, Inc.  All rights reserved.
#
# Additional copyrights may follow.
#
# $HEADER$
#

AC_DEFUN([MCA_ompi_pml_v_COMPILE_MODE], [
    AC_MSG_CHECKING([for MCA component $2:$3 compile mode])
    $4="static"
    AC_MSG_RESULT([$$4])
])

AC_DEFUN([MCA_ompi_pml_v_CONFIG], [
    AC_CONFIG_FILES([ompi/mca/pml/v/Makefile])
    $1
])
