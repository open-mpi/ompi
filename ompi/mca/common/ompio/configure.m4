# -*- shell-script -*-
#
# Copyright (c) 2016-2017 Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# MCA_ompi_common_ompio_CONFIG([action-if-can-compile],
#                         [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_ompi_common_ompio_CONFIG],[
    AC_CONFIG_FILES([ompi/mca/common/ompio/Makefile])

    AS_IF([test "$enable_io_ompio" != "no"],
          [$1],
          [$2])
])dnl
