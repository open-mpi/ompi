# -*- shell-script -*-
#
# Copyright (c) 2011      Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2016      Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
#
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# MCA_ompi_fs_CONFIG(project_name, framework_name)
# -------------------------------------------
AC_DEFUN([MCA_ompi_fs_CONFIG],
[
    OPAL_VAR_SCOPE_PUSH([want_io_ompio])

    AS_IF([test "$enable_mpi_io" != "no" &&
           test "$enable_io_ompio" != "no"],
          [want_io_ompio=1],
          [want_io_ompio=0])

    MCA_CONFIGURE_FRAMEWORK([$1], [$2], [$want_io_ompio])

    OPAL_VAR_SCOPE_POP
])
