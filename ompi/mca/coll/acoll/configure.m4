#
# Copyright (c) 2024 Advanced Micro Devices, Inc. All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

AC_DEFUN([MCA_ompi_coll_acoll_CONFIG],[
        AC_CONFIG_FILES([ompi/mca/coll/acoll/Makefile])

        OPAL_CHECK_XPMEM([coll_acoll], [should_build=1], [should_build=1])

        AC_SUBST([coll_acoll_CPPFLAGS])
        AC_SUBST([coll_acoll_LDFLAGS])
        AC_SUBST([coll_acoll_LIBS])
])dnl
