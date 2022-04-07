# -*- shell-script -*-
#
#
# Copyright (c) 2021      Mellanox Technologies. All rights reserved.
# Copyright (c) 2015      Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# Copyright (c) 2022      Amazon.com, Inc. or its affiliates.  All Rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#


# MCA_coll_ucc_CONFIG([action-if-can-compile],
#                      [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_ompi_coll_ucc_CONFIG],[
    AC_CONFIG_FILES([ompi/mca/coll/ucc/Makefile])

    OMPI_CHECK_UCC([coll_ucc],
                     [coll_ucc_happy="yes"],
                     [coll_ucc_happy="no"])

    AS_IF([test "$coll_ucc_happy" = "yes"],
          [coll_ucc_WRAPPER_EXTRA_LDFLAGS="$coll_ucc_LDFLAGS"
           coll_ucc_CPPFLAGS="$coll_ucc_CPPFLAGS"
           coll_ucc_WRAPPER_EXTRA_LIBS="$coll_ucc_LIBS"
           $1],
          [$2])
    
    OPAL_SUMMARY_ADD([Miscellaneous], [Open UCC], [], [$coll_ucc_happy])

    # substitute in the things needed to build ucc
    AC_SUBST([coll_ucc_CPPFLAGS])
    AC_SUBST([coll_ucc_LDFLAGS])
    AC_SUBST([coll_ucc_LIBS])
])dnl
