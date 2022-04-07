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
dnl Copyright (c) 2004-2006 The Regents of the University of California.
dnl                         All rights reserved.
dnl Copyright (c) 2009-2017 Cisco Systems, Inc.  All rights reserved
dnl Copyright (c) 2008-2018 University of Houston. All rights reserved.
dnl Copyright (c) 2015-2021 Research Organization for Information Science
dnl                         and Technology (RIST).  All rights reserved.
dnl Copyright (c) 2020      Triad National Security, LLC. All rights
dnl                         reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl

# OMPI_CHECK_LUSTRE(prefix, [action-if-found], [action-if-not-found])
# --------------------------------------------------------
# check if LUSTRE support can be found.  sets prefix_{CPPFLAGS,
# LDFLAGS, LIBS} as needed and runs action-if-found if there is
# support, otherwise executes action-if-not-found
AC_DEFUN([OMPI_CHECK_LUSTRE],[
    OPAL_VAR_SCOPE_PUSH([ompi_check_lustre_happy])

    # Get some configuration information
    AC_ARG_WITH([lustre],
        [AS_HELP_STRING([--with-lustre(=DIR)],
             [Build Lustre support, optionally adding DIR/include, DIR/lib, and DIR/lib64 to the search path for headers and libraries])])

     OAC_CHECK_PACKAGE([lustre],
                       [$1],
                       [lustre/lustreapi.h],
                       [lustreapi],
                       [llapi_file_create],
                       [ompi_check_lustre_happy="yes"],
                       [ompi_check_lustre_happy="no"])

    AS_IF([test "$ompi_check_lustre_happy" = "yes"],
          [AC_CACHE_CHECK([for required lustre data structures],
              [ompi_check_lustre_cv_ost_data_v1],
              [ompi_check_lustre_cv_ost_data_v1=yes
               cat > conftest.c <<EOF
#include "lustre/lustreapi.h"
void alloc_lum()
{
  int v1, v3;
  v1 = sizeof(struct lov_user_md_v1) +
   LOV_MAX_STRIPE_COUNT * sizeof(struct lov_user_ost_data_v1);
  v3 = sizeof(struct lov_user_md_v3) +
    LOV_MAX_STRIPE_COUNT * sizeof(struct lov_user_ost_data_v1);
}
EOF
               # Try the compile
               OPAL_LOG_COMMAND([$CC $CFLAGS ${$1_CPPFLAGS} -c conftest.c],
                                [], [ompi_check_lustre_cv_ost_data_v1=no])
               rm -f conftest.c conftest.o])
           AS_IF([test "${ompi_check_lustre_cv_ost_data_v1}" = "no"],
                 [ompi_check_lustre_happy=no
                  fs_lustre_SUMMARY="no"])])

    OPAL_SUMMARY_ADD([OMPIO File Systems], [Lustre], [], [${$1_SUMMARY}])

    AS_IF([test "$ompi_check_lustre_happy" = "yes"],
          [$2],
          [AS_IF([test -n "$with_lustre" && test "$with_lustre" != "no"],
                 [AC_MSG_ERROR([Lustre support requested but not found.  Aborting])])
           $3])

    OPAL_VAR_SCOPE_POP
])
