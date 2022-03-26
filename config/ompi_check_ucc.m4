dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2021      Mellanox Technologies. All rights reserved.
dnl Copyright (c) 2013-2021 Cisco Systems, Inc.  All rights reserved.
dnl Copyright (c) 2015      Research Organization for Information Science
dnl                         and Technology (RIST). All rights reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl

# OMPI_CHECK_UCC(prefix, [action-if-found], [action-if-not-found])
# --------------------------------------------------------
# check if ucc support can be found.  sets prefix_{CPPFLAGS,
# LDFLAGS, LIBS} as needed and runs action-if-found if there is
# support, otherwise executes action-if-not-found
AC_DEFUN([OMPI_CHECK_UCC],[
    OPAL_VAR_SCOPE_PUSH([ompi_check_ucc_happy CPPFLAGS_save LDFLAGS_save LIBS_save])

    AC_ARG_WITH([ucc],
                [AS_HELP_STRING([--with-ucc(=DIR)],
                                [Build UCC (Unified Collective Communication)])])

    OAC_CHECK_PACKAGE([ucc],
                      [$1],
                      [ucc/api/ucc.h],
                      [ucc],
                      [ucc_init_version],
                      [ompi_check_ucc_happy="yes"],
                      [ompi_check_ucc_happy="no"])

    AS_IF([test "$ompi_check_ucc_happy" = "yes"],
          [CPPFLAGS_save=$CPPFLAGS
           LDFLAGS_save=$LDFLAGS
           LIBS_save=$LIBS

           CPPFLAGS="${$1_CPPFLAGS} ${CPPFLAGS}"
           LDFLAGS="${$1_LDFLAGS} ${LDFLAGS}"
           LIBS="${$1_LIBS} ${LIBS}"
           AC_CHECK_FUNCS(ucc_comm_free, [], [])

           CPPFLAGS=$CPPFLAGS_save
           LDFLAGS=$LDFLAGS_save
           LIBS=$LIBS_save])

    AS_IF([test "$ompi_check_ucc_happy" = "yes" && test "$enable_progress_threads" = "yes"],
          [AC_MSG_WARN([ucc driver does not currently support progress threads.  Disabling UCC.])
           ompi_check_ucc_happy="no"])

    AS_IF([test "$ompi_check_ucc_happy" = "yes"],
          [$2],
          [AS_IF([test ! -z "$with_ucc" && test "$with_ucc" != "no"],
                 [AC_MSG_ERROR([UCC support requested but not found.  Aborting])])
           $3])

    OPAL_VAR_SCOPE_POP
])
