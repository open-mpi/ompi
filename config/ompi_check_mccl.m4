dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2020      Mellanox Technologies. All rights reserved.
dnl Copyright (c) 2013      Cisco Systems, Inc.  All rights reserved.
dnl Copyright (c) 2015      Research Organization for Information Science
dnl                         and Technology (RIST). All rights reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl

# OMPI_CHECK_MCCL(prefix, [action-if-found], [action-if-not-found])
# --------------------------------------------------------
# check if mccl support can be found.  sets prefix_{CPPFLAGS,
# LDFLAGS, LIBS} as needed and runs action-if-found if there is
# support, otherwise executes action-if-not-found
AC_DEFUN([OMPI_CHECK_MCCL],[
    OPAL_VAR_SCOPE_PUSH([ompi_check_mccl_dir ompi_check_mccl_libs ompi_check_mccl_happy CPPFLAGS_save LDFLAGS_save LIBS_save])

    AC_ARG_WITH([mccl],
        [AC_HELP_STRING([--with-mccl(=DIR)],
             [Build mccl (Mellanox Collective Communication library) support, optionally adding
              DIR/include and DIR/lib or DIR/lib64 to the search path for headers and libraries])])

    AS_IF([test "$with_mccl" != "no"],
          [ompi_check_mccl_libs=mccl
           AS_IF([test ! -z "$with_mccl" && test "$with_mccl" != "yes"],
                 [ompi_check_mccl_dir=$with_mccl])

           CPPFLAGS_save=$CPPFLAGS
           LDFLAGS_save=$LDFLAGS
           LIBS_save=$LIBS

           OPAL_LOG_MSG([$1_CPPFLAGS : $$1_CPPFLAGS], 1)
           OPAL_LOG_MSG([$1_LDFLAGS  : $$1_LDFLAGS], 1)
           OPAL_LOG_MSG([$1_LIBS     : $$1_LIBS], 1)

           OPAL_CHECK_PACKAGE([$1],
                              [api/mccl.h],
                              [$ompi_check_mccl_libs],
                              [mccl_init_context],
                              [],
                              [$ompi_check_mccl_dir],
                              [],
                              [ompi_check_mccl_happy="yes"],
                              [ompi_check_mccl_happy="no"])

           AS_IF([test "$ompi_check_mccl_happy" = "yes"],
                 [
                     CPPFLAGS=$coll_mccl_CPPFLAGS
                     LDFLAGS=$coll_mccl_LDFLAGS
                     LIBS=$coll_mccl_LIBS
                     AC_CHECK_FUNCS(mccl_comm_free, [], [])
                 ],
                 [])

           CPPFLAGS=$CPPFLAGS_save
           LDFLAGS=$LDFLAGS_save
           LIBS=$LIBS_save],
          [ompi_check_mccl_happy=no])

    AS_IF([test "$ompi_check_mccl_happy" = "yes" && test "$enable_progress_threads" = "yes"],
          [AC_MSG_WARN([mccl driver does not currently support progress threads.  Disabling MCCL.])
           ompi_check_mccl_happy="no"])

    AS_IF([test "$ompi_check_mccl_happy" = "yes"],
          [$2],
          [AS_IF([test ! -z "$with_mccl" && test "$with_mccl" != "no"],
                 [AC_MSG_ERROR([MCCL support requested but not found.  Aborting])])
           $3])

    OPAL_VAR_SCOPE_POP
])
