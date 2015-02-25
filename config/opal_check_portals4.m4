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
dnl Copyright (c) 2006      QLogic Corp. All rights reserved.
dnl Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
dnl Copyright (c) 2015      Research Organization for Information Science
dnl                         and Technology (RIST). All rights reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl

# OPAL_CHECK_PORTALS4(prefix, [action-if-found], [action-if-not-found])
# --------------------------------------------------------
# check if PORTALS4 support can be found.  sets prefix_{CPPFLAGS,
# LDFLAGS, LIBS} as needed and runs action-if-found if there is
# support, otherwise executes action-if-not-found
AC_DEFUN([OPAL_CHECK_PORTALS4],[
    AC_ARG_WITH([portals4],
        [AC_HELP_STRING([--with-portals4(=DIR)],
             [Build Portals4 support, optionally adding DIR/include, DIR/lib, and DIR/lib64 to the search path for headers and libraries])])
    OPAL_CHECK_WITHDIR([portals4], [$with_portals4], [include/portals4.h])
    AC_ARG_WITH([portals4-libdir],
        [AC_HELP_STRING([--with-portals4-libdir=DIR],
             [Search for Portals4 libraries in DIR])])
    OPAL_CHECK_WITHDIR([portals4-libdir], [$with_portals4_libdir], [libportals.*])

    ompi_check_portals4_$1_save_CPPFLAGS="$CPPFLAGS"
    ompi_check_portals4_$1_save_LDFLAGS="$LDFLAGS"
    ompi_check_portals4_$1_save_LIBS="$LIBS"

    AS_IF([test "$with_portals4" != "no"],
          [AS_IF([test ! -z "$with_portals4" && test "$with_portals4" != "yes"],
                 [ompi_check_portals4_dir="$with_portals4"])
           AS_IF([test ! -z "$with_portals4_libdir" && test "$with_portals4_libdir" != "yes"],
                 [ompi_check_portals4_libdir="$with_portals4_libdir"])

           OPAL_CHECK_PACKAGE([$1],
                              [portals4.h],
                              [portals],
                              [PtlLEAppend],
			      [],
                              [$ompi_check_portals4_dir],
                              [$ompi_check_portals4_libdir],
                              [ompi_check_portals4_happy="yes"],
                              [ompi_check_portals4_happy="no"])],
          [ompi_check_portals4_happy="no"])

    CPPFLAGS="$ompi_check_portals4_$1_save_CPPFLAGS"
    LDFLAGS="$ompi_check_portals4_$1_save_LDFLAGS"
    LIBS="$ompi_check_portals4_$1_save_LIBS"

    max_md_size=0
    AC_ARG_WITH([portals4-max-md-size],
      [AC_HELP_STRING([--with-portals4-max-md-size=SIZE],
         [Log base 2 of the maximum size in bytes of a memory descriptor.  Should only be set for implementations which do not support binding all of virtual address space.])])
    AS_IF([test "$with_portals4_max_md_size" = "yes" || test "$with_portals4_max_md_size" = "no"],
          [AC_MSG_ERROR([--with-portals4-max-md-size requires an integer argument])],
          [AS_IF([test -n "$with_portals4_max_md_size"],
                 [max_md_size="$with_portals4_max_md_size"])])
    AC_DEFINE_UNQUOTED([OPAL_PORTALS4_MAX_MD_SIZE], [$max_md_size],
      [Log base 2 of the maximum size in bytes of a memory descriptor.  Set to 0 if MD can bind all of memory.])

    max_va_size=0
    AC_ARG_WITH([portals4-max-va-size],
      [AC_HELP_STRING([--with-portals4-max-va-size=SIZE],
         [Log base 2 of the maximum size in bytes of the user virtual address space.  Should only be set for implementations which do not support binding all of virtual address space.])])
    AS_IF([test "$with_portals4_max_va_size" = "yes" || test "$with_portals4_max_va_size" = "no"],
          [AC_MSG_ERROR([--with-portals4-max-va-size requires an integer argument])],
          [AS_IF([test -n "$with_portals4_max_va_size"],
                 [max_va_size="$with_portals4_max_va_size"])])
    AC_DEFINE_UNQUOTED([OPAL_PORTALS4_MAX_VA_SIZE], [$max_va_size],
      [Log base 2 of the maximum size in bytes of the user virtual address space.  Set to 0 if MD can bind all of memory.])

    AS_IF([(test $max_md_size -eq 0 && test $max_va_size -ne 0 ) || (test $max_md_size -ne 0 && test $max_va_size -eq 0 )],
          [AC_ERROR([If either --with-portals4-max-md-size or --with-portals4-max-va-size is set, both must be set.])])
    AS_IF([test $max_md_size -ge $max_va_size],
          [max_md_size=0
           max_va_size=0])
    AS_IF([test $max_md_size -ne 0 && test $max_va_size -ne 0],
          [AC_MSG_NOTICE([Portals 4 address space size: $max_md_size, $max_va_size])])

    AS_IF([test "$ompi_check_portals4_happy" = "yes"],
          [$2],
          [AS_IF([test ! -z "$with_portals4" && test "$with_portals4" != "no"],
                 [AC_MSG_ERROR([Portals4 support requested but not found.  Aborting])])
           $3])
])

