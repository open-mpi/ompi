# -*- shell-script -*-
#
# Copyright (c) 2004-2005 The Trustees of Indiana University.
#                         All rights reserved.
# Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
#                         All rights reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#


# OMPI_CHECK_XGRID(prefix, [action-if-found], [action-if-not-found])
# --------------------------------------------------------
AC_DEFUN([OMPI_CHECK_XGRID],[
    AC_REQUIRE([AC_PROG_OBJC])

    AC_ARG_WITH([xgrid],
                [AC_HELP_STRING([--with-xgrid],
                                [Build support for the Apple Xgrid batch system (default: yes)])])

    AC_CACHE_CHECK([for XGridFoundation Framework],
                   [ompi_cv_check_xgrid_foundation],
                   [AS_IF([test "$with_xgrid" != "no"], 
                          [_OMPI_CHECK_XGRID([ompi_cv_check_xgrid_foundation="yes"],
                                             [ompi_cv_check_xgrid_foundation="no"])],
                          [ompi_cv_check_xgrid_foundation="no"])])

    AS_IF([test "$ompi_cv_check_xgrid_foundation" = "yes"], 
          [$1_OBJCFLAGS="$$1_OBJCFLAGS -F XGridFoundation"
           $1_LDFLAGS="$$1_LDFLAGS -framework XGridFoundation"
           $2], [$3])
])

# _OMPI_CHECK_XGRID([action-if-found], [action-if-not-found])
# --------------------------------------------------------
AC_DEFUN([_OMPI_CHECK_XGRID],[
    # ok, so in environments where this is going to work, the objc and
    # c compilers are the same thing.  Because our ompi_prog_objc is
    # really lame, you can't push/pop languages into and out of
    # objective-C (as soon as something post Autoconf 2.59 comes out,
    # it will have Objective C support, and this will all be much
    # simpler).  We modify CFLAGS because AC_TRY_LINK is going to use
    # the C compiler.  Automake, however, is smart enough to know what
    # Objective C is, so in the end, we put things in OBJCFLAGS and
    # the right things will happen.  *sigh*

    ompi_check_xgrid_save_CFLAGS="$CFLAGS"
    CFLAGS="$CFLAGS -framework XGridFoundation"
    AC_TRY_LINK([],[;],[ompi_check_xgrid_happy="yes"],[ompi_check_xgrid_happy="no"])
    CFLAGS="$ompi_check_xgrid_save_CFLAGS"

    AS_IF([test "$ompi_check_xgrid_happy" = "no"], [$2], [$1])
])
