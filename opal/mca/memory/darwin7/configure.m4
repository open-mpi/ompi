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

AC_DEFUN([MCA_memory_darwin7_COMPILE_MODE], [
    AC_MSG_CHECKING([for MCA component $2:$3 compile mode])
    $4="static"
    AC_MSG_RESULT([$$4])
])


# MCA_memory_darwin7_CONFIG(action-if-can-compile, 
#                        [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_memory_darwin7_CONFIG],[
    AC_ARG_WITH([memory-manager],
        [AC_HELP_STRING([--with-memory-manager=TYPE],
                        [Use TYPE for intercepting memory management
                         calls to control memory pinning.])])

    # First, a mistaken user test
    AS_IF([test "$with_memory_manager" = "darwin7"],
          [AS_IF([test "`echo $host | grep apple-darwin`" == ""],
                 [AC_MSG_WARN([*** Using Dawrin malloc while not on Darwin system will not work.])
                  AC_MSG_ERROR([*** Aborting to save you the effort])])])

    AS_IF([test "$with_memory_manager" = "darwin7"],
          [memory_darwin7_happy="yes"
           memory_darwin7_should_use=1],
          [memory_darwin7_should_use=0
           AS_IF([test "$with_memory_manager" = ""],
                 [ # make this "yes" if we ever want to be default
                  memory_darwin7_happy="no"],
                 [memory_darwin7_happy="no"])])

    # disable if we aren't on Darwin
    AS_IF([test "$memory_darwin7_happy" = "yes"],
          [AS_IF([test "`echo $host | grep apple-darwin`" == ""],
                 [memory_darwin7_happy="no"])])

   AS_IF([test "$memory_darwin7_happy" = "no" -a \
               "$memory_darwin7_should_use" = "1"],
         [AC_MSG_ERROR([Darwin7 memory management requested but not available.  Aborting.])])

    AS_IF([test "$memory_darwin7_happy" = "yes"],
          [memory_darwin7_WRAPPER_ALWAYS_EXTRA_LDFLAGS="-Wl,-u,_opal_darwin_malloc_linker_hack -Wl,-multiply_defined,suppress -Wl,-force_flat_namespace -Wl,-flat_namespace"
           memory_darwin7_LIBMPI_ALWAYS_EXTRA_LDFLAGS="-Wl,-multiply_defined,suppress"
           $1],
          [$2])
])
