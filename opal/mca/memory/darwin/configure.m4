# -*- shell-script -*-
#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
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

AC_DEFUN([MCA_memory_darwin_COMPILE_MODE], [
    AC_MSG_CHECKING([for MCA component $2:$3 compile mode])
    $4="static"
    AC_MSG_RESULT([$$4])
])


# MCA_memory_darwin_CONFIG(action-if-can-compile, 
#                        [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_memory_darwin_CONFIG],[
    AC_ARG_WITH([memory-manager],
        [AC_HELP_STRING([--with-memory-manager=TYPE],
                       [Use TYPE for intercepting memory management
                        calls to control memory pinning.])])

    AS_IF([test "$with_memory_manager" = "darwin"],
          [memory_darwin_happy="yes"
           memory_darwin_should_use=1],
          [memory_darwin_should_use=0
           AS_IF([test "$with_memory_manager" = ""],
                 [memory_darwin_happy="yes"],
                 [memory_darwin_happy="no"])])

    AS_IF([test "$memory_darwin_happy" = "yes"],
          [# check for malloc/malloc.h
           AC_CHECK_HEADER([malloc/malloc.h],
                           [memory_darwin_happy="yes"],
                           [memory_darwin_happy="no"])])

    AS_IF([test "$memory_darwin_happy" = "yes"],
          [# check for hook to get correct callbacks
           AC_CHECK_FUNC([malloc_default_zone], 
                         [memory_darwin_happy="yes"],
                         [memory_darwin_happy="no"])])

   # this should be true for any reasonably recent version of OS X,
   # but make sure.
    AS_IF([test "$memory_darwin_happy" = "yes"],
          [AC_CHECK_FUNC([dlsym], 
                         [memory_darwin_happy="yes"],
                         [memory_darwin_happy="no"])])

   AS_IF([test "$memory_darwin_happy" = "no" -a \
               "$memory_malloc_hoooks_should_use" = "1"],
         [AC_MSG_ERROR([Darwin memory management requested but not available.  Aborting.])])

    AS_IF([test "$memory_darwin_happy" = "yes"],
          [# Yes, we really do want to screw with LDFLAGS here...
           LDFLAGS="$LDFLAGS -Wl,-u,_munmap -Wl,-multiply_defined,suppress"
           memory_darwin_WRAPPER_EXTRA_LDFLAGS="-Wl,-u,_munmap -Wl,-multiply_defined,suppress"
           memory_base_found=1
           $1], [$2])
])
