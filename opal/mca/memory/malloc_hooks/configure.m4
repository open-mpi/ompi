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

AC_DEFUN([MCA_memory_malloc_hooks_COMPILE_MODE], [
    AC_MSG_CHECKING([for MCA component $2:$3 compile mode])
    $4="static"
    AC_MSG_RESULT([$$4])
])


# MCA_memory_malloc_hooks_CONFIG(action-if-can-compile, 
#                        [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_memory_malloc_hooks_CONFIG],[
    AC_ARG_WITH([memory-manager],
        [AC_HELP_STRING([--with-memory-manager=TYPE],
                       [Use TYPE for intercepting memory management
                        calls to control memory pinning.])])

    AS_IF([test "$with_memory_manager" = "malloc_hooks"],
          [memory_malloc_hooks_happy="yes"
           memory_malloc_hooks_should_use=1],
          [memory_malloc_hooks_should_use=0
           AS_IF([test "$with_memory_manager" = ""],
                 [memory_malloc_hooks_happy="yes"],
                 [memory_malloc_hooks_happy="no"])])

    AS_IF([test "$memory_malloc_hooks_happy" = "yes"],
          [AS_IF([test "$enable_mpi_threads" = "yes" -o \
                       "$enable_progress_threads" = "yes"],
                 [memory_malloc_hooks_happy="no"])])

    AS_IF([test "$memory_malloc_hooks_happy" = "yes"],
          [# check for malloc.h
           AC_CHECK_HEADER([malloc.h],
                           [memory_malloc_hooks_happy="yes"],
                           [memory_malloc_hooks_happy="no"])])

    AS_IF([test "$memory_malloc_hooks_happy" = "yes"],
          [# check for init hook symbol
           AC_CHECK_DECL([__malloc_initialize_hook],
                         [memory_malloc_hooks_happy="yes"],
                         [memory_malloc_hooks_happy="no"],
                         [AC_INCLUDES_DEFAULT
                          #include <malloc.h>])])

    AS_IF([test "$memory_malloc_hooks_happy" = "yes"],
          [# check for the chunk size function
           AC_CHECK_FUNC([malloc_usable_size], 
                         [memory_malloc_hooks_happy="yes"],
                         [memory_malloc_hooks_happy="no"])])

   AS_IF([test "$memory_malloc_hooks_happy" = "no" -a \
               "$memory_malloc_hoooks_should_use" = "1"],
         [AC_MSG_ERROR([malloc_hooks memory management requested but not available.  Aborting.])])

    AS_IF([test "$memory_malloc_hooks_happy" = "yes"],
          [$1], [$2])
])
