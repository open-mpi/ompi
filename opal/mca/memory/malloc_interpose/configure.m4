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

AC_DEFUN([MCA_memory_malloc_interpose_COMPILE_MODE], [
    AC_MSG_CHECKING([for MCA component $2:$3 compile mode])
    $4="static"
    AC_MSG_RESULT([$$4])
])


# MCA_memory_malloc_interpose_CONFIG(action-if-can-compile, 
#                        [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_memory_malloc_interpose_CONFIG],[
    AC_ARG_WITH([memory-manager],
        [AC_HELP_STRING([--with-memory-manager=TYPE],
                       [Use TYPE for intercepting memory management
                        calls to control memory pinning.])])

    AS_IF([test "$with_memory_manager" = "malloc_interpose"],
          [memory_malloc_interpose_happy="yes"
           memory_malloc_interpose_should_use=1],
          [memory_malloc_interpose_should_use=0
           AS_IF([test "$with_memory_manager" = ""],
                 [memory_malloc_interpose_happy="yes"],
                 [memory_malloc_interpose_happy="no"])])

    AS_IF([test "$memory_malloc_interpose_happy" = "yes"],
          [# check for malloc.h
           AC_CHECK_HEADER([malloc.h],
                           [memory_malloc_interpose_happy="yes"],
                           [memory_malloc_interpose_happy="no"])])

    AS_IF([test "$memory_malloc_interpose_happy" = "yes"],
          [AC_CHECK_FUNC([malloc_usable_size], 
                         [memory_malloc_interpose_happy="yes"],
                         [memory_malloc_interpose_happy="no"])])

    memory_malloc_interpose_LIBS_SAVE="$LIBS"
    AS_IF([test "$memory_malloc_interpose_happy" = "yes"],
          [AC_CHECK_LIB([dl],
                        [dlsym],
                        [memory_malloc_interpose_happy="yes"],
                        [memory_malloc_interpose_happy="no"])])
   LIBS="$memory_malloc_interpose_LIBS_SAVE"

    AS_IF([test "$memory_malloc_interpose_happy" = "yes"],
          [memory_malloc_interpose_WRAPPER_EXTRA_LIBS="-ldl"
           memory_malloc_interpose_LIBS="-ldl"])

   AS_IF([test "$memory_malloc_interpose_happy" = "no" -a \
               "$memory_malloc_interpose_should_use" = "1"],
         [AC_MSG_ERROR([malloc interpose memory management requested but not available.  Aborting.])])

    AC_SUBST(memory_malloc_interpose_LIBS)

    AS_IF([test "$memory_malloc_interpose_happy" = "yes"],
          [$1], [$2])
])
