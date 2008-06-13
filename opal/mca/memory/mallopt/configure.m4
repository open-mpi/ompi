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

AC_DEFUN([MCA_memory_mallopt_COMPILE_MODE], [
    AC_MSG_CHECKING([for MCA component $2:$3 compile mode])
    $4="static"
    AC_MSG_RESULT([$$4])
])


# MCA_memory_mallopt_CONFIG(action-if-can-compile, 
#                        [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_memory_mallopt_CONFIG],[

    AS_IF([test "$with_memory_manager" = "mallopt"],
          [memory_mallopt_happy="yes"
           memory_mallopt_should_use=1],
          [memory_mallopt_should_use=0
           AS_IF([test "$with_memory_manager" = ""],
                 [memory_mallopt_happy="yes"],
                 [memory_mallopt_happy="no"])])

    AS_IF([test "$memory_mallopt_happy" = "yes"],
          [AC_CHECK_HEADER([malloc.h], [], [memory_mallopt_happy="no"])])

    AS_IF([test "$memory_mallopt_happy" = "yes"],
          [AC_CHECK_FUNCS([mallopt], [], [memory_mallopt_happy="no"])])

    AS_IF([test "$memory_mallopt_happy" = "yes"],
          [memory_mallopt_munmap=0

           AC_CHECK_HEADER([syscall.h], 
               [AC_CHECK_FUNCS([syscall], [memory_mallopt_munmap=1])])

           AC_CHECK_FUNCS([__munmap], [memory_mallopt_munmap=1])

           # only allow dlsym (and therefore add -ldl) if we
           # really need to
           AS_IF([test "$memory_mallopt_munmap" = "0"],
                 [memory_mallopt_LIBS_SAVE="$LIBS"
                  AC_CHECK_LIB([dl],
                               [dlsym],
                               [memory_mallopt_LIBS="-ldl"
                                memory_mallopt_munmap=1])
                  AC_CHECK_FUNCS([dlsym])
                  LIBS="$memory_mallopt_LIBS_SAVE"])

           AS_IF([test "$memory_mallopt_munmap" = "0"],
                 [memory_mallopt_happy="no"])])

    AS_IF([test "$memory_mallopt_happy" = "yes"],
          [memory_mallopt_WRAPPER_EXTRA_LIBS="$memory_mallopt_LIBS"])

    AS_IF([test "$memory_mallopt_happy" = "no" -a \
                "$memory_malloc_hoooks_should_use" = "1"],
          [AC_MSG_ERROR([mallopt memory management requested but not available.  Aborting.])])

    AC_SUBST(memory_mallopt_LIBS)

    AS_IF([test "$memory_mallopt_happy" = "yes"],
          [memory_base_found=1
           $1], [$2])
])
