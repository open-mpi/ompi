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

AC_DEFUN([MCA_memory_ptmalloc2_COMPILE_MODE], [
    AC_MSG_CHECKING([for MCA component $2:$3 compile mode])
    $4="static"
    AC_MSG_RESULT([$$4])
])


# MCA_memory_ptmalloc2_CONFIG(action-if-can-compile, 
#                        [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_memory_ptmalloc2_CONFIG],[
    AC_ARG_WITH([memory-manager],
        [AC_HELP_STRING([--with-memory-manager=TYPE],
                       [Use TYPE for intercepting memory management
                        calls to control memory pinning.])])

    AC_ARG_ENABLE([ptmalloc2-opt-sbrk],
        [AC_HELP_STRING([--enable-ptmalloc2-opt-sbrk],
            [Only trigger callbacks when sbrk is used for small
             allocations, rather than every call to malloc/free.
             (default: enabled)])])

    if test "$enable_ptmalloc2_opt_sbrk" = "no" ; then
        memory_ptmalloc2_opt_sbrk=0
    else
        memory_ptmalloc2_opt_sbrk=1
    fi
    AC_DEFINE_UNQUOTED([OMPI_MEMORY_PTMALLOC2_OPT_SBRK],
                       [$memory_ptmalloc2_opt_sbrk],
                       [Trigger callbacks on sbrk instead of malloc or free])

    AS_IF([test "$with_memory_manager" = "ptmalloc2"],
          [if test "`echo $host | grep apple-darwin`" != "" ; then
            AC_MSG_WARN([*** Using ptmalloc with OS X will result in failure.])
            AC_MSG_ERROR([*** Aborting to save you the effort])
           fi
           memory_ptmalloc2_happy="yes"
           memory_ptmalloc2_should_use=1],
          [memory_ptmalloc2_should_use=0
           AS_IF([test "$with_memory_manager" = ""],
                 [memory_ptmalloc2_happy="yes"],
                 [memory_ptmalloc2_happy="no"])])

    AS_IF([test "$memory_ptmalloc2_happy" = "yes"],
          [# check for malloc.h
           AC_CHECK_HEADER([malloc.h],
                           [memory_ptmalloc2_happy="yes"],
                           [memory_ptmalloc2_happy="no"])])

    AS_IF([test "$memory_ptmalloc2_happy" = "yes"],
          [# check for init hook symbol
           AC_CHECK_DECL([__malloc_initialize_hook],
                         [memory_ptmalloc2_happy="yes"],
                         [memory_ptmalloc2_happy="no"],
                         [AC_INCLUDES_DEFAULT
                          #include <malloc.h>])])

    #
    # See if we have sbrk prototyped
    #
    AC_CHECK_DECLS([sbrk])

    #
    # Figure out how we're going to call mmap/munmap for real
    #
    AS_IF([test "$memory_ptmalloc2_happy" = "yes"],
          [memory_ptmalloc2_mmap=0
           memory_ptmalloc2_munmap=1

           # it's nearly impossible to call mmap from syscall(), so
           # only go this route if we can't get at munmap any other 
           # way.
           AC_CHECK_HEADER([syscall.h], 
               [AC_CHECK_FUNCS([syscall], [], [memory_ptmalloc2_munmap=0])])

           # Always look for __munmap and __mmap
           AC_CHECK_FUNCS([__munmap], [memory_ptmalloc2_mmap=1])
           AC_CHECK_FUNCS([__mmap])

           # only allow dlsym (and therefore add -ldl) if we
           # really need to
           AS_IF([test "$memory_ptmalloc2_mmap" = "0"],
                 [memory_ptmalloc2_LIBS_SAVE="$LIBS"
                  AC_CHECK_LIB([dl],
                               [dlsym],
                               [LIBS="$LIBS -ldl"
                                memory_ptmalloc2_LIBS="-ldl"
                                memory_ptmalloc2_mmap=1])
                  AC_CHECK_FUNCS([dlsym])
                  LIBS="$memory_ptmalloc2_LIBS_SAVE"])

           AS_IF([test "$memory_ptmalloc2_mmap" = "0" -a "$memory_ptmalloc2_munmap" = "0"],
                 [memory_ptmalloc2_happy="no"])])

   AS_IF([test "$memory_ptmalloc2_happy" = "yes"],
         [memory_ptmalloc2_WRAPPER_EXTRA_LIBS="$memory_ptmalloc2_LIBS"])

   AS_IF([test "$memory_ptmalloc2_happy" = "no" -a \
               "$memory_malloc_hoooks_should_use" = "1"],
         [AC_MSG_ERROR([ptmalloc2 memory management requested but not available.  Aborting.])])

    AC_SUBST([memory_ptmalloc2_LIBS])

    AS_IF([test "$memory_ptmalloc2_happy" = "yes"],
          [memory_base_found=1
           $1], [$2])
])
