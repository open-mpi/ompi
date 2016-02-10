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
# Copyright (c) 2008-2010 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2015      Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#
AC_DEFUN([MCA_opal_memory_linux_PRIORITY], [40])

AC_DEFUN([MCA_opal_memory_linux_COMPILE_MODE], [
    AC_MSG_CHECKING([for MCA component $2:$3 compile mode])
    $4="static"
    AC_MSG_RESULT([$$4])
])


# MCA_memory_linux_CONFIG(action-if-can-compile,
#                        [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_opal_memory_linux_CONFIG],[
    AC_CONFIG_FILES([opal/mca/memory/linux/Makefile])

    OPAL_VAR_SCOPE_PUSH([memory_linux_ptmalloc2_happy memory_linux_ummu_happy memory_linux_requested icc_major_ver icc_minor_ver memory_linux_mmap memory_linux_munmap memory_linux_LIBS_SAVE])

    # Only allow this component to build on Linux-based systems

    AC_MSG_CHECKING([operating system])
    case $host in
    *linux*)
        AC_MSG_RESULT([$host -- supported])
        memory_linux_ptmalloc2_happy=yes
        memory_linux_ummu_happy=yes
        ;;
    *)
        AC_MSG_RESULT([$host -- unsupported])
        memory_linux_ptmalloc2_happy=no
        memory_linux_ummu_happy=no
        ;;
    esac

    AS_IF([test "$with_memory_manager" = "linux"],
          [memory_linux_ptmalloc2_happy=yes
           memory_linux_ummu_happy=yes
           memory_linux_requested=1],
          [memory_linux_requested=0
           AS_IF([test -z "$with_memory_manager" || test "$with_memory_manager" = "yes"],
                 [memory_linux_ptmalloc2_happy=yes
                  memory_linux_ummu_happy=yes],
                 [memory_linux_ptmalloc2_happy=no
                  memory_linux_ummu_happy=no])])


    ######################################################################
    # if memory hook available
    ######################################################################
    memory_hook_found=1
    AS_IF([test "$memory_hook_found" -eq 1],
        [memory_hook_found=0 AC_CHECK_HEADER([malloc.h],
             [AC_CHECK_FUNC([__malloc_initialize_hook],
                 [AC_CHECK_FUNC([__malloc_hook],
                     [AC_CHECK_FUNC([__realloc_hook],
                         [AC_CHECK_FUNC([__free_hook],
                            [memory_hook_found=1])])])])])])
    AC_MSG_CHECKING([whether the system can use malloc hooks])
    AS_IF([test "$memory_hook_found" = "0"],
          [AC_MSG_RESULT([no])],
          [AC_MSG_RESULT([yes])])
    AC_DEFINE_UNQUOTED([MEMORY_LINUX_HAVE_MALLOC_HOOK_SUPPORT], [$memory_hook_found],
                   	   [Whether the system has Memory Allocation Hooks])

    AC_ARG_ENABLE(memory-linux-malloc-alignment,
        AC_HELP_STRING([--enable-memory-linux-malloc-alignment], [Enable support for allocated memory alignment. Default: enabled if supported, disabled otherwise.]))

    malloc_align_enabled=0
    AS_IF([test "$enable_memory_linux_malloc_alignment" != "no"],
        [malloc_align_enabled=$memory_hook_found])

    AS_IF([test "$enable_memory_linux_malloc_alignment" = "yes" && test "$malloc_align_enabled" = "0"],
          [AC_MSG_ERROR([memory linux malloc alignment is requested but __malloc_hook is not available])])
    AC_MSG_CHECKING([whether the memory linux will use malloc alignment])
    AS_IF([test "$malloc_align_enabled" = "0"],
          [AC_MSG_RESULT([no])],
          [AC_MSG_RESULT([yes])])

    AC_DEFINE_UNQUOTED(MEMORY_LINUX_MALLOC_ALIGN_ENABLED, [$malloc_align_enabled],
                       [Whether the memory linux malloc alignment is enabled])

    ######################################################################
    # ptmalloc2
    ######################################################################

    # Per ticket #227, Intel 9.0 v20051201 on ia64 with optimization
    # of -O2 or higher will bork linux in strange in mysterious ways.
    # Doh!  So if the compiler vendor is intel and we're on an ia64
    # box, run "icc --version" and snarf the version string.  If it's
    # 9.0 and the version is <= 20051201, then disable ptmalloc2.
    # Executive decision: ignore optimization levels (even though -O1
    # and -O0 seem to work).  The upgrade to 9.1 is free, so that's a
    # better path than trying to make a much more complicated test
    # here.

    AS_IF([test "$memory_linux_ptmalloc2_happy" = yes],
          [case $host in
           ia64-*)
                AS_IF([test "$opal_c_vendor" = "intel"],
                      [# check for v9.0 <= 20051201
                       icc_major_ver="`$CC --version | head -n 1 | awk '{ print [$]3 }'`"
                       icc_minor_ver="`$CC --version | head -n 1 | awk '{ print [$]4 }'`"
                       AS_IF([test "$icc_major_ver" = "9.0" && test "`expr $icc_minor_ver \<= 20051201`" = "1"],
                             [memory_linux_ptmalloc2_happy=no
                              AC_MSG_WARN([*** Detected Intel C compiler v9.0 <= 20051201 on ia64])
                              AC_MSG_WARN([*** This compiler/platform combination has known problems with ptmalloc2])
                              AC_MSG_WARN([*** Disabling ptmalloc2])])])
                ;;
           esac])

    AS_IF([test "$memory_linux_ptmalloc2_happy" = yes],
          [# check for malloc.h
           AC_CHECK_HEADER([malloc.h],
                           [memory_linux_ptmalloc2_happy=yes],
                           [memory_linux_ptmalloc2_happy=no])])

    AS_IF([test "$memory_linux_ptmalloc2_happy" = yes],
          [# check for init hook symbol
           AC_CHECK_DECL([__malloc_initialize_hook],
                         [memory_linux_ptmalloc2_happy=yes],
                         [memory_linux_ptmalloc2_happy=no],
                         [AC_INCLUDES_DEFAULT
                          #include <malloc.h>])])

    #
    # See if we have sbrk prototyped
    #
    AS_IF([test "$memory_linux_ptmalloc2_happy" = yes],
          [AC_CHECK_DECLS([sbrk])])

    #
    # Figure out how we're going to call mmap/munmap for real
    #
    AS_IF([test "$memory_linux_ptmalloc2_happy" = yes],
          [memory_linux_mmap=0
           memory_linux_munmap=1

           # it's nearly impossible to call mmap from syscall(), so
           # only go this route if we can't get at munmap any other
           # way.
           AC_CHECK_HEADER([syscall.h],
               [AC_CHECK_FUNCS([syscall], [], [memory_linux_munmap=0])])

           # Always look for __munmap and __mmap
           AC_CHECK_FUNCS([__munmap], [memory_linux_mmap=1])
           AC_CHECK_FUNCS([__mmap])

           # only allow dlsym (and therefore add -ldl) if we
           # really need to
           AS_IF([test "$memory_linux_mmap" = "0"],
                 [memory_linux_LIBS_SAVE="$LIBS"
                  AC_CHECK_LIB([dl],
                               [dlsym],
                               [LIBS="$LIBS -ldl"
                                memory_linux_LIBS="-ldl"
                                memory_linux_mmap=1])
                  AC_CHECK_FUNCS([dlsym])
                  LIBS="$memory_linux_LIBS_SAVE"])

           AS_IF([test "$memory_linux_mmap" = "0" && test "$memory_linux_munmap" = "0"],
                 [memory_linux_ptmalloc2_happy=no])])

    # If all is good, save the extra libs for the wrapper
    AS_IF([test "$memory_linux_ptmalloc2_happy" = yes],
          [value=1],
          [value=0])
    AC_DEFINE_UNQUOTED([MEMORY_LINUX_PTMALLOC2], [$value],
                       [Whether ptmalloc2 is supported on this system or not])
    AM_CONDITIONAL([MEMORY_LINUX_PTMALLOC2],
                   [test "$memory_linux_ptmalloc2_happy" = yes])

    ######################################################################
    # ummunotify
    ######################################################################

    # Check for the relevant header
    AS_IF([test "$memory_linux_ummu_happy" = yes],
          [# check for linux/ummunotify.h
           AC_CHECK_HEADER([linux/ummunotify.h],
                           [memory_linux_ummu_happy=yes],
                           [memory_linux_ummu_happy=no])])

    # <stropts.h> has the Linux declaration for ioctl
    AC_CHECK_HEADERS([stropts.h])

    # If all is good, set the header file that we want the rest of the
    # code base to use
    AS_IF([test "$memory_linux_ummu_happy" = yes],
          [memory_base_include="linux/public.h"
           value=1],
          [value=0])
    AC_DEFINE_UNQUOTED([MEMORY_LINUX_UMMUNOTIFY], [$value],
                       [Whether ummunotify is supported on this system or not])
    AM_CONDITIONAL([MEMORY_LINUX_UMMUNOTIFY],
                   [test "$memory_linux_ummu_happy" = yes])

    ######################################################################
    # post processing
    ######################################################################

    AS_IF([test "$memory_malloc_hooks_requested" = 1 && \
           test "$memory_linux_ptmalloc2_happy" = no && \
           test "$memory_linux_ummu_happy" = no],
          [AC_MSG_ERROR([linux memory management requested but neither ptmalloc2 nor ummunotify are available.  Aborting.])])
    AC_SUBST([memory_linux_LIBS])

    AS_IF([test "$memory_linux_ptmalloc2_happy" = yes || \
           test "$memory_linux_ummu_happy" = yes],
          [memory_base_found=1
           $1],
          [memory_base_found=0
           memory_base_include=
           $2])

    OPAL_VAR_SCOPE_POP
])
