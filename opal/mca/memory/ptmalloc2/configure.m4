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
        AC_HELP_STRING([--with-memory-manager=TYPE],
                       [Use TYPE for intercepting memory management
                        calls to control memory pinning.]),
        [memory_ptmalloc2_WANT_MEMORY="$withval"], 
        [memory_ptmalloc2_WANT_MEMORY="none"])

    AS_IF([test "$memory_ptmalloc2_WANT_MEMORY" = "ptmalloc2"],
        [if test "`echo $host | grep apple-darwin`" != "" ; then
            AC_MSG_WARN([*** Using ptmalloc with OS X will result in failure.])
            AC_MSG_ERROR([*** Aborting to save you the effort])
        fi

        #
        # See if we have sbrk prototyped
        #
        AC_CHECK_DECL([sbrk], [have_decl_sbrk=1], [have_decl_sbrk=0])
        AC_DEFINE_UNQUOTED(OMPI_HAVE_DECL_SBRK, $have_decl_sbrk,
                [Whether we have a declaration for sbrk() or not])

        $1], [$2])
])
