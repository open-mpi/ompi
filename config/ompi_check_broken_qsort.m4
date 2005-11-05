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
dnl Copyright (c) 2004-2005 The Regents of the University of California.
dnl                         All rights reserved.
dnl $COPYRIGHT$
dnl 
dnl Additional copyrights may follow
dnl 
dnl $HEADER$
dnl

AC_DEFUN([OMPI_CHECK_BROKEN_QSORT],[
    AC_MSG_CHECKING([for broken qsort])

    result=
    define_result=0
    case "$host" in
	*solaris*)
            result="yes (solaris)"
            define_result=1
        ;;
        *)
            result="no"
            define_result=0
        ;;
    esac

    AC_MSG_RESULT([$result])
    AC_DEFINE_UNQUOTED([OMPI_HAVE_BROKEN_QSORT], [$define_result],
                      [whether qsort is broken or not])
unset result define_result])dnl
