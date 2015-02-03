dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
dnl                         University Research and Technology
dnl                         Corporation.  All rights reserved.
dnl Copyright (c) 2004-2005 The University of Tennessee and The University
dnl                         of Tennessee Research Foundation.  All rights
dnl                         reserved.
dnl Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
dnl                         University of Stuttgart.  All rights reserved.
dnl Copyright (c) 2004-2005 The Regents of the University of California.
dnl                         All rights reserved.
dnl Copyright (c) 2010-2012 Cisco Systems, Inc.  All rights reserved.
dnl Copyright (c) 2015      Research Organization for Information Science
dnl                         and Technology (RIST). All rights reserved.
dnl $COPYRIGHT$
dnl 
dnl Additional copyrights may follow
dnl 
dnl $HEADER$
dnl

# OMPI_FORTRAN_GET_KIND_VALUE(kind, decimal range, variable to set)
# -----------------------------------------------------------------
AC_DEFUN([OMPI_FORTRAN_GET_KIND_VALUE],[
    # Use of m4_translit suggested by Eric Blake:
    # http://lists.gnu.org/archive/html/bug-autoconf/2010-10/msg00016.html
    AS_VAR_PUSHDEF([kind_value_var],
       m4_translit([[ompi_cv_fortran_kind_value_$1]], [*], [p]))

    rm -f conftest.out    
    AC_CACHE_CHECK([KIND value of Fortran $1], kind_value_var,
        [if test $OMPI_WANT_FORTRAN_BINDINGS -eq 0 || test $ompi_fortran_happy -eq 0; then
             value=skipped
         else
             AC_LANG_PUSH([Fortran])
             value=
             AC_RUN_IFELSE(AC_LANG_PROGRAM(, [[
        use, intrinsic :: ISO_C_BINDING
        open(unit = 7, file = "conftest.out")
        write(7, *) $1
        close(7)
]]), [value=`cat conftest.out | awk '{print [$]1}'`], [value=no], [value=cross])
             rm -f conftest.out
         fi

         # If the compiler is ancient enough to not support the
         # ISO_C_BINDING stuff, then we have to fall back to older
         # tests.  Yuck.
         AS_IF([test "$value" = "no"],
               [AC_MSG_RESULT([no ISO_C_BINDING -- fallback])
                _OMPI_FORTRAN_SELECTED_INT_KIND($2, value)])

         AS_IF([test "$value" = "no"],
               [AC_MSG_WARN([Could not determine KIND value of $1])
                AC_MSG_WARN([See config.log for more details])
                AC_MSG_ERROR([Cannot continue])])

         AS_IF([test "$value" = "cross"],
               [AC_MSG_ERROR([Can not determine KIND value of $1 when cross-compiling])])

         AS_VAR_SET(kind_value_var, [$value])
         AC_LANG_POP([Fortran])
         unset value
        ])

    AS_VAR_COPY([$3], [kind_value_var])
    AS_VAR_POPDEF([kind_value_var])
])dnl

# _OMPI_FORTRAN_SELECTED_INT_KIND(decimal range, variable to set)
# -----------------------------------------------------------------
AC_DEFUN([_OMPI_FORTRAN_SELECTED_INT_KIND],[
    AS_VAR_PUSHDEF([sel_int_kind_var], [ompi_cv_fortran_int_kind_$1])
    AC_CACHE_CHECK([Fortran value of selected_int_kind($1)], sel_int_kind_var,
        [outval=no

         AS_IF([test $OMPI_WANT_FORTRAN_BINDINGS -eq 1 && test $ompi_fortran_happy -eq 1],
               [rm -f conftest.out
                AC_LANG_PUSH([Fortran])
                AC_RUN_IFELSE(AC_LANG_PROGRAM(, [[
        open(8, file="conftest.out")
        write(8, fmt="(I5)") selected_int_kind($1)
        close(8)
]]), [outval=`cat conftest.out | awk '{print [$]1}'`], [outval=no], [outval=cross])
                rm -f conftest.out
                AC_LANG_POP([Fortran])
               ])

         AS_VAR_SET(sel_int_kind_var, [$outval])
         unset outval
        ])

    # All analysis of $value is done in the upper-level / calling
    # macro

    AS_VAR_COPY([$2], [sel_int_kind_var])
    AS_VAR_POPDEF([sel_int_kind_var])dnl
])
