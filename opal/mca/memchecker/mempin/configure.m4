# -*- shell-script -*-
#
# Copyright (c) 2011      High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

AC_DEFUN([MCA_opal_memchecker_pin_PRIORITY], [10])

AC_DEFUN([MCA_opal_memchecker_pin_COMPILE_MODE], [
    AC_MSG_CHECKING([for MCA component $2:$3 compile mode])
    $4="static"
    AC_MSG_RESULT([$$4])
])


# MCA_memchecker_pin_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_opal_memchecker_pin_CONFIG],[
    AC_CONFIG_FILES([opal/mca/memchecker/pin/Makefile])

    OPAL_VAR_SCOPE_PUSH([opal_memchecker_pin_save_CPPFLAGS opal_memchecker_pin_happy opal_memchecker_pin_CPPFLAGS])

    AC_ARG_WITH([pin],
        [AC_HELP_STRING([--with-pin(=DIR)],
            [Root directory where the Intel Pin framework is installed])])
    OMPI_CHECK_WITHDIR([valgrind], [$with_valgrind], [include/valgrind/valgrind.h])

	# look for Intel Pin
    AS_IF([test "$with_pin" != "no"],
          [AS_IF([test ! -z "$with_pin" -a "$with_pin" != "yes"],
                 [opal_memchecker_pin_CPPFLAGS="-I$with_pin/include"
                  # We need this -I to stay in CPPFLAGS when we're done
                  CPPFLAGS="$CPPFLAGS -I$with_pin/include"
                  opal_memchecker_pin_save_CPPFLAGS=$CPPFLAGS])
           AC_CHECK_HEADERS([source/pin.H], 
                 [
				 # FixME: if the header is found, then just compile MemPin tool
				 ,
                 ],
                 [AC_MSG_WARN([pin.H not found])
                  AC_MSG_WARN([Compile this component without the MemPin tool])])])
    CPPFLAGS="$opal_memchecker_pin_save_CPPFLAGS"

	# if Intel Pin framework is not found, we can still build the component.
    [$1]

    AC_SUBST([opal_memchecker_pin_CPPFLAGS])

    OPAL_VAR_SCOPE_POP
])dnl
