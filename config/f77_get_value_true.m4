dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2004-2005 The Trustees of Indiana University.
dnl                         All rights reserved.
dnl Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
dnl                         All rights reserved.
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

AC_DEFUN([OMPI_F77_GET_VALUE_TRUE],[
# Determine the value of .TRUE. of this FORTRAN compiler.

AC_MSG_CHECKING([FORTRAN value for .TRUE. logical type])

if test "$1" = "none" -o "$OMPI_WANT_F77_BINDINGS" = "0"; then
    OMPI_FORTRAN_VALUE_TRUE=0
    AC_MSG_RESULT([no Fortran 77 bindings -- skipped])
else

if test "x$ompi_ac_doubleunder" = xy || test "x$ompi_ac_singleunder" = xy; then
    ompi_ac_print_logical_fn=print_
else
    if test "x$ompi_ac_nounder" = xy; then
        ompi_ac_print_logical_fn=print
    else
        if test "x$ompi_ac_caps" = xy; then
            ompi_ac_print_logical_fn=PRINT
        else
            AC_MSG_WARN([*** FORTRAN external naming convention undefined])
            AC_MSG_ERROR([*** Cannot continue.])
        fi
    fi
fi

#
# C module
# We really need the confdefs.h Header file for
# the ompi_fortran_logical_t definition
#
if test \! -f confdefs.h ; then
    AC_MSG_WARN([*** Problem running configure test!])
    AC_MSG_WARN([*** Cannot find confdefs.h file for config test])
    AC_MSG_WARN([*** See config.log for details.])
    AC_MSG_ERROR([*** Cannot continue.])
fi

cat > conftest.c <<EOF
#include <stdio.h>
#include <stdlib.h>
#include "confdefs.h"

#ifdef __cplusplus
extern "C" {
#endif
void $ompi_ac_print_logical_fn(ompi_fortran_logical_t * logical);

void $ompi_ac_print_logical_fn(ompi_fortran_logical_t * logical)
{
    int result = 0;
    FILE *f=fopen("conftestval", "w");
    if (!f) exit(1);

    if( 1 == sizeof(ompi_fortran_logical_t) ) {
        fprintf(f, "%d\n", (int)*logical);
    } else if( 2 == sizeof(ompi_fortran_logical_t) ) {
        fprintf(f, "%d\n", (int)*logical);
    } else if( 4 == sizeof(ompi_fortran_logical_t) ) {
        fprintf(f, "%d\n", (int)*logical);
    } else {
        exit(1);
    }
}

#ifdef __cplusplus
}
#endif
EOF

  cat > conftestf.f <<EOF
      program main
      logical value
      value=.TRUE.
      CALL PRINT(value)
      end
EOF

  #
  # Try the compilation and run.
  #
  OMPI_LOG_COMMAND( [$CC $CFLAGS -I. -c conftest.c],
      OMPI_LOG_COMMAND([$F77 $FFLAGS -o conftest conftest.o conftestf.f $LDFLAGS $LIBS],
          OMPI_LOG_COMMAND([./conftest], [HAPPY=1], [HAPPY=0]),
          [HAPPY=0]),
      [HAPPY=0])

  if test "$HAPPY" = "1" -a -f conftestval; then
      # get rid of leading spaces for eval assignment
      ompi_ac_value_true=`sed 's/  *//' conftestval`

      OMPI_FORTRAN_VALUE_TRUE=$ompi_ac_value_true
      AC_MSG_RESULT([$ompi_ac_value_true])
  else
      AC_MSG_RESULT([unknown])

      OMPI_LOG_MSG([here is the C program:], 1)
      OMPI_LOG_FILE([conftest.c])
      OMPI_LOG_MSG([here is the fortran program:], 1)
      OMPI_LOG_FILE([conftestf.f])

      AC_MSG_WARN([*** Problem running configure test!])
      AC_MSG_WARN([*** See config.log for details.])
      AC_MSG_ERROR([*** Cannot continue.])
  fi

  unset HAPPY
  /bin/rm -f conftest*
fi
AC_DEFINE_UNQUOTED(OMPI_FORTRAN_VALUE_TRUE,
  $OMPI_FORTRAN_VALUE_TRUE,
  [Fortran value for LOGICAL .TRUE. value])
])dnl
