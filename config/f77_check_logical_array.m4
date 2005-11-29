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

AC_DEFUN([OMPI_F77_CHECK_LOGICAL_ARRAY],[
AC_MSG_CHECKING([for correct handling of FORTRAN logical arrays])

if test "$1" = "none" -o "$OMPI_WANT_F77_BINDINGS" = "0"; then
    AC_MSG_RESULT([no Fortran 77 bindings -- skipped])
else

  if test "x$ompi_ac_doubleunder" = xy || test "x$ompi_ac_singleunder" = xy; then
      ompi_ac_check_logical_fn=check_
  else
      if test "x$ompi_ac_nounder" = xy; then
          ompi_ac_check_logical_fn=check
      else 
          if test "x$ompi_ac_caps" = xy; then
              ompi_ac_check_logical_fn=CHECK
          else
              AC_MSG_WARN([*** FORTRAN external naming convention undefined])
              AC_MSG_ERROR([*** Cannot continue.])
          fi
      fi
  fi

  #
  # Cannot use standard AC_TRY macros because we need two different .o
  # files here, and link them together
  #

  #
  # Fortran module
  #
  cat > conftestf.f <<EOF
        program check_logical_array
        external CHECK
        logical l(2)
        l(1)=.FALSE.
        l(2)=.TRUE.
        CALL CHECK(l)
        end
EOF

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
void $ompi_ac_check_logical_fn(ompi_fortran_logical_t * logical);

void $ompi_ac_check_logical_fn(ompi_fortran_logical_t * logical)
{
    int result = 0;
    FILE *f=fopen("conftestval", "w");
    if (!f) exit(1);

    if (logical[[0]] == 0 &&
        logical[[1]] == $ompi_ac_value_true)
      result = 1;
    fprintf(f, "%d\n", result);
}
#ifdef __cplusplus
}
#endif
EOF

  #
  # Try the compilation and run.  Can't use AC_TRY_RUN because it's two
  # module files.
  #

  OMPI_LOG_COMMAND([$CC $CFLAGS -I. -c conftest.c],
      OMPI_LOG_COMMAND([$F77 $FFLAGS conftestf.f conftest.o -o conftest $LDFLAGS $LIBS],
          OMPI_LOG_COMMAND([./conftest],[HAPPY=1],[HAPPY=0]),
          [HAPPY=0]),
      [HAPPY=0])

  if test "$HAPPY" = "1" -a -f conftestval; then
      ompi_result=`cat conftestval`
      if test "$ompi_result" = "1"; then
          AC_MSG_RESULT([yes])
      else
          AC_MSG_RESULT([no])
          AC_MSG_WARN([*** Problem running configure test!])
          AC_MSG_WARN([*** See config.log for details.])
          AC_MSG_ERROR([*** Cannot continue.])
      fi
  else
      AC_MSG_RESULT([unknown])

      OMPI_LOG_MSG([here is the C program:], 1)
      OMPI_LOG_FILE([conftest.c])
      if test -f conftest.h; then
          OMPI_LOG_MSG([here is contest.h:], 1)
          OMPI_LOG_FILE([conftest.h])
      fi
      OMPI_LOG_MSG([here is the fortran program:], 1)
      OMPI_LOG_FILE([conftestf.f])

      AC_MSG_WARN([*** Problem running configure test!])
      AC_MSG_WARN([*** See config.log for details.])
      AC_MSG_ERROR([*** Cannot continue.])
  fi

  unset HAPPY ompi_result
  /bin/rm -f conftest*
fi
])dnl