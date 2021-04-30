dnl
dnl/*D
dnl PAC_HAVE_ROMIO - make mpi.h include mpio.h if romio enabled
dnl
dnl Output Effect:
dnl expands @HAVE_ROMIO@ in mpi.h into #include "mpio.h"
dnl D*/
AC_DEFUN([PAC_HAVE_ROMIO],[
if test "$enable_romio" = "yes" ; then HAVE_ROMIO='#include "mpio.h"'; fi
AC_SUBST(HAVE_ROMIO)
])
