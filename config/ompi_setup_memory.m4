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

AC_DEFUN([OMPI_PTMALLOC_SETUP],[
#
# Call the top-level OMPI threads setup stuff
#
OLD_CPPFLAGS="$CPPFLAGS"
OLD_LDFLAGS="$LDFLAGS"
OLD_LIBS="$LIBS"

CPPFLAGS="$CPPFLAGS $THREADCPPFLAGS"
LDFLAGS="$LDFLAGS $THREADLDFLAGS"
LIBS="$LIBS $THREADLIBS"

if test "`echo $host | grep apple-darwin`" != "" ; then
    AC_MSG_WARN([*** Using ptmalloc with OS X will result in failure.])
    AC_MSG_ERROR([*** Aborting to save you the effort])
fi

#
# See if we have sbrk prototyped
#
AC_CHECK_DECL([sbrk], [have_decl_sbrk=1], [have_decl_sbrk=0])
AC_DEFINE_UNQUOTED(OMPI_HAVE_DECL_SBRK, $have_decl_sbrk,
	[Whether we have a declaration for sbrk() or not])

CPPFLAGS="$OLD_CPPFLAGS"
LDFLAGS="$OLD_LDFLAGS"
LIBS="$OLD_LIBS"
])dnl


AC_DEFUN([OMPI_DARWIN_MALLOC_SETUP],[
case "$host" in
  *apple-darwin*)
    WRAPPER_EXTRA_LDFLAGS="-Wl,-u -Wl,_ompi_darwin_malloc_linker_hack -Wl,-multiply_defined,suppress -Wl,-force_flat_namespace -Wl,-flat_namespace $WRAPPER_EXTRA_LDFLAGS"
    LDFLAGS="-Wl,-multiply_defined,suppress $LDFLAGS"
    ;;
  *)
    AC_MSG_ERROR([Trying to use Darwin malloc while not on a Darwin system.])
    ;;
esac
])dnl

AC_DEFUN([OMPI_MEMORY_SETUP],[

AC_ARG_WITH(memory-manager,
    AC_HELP_STRING([--with-memory-manager=TYPE],
		   [Use TYPE for intercepting memory management calls to control memory pinning (TYPE is one of ptmalloc2,none)]),
    [WANT_MEMORY="$withval"], [WANT_MEMORY="none"])

AC_MSG_CHECKING([for memory management type])
if test "$WANT_MEMORY" = "darwin" ; then
    AC_MSG_RESULT([Darwin / Mac OS X])
    OMPI_DARWIN_MALLOC_SETUP
    OMPI_WANT_DARWIN7MALLOC=1
    OMPI_WANT_PTMALLOC2=0
    AC_MSG_ERROR([Darwin memory manager not currently supported])
elif test "$WANT_MEMORY" = "ptmalloc2" ; then
    AC_MSG_RESULT([ptmalloc2])
    OMPI_PTMALLOC_SETUP
    OMPI_WANT_DARWIN7MALLOC=0
    OMPI_WANT_PTMALLOC2=1
else
    AC_MSG_RESULT([none])
    OMPI_WANT_DARWIN7MALLOC=0
    OMPI_WANT_PTMALLOC2=0
fi

AC_DEFINE_UNQUOTED([OMPI_WANT_PTMALLOC2], $OMPI_WANT_PTMALLOC2,
    [Do we want ptmalloc2 support])
AM_CONDITIONAL(OMPI_WANT_PTMALLOC2, test "$OMPI_WANT_PTMALLOC2" = "1")

AC_DEFINE_UNQUOTED([OMPI_WANT_DARWIN7MALLOC], $OMPI_WANT_DARWIN7MALLOC,
    [Do we want darwin7malloc support])
AM_CONDITIONAL(OMPI_WANT_DARWIN7MALLOC, test "$OMPI_WANT_DARWIN7MALLOC" = "1")

])dnl
