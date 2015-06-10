dnl
dnl Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
dnl                         University Research and Technology
dnl                         Corporation.  All rights reserved.
dnl Copyright (c) 2004-2005 The University of Tennessee and The University
dnl                         of Tennessee Research Foundation.  All rights
dnl                         reserved.
dnl Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
dnl                         University of Stuttgart.  All rights reserved.
dnl Copyright (c) 2004-2005 The Regents of the University of California.
dnl                         All rights reserved.
dnl Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
dnl Copyright (c) 2009-2011 Oak Ridge National Labs.  All rights reserved.
dnl Copyright (c) 2014      Intel, Inc. All rights reserved
dnl $COPYRIGHT$
dnl 
dnl Additional copyrights may follow
dnl 
dnl $HEADER$
dnl

AC_DEFUN([OPAL_CONFIG_THREADS],[
#
# Arguments: none
#
# Dependencies: None
#
# Modifies:
#  none - see called tests
#
# configure threads
#

# create templates
AH_TEMPLATE([OPAL_HAVE_POSIX_THREADS], 
    [Do we have POSIX threads])

#
# Check for thread types - add your type here...
#
OPAL_CONFIG_POSIX_THREADS(HAVE_POSIX_THREADS=1, HAVE_POSIX_THREADS=0)
AC_MSG_CHECKING([for working POSIX threads package])
if test "$HAVE_POSIX_THREADS" = "1" ; then
  AC_MSG_RESULT([yes])
else
  AC_MSG_RESULT([no])
fi
export HAVE_POSIX_THREADS

#
# Ask what threading we want (allow posix right now)
#
AC_MSG_CHECKING([for type of thread support])
AC_ARG_WITH(threads, 
  	AC_HELP_STRING([--with-threads],
		       [Set thread type (posix)]),
	[THREAD_TYPE=$withval])

if test "$THREAD_TYPE" = "posix"; then

    if test "$HAVE_POSIX_THREADS" = "0"; then
	AC_MSG_WARN(["*** You have chosen POSIX threads, which are not"])
	AC_MSG_WARN(["*** available on your system "])
	AC_MSG_ERROR(["*** Can not continue"])
    fi
elif test "$THREAD_TYPE" = "no"; then
    THREAD_TYPE="none"
elif test -z "$THREAD_TYPE" -o "$THREAD_TYPE" = "yes"; then

    if test "$HAVE_POSIX_THREADS" = "1"; then
	THREAD_TYPE="posix"
    else
	THREAD_TYPE="none found"
    fi
else

    AC_MSG_WARN(["*** You have specified a thread type that I do not"])
    AC_MSG_WARN(["*** understand.  Valid options are posix"])
    AC_MSG_ERROR(["*** Can not continue."])
fi
AC_MSG_RESULT($THREAD_TYPE)


#
# Ok, now run the configuration for that thread package.
#
# Blah - this should be made better, but I don't know how...
#
if test "$THREAD_TYPE" = "posix"; then
    AC_DEFINE(OPAL_HAVE_POSIX_THREADS, 1)

    THREAD_CFLAGS="$PTHREAD_CFLAGS"
    THREAD_FCFLAGS="$PTHREAD_FCFLAGS"
    THREAD_CXXFLAGS="$PTHREAD_CXXFLAGS"
    THREAD_CPPFLAGS="$PTHREAD_CPPFLAGS"
    THREAD_CXXCPPFLAGS="$PTHREAD_CXXCPPFLAGS"
    THREAD_LDFLAGS="$PTHREAD_LDFLAGS"
    THREAD_LIBS="$PTHREAD_LIBS"

    OPAL_CHECK_PTHREAD_PIDS
else
    AC_DEFINE(OPAL_HAVE_POSIX_THREADS, 0)

    THREAD_CFLAGS=
    THREAD_FCFLAGS=
    THREAD_CXXFLAGS=
    THREAD_CPPFLAGS=
    THREAD_CXXCPPFLAGS=
    THREAD_LDFLAGS=
    THREAD_LIBS=
    if test "$THREAD_TYPE" != "none" ; then
        cat <<EOF

************************************************************************

Open MPI was unable to find threading support on your system.  The
OMPI development team is considering requiring threading support for
proper OMPI execution.  This is in part because we are not aware of
any OpenFabrics users that do not have thread support -- so we need
you to e-mail the Open MPI Users mailing list to tell us if this is a
problem for you.

************************************************************************

EOF
    fi
fi

AM_CONDITIONAL(OPAL_HAVE_POSIX_THREADS, test "$THREAD_TYPE" = "posix")

# Make sure we have threads
if test "$THREAD_TYPE" = "none" ; then
    AC_MSG_ERROR([User requested MPI threads, but no threading model supported])
fi
AC_DEFINE_UNQUOTED([OPAL_ENABLE_MULTI_THREADS], [1],
                   [Whether we should enable thread support within the OPAL code base])

])dnl

