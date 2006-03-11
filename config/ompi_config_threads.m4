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

AC_DEFUN([OMPI_CONFIG_THREADS],[
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
AH_TEMPLATE([OMPI_HAVE_SOLARIS_THREADS], 
    [Do we have native Solaris threads])
AH_TEMPLATE([OMPI_HAVE_POSIX_THREADS], 
    [Do we have POSIX threads])

#
# Check for thread types - add your type here...
#
OMPI_CONFIG_POSIX_THREADS(HAVE_POSIX_THREADS=1, HAVE_POSIX_THREADS=0)
AC_MSG_CHECKING([for working POSIX threads package])
if test "$HAVE_POSIX_THREADS" = "1" ; then
  AC_MSG_RESULT([yes])
else
  AC_MSG_RESULT([no])
fi
export HAVE_POSIX_THREADS

OMPI_CONFIG_SOLARIS_THREADS(HAVE_SOLARIS_THREADS=1, HAVE_SOLARIS_THREADS=0)
AC_MSG_CHECKING([for working Solaris threads package])
if test "$HAVE_SOLARIS_THREADS" = "1" ; then
  AC_MSG_RESULT([yes])
else
  AC_MSG_RESULT([no])
fi
export HAVE_SOLARIS_THREADS

#
# Ask what threading we want (allow solaris / posix right now)
#
AC_MSG_CHECKING([for type of thread support])
AC_ARG_WITH(threads, 
  	AC_HELP_STRING([--with-threads],
		       [Set thread type (solaris / posix)]),
	[THREAD_TYPE=$withval])

if test "$THREAD_TYPE" = "solaris"; then

    if test "$HAVE_SOLARIS_THREADS" = "0"; then
	AC_MSG_WARN(["*** You have chosen Solaris threads, which are not"])
	AC_MSG_WARN(["*** available on your system "])
	AC_MSG_ERROR(["*** Can not continue"])
    fi
elif test "$THREAD_TYPE" = "posix"; then

    if test "$HAVE_POSIX_THREADS" = "0"; then
	AC_MSG_WARN(["*** You have chosen POSIX threads, which are not"])
	AC_MSG_WARN(["*** available on your system "])
	AC_MSG_ERROR(["*** Can not continue"])
    fi
elif test "$THREAD_TYPE" = "no"; then
    THREAD_TYPE="none"
elif test -z "$THREAD_TYPE" -o "$THREAD_TYPE" = "yes"; then

    # Actual logic here - properly set THREAD_TYPE - we go for system
    # optimized where ever possible
    case "$host" in
	*solaris*)
	    if test "$HAVE_SOLARIS_THREADS" = "1"; then
		THREAD_TYPE="solaris"
	    elif test "$HAVE_POSIX_THREADS" = "1"; then
		THREAD_TYPE="posix"
	    else
		THEAD_TYPE="none found"
	    fi
	    ;;
	*)
	    if test "$HAVE_POSIX_THREADS" = "1"; then
		THREAD_TYPE="posix"
	    else
		THREAD_TYPE="none found"
	    fi
	    ;;
    esac
else

    AC_MSG_WARN(["*** You have specified a thread type that I do not"])
    AC_MSG_WARN(["*** understand.  Valid options are posix and solaris"])
    AC_MSG_ERROR(["*** Can not continue."])
fi
AC_MSG_RESULT($THREAD_TYPE)


#
# Ok, now run the configuration for that thread package.
#
# Blah - this should be made better, but I don't know how...
#
if test "$THREAD_TYPE" = "solaris"; then
    AC_DEFINE(OMPI_HAVE_SOLARIS_THREADS, 1)
    AC_DEFINE(OMPI_HAVE_POSIX_THREADS, 0)
    AC_DEFINE(OMPI_THREADS_HAVE_DIFFERENT_PIDS, 0)

    THREAD_CFLAGS="$STHREAD_CFLAGS"
    THREAD_FFLAGS="$STHREAD_FFLAGS"
    THREAD_CXXFLAGS="$STHREAD_CXXFLAGS"
    THREAD_CPPFLAGS="$STHREAD_CPPFLAGS"
    THREAD_CXXCPPFLAGS="$STHREAD_CXXCPPFLAGS"
    THREAD_LDFLAGS="$STHREAD_LDFLAGS"
    THREAD_LIBS="$STHREAD_LIBS"
elif test "$THREAD_TYPE" = "posix"; then
    AC_DEFINE(OMPI_HAVE_SOLARIS_THREADS, 0)
    AC_DEFINE(OMPI_HAVE_POSIX_THREADS, 1)

    THREAD_CFLAGS="$PTHREAD_CFLAGS"
    THREAD_FFLAGS="$PTHREAD_FFLAGS"
    THREAD_CXXFLAGS="$PTHREAD_CXXFLAGS"
    THREAD_CPPFLAGS="$PTHREAD_CPPFLAGS"
    THREAD_CXXCPPFLAGS="$PTHREAD_CXXCPPFLAGS"
    THREAD_LDFLAGS="$PTHREAD_LDFLAGS"
    THREAD_LIBS="$PTHREAD_LIBS"

    OMPI_CHECK_PTHREAD_PIDS
else
    AC_DEFINE(OMPI_HAVE_SOLARIS_THREADS, 0)
    AC_DEFINE(OMPI_HAVE_POSIX_THREADS, 0)
    AC_DEFINE(OMPI_THREADS_HAVE_DIFFERENT_PIDS, 0)

    TRHEAD_CFLAGS=
    THREAD_FFLAGS=
    THREAD_CXXFLAGS=
    THREAD_CPPFLAGS=
    THREAD_CXXCPPFLAGS=
    THREAD_LDFLAGS=
    THREAD_LIBS=
    if test "$THREAD_TYPE" != "none" ; then
        cat <<EOF

************************************************************************

Open MPI was unable to find threading support on your system.  In the
near future, the OMPI development team is considering requiring
threading support for proper OMPI execution.  This is in part because
we are not aware of any users that do not have thread support - so we
need you to e-mail us at ompi@ompi-mpi.org and let us know about this
problem.

************************************************************************

EOF
    fi
fi

AM_CONDITIONAL(OMPI_HAVE_POSIX_THREADS, test "$THREAD_TYPE" = "posix")
AM_CONDITIONAL(OMPI_HAVE_SOLARIS_THREADS, test "$THREAD_TYPE" = "solaris")

#
# Now configure the whole MPI and progress thread gorp
#
AC_MSG_CHECKING([if want MPI thread support])
AC_ARG_ENABLE([mpi-threads],
    AC_HELP_STRING([--enable-mpi-threads],
        [Enable threads for MPI applications (default: disabled)]),
    [enable_mpi_threads="$enableval"])

if test "$enable_mpi_threads" = "" ; then 
dnl    # no argument given either way.  Default to whether
dnl    # we have threads or not
dnl    if test "$THREAD_TYPE" != "none" ; then
dnl        OMPI_ENABLE_MPI_THREADS=1
dnl        enable_mpi_threads="yes"
dnl    else
dnl        OMPI_ENABLE_MPI_THREADS=0
dnl        enable_mpi_threads="no"
dnl    fi
    # no argument - default to no
    OMPI_ENABLE_MPI_THREADS=0
    enable_mpi_threads="no"
elif test "$enable_mpi_threads" = "no" ; then
    OMPI_ENABLE_MPI_THREADS=0
else
    # they want MPI threads.  Make sure we have threads
    if test "$THREAD_TYPE" != "none" ; then
        OMPI_ENABLE_MPI_THREADS=1
        enable_mpi_threads="yes"
    else
        AC_MSG_ERROR([User requested MPI threads, but no threading model supported])
    fi
fi
AC_DEFINE_UNQUOTED([OMPI_ENABLE_MPI_THREADS], [$OMPI_ENABLE_MPI_THREADS],
                   [Whether we should enable support for multiple user threads])
AC_MSG_RESULT([$enable_mpi_threads])


AC_MSG_CHECKING([if want asynchronous progress thread support])
AC_ARG_ENABLE([progress-threads],
    AC_HELP_STRING([--enable-progress-threads],
        [Enable threads asynchronous communication progress (default: disabled)]),
    [enable_progress_threads="$enableval"])

if test "$enable_progress_threads" = "" ; then
    # no argument given either way.  Default to no.
    OMPI_ENABLE_PROGRESS_THREADS=0
    enable_progress_threads="no"
elif test "$enable_progress_threads" = "no" ; then
    OMPI_ENABLE_PROGRESS_THREADS=0
    enable_progress_threads="no"
else
    # they want threaded progress
    if test "$THREAD_TYPE" != "none" ; then
        OMPI_ENABLE_PROGRESS_THREADS=1
        enable_progress_threads="yes"
    else
        AC_MSG_ERROR([User requested progress threads, but no threading model supported])
    fi
fi
AC_DEFINE_UNQUOTED([OMPI_ENABLE_PROGRESS_THREADS], [$OMPI_ENABLE_PROGRESS_THREADS],
                   [Whether we should use progress threads rather than polling])
AC_MSG_RESULT([$enable_progress_threads])
])dnl

