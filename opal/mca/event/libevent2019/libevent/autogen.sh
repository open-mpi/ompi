#!/bin/sh
###################################
# Open MPI change: add -I for our m4 directory so that we can
# find OPAL_CHECK_VISIBILITY
###################################
OMPI="-I .."
if [ -x "`which autoreconf 2>/dev/null`" ] ; then
   ###################################
   # Open MPI change: add -I for our m4 directory
   ###################################
   exec autoreconf -ivf $OMPI
fi

LIBTOOLIZE=libtoolize
SYSNAME=`uname`
if [ "x$SYSNAME" = "xDarwin" ] ; then
  LIBTOOLIZE=glibtoolize
fi
###################################
# Open MPI change: add -I for our m4 directory
###################################
aclocal -I m4 $OMPI && \
	autoheader && \
	$LIBTOOLIZE && \
	autoconf $OMPI && \
	automake --add-missing --force-missing --copy
