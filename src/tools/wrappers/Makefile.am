# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

# This is ugly, but we need it so that people can change things at
# "make" time (e.g., "make sysconfdir=/foo/bar all") as documented in
# autoconf.

AM_CPPFLAGS		= \
			-DLAM_PREFIX="\"$(prefix)\"" \
			-DLAM_INCDIR="\"$(includedir)\"" \
			-DLAM_LIBDIR="\"$(libdir)\""

# Stay tuned...
#bin_PROGRAMS		= mpicc mpic++ mpif77 mpif90
