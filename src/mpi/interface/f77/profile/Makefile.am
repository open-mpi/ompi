# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

AM_CPPFLAGS		= -DLAM_PROFILING_DEFINES=1

noinst_LTLIBRARIES	= libmpi_f77_profile.la

noinst_HEADERS		= defines.h

nodist_libmpi_f77_profile_la_SOURCES= \
			pcomm_get_name_f.c \
			pcomm_set_name_f.c \
			pinit_f.c \
			pfinalize_f.c

# Sym link in the sources from the real MPI directory

$(nodist_libmpi_f77_profile_la_SOURCES):
	if test ! -r $@ ; then \
		pname=`echo $@ | cut -b '2-'` ; \
		ln -s $(top_srcdir)/src/mpi/f77/$$pname $@ ; \
	fi

# These files were created by targets above

MAINTAINERCLEANFILES = $(nodist_libmpi_f77_profile_la_SOURCES)

# Don't want these targets in here

tags-recursive:
tags:
TAGS:
GTAGS:
ID:
