# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

AM_CPPFLAGS		= -DLAM_PROFILING_DEFINES=1

noinst_LTLIBRARIES	= libmpi_c_profile.la

noinst_HEADERS		= defines.h

nodist_libmpi_c_profile_la_SOURCES= \
			pcomm_get_name.c \
			pcomm_set_name.c \
			pinit.c \
			pfinalize.c

# Sym link in the sources from the real MPI directory

$(nodist_libmpi_c_profile_la_SOURCES):
	if test ! -r $@ ; then \
		pname=`echo $@ | cut -b '2-'` ; \
		ln -s $(top_srcdir)/src/mpi/c/$$pname $@ ; \
	fi

# These files were created by targets above

MAINTAINERCLEANFILES = $(nodist_libmpi_c_profile_la_SOURCES)

# Don't want these targets in here

tags-recursive:
tags:
TAGS:
GTAGS:
ID:
