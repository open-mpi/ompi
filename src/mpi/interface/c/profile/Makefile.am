# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

# JMS: Need more stuff here:
# - Do we want a separate compile for profiling (affects SUBDIRS and LIBADD)

noinst_LTLIBRARIES	= libmpi_c_profile.la

nodist_libmpi_c_profile_la_SOURCES= \
			pc_get_name.c \
			pc_set_name.c \
			pd_get_name.c \
			pd_set_name.c

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
