# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

AM_CPPFLAGS = -DLAM_PROFILING_DEFINES=1

# We must traverse into the profile directory to install the proper
# header files.  So only only expect to find the C profiling library
# if we're compiling profiling separately.

if COMPILE_PROFILING_SEPARATELY
lib = libmpi_c_profile.la
else
lib =
endif

noinst_LTLIBRARIES = $(lib)

headers = defines.h

nodist_libmpi_c_profile_la_SOURCES = \
        palloc_mem.c \
	pcomm_get_name.c \
	pcomm_set_name.c \
	pinit.c \
	pfinalize.c \
        pfree_mem.c

# Sym link in the sources from the real MPI directory

$(nodist_libmpi_c_profile_la_SOURCES):
	if test ! -r $@ ; then \
		pname=`echo $@ | cut -b '2-'` ; \
		ln -s $(top_srcdir)/src/mpi/interface/c/$$pname $@ ; \
	fi

# Conditionally install the header files

if WANT_INSTALL_HEADERS
lamdir = $(includedir)/lam/mpi/c/profile
lam_HEADERS = $(headers)
else
lamdir = $(includedir)
endif

# These files were created by targets above

MAINTAINERCLEANFILES = $(nodist_libmpi_c_profile_la_SOURCES)

# Don't want these targets in here

tags-recursive:
tags:
TAGS:
GTAGS:
ID:
