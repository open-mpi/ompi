# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

AM_CPPFLAGS = -DLAM_PROFILING_DEFINES=1

# We must traverse into this directory to install the proper header
# files.  So only build the f77 profiling bindings only if we're
# building the f77 bindings *and* we're compiling profiling
# separately.

if BUILD_MPI_F77
if COMPILE_PROFILING_SEPARATELY
f77_lib = libmpi_f77_profile.la
else
f77_lib =
endif
else
f77_lib =
endif

noinst_LTLIBRARIES = $(f77_lib)

headers = defines.h

nodist_libmpi_f77_profile_la_SOURCES = \
        palloc_mem_f.c \
	pcomm_get_name_f.c \
	pcomm_set_name_f.c \
	pinit_f.c \
	pfinalize_f.c \
        pfree_mem_f.c

# Sym link in the sources from the real MPI directory

$(nodist_libmpi_f77_profile_la_SOURCES):
	if test ! -r $@ ; then \
		pname=`echo $@ | cut -b '2-'` ; \
		ln -s $(top_srcdir)/src/mpi/interface/f77/$$pname $@ ; \
	fi

# Conditionally install the header files

if WANT_INSTALL_HEADERS
lamdir = $(includedir)/lam/mpi/f77/profile
lam_HEADERS = $(headers)
else
lamdir = $(includedir)
endif

# These files were created by targets above

MAINTAINERCLEANFILES = $(nodist_libmpi_f77_profile_la_SOURCES)

# Don't want these targets in here

tags-recursive:
tags:
TAGS:
GTAGS:
ID:
