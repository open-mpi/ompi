# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

SUBDIRS	= ctnetwork lfc mem os threads util

# If the --enable-single-library flag was given to configure, then the
# user wants to merge liblam and libmpi into a single big, honkin'
# libmpi.  So the libmpi in this directory should be a convenience
# (noinst) library.  Otherwise, it should be installed into lib.

# List the input convenience libraries here

sources 		= \
			ctnetwork/libctnetwork.la \
			lfc/liblfc.la \
			mem/libmem.la \
			threads/libthreads.la \
			util/libutil.la 

# List the names of the output libraries here

convenience_lib_name	= liblam_convenience.la
install_lib_name	= liblam.la

# Setup for deciding which library form we'll build

if WANT_SINGLE_MPI_LIBRARY
install_lib		=
convenience_lib		= $(convenience_lib_name)
else
install_lib		= $(install_lib_name)
convenience_lib		=
endif

lib_LTLIBRARIES		= $(install_lib)
noinst_LTLIBRARIES	= $(convenience_lib)

liblam_la_SOURCES	=
liblam_la_LIBADD	= $(sources)

liblam_convenience_la_SOURCES =
liblam_convenience_la_LIBADD = $(sources)

