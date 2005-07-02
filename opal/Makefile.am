#
# Copyright (c) 2004-2005 The Trustees of Indiana University.
#                         All rights reserved.
# Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
#                         All rights reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

SUBDIRS = \
        include \
        $(LIBLTDL_SUBDIR) \
	asm \
        class \
        event \
        mca \
        runtime \
        threads \
        util

# Build the main OPAL library

lib_LTLIBRARIES = libopal.la
libopal_la_SOURCES =
libopal_la_LIBADD = \
        $(LIBLTDL_LTLIB) \
        asm/libasm.la \
        class/libclass.la \
        event/libevent.la \
        mca/base/libmca_base.la \
        runtime/libruntime.la \
        threads/libthreads.la \
        util/libopalutil.la
libopal_la_DEPENDENCIES = $(libopal_la_LIBADD)

# Conditionally install the header files

# This is somewhat of a hack -- libltdl is installed by libtoolize,
# and us installing the header here is slightly less kludgey than
# hacking their Makefile.am.

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/openmpi/opal/libltdl
ompi_HEADERS = libltdl/ltdl.h
else
ompidir = $(includedir)
endif
