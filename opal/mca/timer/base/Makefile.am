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



noinst_LTLIBRARIES = libmca_timer_base.la

headers = \
        base.h \
	timer_base_null.h

libmca_timer_base_la_SOURCES = \
        $(headers) \
        timer_base_close.c \
        timer_base_open.c

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/openmpi/opal/mca/timer/base
ompi_HEADERS = $(headers)
else
ompidir = $(includedir)
endif

distclean-local:
	rm -f static-components.h
