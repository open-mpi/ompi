#
# Copyright (c) 2004-2005 The Trustees of Indiana University.
#                         All rights reserved.
# Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
#                         All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

SUBDIRS = base $(MCA_allocator_STATIC_SUBDIRS)
DIST_SUBDIRS = base $(MCA_allocator_ALL_SUBDIRS)

# Source code files

headers = allocator.h

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/openmpi/mca/allocator
ompi_HEADERS = $(headers)
else
ompidir = $(includedir)
endif
