#
# Copyright (c) 2004-2005 The Trustees of Indiana University.
#                         All rights reserved.
# Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
#                         All rights reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

EXTRA_DIST = win_makefile op/base/static-components.h op/op.h

SUBDIRS = \
        allocator \
	base \
	coll \
	common \
	errmgr \
	gpr \
	io \
	iof \
	gpr \
	mpool \
	ns \
	oob \
	pls \
	pml \
	ptl \
	rml \
	ras \
	rds \
	rmaps \
	rmgr \
	soh \
	topo

# Source code files

headers = mca.h

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/openmpi/mca
ompi_HEADERS = $(headers)
else
ompidir = $(includedir)
endif
