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



SUBDIRS = alpha amd64 ia32 ia64 mips powerpc sparc sparcv9 win32

noinst_HEADERS = architecture.h \
	atomic.h \
	atomic_impl.h \
	cache.h \
	timer.h

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/openmpi/include/sys
ompi_HEADERS = $(noinst_HEADERS)
else
ompidir = $(includedir)
endif
