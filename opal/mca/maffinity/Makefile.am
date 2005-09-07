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



SUBDIRS = base $(MCA_maffinity_STATIC_SUBDIRS)
DIST_SUBDIRS = base $(MCA_maffinity_ALL_SUBDIRS)

# Source code files

headers = maffinity.h maffinity_types.h

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/openmpi/ompi/mca/maffinity
ompi_HEADERS = $(headers)
else
ompidir = $(includedir)
endif
