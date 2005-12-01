#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
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



dist_noinst_HEADERS = \
	orte_config.h \
	ompi_config_bottom.h \
        ompi_stdint.h

nodist_noinst_HEADERS = \
	ompi_config.h		

nodist_include_HEADERS = \
        mpi.h \
	mpif.h

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(includedir)
dist_ompi_HEADERS = $(dist_noinst_HEADERS)
nodist_ompi_HEADERS = $(nodist_noinst_HEADERS)
else
ompidir = $(includedir)
endif
