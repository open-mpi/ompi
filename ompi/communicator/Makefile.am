# -*- makefile -*-
#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2020 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2013-2017 Los Alamos National Security, LLC. All rights
#                         reserved.
# Copyright (c) 2014      Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# Copyright (c) 2016      IBM Corporation.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# This makefile.am does not stand on its own - it is included from ompi/Makefile.am

headers += \
        communicator/communicator.h \
	communicator/comm_request.h

lib@OMPI_LIBMPI_NAME@_la_SOURCES += \
        communicator/comm_init.c \
	communicator/comm.c \
	communicator/comm_cid.c \
	communicator/comm_request.c

if WANT_FT_MPI
lib@OMPI_LIBMPI_NAME@_la_SOURCES += \
	communicator/ft/comm_ft.c communicator/ft/comm_ft_reliable_bcast.c communicator/ft/comm_ft_propagator.c communicator/ft/comm_ft_detector.c communicator/ft/comm_ft_revoke.c
endif # WANT_FT_MPI

dist_ompidata_DATA += communicator/help-comm.txt
