/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2010-2018 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2026      Triad National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OMPI_FORTRAN_BASE_TOPO_NEIGHBORS_H
#define OMPI_FORTRAN_BASE_TOPO_NEIGHBORS_H

#include "mpi.h"
#include "ompi_config.h"

BEGIN_C_DECLS
/**
 * Return number of neighbors given a supplied communicator
 *
 * @param[in]  c_comm      MPI communicator (must have an associated topology)
 * @param[out] indegree    number of neighbors directed in 
 * @param[out] outdegree   number of neighbors directed out
 *
 * See 8.6 "Neighborhood Collective Communication on Virtual Topologies" of 
 * the MPI 5 standard for additional info about number of neighbors for 
 * the three different topology types supported by MPI as of that version
 * of the standard.
 *
 * Note only top-level 'c' MPI interfaces are used here as the intent
 * is for this function to work in the case that the OMPI fortran interface
 * base is moved to an external package at some point.
 */
OMPI_DECLSPEC int ompi_fortran_neighbor_count(MPI_Comm comm, int *indegree, int *outdegree);

END_C_DECLS

#endif /* OMPI_FORTRAN_BASE_TOPO_NEIGHBORS_H */
