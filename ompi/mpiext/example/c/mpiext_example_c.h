/*
 * Copyright (c) 2004-2009 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2011      Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2012 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 */

/* This file is included in <mpi-ext.h>.  It is unnecessary to protect
   it from multiple inclusion.  Also, you can assume that <mpi.h> has
   already been included, so all of its types and globals are
   available. */

OMPI_DECLSPEC extern int OMPI_Example_global;

OMPI_DECLSPEC int OMPI_Progress(int count, MPI_Comm comm);
