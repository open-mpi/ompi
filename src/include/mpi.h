/*
 * Copyright (c) 2003 The Trustees of Indiana University.  
 *                    All rights reserved.
 * 
 * This file is part of the CMPI software package.  For license
 * information, see the LICENSE file in the top level directory of the
 * CMPI source distribution.
 * 
 * $Id: mpi.h,v 1.1 2003/11/22 16:36:25 jsquyres Exp $
 */

#ifndef CMPI_COMMUNICATOR_H
#define CMPI_COMMUNICATOR_H

#include <cmpi_config.h>

#define MPI_SUCCESS 0

#define MPI_MAX_OBJECT_NAME	64

typedef int MPI_Comm;
typedef struct cmpi_datatype *MPI_Datatype;

extern MPI_Comm MPI_COMM_NULL;
extern MPI_Comm MPI_COMM_WORLD;
extern MPI_Comm MPI_COMM_SELF;

extern MPI_Datatype MPI_TYPE_NULL;


#endif /* CMPI_COMMUNICATOR_H */
