/*
 * Copyright (c) 2003 The Trustees of Indiana University.  
 *                    All rights reserved.
 * 
 * This file is part of the CMPI software package.  For license
 * information, see the LICENSE file in the top level directory of the
 * CMPI source distribution.
 * 
 * $Id: mpi.h,v 1.3 2003/12/22 16:29:12 twoodall Exp $
 */

#ifndef CMPI_H
#define CMPI_H

#define MPI_SUCCESS 0
#define MPI_MAX_OBJECT_NAME 64

typedef struct _lam_communicator *MPI_Comm;
typedef struct _lam_group *MPI_Group;
typedef struct _lam_datatype *MPI_Datatype;

extern MPI_Comm MPI_COMM_NULL;
extern MPI_Comm MPI_COMM_WORLD;
extern MPI_Comm MPI_COMM_SELF;

extern MPI_Datatype MPI_TYPE_NULL;


#endif /* CMPI_H */
