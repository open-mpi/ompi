/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *   $Id: mpioimpl.h,v 1.6 2002/10/24 17:01:18 gropp Exp $    
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */


/* header file for MPI-IO implementation. not intended to be
   user-visible */ 

#ifndef MPIOIMPL_INCLUDE
#define MPIOIMPL_INCLUDE

#include "adio.h"
#include "mpio.h"

/* info is a linked list of these structures */
struct MPIR_Info {
    int cookie;
    char *key, *value;
    struct MPIR_Info *next;
};

#define MPIR_INFO_COOKIE 5835657

MPI_Delete_function ADIOI_End_call;

#include "mpioprof.h"

#ifdef MPI_hpux
#  include "mpioinst.h"
#endif /* MPI_hpux */

#endif

