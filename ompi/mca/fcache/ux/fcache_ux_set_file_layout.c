/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008-2011 University of Houston. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */


#include "ompi_config.h"
#include "fcache_ux.h"

#include "mpi.h"
#include "ompi/constants.h"
#include "ompi/mca/fcache/fcache.h"

int 
mca_fcache_ux_set_file_layout (char* filename, 
                                  int *num_io_servers, 
                                  size_t *depth, 
                                  int *file_io_servers)
{
    printf ("UX SET FILE LAYOUT\n");
    return OMPI_SUCCESS;
}
