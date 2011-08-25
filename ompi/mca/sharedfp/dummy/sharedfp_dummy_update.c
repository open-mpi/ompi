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
 * Copyright (c) 2008-2011 University of Houston. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */


#include "ompi_config.h"
#include "sharedfp_dummy.h"

#include "mpi.h"
#include "ompi/constants.h"
#include "orte/util/show_help.h"
#include "ompi/mca/sharedfp/sharedfp.h"

int
mca_sharedfp_dummy_update (mca_io_ompio_file_t *fh, 
                           int num_bytes,
                           OMPI_MPI_OFFSET_TYPE current_position)
{
    printf ("DUMMY UPDATING\n");
    return OMPI_SUCCESS;
}
