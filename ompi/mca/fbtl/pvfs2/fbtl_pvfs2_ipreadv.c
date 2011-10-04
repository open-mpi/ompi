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
#include "fbtl_pvfs2.h"

#include "mpi.h"
#include "ompi/constants.h"
#include "ompi/mca/fbtl/fbtl.h"

size_t 
mca_fbtl_pvfs2_ipreadv (mca_io_ompio_file_t *file,
                        int *sorted, ompi_request_t **request)
{
    printf ("PVFS2 IPREADV\n");
    return OMPI_SUCCESS;
}
