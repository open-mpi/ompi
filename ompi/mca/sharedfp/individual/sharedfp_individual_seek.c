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
 * Copyright (c) 2013      University of Houston. All rights reserved.
 * Copyright (c) 2018      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */


#include "ompi_config.h"
#include "sharedfp_individual.h"

#include "mpi.h"
#include "ompi/constants.h"
#include "ompi/mca/sharedfp/sharedfp.h"

static int seek_counter=0;

int mca_sharedfp_individual_seek (ompio_file_t *fh,
                         OMPI_MPI_OFFSET_TYPE offset, int whence)
{
    if ( 0 == seek_counter && 
         0 == offset       && 
         MPI_SEEK_SET == whence ) {
        /* This is occuring when setting the default file view. THat is ok.
        ** The component doesn't support however further seek operations. 
        */
        
        seek_counter++;
        return OMPI_SUCCESS;
    }

    opal_output(0,"mca_sharedfp_individual_seek: NOT IMPLEMENTED\n");
    return OMPI_ERROR;
}
