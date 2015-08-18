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
 * Copyright (c) 2008      University of Houston. All rights reserved.
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
#include "ompi/mca/sharedfp/base/base.h"

int mca_sharedfp_individual_read ( mca_io_ompio_file_t *fh,
				   void *buf, int count, MPI_Datatype datatype, MPI_Status *status)
{
    opal_output(0,"mca_sharedfp_individual_read: NOT SUPPORTED by this component\n");
    return OMPI_ERROR;
}

int mca_sharedfp_individual_read_ordered ( mca_io_ompio_file_t *fh,
                                   void *buf, int count, MPI_Datatype datatype, MPI_Status *status)
{
    opal_output(0,"mca_sharedfp_individual_read_ordered: NOT SUPPORTED by this component\n");
    return OMPI_ERROR;
}

int mca_sharedfp_individual_iread(mca_io_ompio_file_t *fh,
                                   void *buf,
                                   int count,
                                   ompi_datatype_t *datatype,
                                   MPI_Request * request)
{
    opal_output(0,"mca_sharedfp_individual_iread: NOT SUPPORTED by this component\n");
    return OMPI_ERROR;
}

int mca_sharedfp_individual_read_ordered_begin(mca_io_ompio_file_t *fh,
                                                void *buf,
                                                int count,
                                                struct ompi_datatype_t *datatype)
{
    opal_output(0,"mca_sharedfp_individual_read_ordered_begin: NOT SUPPORTED by this component\n");
    return OMPI_ERROR;
}

int mca_sharedfp_individual_read_ordered_end(mca_io_ompio_file_t *fh,
                                              void *buf,
                                              ompi_status_public_t *status)
{
    opal_output(0,"mca_sharedfp_individual_read_ordered_end: NOT SUPPORTED by this component\n");
    return OMPI_ERROR;
}
