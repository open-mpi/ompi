/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"
#include "mca/oob/oob.h"
#include "mca/iof/iof.h"


int ompi_mpi_init_io(void)
{
    int fds[2];
    int rc;

    /* setup stdin */
    rc = pipe(fds);
    if(rc < 0) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    dup2(fds[0], 0);
    close(fds[0]);

    rc = mca_iof.iof_publish(
        MCA_OOB_NAME_SELF,
        MCA_IOF_SINK,
        MCA_IOF_STDIN,
        fds[1]);
    if(rc != OMPI_SUCCESS)
        return rc;
 
    /* setup stdout */
    rc = pipe(fds);
    if(rc < 0) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    dup2(fds[1], 1);
    close(fds[1]);

    rc = mca_iof.iof_publish(
        MCA_OOB_NAME_SELF,
        MCA_IOF_SOURCE,
        MCA_IOF_STDOUT,
        fds[0]);
    if(rc != OMPI_SUCCESS)
        return rc;
 
    /* setup stderr */
    rc = pipe(fds);
    if(rc < 0) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    dup2(fds[1], 2);
    close(fds[1]);

    rc = mca_iof.iof_publish(
        MCA_OOB_NAME_SELF,
        MCA_IOF_SOURCE,
        MCA_IOF_STDERR,
        fds[0]);
    if(rc != OMPI_SUCCESS)
        return rc;
    return OMPI_SUCCESS;
}


