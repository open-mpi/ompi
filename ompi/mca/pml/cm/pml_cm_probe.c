/*
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2012      Sandia National Laboratories.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "opal/runtime/opal_progress.h"
#include "ompi/mca/mtl/mtl.h"

#include "pml_cm.h"

int
mca_pml_cm_iprobe(int src, int tag,
                   struct ompi_communicator_t *comm,
                   int *matched, ompi_status_public_t * status)
{
    return OMPI_MTL_CALL(iprobe(ompi_mtl,
                                comm, src, tag,
                                matched, status));
}


int
mca_pml_cm_probe(int src, int tag,
                  struct ompi_communicator_t *comm,
                  ompi_status_public_t * status)
{
    int ret, matched = 0;

    while (true) {
        ret = OMPI_MTL_CALL(iprobe(ompi_mtl,
                                   comm, src, tag,
                                   &matched, status));
        if (OMPI_SUCCESS != ret) break;
        if (matched) break;
        opal_progress();
    }

    return ret;
}


int
mca_pml_cm_improbe(int src,
                   int tag,
                   struct ompi_communicator_t* comm,
                   int *matched,
                   struct ompi_message_t **message,
                   ompi_status_public_t* status)
{
    return OMPI_MTL_CALL(improbe(ompi_mtl,
                                 comm, src, tag,
                                 matched, message,
                                 status));
}


int
mca_pml_cm_mprobe(int src,
                  int tag,
                  struct ompi_communicator_t* comm,
                  struct ompi_message_t **message,
                  ompi_status_public_t* status)
{
    int ret, matched = 0;

    while (true) {
        ret = OMPI_MTL_CALL(improbe(ompi_mtl,
                                    comm, src, tag,
                                    &matched, message,
                                    status));
        if (OMPI_SUCCESS != ret) break;
        if (matched) break;
        opal_progress();
    }

    return ret;
}
