/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include <errno.h>

#include "include/constants.h"
#include "runtime/runtime.h"
#include "runtime/runtime_types.h"
#include "mca/pcm/pcm.h"


ompi_list_t*
ompi_rte_allocate_resources(ompi_rte_spawn_handle_t *handle,
                            mca_ns_base_jobid_t jobid, 
                            int nodes, int procs)
{
    mca_pcm_base_module_t *active;

    if (NULL == handle) {
        errno = OMPI_ERR_BAD_PARAM;
        return NULL;
    }
    /* check for invalide jobid */
    if (nodes < 0) {
        errno = OMPI_ERR_BAD_PARAM;
        return NULL;
    }
    if (procs < 0) {
        errno = OMPI_ERR_BAD_PARAM;
        return NULL;
    }
    if (nodes != 0 && procs == 0) {
        errno = OMPI_ERR_BAD_PARAM;
        return NULL;
    }

    /* remove for multi-cell */
    assert(1 == handle->modules_len);

    active = handle->modules[0];

    if (NULL == active->pcm_allocate_resources) {
        errno = OMPI_ERR_NOT_IMPLEMENTED;
        return NULL;
    }

    return active->pcm_allocate_resources(active, jobid, nodes, procs);
}


int
ompi_rte_deallocate_resources(ompi_rte_spawn_handle_t *handle,
                              mca_ns_base_jobid_t jobid, 
                              ompi_list_t *nodelist)
{
    mca_pcm_base_module_t *active;

    if (NULL == handle) return OMPI_ERR_BAD_PARAM;
    /* check for invalide jobid */
    if (NULL == nodelist) return OMPI_ERR_BAD_PARAM;

    /* remove for multi-cell */
    assert(1 == handle->modules_len);

    active = handle->modules[0];

    if (NULL == active->pcm_deallocate_resources) {
        return OMPI_ERR_NOT_IMPLEMENTED;
    }

    return active->pcm_deallocate_resources(active, jobid, nodelist);
}
