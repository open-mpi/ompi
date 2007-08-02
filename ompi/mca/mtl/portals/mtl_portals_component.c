/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "opal/event/event.h"
#include "opal/mca/base/mca_base_param.h"
#include "ompi/datatype/convertor.h"
#include "ompi/mca/common/portals/common_portals.h"

#include "mtl_portals.h"
#include "mtl_portals_request.h"


static int ompi_mtl_portals_component_open(void);
static int ompi_mtl_portals_component_close(void);
static mca_mtl_base_module_t* ompi_mtl_portals_component_init(
              bool enable_progress_threads, bool enable_mpi_threads);

mca_mtl_base_component_1_0_0_t mca_mtl_portals_component = {

    /* First, the mca_base_component_t struct containing meta
     * information about the component itself */

    {
        /* Indicate that we are a mtl v1.0.0 component (which also implies
	 *          a specific MCA version) */

         MCA_MTL_BASE_VERSION_1_0_0,

         "portals", /* MCA component name */
         OMPI_MAJOR_VERSION,  /* MCA component major version */
         OMPI_MINOR_VERSION,  /* MCA component minor version */
         OMPI_RELEASE_VERSION,  /* MCA component release version */
         ompi_mtl_portals_component_open,  /* component open */
         ompi_mtl_portals_component_close  /* component close */
     },

     /* Next the MCA v1.0.0 component meta data */

     {
         /* The component is not checkpoint ready */
         false
     },

     ompi_mtl_portals_component_init,  /* component init */
};

static int
ompi_mtl_portals_component_open(void)
{
    int tmp;

    ompi_common_portals_register_mca();

    ompi_mtl_portals.base.mtl_request_size = 
        sizeof(ompi_mtl_portals_request_t) -
        sizeof(struct mca_mtl_request_t);

    mca_base_param_reg_int(&mca_mtl_portals_component.mtl_version,
                           "eager_limit",
                           "Cross-over point from eager to rendezvous sends",
                           false,
                           false,
                           1024,
                           &tmp);

    ompi_mtl_portals.eager_limit = tmp;

    mca_base_param_reg_int(&mca_mtl_portals_component.mtl_version,
                           "short_recv_mds_num",
                           "Number of short message receive blocks",
                           false,
                           false,
                           3,
                           &ompi_mtl_portals.ptl_recv_short_mds_num);

    mca_base_param_reg_int(&mca_mtl_portals_component.mtl_version,
                           "short_recv_mds_size",
                           "Size of short message receive blocks",
                           false,
                           false,
                           15 * 1024 * 1024,
                           &ompi_mtl_portals.ptl_recv_short_mds_size);

    ompi_mtl_portals.ptl_ni_h = PTL_INVALID_HANDLE;

    mca_base_param_reg_int(&mca_mtl_portals_component.mtl_version,
                           "expected_queue_size",
                           "Size of the expected receive queue in bytes",
                           false,
                           false,
                           1024,
                           &ompi_mtl_portals.ptl_expected_queue_size);

    mca_base_param_reg_int(&mca_mtl_portals_component.mtl_version,
                           "unexpected_queue_size",
                           "Size of the unexpected receive queue in bytes",
                           false,
                           false,
                           1024,
                           &ompi_mtl_portals.ptl_unexpected_queue_size);

    return OMPI_SUCCESS;
}


static int
ompi_mtl_portals_component_close(void)
{
    return OMPI_SUCCESS;
}


static mca_mtl_base_module_t*
ompi_mtl_portals_component_init(bool enable_progress_threads,
                                bool enable_mpi_threads)
{
    /* we don't run with no stinkin' threads */
    if (enable_progress_threads || enable_mpi_threads) return NULL;

    /* initialize our interface */
    if (OMPI_SUCCESS != ompi_common_portals_initialize()) {
        return NULL;
    }

    OBJ_CONSTRUCT(&ompi_mtl_portals.event_fl, ompi_free_list_t);
    ompi_free_list_init(&ompi_mtl_portals.event_fl,
                        sizeof(ompi_mtl_portals_event_t),
                        OBJ_CLASS(ompi_mtl_portals_event_t),
                        1, -1, 1, NULL);

    OBJ_CONSTRUCT(&ompi_mtl_portals.ptl_recv_short_blocks, opal_list_t);
    OBJ_CONSTRUCT(&ompi_mtl_portals.unexpected_messages, opal_list_t);

    return &ompi_mtl_portals.base;
}
