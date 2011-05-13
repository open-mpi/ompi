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
 * Copyright (c) 2010      Sandia National Laboratories.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "opal/mca/event/event.h"
#include "opal/util/output.h"
#include "opal/mca/base/mca_base_param.h"

#include "mtl_portals4.h"
#include "mtl_portals4_request.h"


static int ompi_mtl_portals4_component_open(void);
static int ompi_mtl_portals4_component_close(void);
static mca_mtl_base_module_t* ompi_mtl_portals4_component_init(
                                                               bool enable_progress_threads, bool enable_mpi_threads);

OMPI_MODULE_DECLSPEC extern mca_mtl_base_component_2_0_0_t mca_mtl_portals4_component;

mca_mtl_base_component_2_0_0_t mca_mtl_portals4_component = {

    /* First, the mca_base_component_t struct containing meta
     * information about the component itself */

    {
        MCA_MTL_BASE_VERSION_2_0_0,

        "portals4", /* MCA component name */
        OMPI_MAJOR_VERSION,  /* MCA component major version */
        OMPI_MINOR_VERSION,  /* MCA component minor version */
        OMPI_RELEASE_VERSION,  /* MCA component release version */
        ompi_mtl_portals4_component_open,  /* component open */
        ompi_mtl_portals4_component_close  /* component close */
    },
    {
        /* The component is not checkpoint ready */
        MCA_BASE_METADATA_PARAM_NONE
    },

    ompi_mtl_portals4_component_init,  /* component init */
};


static int
ompi_mtl_portals4_component_open(void)
{
    int tmp;

    ompi_mtl_portals4.base.mtl_request_size = 
        sizeof(ompi_mtl_portals4_request_t) -
        sizeof(struct mca_mtl_request_t);

    mca_base_param_reg_int(&mca_mtl_portals4_component.mtl_version,
                           "eager_limit",
                           "Cross-over point from eager to rendezvous sends",
                           false,
                           false,
                           2 * 1024,
                           &tmp);
    ompi_mtl_portals4.eager_limit = tmp;

    mca_base_param_reg_int(&mca_mtl_portals4_component.mtl_version,
                           "short_recv_num",
                           "Number of short message receive blocks",
                           false,
                           false,
                           8,
                           &ompi_mtl_portals4.recv_short_num);

    mca_base_param_reg_int(&mca_mtl_portals4_component.mtl_version,
                           "short_recv_size",
                           "Size of short message receive blocks",
                           false,
                           false,
                           1024 * 1024,
                           &tmp);
    ompi_mtl_portals4.recv_short_size = tmp;

    mca_base_param_reg_int(&mca_mtl_portals4_component.mtl_version,
                           "queue_size",
                           "Size of the event queue in entries",
                           false,
                           false,
                           1024,
                           &ompi_mtl_portals4.queue_size);

    ompi_mtl_portals4.ni_h = PTL_INVALID_HANDLE;

    return ompi_mtl_portals4_get_error(PtlInit());
}


static int
ompi_mtl_portals4_component_close(void)
{
    return OMPI_SUCCESS;
}


static mca_mtl_base_module_t*
ompi_mtl_portals4_component_init(bool enable_progress_threads,
                                 bool enable_mpi_threads)
{
    if (PTL_OK != PtlNIInit(PTL_IFACE_DEFAULT,
                            PTL_NI_PHYSICAL | PTL_NI_MATCHING,
                            PTL_PID_ANY,
                            NULL,
                            NULL,
                            0,
                            NULL,
                            NULL,
                            &ompi_mtl_portals4.ni_h)) {
        return NULL;
    }

    ompi_mtl_portals4.protocol = rndv;

    return &ompi_mtl_portals4.base;
}


int
ompi_mtl_portals4_get_error(int ptl_error)
{
    int ret;

    switch (ptl_error) {
    case PTL_OK:
        ret = OMPI_SUCCESS;
        break;
    case PTL_ARG_INVALID:
        ret = OMPI_ERR_BAD_PARAM;
        break;
    case PTL_CT_NONE_REACHED:
        ret = OMPI_ERR_TIMEOUT;
        break;
    case PTL_EQ_DROPPED:
        ret = OMPI_ERR_OUT_OF_RESOURCE;
        break;
    case PTL_EQ_EMPTY:
        ret = OMPI_ERR_TEMP_OUT_OF_RESOURCE;
        break;
    case PTL_FAIL:
        ret = OMPI_ERROR;
        break;
    case PTL_IN_USE:
        ret = OMPI_ERR_RESOURCE_BUSY;
        break;
    case PTL_INTERRUPTED:
        ret = OMPI_ERR_RESOURCE_BUSY;
        break;
    case PTL_LIST_TOO_LONG:
        ret = OMPI_ERR_OUT_OF_RESOURCE;
        break;
    case PTL_NI_NOT_LOGICAL:
        ret = OMPI_ERR_FATAL;
        break;
    case PTL_NO_INIT:
        ret = OMPI_ERR_FATAL;
        break;
    case PTL_NO_SPACE:
        ret = OMPI_ERR_OUT_OF_RESOURCE;
        break;
    case PTL_PID_IN_USE:
        ret = OMPI_ERR_BAD_PARAM;
        break;
    case PTL_PT_FULL:
        ret = OMPI_ERR_OUT_OF_RESOURCE;
        break;
    case PTL_PT_EQ_NEEDED:
        ret = OMPI_ERR_FATAL;
        break;
    case PTL_PT_IN_USE:
        ret = OMPI_ERR_RESOURCE_BUSY;
        break;
    case PTL_SIZE_INVALID:
        ret = OMPI_ERR_BAD_PARAM;
        break;
    default:
        ret = OMPI_ERROR;
    }

    return ret;
}
