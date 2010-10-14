/*
 * Copyright (c) 2010      Sandia National Laboratories. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "opal/mca/base/mca_base_param.h"

#include "mtl_portals4.h"
#include "mtl_portals4_request.h"

static int ompi_mtl_portals4_component_open(void);
static int ompi_mtl_portals4_component_close(void);
static mca_mtl_base_module_t* ompi_mtl_portals4_component_init(bool enable_progress_threads, 
                                                               bool enable_mpi_threads);

mca_mtl_base_component_2_0_0_t mca_mtl_portals4_component = {
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

    ompi_mtl_portals4.ptl_ni_h = PTL_INVALID_HANDLE;

    return OMPI_SUCCESS;
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
    if (PTL_OK != PtlInit()) {
        return NULL;
    }

    if (PTL_OK != PtlNIInit(PTL_IFACE_DEFAULT, PTL_NI_MATCHING|PTL_NI_PHYSICAL,
                            PTL_PID_ANY, NULL, NULL, 0, NULL, NULL,
                            &ompi_mtl_portals4.ptl_ni_h)) {
        return NULL;
    }

    return &ompi_mtl_portals4.base;
}
