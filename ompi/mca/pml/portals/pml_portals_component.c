/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
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
#include "pml_portals.h"
#include "opal/mca/base/mca_base_param.h"
#include "ompi/datatype/convertor.h"

#if OMPI_BTL_PORTALS_REDSTORM
#include <catamount/cnos_mpi_os.h>
#endif


static int ompi_pml_portals_component_open(void);
static int ompi_pml_portals_component_close(void);
static mca_pml_base_module_t* ompi_pml_portals_component_init( int* priority,
                            bool enable_progress_threads, bool enable_mpi_threads);
static int ompi_pml_portals_component_fini(void);

mca_pml_base_component_1_0_0_t mca_pml_portals_component = {

    /* First, the mca_base_component_t struct containing meta
     * information about the component itself */

    {
        /* Indicate that we are a pml v1.0.0 component (which also implies
	 *          a specific MCA version) */

         MCA_PML_BASE_VERSION_1_0_0,

         "portals", /* MCA component name */
         OMPI_MAJOR_VERSION,  /* MCA component major version */
         OMPI_MINOR_VERSION,  /* MCA component minor version */
         OMPI_RELEASE_VERSION,  /* MCA component release version */
         ompi_pml_portals_component_open,  /* component open */
         ompi_pml_portals_component_close  /* component close */
     },

     /* Next the MCA v1.0.0 component meta data */

     {
         /* Whether the component is checkpointable or not */
         false
     },

     ompi_pml_portals_component_init,  /* component init */
     ompi_pml_portals_component_fini   /* component finalize */
};

static opal_output_stream_t portals_output_stream;

static int
ompi_pml_portals_component_open(void)
{
    /* start up debugging output */
    OBJ_CONSTRUCT(&portals_output_stream, opal_output_stream_t);
    portals_output_stream.lds_is_debugging = true;
    portals_output_stream.lds_want_stdout = true;
    portals_output_stream.lds_file_suffix = "pml-portals";
    mca_base_param_reg_int(&mca_pml_portals_component.pmlm_version,
                           "debug_level",
                           "Debugging verbosity (0 - 100)",
                           false,
                           false,
                           1000,
                           &(portals_output_stream.lds_verbose_level));
#if OMPI_BTL_PORTALS_REDSTORM
    asprintf(&(portals_output_stream.lds_prefix),
             "pml: portals (%2d): ", cnos_get_rank());
#else
    asprintf(&(portals_output_stream.lds_prefix), 
             "pml: portals (%5d): ", getpid());
#endif
    ompi_pml_portals.portals_output = 
        opal_output_open(&portals_output_stream);

    /* utcp configuration */
#if OMPI_BTL_PORTALS_UTCP
    mca_base_param_reg_string(&mca_pml_portals_component.pmlm_version,
                              "ifname",
                              "Interface name to use for communication",
                              false,
                              false,
                              "eth0",
                              &(ompi_pml_portals.portals_ifname));
#endif

    return OMPI_SUCCESS;
}


static int
ompi_pml_portals_component_close(void)
{
#if OMPI_BTL_PORTALS_UTCP
    if (NULL != ompi_pml_portals.portals_ifname) {
        free(ompi_pml_portals.portals_ifname);
    }
#endif

    if (NULL != portals_output_stream.lds_prefix) {
        free(portals_output_stream.lds_prefix);
    }

    /* close debugging stream */
    opal_output_close(ompi_pml_portals.portals_output);
    ompi_pml_portals.portals_output = -1;

    return OMPI_SUCCESS;
}


static mca_pml_base_module_t*
ompi_pml_portals_component_init(int* priority,
                                bool enable_progress_threads,
                                bool enable_mpi_threads)
{
    *priority = 10;

    /* we don't run with no stinkin' threads */
    if (enable_progress_threads || enable_mpi_threads) return NULL;

    /* initialize our interface */
    if (OMPI_SUCCESS != ompi_pml_portals_init_compat()) {
        opal_output_verbose(20, ompi_pml_portals.portals_output,
                            "disabled because compatibility init failed");
        return NULL;
    }

    OBJ_CONSTRUCT(&(ompi_pml_portals.portals_unexpected_events),
                  opal_list_t);

    OBJ_CONSTRUCT(&ompi_pml_portals.portals_blocking_send_convertor,
                  ompi_convertor_t);
    OBJ_CONSTRUCT(&ompi_pml_portals.portals_blocking_receive_convertor,
                  ompi_convertor_t);

    opal_output_verbose(20, ompi_pml_portals.portals_output,
                        "successfully initialized portals pml");

    return &ompi_pml_portals.super;
}


static int
ompi_pml_portals_component_fini(void)
{
    PtlEQFree(ompi_pml_portals.portals_unexpected_receive_queue);
    PtlEQFree(ompi_pml_portals.portals_blocking_receive_queue);
    PtlEQFree(ompi_pml_portals.portals_blocking_send_queue);

    PtlNIFini(ompi_pml_portals.portals_ni_h);

    OBJ_DESTRUCT(&ompi_pml_portals.portals_blocking_send_convertor);
    OBJ_DESTRUCT(&ompi_pml_portals.portals_blocking_receive_convertor);

    opal_output_verbose(20, ompi_pml_portals.portals_output,
                        "successfully finalized portals pml");

    return OMPI_SUCCESS;
}

