/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"

#ifdef HAVE_CNOS_PM_BARRIER
#include <catamount/cnos_mpi_os.h>
#include <portals/portals3.h>
#endif

#include "orte/orte_constants.h"
#include "orte/util/proc_info.h"
#include "opal/util/output.h"
#include "orte/mca/errmgr/errmgr.h"

#include "orte/mca/rds/base/base.h"
#include "orte/mca/ras/base/base.h"
#include "orte/mca/rmaps/base/base.h"
#include "orte/mca/pls/base/base.h"
#include "rmgr_cnos.h"

/*
 * Local functions
 */

static int orte_rmgr_cnos_open(void);
static int orte_rmgr_cnos_close(void);
static orte_rmgr_base_module_t* orte_rmgr_cnos_init(int *priority);


orte_rmgr_base_component_t mca_rmgr_cnos_component = {
      /* First, the mca_base_component_t struct containing meta
         information about the component itself */

      {
        /* Indicate that we are a rmgr v2.0.0 component (which also
           implies a specific MCA version) */

        ORTE_RMGR_BASE_VERSION_2_0_0,

        "cnos", /* MCA component name */
        ORTE_MAJOR_VERSION,  /* MCA component major version */
        ORTE_MINOR_VERSION,  /* MCA component minor version */
        ORTE_RELEASE_VERSION,  /* MCA component release version */
        orte_rmgr_cnos_open,  /* component open  */
        orte_rmgr_cnos_close  /* component close */
      },

      /* Next the MCA v1.0.0 component meta data */
      {
          /* The component is not checkpoint ready */
          MCA_BASE_METADATA_PARAM_NONE
      },

      orte_rmgr_cnos_init
};


/**
  * component open/close/init function
  */
static int orte_rmgr_cnos_open(void)
{
    return ORTE_SUCCESS;
}


static orte_rmgr_base_module_t *orte_rmgr_cnos_init(int* priority)
{
    ptl_interface_t ni_iface = PTL_IFACE_DEFAULT;
    ptl_handle_ni_t ni_handle;
    int ret, max_interfaces;
    ptl_process_id_t ptl_process_id;
    int launcher;

    /* set a priority higher than the proxy component */
    *priority = 10;

    launcher = cnos_launcher();

    /*
     * If we use the YOD launcher we can use the default interface
     * otherwise we need to use the SeaStar Bridged interface (for CNL/APRUN)
     */
    if( launcher != CNOS_LAUNCHER_YOD ) {
        ni_iface = IFACE_FROM_BRIDGE_AND_NALID(PTL_BRIDGE_UK,PTL_IFACE_SS);
    }

    /*
     * Initialize Portals interface
     */
    ret = PtlInit(&max_interfaces);
    if (PTL_OK != ret) {
        opal_output(0, "%5d: PtlInit failed, returning %d\n", 
                    cnos_get_rank(), ret);
        return NULL;
    }

    /*
     * Initialize a network device
     */
    ret = PtlNIInit(ni_iface,          /* interface to initialize */
                    PTL_PID_ANY,       /* let library assign our pid */
                    NULL,              /* no desired limits */
                    NULL,              /* actual limits */
                    &ni_handle         /* our interface handle */
                    );
    if (PTL_OK != ret && PTL_IFACE_DUP != ret) {
        opal_output(0, "%5d: PtlNIInit failed, returning %d [%s : %d]\n", 
                    cnos_get_rank(), ret, __FILE__, __LINE__);
        return NULL;
    }

    /*
     * Initialize the Barrier
     * Note: No return value, assume success
     */
    cnos_barrier_init(ni_handle);

    /*
     * Register the ptl_process_id if *not* using yod.
     * If you do *not* do this before calling the barrier it will hang forever.
     */
    if( launcher != CNOS_LAUNCHER_YOD ) {
        ret = PtlGetId(ni_handle, &ptl_process_id);
        if( PTL_OK != ret ) {
            opal_output(0, "%5d: PtlGetId failed, returning %d\n",
                        cnos_get_rank(), ret);
            return NULL;
        }

        ret = cnos_register_ptlid(ptl_process_id);
        if( PTL_OK != ret ) {
            opal_output(0, "%5d: cnos_register_ptlid failed, returning %d\n",
                        cnos_get_rank(), ret);
            return NULL;
        }
    }

#ifdef HAVE_CNOS_PM_BARRIER
    /*
     * Do not use cnos_pm_barrier() as that serves as a indicator to 
     * the launcher that the job is exiting. Instead always use the
     * normal cnos_barrier().
     * JJH Double check:
     *   register with the process manager so that everyone aborts if
     *   any one process aborts.  This is a bit slower than it needs to
     *   be, but useful.
     *   Replaced: cnos_pm_barrier(0);
     */
    cnos_barrier();
#endif

    return &orte_rmgr_cnos_module;
}


/**
 *  Close all subsystems.
 */
static int orte_rmgr_cnos_close(void)
{
    return ORTE_SUCCESS;
}
