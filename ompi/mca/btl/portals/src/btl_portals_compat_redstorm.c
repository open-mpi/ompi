/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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

#include "include/constants.h"
#include "opal/util/output.h"

#include "btl_portals.h"
#include "btl_portals_compat.h"

#if 0
int cnos_get_size(void);
int cnos_get_rank(void);
int cnos_get_nidpid_map(ptl_process_id_t **map);
#else
#include <catamount/cnos_mpi_os.h>
#endif

int
mca_btl_portals_init_compat(mca_btl_portals_component_t *comp)
{
    int ret, max_interfaces;
    uint32_t i;
    struct mca_btl_portals_module_t *btl;

    /*
     * create module - only ever one "NIC" on red storm
     */
    comp->portals_num_modules = 1;
    comp->portals_modules = calloc(comp->portals_num_modules,
                                   sizeof(mca_btl_portals_module_t));
    if (NULL == comp->portals_modules) {
        opal_output_verbose(10, mca_btl_portals_component.portals_output,
                            "malloc failed in mca_btl_portals_init");
        return OMPI_ERR_TEMP_OUT_OF_RESOURCE;
    }
    btl = &(comp->portals_modules[0]);

    /* compat code is responsible for copying over the "template" onto
       each module instance.  The calling code will create the free
       lists and the like - we're only responsible for the
       Portals-specific entries */
    for (i = 0 ; i < comp->portals_num_modules ; ++i) {
        memcpy(&(comp->portals_modules[i]),
               &mca_btl_portals_module,
               sizeof(mca_btl_portals_module_t));
        /* the defaults are good enough for the rest */
    }

    /*
     * Initialize Portals interface
     */
    ret = PtlInit(&max_interfaces);
    if (PTL_OK != ret) {
        opal_output_verbose(10, mca_btl_portals_component.portals_output,
                            "PtlInit failed, returning %d\n", ret);
        return OMPI_ERR_FATAL;
    }

    /*
     * Initialize a network device
     */
    ret = PtlNIInit(PTL_IFACE_DEFAULT, /* interface to initialize */
                    PTL_PID_ANY,       /* let library assign our pid */
                    NULL,              /* no desired limits */
                    &(btl->portals_ni_limits),    /* save our limits somewhere */
                    &(btl->portals_ni_h)  /* our interface handle */
                    );
    if (PTL_OK != ret && PTL_IFACE_DUP != ret) {
        opal_output_verbose(10, mca_btl_portals_component.portals_output,
                            "PtlNIInit failed, returning %d\n", ret);
        return OMPI_ERR_FATAL;
    }

    opal_output_verbose(10, mca_btl_portals_component.portals_output,
			"max ptl index: %d", btl->portals_ni_limits.max_pt_index);

    return OMPI_SUCCESS;
}


int
mca_btl_portals_add_procs_compat(struct mca_btl_portals_module_t* btl,
                                 size_t nprocs, struct ompi_proc_t **procs,
                                 ptl_process_id_t **portals_procs)
{
    int nptl_procs = 0;
    cnos_nidpid_map_t *map;
    int i;

    /*
     * FIXME - XXX - FIXME 
     * BWB - implicit assumption that cnos procs list will match our
     *       procs list.  Don't know what to do about that...
     */

    nptl_procs = cnos_get_nidpid_map(&map);
    if (nptl_procs <= 0) {
        opal_output_verbose(10, mca_btl_portals_component.portals_output,
                            "cnos_get_nidpid_map() returned %d", nptl_procs);
        return OMPI_ERR_FATAL;
    } else if (nptl_procs != nprocs) {
        opal_output_verbose(10, mca_btl_portals_component.portals_output,
                            "nptl_procs != nprocs (%d, %d)", nptl_procs,
                            nprocs);
        return OMPI_ERR_FATAL;
    }

    /* get space for the portals procs list */
    *portals_procs = calloc(nprocs, sizeof(ptl_process_id_t));
    if (NULL == *portals_procs) {
	opal_output_verbose(10, mca_btl_portals_component.portals_output,
			    "calloc(nprocs, sizeof(ptl_process_id_t)) failed");
	return OMPI_ERR_TEMP_OUT_OF_RESOURCE;
    }

    for (i = 0 ; i < nprocs ; ++i) {
	opal_output_verbose(120, mca_btl_portals_component.portals_output,
			    "rank %d: nid %ld, pid %ld", i,
			    map[i].nid, map[i].pid);

	/* update my local array of proc structs */
	(*portals_procs)[i].nid = map[i].nid;
	(*portals_procs)[i].pid = map[i].pid;
    }

    return OMPI_SUCCESS;
}
