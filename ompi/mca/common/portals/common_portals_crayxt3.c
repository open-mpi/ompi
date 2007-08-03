/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include <catamount/cnos_mpi_os.h>

#include "opal/util/output.h"
#include "ompi/constants.h"
#include "ompi/proc/proc.h"


char *
ompi_common_portals_nodeid(void)
{
    char *ret;
    asprintf(&ret, "%5d", cnos_get_rank());
    return ret;
}


int
ompi_common_portals_register_mca(void)
{
    return OMPI_SUCCESS;
}


int
ompi_common_portals_initialize(void)
{
    int ret, max_interfaces;

    /*
     * Initialize Portals interface
     */
    ret = PtlInit(&max_interfaces);
    if (PTL_OK != ret) {
        opal_output(0, "%5d: PtlInit failed, returning %d\n", 
                    cnos_get_rank(), ret);
        return OMPI_ERR_NOT_AVAILABLE;
    }

    return OMPI_SUCCESS;
}


int
ompi_common_portals_ni_initialize(ptl_handle_ni_t *ni_handle)
{
    ptl_interface_t ni_iface = PTL_IFACE_DEFAULT;
    int max_interfaces;
    int launcher;
    int ret;

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
        return OMPI_ERR_NOT_AVAILABLE;
    }

    /*
     * Initialize a network device
     */
    ret = PtlNIInit(ni_iface,          /* interface to initialize */
                    PTL_PID_ANY,       /* let library assign our pid */
                    NULL,              /* no desired limits */
                    NULL,              /* actual limits */
                    ni_handle          /* our interface handle */
                    );
    if (PTL_OK != ret && PTL_IFACE_DUP != ret) {
        opal_output(0, "%5d: PtlNIInit failed, returning %d (%s : %d)\n", 
                    cnos_get_rank(), ret, __FILE__, __LINE__);
        return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
}


int
ompi_common_portals_get_procs(size_t nprocs,
                              struct ompi_proc_t **procs,
                              ptl_process_id_t *portals_procs)
{
    int nptl_procs = 0;
    cnos_nidpid_map_t *map;
    int i;

    /*
     * assumption that the vpid of the process name is the rank in the
     * nidpid map.  THis will not be true if someone changes the sds
     * component...
     */
    nptl_procs = cnos_get_nidpid_map(&map);
    if (nptl_procs <= 0) {
        opal_output(0, "%5d: cnos_get_nidpid_map() returned %d", 
                    cnos_get_rank(), nptl_procs);
        return OMPI_ERR_FATAL;
    }

    for (i = 0 ; i < nprocs ; ++i) {
        size_t idx = (size_t) procs[i]->proc_name.vpid;
        if (idx >= nptl_procs) return OMPI_ERR_NOT_FOUND;

	portals_procs[i].nid = map[idx].nid;
	portals_procs[i].pid = map[idx].pid;
    }

    return OMPI_SUCCESS;
}


int
ompi_common_portals_ni_finalize(void)
{
    return OMPI_SUCCESS;
}


int
ompi_common_portals_finalize(void)
{
    return OMPI_SUCCESS;
}
