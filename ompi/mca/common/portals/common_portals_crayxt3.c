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
    int ret;

    /*
     * Initialize a network device
     */
    ret = PtlNIInit(PTL_IFACE_DEFAULT, /* interface to initialize */
                    PTL_PID_ANY,       /* let library assign our pid */
                    NULL,              /* no desired limits */
                    NULL,              /* actual limits */
                    ni_handle          /* our interface handle */
                    );
    if (PTL_OK != ret && PTL_IFACE_DUP != ret) {
        opal_output(0, "%5d: PtlNIInit failed, returning %d\n", 
                    cnos_get_rank(), ret);
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
     * FIXME - XXX - FIXME 
     * BWB - implicit assumption that cnos procs list will match our
     *       procs list.  Don't know what to do about that...
     */
    nptl_procs = cnos_get_nidpid_map(&map);
    if (nptl_procs <= 0) {
        opal_output(0, "%5d: cnos_get_nidpid_map() returned %d", 
                    cnos_get_rank(), nptl_procs);
        return OMPI_ERR_FATAL;
    } else if (nptl_procs != nprocs) {
        opal_output(0, "%5d: nptl_procs != nprocs (%d, %d)", nptl_procs,
                    cnos_get_rank(), nprocs);
        return OMPI_ERR_FATAL;
    }

    for (i = 0 ; i < nprocs ; ++i) {
	portals_procs[i].nid = map[i].nid;
	portals_procs[i].pid = map[i].pid;
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
