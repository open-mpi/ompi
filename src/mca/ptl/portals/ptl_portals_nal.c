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

#include <sys/types.h>
#include <unistd.h>
#include <stdio.h>

#include "include/constants.h"
#include "util/output.h"

#include "ptl_portals.h"
#include "ptl_portals_nal.h"


/******************* UTCP CONFIGURATION **********************/

/* how's this for source code diving? */
extern unsigned int utcp_my_nid(const char *if_str);

/* these need to be defined, or things get "unhappy" */
FILE* utcp_api_out;
FILE* utcp_lib_out;


int
mca_ptl_portals_nal_init(void)
{
    ptl_process_id_t info;
    int ret;

    utcp_lib_out = stderr;
    utcp_api_out = stderr;

    info.nid = utcp_my_nid(mca_ptl_portals_component.portals_ifname);
    info.pid = (ptl_pid_t) getpid();
    ompi_output_verbose(100, mca_ptl_portals_component.portals_output,
                        "contact info: %u, %u", info.nid, info.pid);

    ret = mca_base_modex_send(&mca_ptl_portals_component.super.ptlm_version,
                              &info, sizeof(ptl_process_id_t));
    if (OMPI_SUCCESS != ret) {
        ompi_output_verbose(10, mca_ptl_portals_component.portals_output,
                            "mca_base_modex_send failed: %d", ret);
        return ret;
    }

    return OMPI_SUCCESS;
}


int
mca_ptl_portals_nal_configure(size_t nprocs, struct ompi_proc_t **procs)
{
    int ret, my_rid;
    char *env;
    ptl_process_id_t *info;
    char *nidmap = NULL;
    char *pidmap = NULL;
    const size_t map_size = nprocs * 12 + 1;
    size_t size, i;
    char *tmp;
    ompi_proc_t* proc_self = ompi_proc_local();

    env = getenv("PTL_IFACE");
    if (NULL == env) {
        asprintf(&tmp, "PTL_IFACE=%s", mca_ptl_portals_component.portals_ifname);
        putenv(tmp);
    }

    env = getenv("PTL_NIDMAP");
    if (NULL == env) {
        /* build up a list of all the nids/pids for the current world */

        /* each nid is a int, so need 10 there, plus the : */
        nidmap = malloc(map_size);
        pidmap = malloc(map_size);
        if (NULL == nidmap || NULL == pidmap) return OMPI_ERROR;
        
        for (i = 0 ; i < nprocs ; ++i) {
            if (proc_self == procs[i]) my_rid = i;

            ret = mca_base_modex_recv(&mca_ptl_portals_component.super.ptlm_version, 
                                      procs[i], (void**) &info, &size);
            if (OMPI_SUCCESS != ret) {
                ompi_output_verbose(10, mca_ptl_portals_component.portals_output,
                                    "mca_base_modex_recv failed: %d", ret);
                return ret;
            } else if (sizeof(ptl_process_id_t) != size) {
                ompi_output_verbose(10, mca_ptl_portals_component.portals_output,
                                    "mca_base_modex_recv returned size%d", 
                                    size);
                return OMPI_ERROR;
            }

            if (i == 0) {
                snprintf(nidmap, map_size, "%u", info->nid);
                snprintf(pidmap, map_size, "%u", info->pid);
            } else {
                snprintf(nidmap, map_size, "%s:%u", nidmap, info->nid);
                snprintf(pidmap, map_size, "%s:%u", nidmap, info->pid);
            }
        }

        ompi_output_verbose(100, mca_ptl_portals_component.portals_output,
                            "my rid: %u", my_rid);
        ompi_output_verbose(100, mca_ptl_portals_component.portals_output,
                            "nid map: %s", nidmap);
        ompi_output_verbose(100, mca_ptl_portals_component.portals_output,
                            "pid map: %s", pidmap);

        asprintf(&tmp, "PTL_MY_RID=%u", my_rid);
        putenv(tmp);
        asprintf(&tmp, "PTL_NIDMAP=%s", nidmap);
        putenv(tmp);
        asprintf(&tmp, "PTL_PIDMAP=%s", pidmap);
        putenv(tmp);
    }

    return OMPI_SUCCESS;
}


/******************* CRAY CONFIGURATION **********************/

