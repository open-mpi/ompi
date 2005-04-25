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
#include "portals_config.h"

#include <sys/types.h>
#include <unistd.h>
#include <stdio.h>
#include <errno.h>

#include "include/constants.h"
#include "util/output.h"

#include "ptl_portals.h"

/* how's this for source code diving? - find private method for
   getting interface */
extern unsigned int utcp_my_nid(const char *if_str);

/* these need to be defined, or things get "unhappy" */
FILE* utcp_api_out;
FILE* utcp_lib_out;

int
mca_ptl_portals_init(mca_ptl_portals_component_t *comp)
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

    /* with the utcp interface, only ever one "NIC" */
    comp->portals_num_modules = 1;
    comp->portals_modules = calloc(comp->portals_num_modules,
                                   sizeof(mca_ptl_portals_module_t *));
    if (NULL == comp->portals_modules) {
        ompi_output_verbose(10, mca_ptl_portals_component.portals_output,
                            "malloc failed in mca_ptl_portals_init");
        return OMPI_ERR_TEMP_OUT_OF_RESOURCE;
    }
    comp->portals_modules[0] = malloc(sizeof(mca_ptl_portals_module_t));
    if (NULL == comp->portals_modules) {
        ompi_output_verbose(10, mca_ptl_portals_component.portals_output,
                            "malloc failed in mca_ptl_portals_init");
        return OMPI_ERR_TEMP_OUT_OF_RESOURCE;
    }

    return OMPI_SUCCESS;
}


int
mca_ptl_portals_add_procs_compat(struct mca_ptl_base_module_t* ptl_base,
                                 size_t nprocs, struct ompi_proc_t **procs,
                                 struct mca_ptl_base_peer_t** peers,
                                 ompi_bitmap_t* reachable)
{
    int ret, my_rid;
    char *env;
    ptl_process_id_t *info;
    char *nidmap = NULL;
    char *pidmap = NULL;
    const size_t map_size = nprocs * 12 + 1; /* 12 is max length of long in decimal */
    size_t size, i;
    char *tmp;
    ompi_proc_t* proc_self = ompi_proc_local();
    int max_interfaces;
    struct mca_ptl_portals_module_t *ptl = (struct mca_ptl_portals_module_t*) ptl_base;

    /*
     * Do all the NID/PID map setup
     */
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
    ompi_output_verbose(100, mca_ptl_portals_component.portals_output,
                        "iface: %s", mca_ptl_portals_component.portals_ifname);

    asprintf(&tmp, "PTL_MY_RID=%u", my_rid);
    putenv(tmp);
    asprintf(&tmp, "PTL_NIDMAP=%s", nidmap);
    putenv(tmp);
    asprintf(&tmp, "PTL_PIDMAP=%s", pidmap);
    putenv(tmp);
    asprintf(&tmp, "PTL_IFACE=%s", mca_ptl_portals_component.portals_ifname);
    putenv(tmp);

    /*
     * Initialize Portals
     */
    ret = PtlInit(&max_interfaces);
    if (PTL_OK != ret) {
        ompi_output_verbose(10, mca_ptl_portals_component.portals_output,
                            "PtlInit failed, returning %d\n", ret);
        return OMPI_ERR_FATAL;
    }

    ret = PtlNIInit(PTL_IFACE_DEFAULT, /* interface to initialize */
                    PTL_PID_ANY,       /* let library assign our pid */
                    NULL,              /* no desired limits */
                    &(ptl->limits),    /* save our limits somewhere */
                    &(ptl->ni_handle)  /* our interface handle */
                    );
    if (PTL_OK != ret) {
        ompi_output_verbose(10, mca_ptl_portals_component.portals_output,
                            "PtlNIInit failed, returning %d\n", ret);
        return OMPI_ERR_FATAL;
    }

    return OMPI_SUCCESS;
}

