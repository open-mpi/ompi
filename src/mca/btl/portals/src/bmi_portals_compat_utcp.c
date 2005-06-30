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
#include <netinet/in.h>

#include "include/constants.h"
#include "util/output.h"

#include "bmi_portals.h"
#include "bmi_portals_compat.h"

#include <p3api/debug.h>

/* how's this for source code diving? - find private method for
   getting interface */
extern unsigned int utcp_my_nid(const char *if_str);

/* these need to be defined, or things get "unhappy" */
FILE* utcp_api_out;
FILE* utcp_lib_out;

int
mca_bmi_portals_init(mca_bmi_portals_component_t *comp)
{
    ptl_process_id_t info;
    int ret, i;
#if 0
    FILE *output;
    char *tmp;

    asprintf(&tmp, "portals.%d", getpid());
    output = fopen(tmp, "w");
    free(tmp);

    utcp_lib_out = output;
    utcp_api_out = output;
#else
    utcp_lib_out = stderr;
    utcp_api_out = stderr;
#endif

    info.nid = htonl(utcp_my_nid(mca_bmi_portals_component.portals_ifname));
    info.pid = htonl((ptl_pid_t) getpid());
    ompi_output_verbose(100, mca_bmi_portals_component.portals_output,
                        "contact info: %u, %u", ntohl(info.nid), 
                        ntohl(info.pid));

    ret = mca_base_modex_send(&mca_bmi_portals_component.super.bmi_version,
                              &info, sizeof(ptl_process_id_t));
    if (OMPI_SUCCESS != ret) {
        ompi_output_verbose(10, mca_bmi_portals_component.portals_output,
                            "mca_base_modex_send failed: %d", ret);
        return ret;
    }

    /* with the utcp interface, only ever one "NIC" */
    comp->portals_num_modules = 1;
    comp->portals_modules = calloc(comp->portals_num_modules,
                                   sizeof(mca_bmi_portals_module_t));
    if (NULL == comp->portals_modules) {
        ompi_output_verbose(10, mca_bmi_portals_component.portals_output,
                            "malloc failed in mca_bmi_portals_init");
        return OMPI_ERR_TEMP_OUT_OF_RESOURCE;
    }

    /* compat code is responsible for copying over the "template" onto
       each module instance.  The calling code will create the free
       lists and the like - we're only responsible for the
       Portals-specific entries */
    for (i = 0 ; i < comp->portals_num_modules ; ++i) {
        memcpy(&(comp->portals_modules[i]), 
               mca_bmi_portals_module,
               sizeof(mca_bmi_portals_module_t));
        /* the defaults are good enough for the rest */
    }

    return OMPI_SUCCESS;
}


int
mca_bmi_portals_add_procs_compat(struct mca_bmi_portals_module_t* bmi,
                                 size_t nprocs, struct ompi_proc_t **procs,
                                 ptl_process_id_t **portals_procs)
{
    int ret, my_rid;
    ptl_process_id_t *info;
    char *nidmap = NULL;
    char *pidmap = NULL;
    char *nid_str;
    char *pid_str;
    const size_t map_size = nprocs * 12 + 1; /* 12 is max length of long in decimal */
    size_t size, i;
    char *tmp;
    ompi_proc_t* proc_self = ompi_proc_local();
    int max_interfaces;

    /*
     * Do all the NID/PID map setup
     */
    /* each nid is a int, so need 10 there, plus the : */
    nidmap = malloc(map_size);
    pidmap = malloc(map_size);
    nid_str = malloc(12 + 1);
    pid_str = malloc(12 + 1);
    if (NULL == nidmap || NULL == pidmap || NULL == nid_str || NULL == pid_str)
        return OMPI_ERROR;

    /* get space for the portals procs list */
    *portals_procs = calloc(nprocs, sizeof(ptl_process_id_t));
    if (NULL == *portals_procs) {
        ompi_output_verbose(10, mca_bmi_portals_component.portals_output,
                            "calloc(nprocs, sizeof(ptl_process_id_t)) failed");
        return OMPI_ERR_TEMP_OUT_OF_RESOURCE;
    }
         
    for (i = 0 ; i < nprocs ; ++i) {
        if (proc_self == procs[i]) my_rid = i;

        ret = mca_base_modex_recv(&mca_bmi_portals_component.super.bmi_version, 
                                  procs[i], (void**) &info, &size);
        if (OMPI_SUCCESS != ret) {
            ompi_output_verbose(10, mca_bmi_portals_component.portals_output,
                                "mca_base_modex_recv failed: %d", ret);
            return ret;
        } else if (sizeof(ptl_process_id_t) != size) {
            ompi_output_verbose(10, mca_bmi_portals_component.portals_output,
                                "mca_base_modex_recv returned size %d, expected %d", 
                                size, sizeof(ptl_process_id_t));
            return OMPI_ERROR;
        }

        if (i == 0) {
            snprintf(nidmap, map_size, "%u", ntohl(info->nid));
            snprintf(pidmap, map_size, "%u", ntohl(info->pid));
        } else {
            snprintf(nid_str, 12 + 1, ":%u", ntohl(info->nid));
            snprintf(pid_str, 12 + 1, ":%u", ntohl(info->pid));
            strncat(nidmap, nid_str, 12);
            strncat(pidmap, pid_str, 12);
        }

        /* update my local array of proc structs */
        (*portals_procs)[i].nid = ntohl(info->nid);
        (*portals_procs)[i].pid = ntohl(info->pid);

        free(info);
    }

    ompi_output_verbose(100, mca_bmi_portals_component.portals_output,
                        "my rid: %u", my_rid);
    ompi_output_verbose(100, mca_bmi_portals_component.portals_output,
                        "nid map: %s", nidmap);
    ompi_output_verbose(100, mca_bmi_portals_component.portals_output,
                        "pid map: %s", pidmap);
    ompi_output_verbose(100, mca_bmi_portals_component.portals_output,
                        "iface: %s",
                        mca_bmi_portals_component.portals_ifname);

    asprintf(&tmp, "PTL_MY_RID=%u", my_rid);
    putenv(tmp);
    asprintf(&tmp, "PTL_NIDMAP=%s", nidmap);
    putenv(tmp);
    asprintf(&tmp, "PTL_PIDMAP=%s", pidmap);
    putenv(tmp);
    asprintf(&tmp, "PTL_IFACE=%s", mca_bmi_portals_component.portals_ifname);
    putenv(tmp);

    free(pidmap);
    free(nidmap);
    free(pid_str);
    free(nid_str);

    /*
     * Initialize Portals
     */
    ret = PtlInit(&max_interfaces);
    if (PTL_OK != ret) {
        ompi_output_verbose(10, mca_bmi_portals_component.portals_output,
                            "PtlInit failed, returning %d\n", ret);
        return OMPI_ERR_FATAL;
    }

    ret = PtlNIInit(PTL_IFACE_DEFAULT, /* interface to initialize */
                    PTL_PID_ANY,       /* let library assign our pid */
                    NULL,              /* no desired limits */
                    &(bmi->portals_limits),    /* save our limits somewhere */
                    &(bmi->portals_ni_h)  /* our interface handle */
                    );
    if (PTL_OK != ret) {
        ompi_output_verbose(10, mca_bmi_portals_component.portals_output,
                            "PtlNIInit failed, returning %d\n", ret);
        return OMPI_ERR_FATAL;
    }

#if 0
    PtlNIDebug(bmi->portals_ni_h, PTL_DBG_ALL | PTL_DBG_NI_ALL);
#endif

    return OMPI_SUCCESS;
}

