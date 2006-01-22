/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
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

#include <sys/types.h>
#include <unistd.h>
#include <stdio.h>
#include <errno.h>
#include <netinet/in.h>

#include "ompi/include/constants.h"
#include "opal/util/output.h"
#include "ompi/proc/proc.h"
#include "ompi/mca/pml/base/pml_base_module_exchange.h"

#include "btl_portals.h"
#include "btl_portals_compat.h"

#include <p3api/debug.h>

/* how's this for source code diving? - find private method for
   getting interface */
extern unsigned int utcp_my_nid(const char *if_str);

/* these need to be defined, or things get "unhappy" */
FILE* utcp_api_out;
FILE* utcp_lib_out;

static bool use_modex = true;

int
mca_btl_portals_init_compat(mca_btl_portals_component_t *comp)
{
    int ret, max_interfaces;
#if 0 /* send all the portals internal debug to a file or stderr */
    FILE *output;
    char *tmp;

    asprintf(&tmp, "portals.%d", getpid());
    output = fopen(tmp, "w");
    free(tmp);

    utcp_lib_out = output;
    utcp_api_out = output;
#endif

    /* if the environment variables for the utcp implementation are
       already set, assume the user is running without the full Open
       RTE and is doing RTE testing for a more tightly-coupled
       platform (like, say, Red Storm).  Otherwise, be nice and use
       the modex to setup everything for the user */
    if (NULL == getenv("PTL_MY_RID")) {
        char *iface_name;
        use_modex = true;
        asprintf(&iface_name, "PTL_IFACE=%s", 
                 mca_btl_portals_component.portals_ifname);
        putenv(iface_name);
    } else {
        use_modex = false;
    }

    /* Initialize Portals interface */
    ret = PtlInit(&max_interfaces);
    if (PTL_OK != ret) {
        opal_output_verbose(10, mca_btl_portals_component.portals_output,
                            "PtlInit failed, returning %d", ret);
        return OMPI_ERR_FATAL;
    }

    /* Initialize a network device */
    ret = PtlNIInit(PTL_IFACE_DEFAULT, /* interface to initialize */
                    PTL_PID_ANY,       /* let library assign our pid */
                    NULL,              /* no desired limits */
                    NULL,              /* no need to have limits around */
                    &mca_btl_portals_module.portals_ni_h  /* our interface handle */
                    );
    if (PTL_OK != ret) {
        opal_output_verbose(10, mca_btl_portals_component.portals_output,
                            "PtlNIInit failed, returning %d", ret);
        return OMPI_ERR_FATAL;
    }

    if (use_modex) {
        ptl_process_id_t proc_id;

        ret = PtlGetId(mca_btl_portals_module.portals_ni_h, &proc_id);
        if (PTL_OK != ret) {
            opal_output_verbose(10, mca_btl_portals_component.portals_output,
                                "PtlGetId failed, returning %d", ret);
            return OMPI_ERR_FATAL;
        }

        /* post our contact info in the registry */
        proc_id.nid = htonl(proc_id.nid);
        proc_id.pid = htonl(proc_id.pid);

        opal_output_verbose(100, mca_btl_portals_component.portals_output,
                            "contact info: %u, %u", ntohl(proc_id.nid), 
                            ntohl(proc_id.pid));

        ret = mca_pml_base_modex_send(&mca_btl_portals_component.super.btl_version,
                                      &proc_id, sizeof(ptl_process_id_t));
        if (OMPI_SUCCESS != ret) {
            opal_output_verbose(10, mca_btl_portals_component.portals_output,
                                "mca_pml_base_modex_send failed: %d", ret);
            return ret;
        }
    } else {
        /* Initialize the NID/PID data, per the 3.3.2 p3rt api */
        ret = PtlSetRank(mca_btl_portals_module.portals_ni_h, -1, -1);
        if (PTL_OK != ret) {
            opal_output_verbose(10, mca_btl_portals_component.portals_output,
                                "PtlSetRank(handle, -1, -1) failed, returning %d",
                                ret);
            return OMPI_ERR_FATAL;
        }
    }

    return OMPI_SUCCESS;
}


int
mca_btl_portals_add_procs_compat(struct mca_btl_portals_module_t* btl,
                                 size_t nprocs, struct ompi_proc_t **procs,
                                 ptl_process_id_t **portals_procs)
{
    int ret;
    unsigned int nptl_procs, rank, i;

    if (use_modex) {
        int my_rid;
        ptl_process_id_t *proc_id;
        ptl_nid_t *nidmap = NULL;
        ptl_pid_t *pidmap = NULL;
        size_t j, size;
        ompi_proc_t* proc_self = ompi_proc_local();

        /* Create and set the NID / PID maps as needed */

        nidmap = malloc(sizeof(ptl_nid_t) * nprocs);
        pidmap = malloc(sizeof(ptl_pid_t) * nprocs);
        if (NULL == nidmap || NULL == pidmap) return OMPI_ERROR;

        for (j = 0 ; j < nprocs ; ++j) {
            if (proc_self == procs[j]) my_rid = j;

            ret = mca_pml_base_modex_recv(&mca_btl_portals_component.super.btl_version, 
                                          procs[j], (void**) &proc_id, &size);
            if (OMPI_SUCCESS != ret) {
                opal_output_verbose(10, mca_btl_portals_component.portals_output,
                                    "mca_pml_base_modex_recv failed: %d", ret);
                return ret;
            } else if (sizeof(ptl_process_id_t) != size) {
                opal_output_verbose(10, mca_btl_portals_component.portals_output,
                                    "mca_pml_base_modex_recv returned size %d, expected %d", 
                                    size, sizeof(ptl_process_id_t));
                return OMPI_ERROR;
            }

            nidmap[j] = ntohl(proc_id->nid);
            pidmap[j] = ntohl(proc_id->pid);

            free(proc_id);
        }

        PtlSetRank(mca_btl_portals_module.portals_ni_h, 
                   my_rid, (unsigned) nprocs);
        PtlSetNIDMap(mca_btl_portals_module.portals_ni_h, 
                     nidmap, (unsigned) nprocs);
        PtlSetPIDMap(mca_btl_portals_module.portals_ni_h, 
                     pidmap, (unsigned) nprocs);

        free(pidmap);
        free(nidmap);
    }

    ret = PtlGetRank(mca_btl_portals_module.portals_ni_h, &rank, &nptl_procs);
    if (ret != PTL_OK) {
        opal_output_verbose(10, mca_btl_portals_component.portals_output,
                            "PtlGetRank() returned %d", ret);
        return OMPI_ERR_FATAL;
    } else if (nptl_procs != nprocs) {
        opal_output_verbose(10, mca_btl_portals_component.portals_output,
                            "nptl_procs != nprocs (%d, %d)", nptl_procs,
                            nprocs);
        return OMPI_ERR_FATAL;
    }

    /* create enough space for all the proc info structs */
    *portals_procs = calloc(nprocs, sizeof(ptl_process_id_t));
    if (NULL == *portals_procs) {
        opal_output_verbose(10, mca_btl_portals_component.portals_output,
                            "calloc(nprocs, sizeof(ptl_process_id_t)) failed");
        return OMPI_ERR_TEMP_OUT_OF_RESOURCE;
    }
    /* fill in all the proc info structs */
    for (i = 0 ; i < nprocs ; ++i) {
        ret = PtlGetRankId(mca_btl_portals_module.portals_ni_h, 
                           i, &((*portals_procs)[i]));
        if (PTL_OK != ret) {
            opal_output_verbose(10,
                                mca_btl_portals_component.portals_output,
                                "PtlGetRankId(%d) failed: %d\n", i, ret);
            return OMPI_ERR_FATAL;
        }
    }

#if 1
    PtlNIDebug(mca_btl_portals_module.portals_ni_h, PTL_DBG_ALL);
#endif

    return OMPI_SUCCESS;
}

