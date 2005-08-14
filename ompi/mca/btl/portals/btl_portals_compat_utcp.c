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
#include <errno.h>
#include <netinet/in.h>

#include "ompi/include/constants.h"
#include "opal/util/output.h"

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
    ptl_process_id_t info;
    int ret, max_interfaces;
#if 0 /* send all the portals internal debug to a file or stderr */
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

    /* if the environment variables for the utcp implementation are
       already set, assume the user is running without the full Open
       RTE and is doing RTE testing for a more tightly-coupled
       platform (like, say, Red Storm).  Otherwise, be nice and use
       the modex to setup everything for the user */
    if (NULL == getenv("PTL_MY_RID")) {
        use_modex = true;
    } else {
        use_modex = false;
    }

    if (use_modex) {
        /* post our contact info in the registry */
        info.nid = htonl(utcp_my_nid(mca_btl_portals_component.portals_ifname));
        info.pid = htonl((ptl_pid_t) getpid());
        opal_output_verbose(100, mca_btl_portals_component.portals_output,
                            "contact info: %u, %u", ntohl(info.nid), 
                            ntohl(info.pid));

        ret = mca_pml_base_modex_send(&mca_btl_portals_component.super.btl_version,
                                      &info, sizeof(ptl_process_id_t));
        if (OMPI_SUCCESS != ret) {
            opal_output_verbose(10, mca_btl_portals_component.portals_output,
                                "mca_pml_base_modex_send failed: %d", ret);
            return ret;
        }
    } else {
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
                        NULL,              /* no need to have limits around */
                        &mca_btl_portals_module.portals_ni_h  /* our interface handle */
                        );
        if (PTL_OK != ret) {
            opal_output_verbose(10, mca_btl_portals_component.portals_output,
                                "PtlNIInit failed, returning %d\n", ret);
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

    if (use_modex) {
        int my_rid;
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
        if (NULL == nidmap || NULL == pidmap || 
            NULL == nid_str || NULL == pid_str)
            return OMPI_ERROR;

        /* get space for the portals procs list */
        *portals_procs = calloc(nprocs, sizeof(ptl_process_id_t));
        if (NULL == *portals_procs) {
            opal_output_verbose(10, mca_btl_portals_component.portals_output,
                                "calloc(nprocs, sizeof(ptl_process_id_t)) failed");
            return OMPI_ERR_TEMP_OUT_OF_RESOURCE;
        }
         
        for (i = 0 ; i < nprocs ; ++i) {
            if (proc_self == procs[i]) my_rid = i;

            ret = mca_pml_base_modex_recv(&mca_btl_portals_component.super.btl_version, 
                                          procs[i], (void**) &info, &size);
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

        opal_output_verbose(100, mca_btl_portals_component.portals_output,
                            "my rid: %u", my_rid);
        opal_output_verbose(100, mca_btl_portals_component.portals_output,
                            "nid map: %s", nidmap);
        opal_output_verbose(100, mca_btl_portals_component.portals_output,
                            "pid map: %s", pidmap);
        opal_output_verbose(100, mca_btl_portals_component.portals_output,
                            "iface: %s",
                            mca_btl_portals_component.portals_ifname);

        asprintf(&tmp, "PTL_MY_RID=%u", my_rid);
        putenv(tmp);
        asprintf(&tmp, "PTL_NIDMAP=%s", nidmap);
        putenv(tmp);
        asprintf(&tmp, "PTL_PIDMAP=%s", pidmap);
        putenv(tmp);
        asprintf(&tmp, "PTL_IFACE=%s", mca_btl_portals_component.portals_ifname);
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
            opal_output_verbose(10, mca_btl_portals_component.portals_output,
                                "PtlInit failed, returning %d\n", ret);
            return OMPI_ERR_FATAL;
        }

        ret = PtlNIInit(PTL_IFACE_DEFAULT, /* interface to initialize */
                        PTL_PID_ANY,       /* let library assign our pid */
                        NULL,              /* no desired limits */
                        NULL,    /* save our limits somewhere */
                        &(mca_btl_portals_module.portals_ni_h)  /* our interface handle */
                        );
        if (PTL_OK != ret) {
            opal_output_verbose(10, mca_btl_portals_component.portals_output,
                                "PtlNIInit failed, returning %d\n", ret);
            return OMPI_ERR_FATAL;
        }
    } else { /* use_modex */
        unsigned int nptl_procs, rank, i;

        /*
         * FIXME - XXX - FIXME 
         * BWB - implicit assumption that cnos procs list will match our
         *       procs list.  Don't know what to do about that...
         */
        ret = PtlGetRank(&rank, &nptl_procs);
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
            ret = PtlGetRankId(i, &((*portals_procs)[i]));
            if (PTL_OK != ret) {
                opal_output_verbose(10,
                                    mca_btl_portals_component.portals_output,
                                    "PtlGetRankId(%d) failed: %d\n", i, ret);
                return OMPI_ERR_FATAL;
            }
        }
    }

#if 0
    PtlNIDebug(mca_btl_portals_module.portals_ni_h, PTL_DBG_NI_ALL);
#endif

    return OMPI_SUCCESS;
}

