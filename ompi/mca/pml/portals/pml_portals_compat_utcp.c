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

#include "ompi/constants.h"
#include "opal/util/output.h"
#include "ompi/proc/proc.h"
#include "ompi/mca/pml/base/pml_base_module_exchange.h"

#include "pml_portals.h"
#include "pml_portals_compat.h"

#include <p3api/debug.h>

/* how's this for source code diving? - find private method for
   getting interface */
extern int p3tcp_my_nid(const char *if_str, unsigned int *nid);

static bool use_modex = true;

int
ompi_pml_portals_init_compat(void)
{
    ptl_process_id_t info;
    int ret, max_interfaces;

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
        unsigned int nid;

        p3tcp_my_nid(ompi_pml_portals.portals_ifname, &nid);

        /* post our contact info in the registry */
        info.nid = htonl(nid);
        info.pid = htonl((ptl_pid_t) getpid());
        opal_output_verbose(100, ompi_pml_portals.portals_output,
                            "contact info: %u, %u", ntohl(info.nid), 
                            ntohl(info.pid));

        ret = mca_pml_base_modex_send(&mca_pml_portals_component.pmlm_version,
                                      &info, sizeof(ptl_process_id_t));
        if (OMPI_SUCCESS != ret) {
            opal_output_verbose(10, ompi_pml_portals.portals_output,
                                "mca_pml_base_modex_send failed: %d", ret);
            return ret;
        }
    } else {
        /*
         * Initialize Portals interface
         */
        ret = PtlInit(&max_interfaces);
        if (PTL_OK != ret) {
            opal_output_verbose(10, ompi_pml_portals.portals_output,
                                "PtlInit failed, returning %d\n", ret);
            return OMPI_ERR_FATAL;
        }

        /* tell the UTCP runtime code to read the env variables */
        PtlSetRank(PTL_INVALID_HANDLE, -1, -1);

        /*
         * Initialize a network device
         */
        ret = PtlNIInit(PTL_IFACE_DEFAULT, /* interface to initialize */
                        PTL_PID_ANY,       /* let library assign our pid */
                        NULL,              /* no desired limits */
                        NULL,              /* no need to have limits around */
                        &ompi_pml_portals.portals_ni_h  /* our interface handle */
                        );
        if (PTL_OK != ret) {
            opal_output_verbose(10, ompi_pml_portals.portals_output,
                                "PtlNIInit failed, returning %d\n", ret);
            return OMPI_ERR_FATAL;
        }
    }

    return OMPI_SUCCESS;
}


int
ompi_pml_portals_add_procs_compat(struct ompi_proc_t **procs, size_t nprocs)
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
         
        for (i = 0 ; i < nprocs ; ++i) {
            if (proc_self == procs[i]) my_rid = i;

            ret = mca_pml_base_modex_recv(&mca_pml_portals_component.pmlm_version, 
                                          procs[i], (void**) &info, &size);
            if (OMPI_SUCCESS != ret) {
                opal_output_verbose(10, ompi_pml_portals.portals_output,
                                    "mca_pml_base_modex_recv failed: %d", ret);
                return ret;
            } else if (sizeof(ptl_process_id_t) != size) {
                opal_output_verbose(10, ompi_pml_portals.portals_output,
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

            /* update pml_proc information */
            ((ompi_pml_portals_proc_t*) procs[i]->proc_pml)->proc_id.nid = 
                ntohl(info->nid);
            ((ompi_pml_portals_proc_t*) procs[i]->proc_pml)->proc_id.pid = 
                ntohl(info->pid);

            free(info);
        }

        opal_output_verbose(100, ompi_pml_portals.portals_output,
                            "my rid: %u", my_rid);
        opal_output_verbose(100, ompi_pml_portals.portals_output,
                            "nid map: %s", nidmap);
        opal_output_verbose(100, ompi_pml_portals.portals_output,
                            "pid map: %s", pidmap);
        opal_output_verbose(100, ompi_pml_portals.portals_output,
                            "iface: %s",
                            ompi_pml_portals.portals_ifname);

        asprintf(&tmp, "PTL_MY_RID=%u", my_rid);
        putenv(tmp);
        asprintf(&tmp, "PTL_NIDMAP=%s", nidmap);
        putenv(tmp);
        asprintf(&tmp, "PTL_PIDMAP=%s", pidmap);
        putenv(tmp);
        asprintf(&tmp, "PTL_IFACE=%s", ompi_pml_portals.portals_ifname);
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
            opal_output_verbose(10, ompi_pml_portals.portals_output,
                                "PtlInit failed, returning %d\n", ret);
            return OMPI_ERR_FATAL;
        }

        /* tell the UTCP runtime code to read the env variables */
        PtlSetRank(PTL_INVALID_HANDLE, -1, -1);

        ret = PtlNIInit(PTL_IFACE_DEFAULT, /* interface to initialize */
                        PTL_PID_ANY,       /* let library assign our pid */
                        NULL,              /* no desired limits */
                        NULL,    /* save our limits somewhere */
                        &(ompi_pml_portals.portals_ni_h)  /* our interface handle */
                        );
        if (PTL_OK != ret) {
            opal_output_verbose(10, ompi_pml_portals.portals_output,
                                "PtlNIInit failed, returning %d\n", ret);
            return OMPI_ERR_FATAL;
        }
    } else { /* use_modex */
        unsigned int nptl_procs, rank, i;

        /*
         */
        ret = PtlGetRank(ompi_pml_portals.portals_ni_h, &rank, &nptl_procs);
        if (ret != PTL_OK) {
            opal_output_verbose(10, ompi_pml_portals.portals_output,
                                "PtlGetRank() returned %d", ret);
            return OMPI_ERR_FATAL;
        } else if (nptl_procs != nprocs) {
            opal_output_verbose(10, ompi_pml_portals.portals_output,
                                "nptl_procs != nprocs (%d, %d)", nptl_procs,
                                nprocs);
            return OMPI_ERR_FATAL;
        }

        /* fill in all the proc info structs */
        for (i = 0 ; i < nprocs ; ++i) {
            ret = PtlGetRankId(ompi_pml_portals.portals_ni_h, i,
                &(((ompi_pml_portals_proc_t*) procs[i]->proc_pml)->proc_id));
            if (PTL_OK != ret) {
                opal_output_verbose(10,
                                    ompi_pml_portals.portals_output,
                                    "PtlGetRankId(%d) failed: %d\n", i, ret);
                return OMPI_ERR_FATAL;
            }
        }
    }

#if 0
    PtlNIDebug(ompi_pml_portals.portals_ni_h, PTL_DBG_API |
               PTL_DBG_MOVE | PTL_DBG_DROP | PTL_DBG_REQUEST | 
               PTL_DBG_DELIVERY | PTL_DBG_MD | PTL_DBG_UNLINK |
               PTL_DBG_EQ | PTL_DBG_EVENT | PTL_DBG_MEMORY );
#endif

    return OMPI_SUCCESS;
}

