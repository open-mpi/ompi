/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2021 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2022 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2006-2009 University of Houston.  All rights reserved.
 * Copyright (c) 2009      Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2011-2015 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2013-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2013-2017 Intel, Inc. All rights reserved.
 * Copyright (c) 2014-2020 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2018      Amazon.com, Inc. or its affiliates.  All Rights reserved.
 * Copyright (c) 2021-2022 Nanook Consulting.  All rights reserved.
 * Copyright (c) 2018-2022 Triad National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2022      IBM Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "ompi/constants.h"

#include <string.h>
#include <stdio.h>
#include <ctype.h>
#include <time.h>
#if HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#include <fcntl.h>

#include "opal/util/alfg.h"
#include "opal/util/argv.h"
#include "opal/util/opal_getcwd.h"
#include "opal/util/opal_environ.h"
#include "opal/util/path.h"
#include "opal/util/proc.h"
#include "opal/util/show_help.h"
#include "opal/util/printf.h"
#include "opal/mca/hwloc/base/base.h"
#include "opal/mca/pmix/base/base.h"

#include "ompi/communicator/communicator.h"
#include "ompi/group/group.h"
#include "ompi/proc/proc.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/runtime/ompi_rte.h"
#include "ompi/info/info.h"

#include "ompi/dpm/dpm.h"

static opal_rng_buff_t rnd;

typedef struct {
    ompi_communicator_t       *comm;
    int                       size;
    struct ompi_request_t     **reqs;
    int                       buf;
} ompi_dpm_disconnect_obj;
static int disconnect_waitall (int count, ompi_dpm_disconnect_obj **objs);
static ompi_dpm_disconnect_obj *disconnect_init(ompi_communicator_t *comm);
static int start_dvm(char **hostfiles, char **dash_host);

typedef struct {
    opal_list_item_t super;
    ompi_proc_t *p;
} ompi_dpm_proct_caddy_t;
static OBJ_CLASS_INSTANCE(ompi_dpm_proct_caddy_t,
                          opal_list_item_t,
                          NULL, NULL);

/*
 * Init the module
 */
int ompi_dpm_init(void)
{
    time_t now;

    /* seed our random number generator */
    now = time(NULL);
    if (!opal_srand(&rnd, now)) {
        return OMPI_ERROR;
    }
    return OMPI_SUCCESS;
}

static int compare_pmix_proc(const void *a, const void *b)
{
    const pmix_proc_t *proc_a = (pmix_proc_t *)a;
    const pmix_proc_t *proc_b = (pmix_proc_t *)b;

    int nspace_dif = strncmp(proc_a->nspace, proc_b->nspace, PMIX_MAX_NSLEN);
    if (nspace_dif != 0)
        return nspace_dif;

    return proc_a->rank - proc_b->rank;
}

int ompi_dpm_connect_accept(ompi_communicator_t *comm, int root,
                            const char *port_string, bool send_first,
                            ompi_communicator_t **newcomm)
{
    int k, size, rsize, rank, rc, rportlen=0;
    char **members = NULL, *nstring, *rport=NULL, *key, *pkey;
    bool dense, isnew;
    opal_process_name_t pname;
    opal_list_t ilist, mlist, rlist;
    pmix_info_t info, tinfo;
    pmix_value_t pval;
    pmix_pdata_t pdat;
    pmix_proc_t *procs, pxproc;
    size_t nprocs, n;
    pmix_status_t pret;
    opal_proclist_t *plt;

    ompi_communicator_t *newcomp=MPI_COMM_NULL;
    ompi_proc_t *proc;
    ompi_group_t *group=comm->c_local_group;
    ompi_proc_t **proc_list=NULL, **new_proc_list = NULL;
    int32_t i;
    ompi_group_t *new_group_pointer;
    ompi_dpm_proct_caddy_t *cd;

    /* set default error return */
    *newcomm = MPI_COMM_NULL;

    size = ompi_comm_size ( comm );
    rank = ompi_comm_rank ( comm );

    /* the "send_first" end will append ":connect" to the port name and publish
     * the list of its participating procs on that key. The receiving root proc
     * will append ":accept" to the port name and publish the list of its
     * participants on that key. Each proc will then block waiting for lookup
     * to complete on the other's key. Once that completes, the list of remote
     * procs is used to complete construction of the intercommunicator. */

    /* If there was an error during the COMM_SPAWN stage, the port string will
     * be set (in mpi/c/comm_spawn.c) with a special value that contains the error
     * code. Extract said error code, and store it in rportlen, as this value will
     * be exchanged with other peers on comm. We need to perform this exchange even
     * in error cases to avoid leaving some processes deadlock waiting on the
     * root to broadcast.
     */
    if (NULL != port_string && strstr(port_string, ":error=")) {
        /* we will set the rportlen to a negative value corresponding to the
         * error code produced by pmix spawn */
        char *value = strrchr(port_string, '=');
        assert(NULL != value);
        rportlen = atoi(++value);
        if (rportlen > 0) rportlen *= -1;
        goto bcast_rportlen;
    }

    /* everyone constructs the list of members from their communicator */
    pname.jobid = OMPI_PROC_MY_NAME->jobid;
    pname.vpid = OPAL_VPID_WILDCARD;
    if (MPI_COMM_WORLD == comm) {
        PMIX_LOAD_PROCID(&pxproc, ompi_process_info.myprocid.nspace, PMIX_RANK_WILDCARD);
        OPAL_PMIX_CONVERT_PROCT_TO_STRING(&nstring, &pxproc);
        opal_argv_append_nosize(&members, nstring);
        free(nstring);
        /* add the number of procs in this job */
        (void)opal_asprintf(&nstring, "%d", size);
        opal_argv_append_nosize(&members, nstring);
        free(nstring);
    } else {
        if (OMPI_GROUP_IS_DENSE(group)) {
            proc_list = group->grp_proc_pointers;
            dense = true;
        } else {
            proc_list = (ompi_proc_t**)calloc(group->grp_proc_count,
                                              sizeof(ompi_proc_t *));
            for (i=0 ; i<group->grp_proc_count ; i++) {
                if (NULL == (proc_list[i] = ompi_group_peer_lookup(group,i))) {
                    OMPI_ERROR_LOG(OMPI_ERR_NOT_FOUND);
                    rc = OMPI_ERR_NOT_FOUND;
                    free(proc_list);
                    goto exit;
                }
            }
            dense = false;
        }
        for (i=0; i < size; i++) {
            opal_process_name_t proc_name;
            if (ompi_proc_is_sentinel (proc_list[i])) {
                proc_name = ompi_proc_sentinel_to_name ((uintptr_t) proc_list[i]);
            } else {
                proc_name = proc_list[i]->super.proc_name;
            }
            OPAL_PMIX_CONVERT_NAME(&pxproc, &proc_name);
            OPAL_PMIX_CONVERT_PROCT_TO_STRING(&nstring, &pxproc);
            opal_argv_append_nosize(&members, nstring);
            free(nstring);
        }
        if (!dense) {
            free(proc_list);
            proc_list = NULL;
        }
    }

    if (rank == root) {
        /* the roots for each side exchange their list of participants */
        if (send_first) {
            (void)opal_asprintf(&key, "%s:connect", port_string);
            (void)opal_asprintf(&pkey, "%s:accept", port_string);
        } else {
            (void)opal_asprintf(&key, "%s:accept", port_string);
            (void)opal_asprintf(&pkey, "%s:connect", port_string);
        }
        nstring = opal_argv_join(members, ':');
        PMIX_INFO_LOAD(&info, key, nstring, PMIX_STRING);
        PMIX_LOAD_KEY(pdat.key, pkey);
        free(nstring);
        free(key);
        free(pkey);

        rc = opal_pmix_base_exchange(&info, &pdat, 600);  // give them 10 minutes
        PMIX_INFO_DESTRUCT(&info);
        if (OPAL_SUCCESS != rc) {
            PMIX_PDATA_DESTRUCT(&pdat);
            return rc;
        }

        /* save the result */
        rport = strdup(pdat.value.data.string);  // need this later
        rportlen = strlen(rport) + 1;  // retain the NULL terminator
        PMIX_PDATA_DESTRUCT(&pdat);
    }

bcast_rportlen:
    /* if we aren't in a comm_spawn, the non-root members won't have
     * the port_string - so let's make sure everyone knows the other
     * side's participants */

    /* bcast the list-length to all processes in the local comm */
    rc = comm->c_coll->coll_bcast(&rportlen, 1, MPI_INT, root, comm,
                                 comm->c_coll->coll_bcast_module);
    if (OMPI_SUCCESS != rc) {
        free(rport);
        goto exit;
    }

    /* This is the comm_spawn error case: the root couldn't do the pmix spawn
     * and is now propagating to the local group that this operation has to
     * fail. */
    if (0 >= rportlen) {
        rc = rportlen;
        /* no need to free here, the root has already done it and everyone else has not yet allocated the rport array */
        goto exit;
    }

    if (rank != root) {
        /* non root processes need to allocate the buffer manually */
        rport = (char*)malloc(rportlen);
        if (NULL == rport) {
            rc = OMPI_ERR_OUT_OF_RESOURCE;
            goto exit;
        }
    }
    /* now share the list of remote participants */
    rc = comm->c_coll->coll_bcast(rport, rportlen, MPI_BYTE, root, comm,
                                 comm->c_coll->coll_bcast_module);
    if (OMPI_SUCCESS != rc) {
        free(rport);
        goto exit;
    }

    /* initiate a list of participants for the connect,
     * starting with our own members */
    OBJ_CONSTRUCT(&mlist, opal_list_t);
    assert(NULL != members /* would mean comm had 0-sized group! */);
    for (i=0; NULL != members[i]; i++) {
        OPAL_PMIX_CONVERT_STRING_TO_PROCT(&pxproc, members[i]);
        plt = OBJ_NEW(opal_proclist_t);
        memcpy(&plt->procid, &pxproc, sizeof(pmix_proc_t));
        opal_list_append(&mlist, &plt->super);
        /* if the rank is wildcard, then we need to skip
         * the next position */
        if (PMIX_RANK_WILDCARD == pxproc.rank) {
            ++i;
        }
    }
    opal_argv_free(members);
    members = NULL;
    /* rport contains a colon-delimited list
     * of process names for the remote procs - convert it
     * into an argv array */
    members = opal_argv_split(rport, ':');
    free(rport);

    /* add the list of remote procs to our list, and
     * keep a list of them for later */
    OBJ_CONSTRUCT(&ilist, opal_list_t);
    OBJ_CONSTRUCT(&rlist, opal_list_t);

    for (i=0; NULL != members[i]; i++) {
        OPAL_PMIX_CONVERT_STRING_TO_PROCT(&pxproc, members[i]);
        plt = OBJ_NEW(opal_proclist_t);
        memcpy(&plt->procid, &pxproc, sizeof(pmix_proc_t));
        opal_list_append(&mlist, &plt->super);

        if (PMIX_RANK_WILDCARD == pxproc.rank) {
            /* if the rank is wildcard, then we are including all ranks
             * of that job, and the next entry in members should be the
             * number of procs in the job */
            if (NULL == members[i+1]) {
                /* just protect against the error */
                OMPI_ERROR_LOG(OMPI_ERR_BAD_PARAM);
                opal_argv_free(members);
                OPAL_LIST_DESTRUCT(&ilist);
                OPAL_LIST_DESTRUCT(&rlist);
                OPAL_LIST_DESTRUCT(&mlist);
                rc = OMPI_ERR_BAD_PARAM;
                goto exit;
            }
            rsize = strtoul(members[i+1], NULL, 10);
            ++i;
            for (k=0; k < rsize; k++) {
                pxproc.rank = k;
                OPAL_PMIX_CONVERT_PROCT(rc, &pname, &pxproc);
                if (OPAL_SUCCESS != rc) {
                    OMPI_ERROR_LOG(rc);
                    opal_argv_free(members);
                    OPAL_LIST_DESTRUCT(&ilist);
                    OPAL_LIST_DESTRUCT(&rlist);
                    OPAL_LIST_DESTRUCT(&mlist);
                    goto exit;
                }
                /* see if this needs to be added to our ompi_proc_t array */
                proc = ompi_proc_find_and_add(&pname, &isnew);
                if (isnew) {
                    cd = OBJ_NEW(ompi_dpm_proct_caddy_t);
                    cd->p = proc;
                    opal_list_append(&ilist, &cd->super);
                }
                /* either way, add to the remote list */
                cd = OBJ_NEW(ompi_dpm_proct_caddy_t);
                cd->p = proc;
                opal_list_append(&rlist, &cd->super);
            }
        } else {
            OPAL_PMIX_CONVERT_PROCT(rc, &pname, &pxproc);
            if (OPAL_SUCCESS != rc) {
                OMPI_ERROR_LOG(rc);
                opal_argv_free(members);
                OPAL_LIST_DESTRUCT(&ilist);
                OPAL_LIST_DESTRUCT(&rlist);
                OPAL_LIST_DESTRUCT(&mlist);
                goto exit;
            }
            /* see if this needs to be added to our ompi_proc_t array */
            proc = ompi_proc_find_and_add(&pname, &isnew);
            if (isnew) {
                cd = OBJ_NEW(ompi_dpm_proct_caddy_t);
                cd->p = proc;
                opal_list_append(&ilist, &cd->super);
            }
            /* either way, add to the remote list */
            cd = OBJ_NEW(ompi_dpm_proct_caddy_t);
            cd->p = proc;
            opal_list_append(&rlist, &cd->super);
        }
    }
    opal_argv_free(members);

    /* convert the list of members to a pmix_proc_t array */
    nprocs = opal_list_get_size(&mlist);
    PMIX_PROC_CREATE(procs, nprocs);
    n = 0;
    OPAL_LIST_FOREACH(plt, &mlist, opal_proclist_t) {
        memcpy(&procs[n], &plt->procid, sizeof(pmix_proc_t));
        ++n;
    }
    OPAL_LIST_DESTRUCT(&mlist);

    /* tell the host RTE to connect us - this will download
     * all known data for the nspace's of participating procs
     * so that add_procs will not result in a slew of lookups */
    PMIX_INFO_CONSTRUCT(&tinfo);
    PMIX_INFO_LOAD(&tinfo, PMIX_TIMEOUT, &ompi_pmix_connect_timeout, PMIX_UINT32);

    /*
     * sort procs so that all ranks call PMIx_Connect() with the processes in same order
     */
    qsort(procs, nprocs, sizeof(pmix_proc_t), compare_pmix_proc);
    pret = PMIx_Connect(procs, nprocs, &tinfo, 1);
    PMIX_INFO_DESTRUCT(&tinfo);
    PMIX_PROC_FREE(procs, nprocs);
    rc = opal_pmix_convert_status(pret);
    if (OPAL_SUCCESS != rc) {
        OMPI_ERROR_LOG(rc);
        OPAL_LIST_DESTRUCT(&ilist);
        OPAL_LIST_DESTRUCT(&rlist);
        goto exit;
    }
    if (!opal_list_is_empty(&ilist)) {
        int prn, nprn = 0;
        char *val;
        opal_process_name_t wildcard_rank;
        i = 0;  /* start from the begining */

        /* convert the list of new procs to a proc_t array */
        new_proc_list = (ompi_proc_t**)calloc(opal_list_get_size(&ilist),
                                              sizeof(ompi_proc_t *));
        /* Extract the modex info for the first proc on the ilist, and then
         * remove all processors in the same jobid from the list by getting
         * their connection information and moving them into the proc array.
         */
        do {
            uint32_t *local_ranks_in_jobid = NULL;
            ompi_dpm_proct_caddy_t* next = NULL;
            cd = (ompi_dpm_proct_caddy_t*)opal_list_get_first(&ilist);
            proc = cd->p;
            wildcard_rank.jobid = proc->super.proc_name.jobid;
            wildcard_rank.vpid = OMPI_NAME_WILDCARD->vpid;
            /* retrieve the local peers for the specified jobid */
            OPAL_MODEX_RECV_VALUE_OPTIONAL(rc, PMIX_LOCAL_PEERS,
                                           &wildcard_rank, &val, PMIX_STRING);
            if (OPAL_SUCCESS == rc && NULL != val) {
                char **peers = opal_argv_split(val, ',');
                free(val);
                nprn = opal_argv_count(peers);
                local_ranks_in_jobid = (uint32_t*)calloc(nprn, sizeof(uint32_t));
                for (prn = 0; NULL != peers[prn]; prn++) {
                    local_ranks_in_jobid[prn] = strtoul(peers[prn], NULL, 10);
                }
                opal_argv_free(peers);
            }

            OPAL_LIST_FOREACH_SAFE(cd, next, &ilist, ompi_dpm_proct_caddy_t) {
                proc = cd->p;
                if( proc->super.proc_name.jobid != wildcard_rank.jobid )
                    continue;  /* not a proc from this jobid */

                new_proc_list[i] = proc;
                opal_list_remove_item(&ilist, (opal_list_item_t*)cd);  // TODO: do we need to release cd ?
                OBJ_RELEASE(cd);
                /* ompi_proc_complete_init_single() initializes and optionally retrieves
                 * OPAL_PMIX_LOCALITY and OPAL_PMIX_HOSTNAME. since we can live without
                 * them, we are just fine */
                ompi_proc_complete_init_single(proc);
                /* if this proc is local, then get its locality */
                if (NULL != local_ranks_in_jobid) {
                    uint16_t u16;
                    for (prn=0; prn < nprn; prn++) {
                        if (local_ranks_in_jobid[prn] == proc->super.proc_name.vpid) {
                            /* get their locality string */
                            val = NULL;
                            OPAL_MODEX_RECV_VALUE_IMMEDIATE(rc, PMIX_LOCALITY_STRING,
                                                           &proc->super.proc_name, &val, PMIX_STRING);
                            if (OPAL_SUCCESS == rc && NULL != ompi_process_info.locality) {
                                u16 = opal_hwloc_compute_relative_locality(ompi_process_info.locality, val);
                                free(val);
                            } else {
                                /* all we can say is that it shares our node */
                                u16 = OPAL_PROC_ON_CLUSTER | OPAL_PROC_ON_CU | OPAL_PROC_ON_NODE;
                            }
                            proc->super.proc_flags = u16;
                            /* save the locality for later */
                            OPAL_PMIX_CONVERT_NAME(&pxproc, &proc->super.proc_name);
                            pval.type = PMIX_UINT16;
                            pval.data.uint16 = proc->super.proc_flags;
                            PMIx_Store_internal(&pxproc, PMIX_LOCALITY, &pval);
                            break;
                        }
                    }
                }
                ++i;
            }
            if (NULL != local_ranks_in_jobid) {
                free(local_ranks_in_jobid);
            }
        } while (!opal_list_is_empty(&ilist));

        /* call add_procs on the new ones */
        rc = MCA_PML_CALL(add_procs(new_proc_list, opal_list_get_size(&ilist)));
        free(new_proc_list);
        new_proc_list = NULL;
        if (OMPI_SUCCESS != rc) {
            OMPI_ERROR_LOG(rc);
            OPAL_LIST_DESTRUCT(&ilist);
            goto exit;
        }
    }
    OPAL_LIST_DESTRUCT(&ilist);

    /* now deal with the remote group */
    rsize = opal_list_get_size(&rlist);
    new_group_pointer=ompi_group_allocate(NULL, rsize);
    if (NULL == new_group_pointer) {
        rc = OMPI_ERR_OUT_OF_RESOURCE;
        OPAL_LIST_DESTRUCT(&rlist);
        goto exit;
    }
    /* assign group elements */
    i=0;
    OPAL_LIST_FOREACH(cd, &rlist, ompi_dpm_proct_caddy_t) {
        new_group_pointer->grp_proc_pointers[i++] = cd->p;
        /* retain the proc */
        OBJ_RETAIN(cd->p);
    }
    OPAL_LIST_DESTRUCT(&rlist);

    /* set up communicator structure */
    rc = ompi_comm_set ( &newcomp,                 /* new comm */
                         comm,                     /* old comm */
                         group->grp_proc_count,    /* local_size */
                         NULL,                     /* local_procs */
                         rsize,                    /* remote_size */
                         NULL  ,                   /* remote_procs */
                         NULL,                     /* attrs */
                         comm->error_handler,      /* error handler */
                         group,                    /* local group */
                         new_group_pointer,        /* remote group */
                         0);                       /* flags */
    if (OMPI_SUCCESS != rc) {
        goto exit;
    }

    OBJ_RELEASE(new_group_pointer);
    new_group_pointer = MPI_GROUP_NULL;

    /* allocate comm_cid */
    rc = ompi_comm_nextcid ( newcomp,                   /* new communicator */
                             comm,                      /* old communicator */
                             NULL,                      /* bridge comm */
                             &root,                     /* local leader */
                             (void*)port_string,        /* rendezvous point */
                             send_first,                /* send or recv first */
                             OMPI_COMM_CID_INTRA_PMIX); /* mode */
    if (OMPI_SUCCESS != rc) {
        goto exit;
    }

    /* activate comm and init coll-component */
    rc = ompi_comm_activate ( &newcomp,                  /* new communicator */
                              comm,                      /* old communicator */
                              NULL,                      /* bridge comm */
                              &root,                     /* local leader */
                              (void*)port_string,        /* rendezvous point */
                              send_first,                /* send or recv first */
                              OMPI_COMM_CID_INTRA_PMIX); /* mode */
    if (OMPI_SUCCESS != rc) {
        goto exit;
    }

    /* Question: do we have to re-start some low level stuff
       to enable the usage of fast communication devices
       between the two worlds ?
    */

 exit:
    if (OMPI_SUCCESS != rc) {
        if (MPI_COMM_NULL != newcomp && NULL != newcomp) {
            OBJ_RELEASE(newcomp);
            newcomp = MPI_COMM_NULL;
        }
    }

    *newcomm = newcomp;
    return rc;
}

static int construct_peers(ompi_group_t *group, opal_list_t *peers)
{
    int i;
    opal_namelist_t *nm, *n2;
    ompi_proc_t *proct;
    opal_process_name_t proc_name;

    for (i=0; i < group->grp_proc_count; i++) {
        if (OMPI_GROUP_IS_DENSE(group)) {
            proct = group->grp_proc_pointers[i];
        } else {
            proct = ompi_group_peer_lookup(group, i);
        }
        if (NULL == proct) {
            OMPI_ERROR_LOG(OMPI_ERR_NOT_FOUND);
            return OMPI_ERR_NOT_FOUND;
        }
        if (ompi_proc_is_sentinel (proct)) {
            proc_name = ompi_proc_sentinel_to_name ((uintptr_t)proct);
        } else {
            proc_name = proct->super.proc_name;
        }

        /* add to the list of peers */
        nm = OBJ_NEW(opal_namelist_t);
        nm->name = proc_name;
        /* need to maintain an ordered list to ensure the tracker signatures
         * match across all procs */
        OPAL_LIST_FOREACH(n2, peers, opal_namelist_t) {
            if (opal_compare_proc(nm->name, n2->name) < 0) {
                opal_list_insert_pos(peers, &n2->super, &nm->super);
                nm = NULL;
                break;
            }
        }
        if (NULL != nm) {
            /* append to the end */
            opal_list_append(peers, &nm->super);
        }
    }
    return OMPI_SUCCESS;
}

int ompi_dpm_disconnect(ompi_communicator_t *comm)
{
    int ret;
    pmix_status_t rc;
    ompi_group_t *group;
    opal_list_t coll;
    opal_namelist_t *nm;
    pmix_proc_t *procs;
    size_t nprocs, n;

    /* Note that we explicitly use an RTE-based barrier (vs. an MPI
       barrier).  See a lengthy comment in
       ompi/runtime/ompi_mpi_finalize.c for a much more detailed
       rationale. */

    /* setup the collective */
    OBJ_CONSTRUCT(&coll, opal_list_t);
    /* RHC: assuming for now that this must flow across all
     * local and remote group members */
    group = comm->c_local_group;
    if (OMPI_SUCCESS != (ret = construct_peers(group, &coll))) {
        OMPI_ERROR_LOG(ret);
        OPAL_LIST_DESTRUCT(&coll);
        return ret;
    }
    /* do the same for the remote group */
    group = comm->c_remote_group;
    if (OMPI_SUCCESS != (ret = construct_peers(group, &coll))) {
        OMPI_ERROR_LOG(ret);
        OPAL_LIST_DESTRUCT(&coll);
        return ret;
    }
    nprocs = opal_list_get_size(&coll);
    PMIX_PROC_CREATE(procs, nprocs);
    n = 0;
    OPAL_LIST_FOREACH(nm, &coll, opal_namelist_t) {
        OPAL_PMIX_CONVERT_NAME(&procs[n], &nm->name);
        ++n;
    }
    OPAL_LIST_DESTRUCT(&coll);

    /* ensure we tell the host RM to disconnect us - this
     * is a blocking operation so just use a fence */
    if (PMIX_SUCCESS != (rc = PMIx_Fence(procs, nprocs, NULL, 0))) {
        ret = opal_pmix_convert_status(rc);
        OMPI_ERROR_LOG(ret);
        PMIX_PROC_FREE(procs, nprocs);
        return ret;
    }
    PMIX_PROC_FREE(procs, nprocs);

    return ret;
}

typedef struct {
    char *name;
    char **conflicts;
} dpm_conflicts_t;

static dpm_conflicts_t mapby_modifiers[] = {
    {.name = "oversubscribe", .conflicts = (char *[]){"nooversubscribe", NULL}},
    {.name = "nooversubscribe", .conflicts = (char *[]){"oversubscribe", NULL}},
    {.name = ""}
};

static dpm_conflicts_t rankby_modifiers[] = {
    {.name = ""}
};

static dpm_conflicts_t bindto_modifiers[] = {
    {.name = ""}
};

static int check_modifiers(const char *modifier, char **checks, dpm_conflicts_t *conflicts)
{
    int n, m, k;

    for (n=0; 0 != strlen(conflicts[n].name); n++) {
        if (0 == strcasecmp(conflicts[n].name, modifier)) {
            for (m=0; NULL != checks[m]; m++) {
                for (k=0; NULL != conflicts[n].conflicts[k]; k++) {
                    if (0 == strcasecmp(checks[m], conflicts[n].conflicts[k])) {
                        return OMPI_ERR_BAD_PARAM;
                    }
                }
            }
            break;
        }
    }
    return OMPI_SUCCESS;
}

static int dpm_convert(opal_list_t *infos,
                       const char *infokey,
                       const char *option,
                       const char *directive,
                       const char *modifier,
                       bool deprecated)
{
    opal_info_item_t *iptr;
    char *ck, *ptr, *help_str = NULL;
    int rc;
    char **tmp;
    dpm_conflicts_t *modifiers = NULL;
    const char *attr;

    /* pick the modifiers to be checked */
    if (NULL != modifier) {
        if (0 == strcmp(option, PMIX_MAPBY)) {
            modifiers = mapby_modifiers;
        } else if (0 == strcmp(option, PMIX_RANKBY)) {
            modifiers = rankby_modifiers;
        } else if (0 == strcmp(option, PMIX_BINDTO)) {
            modifiers = bindto_modifiers;
        } else  {
            return OMPI_ERR_BAD_PARAM;
        }
    }

    /* does the matching option already exist? */
    OPAL_LIST_FOREACH(iptr, infos, opal_info_item_t) {
        if (PMIX_CHECK_KEY(&iptr->info, option)) {
            ck = strdup(iptr->info.value.data.string);
            if (NULL != (ptr = strchr(ck, ':'))) {
                *ptr = '\0';
                ++ptr;
            }
             /* were we given a directive? */
            if (NULL != directive) {
                /* does it conflict? */
               if (0 != strncasecmp(ck, directive, strlen(directive))) {
                    opal_asprintf(&help_str, "Conflicting directives \"%s %s\"", ck, directive);
#if PMIX_NUMERIC_VERSION >= 0x00040000
                    attr = PMIx_Get_attribute_string(option);
#else
                    attr = option;
#endif
                    opal_show_help("help-dpm.txt", "deprecated-fail", true,
                                   infokey, attr, help_str);
                    free(help_str);
                    free(ck);
                    return OMPI_ERR_BAD_PARAM;
                }
                /* if they match, then nothing further to do */
            }
            /* were we given a modifier? */
            if (NULL != modifier) {
                if (NULL == ptr) {
                    /* no modifiers in the existing directive - just add the new one */
                    opal_asprintf(&ptr, "%s:%s", ck, modifier);
                    free(iptr->info.value.data.string);
                    iptr->info.value.data.string = ptr;
                    free(ck);
                    return OMPI_SUCCESS;
                } else {
                    /* we already have modifiers - need to check for conflict with
                     * the one we were told to add */
                    tmp = opal_argv_split(ptr, ',');
                    rc = check_modifiers(modifier, tmp, modifiers);
                    opal_argv_free(tmp);
                    if (OMPI_SUCCESS != rc) {
                        /* we have a conflict */
                        opal_asprintf(&ptr, "  Option %s\n  Conflicting modifiers \"%s %s\"", option, infokey, modifier);
#if PMIX_NUMERIC_VERSION >= 0x00040000
                        /* TODO: remove strdup if PMIx_Get_attribute_string takes const char* */
                        char *option_dup = strdup(option);
                        attr = PMIx_Get_attribute_string(option_dup);
                        free(option_dup);
#else
                        attr = option;
#endif
                        opal_show_help("help-dpm.txt", "deprecated-fail", true,
                                       infokey, attr, ptr);
                        free(ptr);
                        free(ck);
                        return OMPI_ERR_BAD_PARAM;
                    }
                    /* add the modifier to the end */
                    opal_asprintf(&ptr, "%s,%s", iptr->info.value.data.string, modifier);
                    free(iptr->info.value.data.string);
                    iptr->info.value.data.string = ptr;
                    free(ck);
                    opal_show_help("help-dpm.txt", "deprecated-converted", true,
                                   infokey, iptr->info.value.data.string);
                    return OMPI_SUCCESS;
                }
            }
            free(ck);
        }
    }

    /**** Get here if the specified option is not found in the
     **** current list - add it
     ****/
    if (NULL == directive && NULL == modifier) {
        return OMPI_ERR_BAD_PARAM;
    } else if (NULL == directive) {
        opal_asprintf(&ptr, ":%s", modifier);
    } else if (NULL == modifier) {
        ptr = strdup(directive);
    } else {
        opal_asprintf(&ptr, "%s:%s", directive, modifier);
    }
    iptr = OBJ_NEW(opal_info_item_t);
    PMIX_INFO_LOAD(&iptr->info, option, ptr, PMIX_STRING);
    opal_list_append(infos, &iptr->super);

    /* alert them */
    if(deprecated) {
        opal_asprintf(&help_str, "Key: %s Value: %s", option, ptr);
        opal_show_help("help-dpm.txt", "deprecated-converted", true,
                       infokey, help_str);
    }
    free(help_str);
    free(ptr);

    return OMPI_SUCCESS;
}


int ompi_dpm_spawn(int count, const char *array_of_commands[],
                   char **array_of_argv[],
                   const int array_of_maxprocs[],
                   const MPI_Info array_of_info[],
                   const char *port_name)
{
    int rc, i, j;
    int have_wdir=0;
    int flag=0;
    opal_cstring_t *info_str;
    uint32_t ui32;
    bool personality = false;
    char *tmp;
    pmix_app_t *apps, *app;
    opal_list_t job_info;
    opal_list_t app_info;
    opal_info_item_t *info;
    bool local_spawn, non_mpi;
    char **envars;
    size_t ninfo, n;
    pmix_info_t *pinfo = NULL;
    pmix_status_t pret;
    pmix_nspace_t nspace;
    size_t scount = count;
    char **hostfiles = NULL, **dash_host = NULL;
#if PMIX_NUMERIC_VERSION >= 0x00040000
    const char *checkkey;
#endif

    /* parse the info object */
    /* check potentially for
      Standard keys:
       - "arch": desired architecture
       - "wdir": directory, where executable can be found
       - "path": list of directories where to look for the executable
       - "file": filename, where additional information is provided.
       - "soft": see page 92 of MPI-2.
       - "host": desired host where to spawn the processes
       - "mpi_initial_errhandler": the error handler attached to predefined communicators.
      Non-standard keys:
       - "hostfile": hostfile containing hosts where procs are
       to be spawned
       - "add-host": add the specified hosts to the known list
       of available resources and spawn these
       procs on them
       - "add-hostfile": add the hosts in the hostfile to the
       known list of available resources and spawn
       these procs on them
       - "env": a newline-delimited list of envar values to be
       placed into the app's environment (of form "foo=bar")
       - "ompi_prefix": the path to the root of the directory tree where ompi
       executables and libraries can be found on all nodes
       used to spawn these procs
       - "mapper": indicate the mapper to be used for the job
       - "display_map": display the map of the spawned job
       - "npernode": number of procs/node to spawn
       - "pernode": spawn one proc/node
       - "ppr": spawn specified number of procs per specified object
       - "map_by": specify object by which the procs should be mapped
       - "rank_by": specify object by which the procs should be ranked
       - "bind_to": specify object to which the procs should be bound
       - "ompi_preload_binary": move binaries to nodes prior to execution
       - "ompi_preload_files": move specified files to nodes prior to execution
       - "ompi_non_mpi": spawned job will not call MPI_Init
       - "ompi_param": list of MCA params to be in the spawned job's environment
    */

    /* setup the job object */
    OBJ_CONSTRUCT(&job_info, opal_list_t);
    PMIX_APP_CREATE(apps, scount);

    /* Convert the list of commands to array of pmix_app_t */
    for (i = 0; i < count; ++i) {
        app = &apps[i];
        OBJ_CONSTRUCT(&app_info, opal_list_t);
        /* copy over the name of the executable */
        app->cmd = strdup(array_of_commands[i]);
        opal_argv_append_nosize(&app->argv, app->cmd);

        /* record the number of procs to be generated */
        app->maxprocs = array_of_maxprocs[i];

        /* copy over the argv array */
        if (MPI_ARGVS_NULL != array_of_argv &&
            MPI_ARGV_NULL != array_of_argv[i]) {
            for (j=0; NULL != array_of_argv[i][j]; j++) {
                opal_argv_append_nosize(&app->argv, array_of_argv[i][j]);
            }
        }

        /* Add environment variable with the contact information for the
           child processes.
        */
        opal_setenv("OMPI_PARENT_PORT", port_name, true, &app->env);
        for (j = 0; NULL != environ[j]; ++j) {
            if (0 == strncmp(OPAL_MCA_PREFIX, environ[j], strlen(OPAL_MCA_PREFIX))) {
                opal_argv_append_nosize(&app->env, environ[j]);
            }
        }

        /* Check for well-known info keys */
        have_wdir = 0;
        if ( array_of_info != NULL && array_of_info[i] != MPI_INFO_NULL ) {
            /* check for personality - this is a job-level key */
            ompi_info_get (array_of_info[i], "personality", &info_str, &flag);
            if ( flag ) {
                /* deprecate --> PMIX_PERSONALITY */
                opal_show_help("help-dpm.txt", "deprecated-converted", true,
                                "personality", "PMIX_PERSONALITY");
                personality = true;
                info = OBJ_NEW(opal_info_item_t);
                PMIX_INFO_LOAD(&info->info, PMIX_PERSONALITY, info_str->string, PMIX_STRING);
                opal_list_append(&job_info, &info->super);
                OBJ_RELEASE(info_str);
            }
            ompi_info_get (array_of_info[i], "PMIX_PERSONALITY", &info_str, &flag);
            if ( flag ) {
                personality = true;
                info = OBJ_NEW(opal_info_item_t);
                PMIX_INFO_LOAD(&info->info, PMIX_PERSONALITY, info_str->string, PMIX_STRING);
                opal_list_append(&job_info, &info->super);
                OBJ_RELEASE(info_str);
            }
#if PMIX_NUMERIC_VERSION >= 0x00040000
            checkkey = PMIx_Get_attribute_string("PMIX_PERSONALITY");
            ompi_info_get (array_of_info[i], checkkey, &info_str, &flag);
            if ( flag ) {
                personality = true;
                info = OBJ_NEW(opal_info_item_t);
                PMIX_INFO_LOAD(&info->info, PMIX_PERSONALITY, info_str->string, PMIX_STRING);
                opal_list_append(&job_info, &info->super);
                OBJ_RELEASE(info_str);
            }
#endif

            /* check for standard form keys, do not deprecate they are part of
             * MPI standard ch. 10.3.4 */

            /* check for 'host' */
            ompi_info_get (array_of_info[i], "host", &info_str, &flag);
            if ( flag ) {
                info = OBJ_NEW(opal_info_item_t);
                PMIX_INFO_LOAD(&info->info, PMIX_HOST, info_str->string, PMIX_STRING);
                opal_list_append(&app_info, &info->super);
                opal_argv_append_nosize(&dash_host, info_str->string);
                OBJ_RELEASE(info_str);
            }
            ompi_info_get (array_of_info[i], "PMIX_HOST", &info_str, &flag);
            if ( flag ) {
                info = OBJ_NEW(opal_info_item_t);
                PMIX_INFO_LOAD(&info->info, PMIX_HOST, info_str->string, PMIX_STRING);
                opal_list_append(&app_info, &info->super);
                opal_argv_append_nosize(&dash_host, info_str->string);
                OBJ_RELEASE(info_str);
            }
#if PMIX_NUMERIC_VERSION >= 0x00040000
            checkkey = PMIx_Get_attribute_string("PMIX_HOST");
            ompi_info_get (array_of_info[i], checkkey, &info_str, &flag);
            if ( flag ) {
                info = OBJ_NEW(opal_info_item_t);
                PMIX_INFO_LOAD(&info->info, PMIX_HOST, info_str->string, PMIX_STRING);
                opal_list_append(&app_info, &info->super);
                opal_argv_append_nosize(&dash_host, info_str->string);
                OBJ_RELEASE(info_str);
            }
#endif

            /* check for 'wdir' */
            ompi_info_get (array_of_info[i], "wdir", &info_str, &flag);
            if ( flag ) {
                info = OBJ_NEW(opal_info_item_t);
                PMIX_INFO_LOAD(&info->info, PMIX_WDIR, info_str->string, PMIX_STRING);
                opal_list_append(&app_info, &info->super);
                OBJ_RELEASE(info_str);
                have_wdir = 1;
            }
            if (!have_wdir) {
                ompi_info_get (array_of_info[i], "PMIX_WDIR", &info_str, &flag);
                if ( flag ) {
                    info = OBJ_NEW(opal_info_item_t);
                    PMIX_INFO_LOAD(&info->info, PMIX_WDIR, info_str->string, PMIX_STRING);
                    opal_list_append(&app_info, &info->super);
                    OBJ_RELEASE(info_str);
                    have_wdir = 1;
                }
            }
#if PMIX_NUMERIC_VERSION >= 0x00040000
            if (!have_wdir) {
                checkkey = PMIx_Get_attribute_string("PMIX_WDIR");
                ompi_info_get (array_of_info[i], checkkey, &info_str, &flag);
                if ( flag ) {
                    info = OBJ_NEW(opal_info_item_t);
                    PMIX_INFO_LOAD(&info->info, PMIX_WDIR, info_str->string, PMIX_STRING);
                    opal_list_append(&app_info, &info->super);
                    OBJ_RELEASE(info_str);
                    have_wdir = 1;
                }
            }
#endif

            /* check for 'mpi_initial_errhandler' */
            ompi_info_get (array_of_info[i], "mpi_initial_errhandler", &info_str, &flag);
            if ( flag ) {
                /* this is set as an environment because it must be available
                 * before pmix_init */
                opal_setenv("OMPI_MCA_mpi_initial_errhandler", info_str->string, true, &app->env);
                OBJ_RELEASE(info_str);
            }

            /* 'path', 'arch', 'file', 'soft'  -- to be implemented */

            /* non-standard keys
             * Keys that correspond to prun/mpiexec parameters
             * do not deprecate PMIX unprefixed forms to remain identical
             * to the command line parameter;
             * Keys that are not corresponding to an mpiexec parameter are
             * deprecated in the non-prefixed form */

            /* check for 'hostfile' */
            ompi_info_get (array_of_info[i], "hostfile", &info_str, &flag);
            if ( flag ) {
                info = OBJ_NEW(opal_info_item_t);
                PMIX_INFO_LOAD(&info->info, PMIX_HOSTFILE, info_str->string, PMIX_STRING);
                opal_list_append(&app_info, &info->super);
                opal_argv_append_nosize(&hostfiles, info_str->string);
                OBJ_RELEASE(info_str);
            }
            ompi_info_get (array_of_info[i], "PMIX_HOSTFILE", &info_str, &flag);
            if ( flag ) {
                info = OBJ_NEW(opal_info_item_t);
                PMIX_INFO_LOAD(&info->info, PMIX_HOSTFILE, info_str->string, PMIX_STRING);
                opal_list_append(&app_info, &info->super);
                opal_argv_append_nosize(&hostfiles, info_str->string);
                OBJ_RELEASE(info_str);
            }
#if PMIX_NUMERIC_VERSION >= 0x00040000
            checkkey = PMIx_Get_attribute_string("PMIX_HOSTFILE");
            ompi_info_get (array_of_info[i], checkkey, &info_str, &flag);
            if ( flag ) {
                info = OBJ_NEW(opal_info_item_t);
                PMIX_INFO_LOAD(&info->info, PMIX_HOSTFILE, info_str->string, PMIX_STRING);
                opal_list_append(&app_info, &info->super);
                opal_argv_append_nosize(&hostfiles, info_str->string);
                OBJ_RELEASE(info_str);
            }
#endif

            /* check for 'add-hostfile' */
            ompi_info_get (array_of_info[i], "add-hostfile", &info_str, &flag);
            if ( flag ) {
                /* deprecate --> PMIX_ADD_HOSTFILE */
                opal_show_help("help-dpm.txt", "deprecated-converted", true,
                                "add-hostfile", "PMIX_ADD_HOSTFILE");
                info = OBJ_NEW(opal_info_item_t);
                PMIX_INFO_LOAD(&info->info, PMIX_ADD_HOSTFILE, info_str->string, PMIX_STRING);
                opal_list_append(&app_info, &info->super);
                OBJ_RELEASE(info_str);
            }
            ompi_info_get (array_of_info[i], "PMIX_ADD_HOSTFILE", &info_str, &flag);
            if ( flag ) {
                info = OBJ_NEW(opal_info_item_t);
                PMIX_INFO_LOAD(&info->info, PMIX_ADD_HOSTFILE, info_str->string, PMIX_STRING);
                opal_list_append(&app_info, &info->super);
                OBJ_RELEASE(info_str);
            }
#if PMIX_NUMERIC_VERSION >= 0x00040000
            checkkey = PMIx_Get_attribute_string("PMIX_ADD_HOSTFILE");
            ompi_info_get (array_of_info[i], checkkey, &info_str, &flag);
            if ( flag ) {
                info = OBJ_NEW(opal_info_item_t);
                PMIX_INFO_LOAD(&info->info, PMIX_ADD_HOSTFILE, info_str->string, PMIX_STRING);
                opal_list_append(&app_info, &info->super);
                OBJ_RELEASE(info_str);
            }
#endif

            /* check for 'add-host' */
            ompi_info_get (array_of_info[i], "add-host", &info_str, &flag);
            if ( flag ) {
                /* deprecate --> PMIX_ADD_HOST */
                opal_show_help("help-dpm.txt", "deprecated-converted", true,
                                "add-host", "PMIX_ADD_HOST");
                info = OBJ_NEW(opal_info_item_t);
                PMIX_INFO_LOAD(&info->info, PMIX_ADD_HOST, info_str->string, PMIX_STRING);
                opal_list_append(&app_info, &info->super);
                OBJ_RELEASE(info_str);
            }
            ompi_info_get (array_of_info[i], "PMIX_ADD_HOST", &info_str, &flag);
            if ( flag ) {
                info = OBJ_NEW(opal_info_item_t);
                PMIX_INFO_LOAD(&info->info, PMIX_ADD_HOST, info_str->string, PMIX_STRING);
                opal_list_append(&app_info, &info->super);
                OBJ_RELEASE(info_str);
            }
#if PMIX_NUMERIC_VERSION >= 0x00040000
            checkkey = PMIx_Get_attribute_string("PMIX_ADD_HOST");
            ompi_info_get (array_of_info[i], checkkey, &info_str, &flag);
            if ( flag ) {
                info = OBJ_NEW(opal_info_item_t);
                PMIX_INFO_LOAD(&info->info, PMIX_ADD_HOST, info_str->string, PMIX_STRING);
                opal_list_append(&app_info, &info->super);
                OBJ_RELEASE(info_str);
            }
#endif

            /* check for env */
            ompi_info_get (array_of_info[i], "env", &info_str, &flag);
            if ( flag ) {
                /* deprecate --> PMIX_ENVAR */
                opal_show_help("help-dpm.txt", "deprecated-converted", true,
                                "env", "PMIX_ENVAR");
                envars = opal_argv_split(info_str->string, '\n');
                OBJ_RELEASE(info_str);
                for (j=0; NULL != envars[j]; j++) {
                    opal_argv_append_nosize(&app->env, envars[j]);
                }
                opal_argv_free(envars);
            }
            ompi_info_get (array_of_info[i], "PMIX_ENVAR", &info_str, &flag);
            if ( flag ) {
                envars = opal_argv_split(info_str->string, '\n');
                OBJ_RELEASE(info_str);
                for (j=0; NULL != envars[j]; j++) {
                    opal_argv_append_nosize(&app->env, envars[j]);
                }
                opal_argv_free(envars);
            }
#if PMIX_NUMERIC_VERSION >= 0x00040000
            checkkey = PMIx_Get_attribute_string("PMIX_ENVAR");
            ompi_info_get (array_of_info[i], "PMIX_ENVAR", &info_str, &flag);
            if ( flag ) {
                envars = opal_argv_split(info_str->string, '\n');
                OBJ_RELEASE(info_str);
                for (j=0; NULL != envars[j]; j++) {
                    opal_argv_append_nosize(&app->env, envars[j]);
                }
                opal_argv_free(envars);
            }
#endif

            /* check for 'ompi_prefix' (OMPI-specific -- to effect the same
             * behavior as --prefix option to prun)
             *
             * This is a job-level key
             */
            ompi_info_get (array_of_info[i], "ompi_prefix", &info_str, &flag);
            if ( flag ) {
                /* deprecate --> PMIX_PREFIX */
                opal_show_help("help-dpm.txt", "deprecated-converted", true,
                                "ompi_prefix", "PMIX_PREFIX");
                info = OBJ_NEW(opal_info_item_t);
                PMIX_INFO_LOAD(&info->info, PMIX_PREFIX, info_str->string, PMIX_STRING);
                opal_list_append(&job_info, &info->super);
                OBJ_RELEASE(info_str);
            }
            ompi_info_get (array_of_info[i], "PMIX_PREFIX", &info_str, &flag);
            if ( flag ) {
                info = OBJ_NEW(opal_info_item_t);
                PMIX_INFO_LOAD(&info->info, PMIX_PREFIX, info_str->string, PMIX_STRING);
                opal_list_append(&job_info, &info->super);
                OBJ_RELEASE(info_str);
            }
#if PMIX_NUMERIC_VERSION >= 0x00040000
            checkkey = PMIx_Get_attribute_string("PMIX_PREFIX");
            ompi_info_get (array_of_info[i], checkkey, &info_str, &flag);
            if ( flag ) {
                info = OBJ_NEW(opal_info_item_t);
                PMIX_INFO_LOAD(&info->info, PMIX_PREFIX, info_str->string, PMIX_STRING);
                opal_list_append(&job_info, &info->super);
                OBJ_RELEASE(info_str);
            }
#endif

            /* check for 'mapper' - a job-level key */
            ompi_info_get(array_of_info[i], "mapper", &info_str, &flag);
            if ( flag ) {
                /* deprecate --> PMIX_MAPPER */
                opal_show_help("help-dpm.txt", "deprecated-converted", true,
                                "mapper", "PMIX_MAPPER");
                info = OBJ_NEW(opal_info_item_t);
                PMIX_INFO_LOAD(&info->info, PMIX_MAPPER, info_str->string, PMIX_STRING);
                opal_list_append(&job_info, &info->super);
                OBJ_RELEASE(info_str);
            }
            ompi_info_get(array_of_info[i], "PMIX_MAPPER", &info_str, &flag);
            if ( flag ) {
                info = OBJ_NEW(opal_info_item_t);
                PMIX_INFO_LOAD(&info->info, PMIX_MAPPER, info_str->string, PMIX_STRING);
                opal_list_append(&job_info, &info->super);
                OBJ_RELEASE(info_str);
            }
#if PMIX_NUMERIC_VERSION >= 0x00040000
            checkkey = PMIx_Get_attribute_string("PMIX_MAPPER");
            ompi_info_get(array_of_info[i], checkkey, &info_str, &flag);
            if ( flag ) {
                info = OBJ_NEW(opal_info_item_t);
                PMIX_INFO_LOAD(&info->info, PMIX_MAPPER, info_str->string, PMIX_STRING);
                opal_list_append(&job_info, &info->super);
                OBJ_RELEASE(info_str);
            }
#endif

            /* check for 'display_map' - a job-level key */
            ompi_info_get_bool(array_of_info[i], "display_map", &local_spawn, &flag);
            if ( flag ) {
                rc = dpm_convert(&job_info, "display_map", PMIX_MAPBY, NULL, "DISPLAY", true);
                if (OMPI_SUCCESS != rc) {
                    OPAL_LIST_DESTRUCT(&job_info);
                    OPAL_LIST_DESTRUCT(&app_info);
                    PMIX_APP_FREE(apps, scount);
                    if (NULL != hostfiles) {
                        opal_argv_free(hostfiles);
                    }
                    if (NULL != dash_host) {
                        opal_argv_free(dash_host);
                    }
                    return MPI_ERR_SPAWN;
                }
            }

            /* check for 'npernode' and 'ppr' - job-level key */
            ompi_info_get (array_of_info[i], "npernode", &info_str, &flag);
            if ( flag ) {
                opal_asprintf(&tmp, "PPR:%s:NODE", info_str->string);
                rc = dpm_convert(&job_info, "npernode", PMIX_MAPBY, tmp, NULL, true);
                free(tmp);
                OBJ_RELEASE(info_str);
                if (OMPI_SUCCESS != rc) {
                    OPAL_LIST_DESTRUCT(&job_info);
                    OPAL_LIST_DESTRUCT(&app_info);
                    PMIX_APP_FREE(apps, scount);
                    if (NULL != hostfiles) {
                        opal_argv_free(hostfiles);
                    }
                    if (NULL != dash_host) {
                        opal_argv_free(dash_host);
                    }
                    return MPI_ERR_SPAWN;
                }
            }
            ompi_info_get (array_of_info[i], "pernode", &info_str, &flag);
            if ( flag ) {
                rc = dpm_convert(&job_info, "pernode", PMIX_MAPBY, "PPR:1:NODE", NULL, true);
                OBJ_RELEASE(info_str);
                if (OMPI_SUCCESS != rc) {
                    OPAL_LIST_DESTRUCT(&job_info);
                    OPAL_LIST_DESTRUCT(&app_info);
                    PMIX_APP_FREE(apps, scount);
                    if (NULL != hostfiles) {
                        opal_argv_free(hostfiles);
                    }
                    if (NULL != dash_host) {
                        opal_argv_free(dash_host);
                    }
                    return MPI_ERR_SPAWN;
                }
            }
            ompi_info_get (array_of_info[i], "ppr", &info_str, &flag);
            if ( flag ) {
                /* must have correct syntax with two colons */
                if (NULL == (tmp = strchr(info_str->string, ':'))) {
                    opal_show_help("help-dpm.txt", "bad-ppr", true, info_str->string);
                    OPAL_LIST_DESTRUCT(&job_info);
                    OPAL_LIST_DESTRUCT(&app_info);
                    PMIX_APP_FREE(apps, scount);
                    if (NULL != hostfiles) {
                        opal_argv_free(hostfiles);
                    }
                    if (NULL != dash_host) {
                        opal_argv_free(dash_host);
                    }
                    OBJ_RELEASE(info_str);
                    return MPI_ERR_SPAWN;
                }
                ++tmp; // step over first colon
                if (NULL == strchr(tmp, ':')) {
                    opal_show_help("help-dpm.txt", "bad-ppr", true, info_str->string);
                    OPAL_LIST_DESTRUCT(&job_info);
                    OPAL_LIST_DESTRUCT(&app_info);
                    PMIX_APP_FREE(apps, scount);
                    if (NULL != hostfiles) {
                        opal_argv_free(hostfiles);
                    }
                    if (NULL != dash_host) {
                        opal_argv_free(dash_host);
                    }
                    OBJ_RELEASE(info_str);
                    return MPI_ERR_SPAWN;
                }
                rc = dpm_convert(&job_info, "ppr", PMIX_MAPBY, info_str->string, NULL, true);
                OBJ_RELEASE(info_str);
                if (OMPI_SUCCESS != rc) {
                    OPAL_LIST_DESTRUCT(&job_info);
                    OPAL_LIST_DESTRUCT(&app_info);
                    PMIX_APP_FREE(apps, scount);
                    if (NULL != hostfiles) {
                        opal_argv_free(hostfiles);
                    }
                    if (NULL != dash_host) {
                        opal_argv_free(dash_host);
                    }
                    return MPI_ERR_SPAWN;
                }
            }

            /* check for 'map_by' - job-level key */
            ompi_info_get(array_of_info[i], "map_by", &info_str, &flag);
            if ( flag ) {
                rc = dpm_convert(&job_info, "map_by", PMIX_MAPBY, info_str->string, NULL, false);
                OBJ_RELEASE(info_str);
                if (OMPI_SUCCESS != rc) {
                    OPAL_LIST_DESTRUCT(&job_info);
                    OPAL_LIST_DESTRUCT(&app_info);
                    PMIX_APP_FREE(apps, scount);
                    if (NULL != hostfiles) {
                        opal_argv_free(hostfiles);
                    }
                    if (NULL != dash_host) {
                        opal_argv_free(dash_host);
                    }
                    return MPI_ERR_SPAWN;
                }
            }
            ompi_info_get(array_of_info[i], "PMIX_MAPBY", &info_str, &flag);
            if ( flag ) {
                info = OBJ_NEW(opal_info_item_t);
                PMIX_INFO_LOAD(&info->info, PMIX_MAPBY, info_str->string, PMIX_STRING);
                opal_list_append(&job_info, &info->super);
                OBJ_RELEASE(info_str);
            }
#if PMIX_NUMERIC_VERSION >= 0x00040000
            checkkey = PMIx_Get_attribute_string("PMIX_MAPBY");
            ompi_info_get(array_of_info[i], checkkey, &info_str, &flag);
            if ( flag ) {
                info = OBJ_NEW(opal_info_item_t);
                PMIX_INFO_LOAD(&info->info, PMIX_MAPBY, info_str->string, PMIX_STRING);
                opal_list_append(&job_info, &info->super);
                OBJ_RELEASE(info_str);
            }
#endif

            /* check for 'rank_by' - job-level key */
            ompi_info_get(array_of_info[i], "rank_by", &info_str, &flag);
            if ( flag ) {
                rc = dpm_convert(&job_info, "rank_by", PMIX_RANKBY, info_str->string, NULL, false);
                OBJ_RELEASE(info_str);
                if (OMPI_SUCCESS != rc) {
                    OPAL_LIST_DESTRUCT(&job_info);
                    OPAL_LIST_DESTRUCT(&app_info);
                    PMIX_APP_FREE(apps, scount);
                    return MPI_ERR_SPAWN;
                }
            }
            ompi_info_get(array_of_info[i], "PMIX_RANKBY", &info_str, &flag);
            if ( flag ) {
                info = OBJ_NEW(opal_info_item_t);
                PMIX_INFO_LOAD(&info->info, PMIX_RANKBY, info_str->string, PMIX_STRING);
                opal_list_append(&job_info, &info->super);
                OBJ_RELEASE(info_str);
            }
#if PMIX_NUMERIC_VERSION >= 0x00040000
            checkkey = PMIx_Get_attribute_string("PMIX_RANKBY");
            ompi_info_get(array_of_info[i], checkkey, &info_str, &flag);
            if ( flag ) {
                info = OBJ_NEW(opal_info_item_t);
                PMIX_INFO_LOAD(&info->info, PMIX_RANKBY, info_str->string, PMIX_STRING);
                opal_list_append(&job_info, &info->super);
                OBJ_RELEASE(info_str);
            }
#endif

            /* check for 'bind_to' - job-level key */
            ompi_info_get(array_of_info[i], "bind_to", &info_str, &flag);
            if ( flag ) {
                rc = dpm_convert(&job_info, "bind_to", PMIX_BINDTO, info_str->string, NULL, false);
                OBJ_RELEASE(info_str);
                if (OMPI_SUCCESS != rc) {
                    OPAL_LIST_DESTRUCT(&job_info);
                    OPAL_LIST_DESTRUCT(&app_info);
                    PMIX_APP_FREE(apps, scount);
                    return MPI_ERR_SPAWN;
                }
            }
            ompi_info_get(array_of_info[i], "PMIX_BINDTO", &info_str, &flag);
            if ( flag ) {
                info = OBJ_NEW(opal_info_item_t);
                PMIX_INFO_LOAD(&info->info, PMIX_BINDTO, info_str->string, PMIX_STRING);
                opal_list_append(&job_info, &info->super);
                OBJ_RELEASE(info_str);
            }
#if PMIX_NUMERIC_VERSION >= 0x00040000
            checkkey = PMIx_Get_attribute_string("PMIX_BINDTO");
            ompi_info_get(array_of_info[i], checkkey, &info_str, &flag);
            if ( flag ) {
                info = OBJ_NEW(opal_info_item_t);
                PMIX_INFO_LOAD(&info->info, PMIX_BINDTO, info_str->string, PMIX_STRING);
                opal_list_append(&job_info, &info->super);
                OBJ_RELEASE(info_str);
            }
#endif

            /* check for 'preload_binary' - job-level key */
            ompi_info_get_bool(array_of_info[i], "ompi_preload_binary", &local_spawn, &flag);
            if ( flag ) {
                /* deprecate --> PMIX_PRELOAD_BIN */
                opal_show_help("help-dpm.txt", "deprecated-converted", true,
                                "ompi_preload_binary", "PMIX_PRELOAD_BIN");
                info = OBJ_NEW(opal_info_item_t);
                PMIX_INFO_LOAD(&info->info, PMIX_PRELOAD_BIN, &local_spawn, PMIX_BOOL);
                opal_list_append(&job_info, &info->super);
            }
            ompi_info_get_bool(array_of_info[i], "PMIX_PRELOAD_BIN", &local_spawn, &flag);
            if ( flag ) {
                info = OBJ_NEW(opal_info_item_t);
                PMIX_INFO_LOAD(&info->info, PMIX_PRELOAD_BIN, &local_spawn, PMIX_BOOL);
                opal_list_append(&job_info, &info->super);
            }
#if PMIX_NUMERIC_VERSION >= 0x00040000
            checkkey = PMIx_Get_attribute_string("PMIX_PRELOAD_BIN");
            ompi_info_get_bool(array_of_info[i], checkkey, &local_spawn, &flag);
            if ( flag ) {
                info = OBJ_NEW(opal_info_item_t);
                PMIX_INFO_LOAD(&info->info, PMIX_PRELOAD_BIN, &local_spawn, PMIX_BOOL);
                opal_list_append(&job_info, &info->super);
            }
#endif

            /* check for 'preload_files' - job-level key */
            ompi_info_get (array_of_info[i], "ompi_preload_files", &info_str, &flag);
            if ( flag ) {
                /* deprecate --> PMIX_PRELOAD_FILES */
                opal_show_help("help-dpm.txt", "deprecated-converted", true,
                                "ompi_preload_files", "PMIX_PRELOAD_FILES");
                info = OBJ_NEW(opal_info_item_t);
                PMIX_INFO_LOAD(&info->info, PMIX_PRELOAD_FILES, info_str->string, PMIX_STRING);
                opal_list_append(&job_info, &info->super);
                OBJ_RELEASE(info_str);
            }
            ompi_info_get (array_of_info[i], "PMIX_PRELOAD_FILES", &info_str, &flag);
            if ( flag ) {
                info = OBJ_NEW(opal_info_item_t);
                PMIX_INFO_LOAD(&info->info, PMIX_PRELOAD_FILES, info_str->string, PMIX_STRING);
                opal_list_append(&job_info, &info->super);
                OBJ_RELEASE(info_str);
            }
#if PMIX_NUMERIC_VERSION >= 0x00040000
            checkkey = PMIx_Get_attribute_string("PMIX_PRELOAD_FILES");
            ompi_info_get (array_of_info[i], checkkey, &info_str, &flag);
            if ( flag ) {
                info = OBJ_NEW(opal_info_item_t);
                PMIX_INFO_LOAD(&info->info, PMIX_PRELOAD_FILES, info_str->string, PMIX_STRING);
                opal_list_append(&job_info, &info->super);
                OBJ_RELEASE(info_str);
            }
#endif

            /* see if this is a non-mpi job - if so, then set the flag so PRTE
             * knows what to do - job-level key
             */
            ompi_info_get_bool(array_of_info[i], "ompi_non_mpi", &non_mpi, &flag);
            if (flag && non_mpi) {
                opal_show_help("help-dpm.txt", "deprecated-inform", true,
                                "ompi_non_mpi", "No longer relevant as RTE automatically detects this scenario");
            }

            /* see if this is an MCA param that the user wants applied to the child job */
            ompi_info_get (array_of_info[i], "ompi_param", &info_str, &flag);
            if ( flag ) {
                opal_show_help("help-dpm.txt", "deprecated-converted", true,
                                "ompi_param", "PMIX_ENVAR");
                opal_argv_append_unique_nosize(&app->env, info_str->string, true);
                OBJ_RELEASE(info_str);
            }

            /* see if user specified what to do with stdin - defaults to
             * not forwarding stdin to child processes - job-level key
             */
            ompi_info_get (array_of_info[i], "ompi_stdin_target", &info_str, &flag);
            if ( flag ) {
                /* deprecate --> PMIX_STDIN_TGT */
                opal_show_help("help-dpm.txt", "deprecated-converted", true,
                                "ompi_stdin_target", "PMIX_STDIN_TGT");
                if (0 == strcmp(info_str->string, "all")) {
                    ui32 = OPAL_VPID_WILDCARD;
                } else if (0 == strcmp(info_str->string, "none")) {
                    ui32 = OPAL_VPID_INVALID;
                } else {
                    ui32 = strtoul(info_str->string, NULL, 10);
                }
                info = OBJ_NEW(opal_info_item_t);
                PMIX_INFO_LOAD(&info->info, PMIX_STDIN_TGT, &ui32, PMIX_UINT32);
                opal_list_append(&job_info, &info->super);
                OBJ_RELEASE(info_str);
            }
            ompi_info_get (array_of_info[i], "PMIX_STDIN_TGT", &info_str, &flag);
            if ( flag ) {
                if (0 == strcmp(info_str->string, "all")) {
                    ui32 = OPAL_VPID_WILDCARD;
                } else if (0 == strcmp(info_str->string, "none")) {
                    ui32 = OPAL_VPID_INVALID;
                } else {
                    ui32 = strtoul(info_str->string, NULL, 10);
                }
                info = OBJ_NEW(opal_info_item_t);
                PMIX_INFO_LOAD(&info->info, PMIX_STDIN_TGT, &ui32, PMIX_UINT32);
                opal_list_append(&job_info, &info->super);
                OBJ_RELEASE(info_str);
            }
#if PMIX_NUMERIC_VERSION >= 0x00040000
            checkkey = PMIx_Get_attribute_string("PMIX_STDIN_TGT");
            ompi_info_get (array_of_info[i], checkkey, &info_str, &flag);
            if ( flag ) {
                if (0 == strcmp(info_str->string, "all")) {
                    ui32 = OPAL_VPID_WILDCARD;
                } else if (0 == strcmp(info_str->string, "none")) {
                    ui32 = OPAL_VPID_INVALID;
                } else {
                    ui32 = strtoul(info_str->string, NULL, 10);
                }
                info = OBJ_NEW(opal_info_item_t);
                PMIX_INFO_LOAD(&info->info, PMIX_STDIN_TGT, &ui32, PMIX_UINT32);
                opal_list_append(&job_info, &info->super);
                OBJ_RELEASE(info_str);
            }
#endif
        }

        /* default value: If the user did not tell us where to look for the
         * executable, we assume the current working directory
         */
        if ( !have_wdir ) {
            char cwd[OPAL_PATH_MAX];
            if (OMPI_SUCCESS != (rc = opal_getcwd(cwd, OPAL_PATH_MAX))) {
                OMPI_ERROR_LOG(rc);
                PMIX_APP_FREE(apps, (size_t)count);
                if (NULL != hostfiles) {
                    opal_argv_free(hostfiles);
                }
                if (NULL != dash_host) {
                    opal_argv_free(dash_host);
                }
                return rc;
            }
            info = OBJ_NEW(opal_info_item_t);
            PMIX_INFO_LOAD(&info->info, PMIX_WDIR, cwd, PMIX_STRING);
            opal_list_append(&app_info, &info->super);
        }

        /* leave the map info alone - the launcher will
         * decide where to put things
         */

        ninfo = opal_list_get_size(&app_info);
        if (0 < ninfo) {
            PMIX_INFO_CREATE(app->info, ninfo);
            app->ninfo = ninfo;
            n = 0;
            OPAL_LIST_FOREACH(info, &app_info, opal_info_item_t) {
                PMIX_INFO_XFER(&app->info[n], &info->info);
                ++n;
            }
        }
        OPAL_LIST_DESTRUCT(&app_info);
    } /* for (i = 0 ; i < count ; ++i) */

    /* default the personality - job-level key */
    if (!personality) {
        info = OBJ_NEW(opal_info_item_t);
        PMIX_INFO_LOAD(&info->info, PMIX_PERSONALITY, "ompi5", PMIX_STRING);
        opal_list_append(&job_info, &info->super);
    }

    if (opal_process_info.is_singleton) {
        /* The GDS 'hash' component is known to work for singleton, so
         * recommend it. The user may set this envar to override the setting.
         */
        setenv("PMIX_MCA_gds", "hash", 0);
        /* Start the DVM */
        rc = start_dvm(hostfiles, dash_host);
        if (OPAL_SUCCESS != rc) {
            if (NULL != pinfo) {
                PMIX_INFO_FREE(pinfo, ninfo);
            }
            PMIX_APP_FREE(apps, scount);
            if (NULL != hostfiles) {
                opal_argv_free(hostfiles);
            }
            if (NULL != dash_host) {
                opal_argv_free(dash_host);
            }
            return MPI_ERR_SPAWN;
        }
        /* tell it to forward output to us */
        info = OBJ_NEW(opal_info_item_t);
        PMIX_INFO_LOAD(&info->info, PMIX_FWD_STDOUT, NULL, PMIX_BOOL);
        opal_list_append(&job_info, &info->super);
        info = OBJ_NEW(opal_info_item_t);
        PMIX_INFO_LOAD(&info->info, PMIX_FWD_STDERR, NULL, PMIX_BOOL);
        opal_list_append(&job_info, &info->super);
        info = OBJ_NEW(opal_info_item_t);
        PMIX_INFO_LOAD(&info->info, PMIX_FWD_STDDIAG, NULL, PMIX_BOOL);
        opal_list_append(&job_info, &info->super);
    }
    if (NULL != hostfiles) {
        opal_argv_free(hostfiles);
    }
    if (NULL != dash_host) {
        opal_argv_free(dash_host);
    }

    /* spawn procs */
    ninfo = opal_list_get_size(&job_info);
    if (0 < ninfo) {
        PMIX_INFO_CREATE(pinfo, ninfo);
        n = 0;
        OPAL_LIST_FOREACH(info, &job_info, opal_info_item_t) {
            PMIX_INFO_XFER(&pinfo[n], &info->info);
            ++n;
        }
    }
    OPAL_LIST_DESTRUCT(&job_info);

    pret = PMIx_Spawn(pinfo, ninfo, apps, count, nspace);
    rc = opal_pmix_convert_status(pret);
    if (NULL != pinfo) {
        PMIX_INFO_FREE(pinfo, ninfo);
    }
    PMIX_APP_FREE(apps, scount);

    if (OPAL_SUCCESS != rc) {
        return MPI_ERR_SPAWN;
    }

    return OMPI_SUCCESS;
}

/* Create a rendezvous tag consisting of our name + a random number */
int ompi_dpm_open_port(char *port_name)
{
    uint32_t r;
    char *tmp;

    r = opal_rand(&rnd);
    opal_convert_process_name_to_string(&tmp, OMPI_PROC_MY_NAME);
    snprintf(port_name, MPI_MAX_PORT_NAME-1, "%s:%u", tmp, r);
    port_name[MPI_MAX_PORT_NAME - 1] = '\0';
    free(tmp);
    return OMPI_SUCCESS;
}

int ompi_dpm_close_port(const char *port_name)
{
    /* nothing to do here - user is responsible for the memory */
    return OMPI_SUCCESS;
}

int ompi_dpm_dyn_init(void)
{
    int root=0, rc;
    bool send_first = true;
    ompi_communicator_t *newcomm=NULL;
    char *port_name=NULL, *tmp, *ptr;

    /* check for appropriate env variable */
    tmp = getenv("OMPI_PARENT_PORT");
    if (NULL == tmp) {
        /* nothing to do */
        return OMPI_SUCCESS;
    }

    /* the value passed to us may have quote marks around it to protect
     * the value if passed on the command line. We must remove those
    * to have a correct string
     */
     if ('"' == tmp[0]) {
        /* if the first char is a quote, then so will the last one be */
        tmp[strlen(tmp)-1] = '\0';
        ptr = &tmp[1];
    } else {
        ptr = &tmp[0];
    }
    port_name = strdup(ptr);
    if (NULL == port_name) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    rc = ompi_dpm_connect_accept(MPI_COMM_WORLD, root, port_name, send_first, &newcomm);
    free(port_name);
    if (OMPI_SUCCESS != rc) {
        return rc;
    }

    /* originally, we set comm_parent to comm_null (in comm_init),
     * now we have to decrease the reference counters to the according
     * objects
     */
    OBJ_RELEASE(ompi_mpi_comm_parent->c_local_group);
    OBJ_RELEASE(ompi_mpi_comm_parent->error_handler);
    OBJ_RELEASE(ompi_mpi_comm_parent);

    /* Set the parent communicator */
    ompi_mpi_comm_parent = newcomm;

    /* Set name for debugging purposes */
    snprintf(newcomm->c_name, MPI_MAX_OBJECT_NAME, "MPI_COMM_PARENT");
    newcomm->c_flags |= OMPI_COMM_NAMEISSET;

    return OMPI_SUCCESS;
}

static void cleanup_dpm_disconnect_objs(ompi_dpm_disconnect_obj **objs, int count)
{
    for(int i = 0; i < count; i++) {
        if (NULL != objs[i]->reqs) {
            free(objs[i]->reqs);
        }
        free(objs[i]);
    }
    free(objs);
}

/**********************************************************************/
/**********************************************************************/
/**********************************************************************/
/* this routine runs through the list of communicators
   and does the disconnect for all dynamic communicators */
int ompi_dpm_dyn_finalize(void)
{
    int i,j=0, max=0;
    ompi_dpm_disconnect_obj **objs=NULL;
    ompi_communicator_t *comm=NULL;

    if (1 <ompi_comm_num_dyncomm) {
        objs = (ompi_dpm_disconnect_obj**)malloc(ompi_comm_num_dyncomm *
                               sizeof(ompi_dpm_disconnect_obj*));
        if (NULL == objs) {
            return OMPI_ERR_OUT_OF_RESOURCE;
        }

        max = opal_pointer_array_get_size(&ompi_mpi_communicators);
        for (i=3; i<max; i++) {
            comm = (ompi_communicator_t*)opal_pointer_array_get_item(&ompi_mpi_communicators,i);
            if (NULL != comm &&  OMPI_COMM_IS_DYNAMIC(comm)) {
                objs[j++] = disconnect_init(comm);
            }
        }

        if (j != ompi_comm_num_dyncomm) {
            cleanup_dpm_disconnect_objs(objs, j);
            return OMPI_ERROR;
        }

        disconnect_waitall(ompi_comm_num_dyncomm, objs);
        cleanup_dpm_disconnect_objs(objs, ompi_comm_num_dyncomm);
    }

    return OMPI_SUCCESS;
}

/* the next two routines implement a kind of non-blocking barrier.
the only difference is, that you can wait for the completion
of more than one initiated ibarrier. This is required for waiting
for all still connected processes in MPI_Finalize.

disconnect_init returns a handle, which has to be passed in
to disconnect_waitall. The second routine blocks, until
all non-blocking barriers described by the handles are finished.
The communicators can than be released.
*/
/**********************************************************************/
/**********************************************************************/
/**********************************************************************/

static ompi_dpm_disconnect_obj *disconnect_init(ompi_communicator_t *comm)
{
    ompi_dpm_disconnect_obj *obj=NULL;
    int ret;
    int i;

    obj = (ompi_dpm_disconnect_obj*)calloc(1,sizeof(ompi_dpm_disconnect_obj));
    if (NULL == obj) {
        opal_output(0, "Could not allocate disconnect object");
        return NULL;
    }

    if (OMPI_COMM_IS_INTER(comm)) {
        obj->size = ompi_comm_remote_size(comm);
    } else {
        obj->size = ompi_comm_size(comm);
    }

    obj->comm = comm;
    obj->reqs = (ompi_request_t**)malloc(2*obj->size*sizeof(ompi_request_t *));
    if (NULL == obj->reqs) {
        opal_output(0, "Could not allocate request array for disconnect object");
        free(obj);
        return NULL;
    }

    /* initiate all isend_irecvs. We use a dummy buffer stored on
       the object, since we are sending zero size messages anyway. */
    for (i=0; i < obj->size; i++) {
        ret = MCA_PML_CALL(irecv(&(obj->buf), 0, MPI_INT, i,
                                 OMPI_COMM_BARRIER_TAG, comm,
                                 &(obj->reqs[2*i])));

        if (OMPI_SUCCESS != ret) {
            opal_output(0, "dpm_disconnect_init: error %d in irecv to process %d", ret, i);
            free(obj->reqs);
            free(obj);
            return NULL;
        }
        ret = MCA_PML_CALL(isend(&(obj->buf), 0, MPI_INT, i,
                                 OMPI_COMM_BARRIER_TAG,
                                 MCA_PML_BASE_SEND_SYNCHRONOUS,
                                 comm, &(obj->reqs[2*i+1])));

        if (OMPI_SUCCESS != ret) {
            opal_output(0, "dpm_disconnect_init: error %d in isend to process %d", ret, i);
            free(obj->reqs);
            free(obj);
            return NULL;
        }
    }

    /* return handle */
    return obj;
}
/**********************************************************************/
/**********************************************************************/
/**********************************************************************/
/* - count how many requests are active
 * - generate a request array large enough to hold
     all active requests
 * - call waitall on the overall request array
 * - free the objects
 */
static int disconnect_waitall (int count, ompi_dpm_disconnect_obj **objs)
{

    ompi_request_t **reqs=NULL;
    char *treq=NULL;
    int totalcount = 0;
    int i;
    int ret;

    for (i=0; i<count; i++) {
        if (NULL == objs[i]) {
            opal_output(0, "Error in comm_disconnect_waitall");
            return OMPI_ERROR;
        }

        totalcount += objs[i]->size;
    }

    reqs = (ompi_request_t**)malloc(2*totalcount*sizeof(ompi_request_t *));
    if (NULL == reqs) {
        opal_output(0, "ompi_comm_disconnect_waitall: error allocating memory");
        return OMPI_ERROR;
    }

    /* generate a single, large array of pending requests */
    treq = (char *)reqs;
    for (i=0; i<count; i++) {
        memcpy(treq, objs[i]->reqs, 2*objs[i]->size * sizeof(ompi_request_t *));
        treq += 2*objs[i]->size * sizeof(ompi_request_t *);
    }

    /* force all non-blocking all-to-alls to finish */
    ret = ompi_request_wait_all(2*totalcount, reqs, MPI_STATUSES_IGNORE);

    free(reqs);

    return ret;
}

/**********************************************************************/
/**********************************************************************/
/**********************************************************************/
static bool ompi_dpm_group_is_dyn (ompi_group_t *group, ompi_jobid_t thisjobid)
{
    int size = group ? ompi_group_size (group) : 0;

    for (int i = 0 ; i < size ; ++i) {
        opal_process_name_t name = ompi_group_get_proc_name (group, i);

        if (thisjobid != ((ompi_process_name_t *) &name)->jobid) {
            /* at least one is different */
            return true;
        }
    }

    return false;
}

/* All we want to do in this function is determine if the number of
 * jobids in the local and/or remote group is > 1. This tells us to
 * set the disconnect flag. We don't actually care what the true
 * number -is-, only that it is > 1
 */
void ompi_dpm_mark_dyncomm(ompi_communicator_t *comm)
{
    bool found;
    ompi_jobid_t thisjobid;

    /* special case for MPI_COMM_NULL */
    if (comm == MPI_COMM_NULL) {
        return;
    }

    thisjobid = ompi_group_get_proc_name (comm->c_local_group, 0).jobid;

    /* loop over all processes in local group and check for
     * a different jobid
     */
    found = ompi_dpm_group_is_dyn (comm->c_local_group, thisjobid);
    if (!found) {
        /* if inter-comm, loop over all processes in remote_group
         * and see if any are different from thisjobid
         */
        found = ompi_dpm_group_is_dyn (comm->c_remote_group, thisjobid);
    }

    /* if a different jobid was found, set the disconnect flag*/
    if (found) {
        ompi_comm_num_dyncomm++;
        OMPI_COMM_SET_DYNAMIC(comm);
    }
}

#if OMPI_HAVE_PRRTE

#define DVM_URI_MSG_LGTH   256

static void set_handler_default(int sig)
{
    struct sigaction act;

    act.sa_handler = SIG_DFL;
    act.sa_flags = 0;
    sigemptyset(&act.sa_mask);

    sigaction(sig, &act, (struct sigaction *)0);
}

static int start_dvm(char **hostfiles, char **dash_host)
{
    pmix_status_t pret;
    int rc;
    char **args = NULL;
    char *cmd, *tmp;
    int p[2];
    int death_pipe[2];
    pid_t pid;
    sigset_t sigs;
    pmix_info_t info;
    int buffer_length, num_chars_read, chunk;
    char *uri;

    /* find the prte binary using the install_dirs support - this also
     * checks to ensure that we can see this executable and it *is* executable by us
     */
    cmd = opal_find_absolute_path("prte");
    if (NULL == cmd) {
        /* guess we couldn't do it - best to abort */
        OMPI_ERROR_LOG(OMPI_ERROR);
        return OMPI_ERROR;
    }

    /* A pipe is used to communicate between the parent and child to
     indicate whether the exec ultimately succeeded or failed, and to
     pass back the URL of the DVM so we can connect to it
     */
    if (pipe(p) < 0) {
        OMPI_ERROR_LOG(OMPI_ERROR);
        free(cmd);
        return OMPI_ERROR;
    }

    /* we also have to give the DVM a pipe it can watch to know when
     * we terminated. Since the DVM is going to be a child of us, it
     * can't just use waitpid to see when we leave - so it will watch
     * the pipe instead
     */
    if (pipe(death_pipe) < 0) {
        OMPI_ERROR_LOG(OMPI_ERROR);
        close(p[0]);
        close(p[1]);
        free(cmd);
        return OMPI_ERROR;
    }

   /* we need to start the PRRTE DVM first so we can
     * spawn processes - see if they gave us any hostfile
     * or dash-host options we should pass along */
    opal_argv_append_nosize(&args, "prte");
    /* ensure we use the PRRTE personality */
    opal_argv_append_nosize(&args, "--prtemca");
    opal_argv_append_nosize(&args, "schizo");
    opal_argv_append_nosize(&args, "prte");
    if (NULL != hostfiles) {
        tmp = opal_argv_join(hostfiles, ',');
        opal_argv_append_nosize(&args, "--hostfile");
        opal_argv_append_nosize(&args, tmp);
        free(tmp);
    }
    if (NULL != dash_host) {
        tmp = opal_argv_join(dash_host, ',');
        opal_argv_append_nosize(&args, "--host");
        opal_argv_append_nosize(&args, tmp);
        free(tmp);
    }
    opal_argv_append_nosize(&args, "--no-ready-msg");
    opal_argv_append_nosize(&args, "--report-uri");
    opal_asprintf(&tmp, "%d", p[1]);
    opal_argv_append_nosize(&args, tmp);
    free(tmp);
    opal_argv_append_nosize(&args, "--singleton");
    opal_argv_append_nosize(&args, OMPI_PRINT_ID(OMPI_PROC_MYID));
    opal_argv_append_nosize(&args, "--keepalive");
    opal_asprintf(&tmp, "%d", death_pipe[0]);
    opal_argv_append_nosize(&args, tmp);
    free(tmp);

    /* Fork off the child */
    pid = fork();
    if (pid < 0) {
        OMPI_ERROR_LOG(OMPI_ERROR);
        close(p[0]);
        close(p[1]);
        close(death_pipe[0]);
        close(death_pipe[1]);
        free(cmd);
        opal_argv_free(args);
        return OMPI_ERROR;
    }

    if (pid == 0) {
        close(p[0]);
        close(death_pipe[1]);
        /* I am the child - exec me */

        /* Set signal handlers back to the default.  Do this close
         to the execve() because the event library may (and likely
         will) reset them.  If we don't do this, the event
         library may have left some set that, at least on some
         OS's, don't get reset via fork() or exec().  Hence, the
         prted could be unkillable (for example). */
        set_handler_default(SIGTERM);
        set_handler_default(SIGINT);
        set_handler_default(SIGHUP);
        set_handler_default(SIGPIPE);
        set_handler_default(SIGCHLD);

        /* Unblock all signals, for many of the same reasons that
         we set the default handlers, above.  This is noticeable
         on Linux where the event library blocks SIGTERM, but we
         don't want that blocked by the prted (or, more
         specifically, we don't want it to be blocked by the
         prted and then inherited by the PRTE processes that it
         forks, making them unkillable by SIGTERM). */
        sigprocmask(0, 0, &sigs);
        sigprocmask(SIG_UNBLOCK, &sigs, 0);

        execv(cmd, args);

        /* if I get here, the execv failed! */
        opal_show_help("help-dpm.txt", "execv-error",
                       true, cmd, strerror(errno));
        exit(1);

    }

    free(cmd);
    /* I am the parent - wait to hear something back and
     * report results
     */
    close(p[1]);  /* parent closes the write - prte will write its contact info to it*/
    close(death_pipe[0]);  /* parent closes the death_pipe's read */
    opal_argv_free(args);

    /* setup the buffer to read the DVM's uri */
    buffer_length = DVM_URI_MSG_LGTH;
    chunk = DVM_URI_MSG_LGTH-1;
    num_chars_read = 0;
    uri = (char*)malloc(buffer_length);
    memset(uri, 0, buffer_length);

    while (0 != (rc = read(p[0], &uri[num_chars_read], chunk))) {
        if (rc < 0 && (EAGAIN == errno || EINTR == errno)) {
            continue;
        } else if (rc < 0) {
            num_chars_read = -1;
            break;
        }
        /* we read something - better get more */
        num_chars_read += rc;
        chunk -= rc;
        if (0 == chunk) {
            chunk = DVM_URI_MSG_LGTH;
            uri = realloc((void*)uri, buffer_length+chunk);
            memset(&uri[buffer_length], 0, chunk);
            buffer_length += chunk;
        }
    }
    close(p[0]);

    if (num_chars_read <= 0) {
        /* we didn't get anything back - this is bad */
        OMPI_ERROR_LOG(OMPI_ERROR);
        free(uri);
        return OMPI_ERROR;
    }

    /* connect to the spawned PRRTE daemon */
    PMIX_INFO_LOAD(&info, PMIX_SERVER_URI, uri, PMIX_STRING);
    free(uri);
    pret = PMIx_Init(NULL, &info, 1);
    rc = opal_pmix_convert_status(pret);
    if (OPAL_SUCCESS != rc) {
        return MPI_ERR_SPAWN;
    }
    /* decrement the PMIx init refcount */
    PMIx_Finalize(NULL, 0);

    /* push our information up to the server */
    PMIx_Commit();

    /* we are no longer a singleton */
     opal_process_info.is_singleton = false;

    return OMPI_SUCCESS;
}
#else
static int start_dvm(char **hostfiles, char **dash_host)
{
    return OMPI_ERROR;
}
#endif
