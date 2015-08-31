/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2015 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2006-2009 University of Houston.  All rights reserved.
 * Copyright (c) 2009      Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2011-2013 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2013-2015 Intel, Inc. All rights reserved
 * Copyright (c) 2014-2015 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
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

#include "opal/util/alfg.h"
#include "opal/util/argv.h"
#include "opal/util/opal_getcwd.h"
#include "opal/util/proc.h"
#include "opal/dss/dss.h"
#include "opal/mca/hwloc/base/base.h"
#include "opal/mca/pmix/pmix.h"

#include "ompi/communicator/communicator.h"
#include "ompi/group/group.h"
#include "ompi/proc/proc.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/rte/rte.h"
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

typedef struct {
    opal_list_item_t super;
    ompi_proc_t *p;
} ompi_dpm_proct_caddy_t;
static OBJ_CLASS_INSTANCE(ompi_dpm_proct_caddy_t,
                          opal_list_item_t,
                          NULL, NULL);

struct lookup_caddy_t {
    bool active;
    opal_pmix_pdata_t *pdat;
};

static void lookup_cbfunc(int status, opal_list_t *data, void *cbdata)
{
    struct lookup_caddy_t *cd = (struct lookup_caddy_t*)cbdata;
    opal_pmix_pdata_t *p = (opal_pmix_pdata_t*)opal_list_get_first(data);
    if (NULL != p && OPAL_STRING == p->value.type &&
        NULL != p->value.data.string) {
        cd->pdat->value.data.string = strdup(p->value.data.string);
    }
    cd->active = false;
}


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

int ompi_dpm_connect_accept(ompi_communicator_t *comm, int root,
                            const char *port_string, bool send_first,
                            ompi_communicator_t **newcomm)
{
    int size, rsize, rank, rc;
    char **members = NULL, *nstring;
    bool dense, isnew;
    opal_process_name_t pname;
    opal_list_t ilist, mlist, rlist;
    opal_pmix_info_t *info;
    opal_pmix_pdata_t *pdat;
    opal_namelist_t *nm;

    ompi_communicator_t *newcomp=MPI_COMM_NULL;
    ompi_proc_t *proc;
    ompi_group_t *group=comm->c_local_group;
    ompi_proc_t **proc_list=NULL, **new_proc_list = NULL;
    int32_t i,j;
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

    /* everyone constructs the list of members from their communicator */
    if (MPI_COMM_WORLD == comm) {
        pname.jobid = OMPI_PROC_MY_NAME->jobid;
        pname.vpid = OPAL_VPID_WILDCARD;
        rc = opal_convert_process_name_to_string(&nstring, &pname);
        if (OPAL_SUCCESS != rc) {
            return OMPI_ERROR;
        }
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
                    ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
                    rc = ORTE_ERR_NOT_FOUND;
                    goto exit;
                }
            }
            dense = false;
        }
        for (i=0; i < size; i++) {
            rc = opal_convert_process_name_to_string(&nstring, &(proc_list[i]->super.proc_name));
            if (OPAL_SUCCESS != rc) {
                if (!dense) {
                    free(proc_list);
                }
                return OMPI_ERROR;
            }
            opal_argv_append_nosize(&members, nstring);
            free(nstring);
        }
        if (!dense) {
            free(proc_list);
        }
    }

    if (rank == root) {
        /* put my name at the front of the list of members - my
         * name will therefore be on the list twice, but the
         * other side needs to know the root from this side */
        rc = opal_convert_process_name_to_string(&nstring, OMPI_PROC_MY_NAME);
        if (OPAL_SUCCESS != rc) {
                return OMPI_ERROR;
        }
        opal_argv_prepend_nosize(&members, nstring);
        free(nstring);
        /* the root for each side publishes their list of participants */
        OBJ_CONSTRUCT(&ilist, opal_list_t);
        info = OBJ_NEW(opal_pmix_info_t);
        opal_list_append(&ilist, &info->super);

        if (send_first) {
            (void)asprintf(&info->key, "%s:connect", port_string);
            info->value.type = OPAL_STRING;
            info->value.data.string = opal_argv_join(members, ':');
        } else {
            (void)asprintf(&info->key, "%s:accept", port_string);
            info->value.type = OPAL_STRING;
            info->value.data.string = opal_argv_join(members, ':');
        }
        /* publish it with "session" scope */
        rc = opal_pmix.publish(OPAL_PMIX_SESSION,
                               OPAL_PMIX_PERSIST_APP,
                               &ilist);
        OPAL_LIST_DESTRUCT(&ilist);
        if (OPAL_SUCCESS != rc) {
            opal_argv_free(members);
            return OMPI_ERROR;
        }
    }

    /* lookup the other side's info - if a non-blocking form
     * of lookup isn't available, then we use the blocking
     * form and trust that the underlying system will WAIT
     * until the other side publishes its data */
    OBJ_CONSTRUCT(&ilist, opal_list_t);
    pdat = OBJ_NEW(opal_pmix_pdata_t);
    if (send_first) {
        (void)asprintf(&pdat->key, "%s:accept", port_string);
    } else {
        (void)asprintf(&pdat->key, "%s:connect", port_string);
    }
    opal_list_append(&ilist, &pdat->super);
    if (NULL == opal_pmix.lookup_nb) {
        rc = opal_pmix.lookup(OPAL_PMIX_SESSION, &ilist);
        if (OPAL_SUCCESS != rc) {
            OPAL_LIST_DESTRUCT(&ilist);
            opal_argv_free(members);
            return OMPI_ERROR;
        }
    } else {
        /* specifically request that the lookup wait until
         * the given data has been published */
        char **keys = NULL;
        struct lookup_caddy_t caddy;
        opal_argv_append_nosize(&keys, pdat->key);
        caddy.active = true;
        caddy.pdat = pdat;
        rc = opal_pmix.lookup_nb(OPAL_PMIX_SESSION, true, keys,
                                 lookup_cbfunc, &caddy);
        if (OPAL_SUCCESS != rc) {
            OPAL_LIST_DESTRUCT(&ilist);
            opal_argv_free(keys);
            opal_argv_free(members);
            return OMPI_ERROR;
        }
        OMPI_WAIT_FOR_COMPLETION(caddy.active);
    }
    /* initiate a list of participants for the connect,
     * starting with our own members, remembering to
     * skip the first member if we are the root rank */
    if (rank == root) {
        j = 1;
    } else {
        j = 0;
    }
    OBJ_CONSTRUCT(&mlist, opal_list_t);
    for (i=j; NULL != members[i]; i++) {
        nm = OBJ_NEW(opal_namelist_t);
        opal_convert_string_to_process_name(&nm->name, members[i]);
        opal_list_append(&mlist, &nm->super);
    }
    opal_argv_free(members);
    members = NULL;

    /* the pdat object will contain a colon-delimited list
     * of process names for the remote procs - convert it
     * into an argv array */
    members = opal_argv_split(pdat->value.data.string, ':');
    OPAL_LIST_DESTRUCT(&ilist);

    /* the first entry is the root for the remote side */
    opal_convert_string_to_process_name(&pname, members[0]);

    /* add the list of remote procs to our list, and
     * keep a list of them for later */
    OBJ_CONSTRUCT(&ilist, opal_list_t);
    OBJ_CONSTRUCT(&rlist, opal_list_t);
    for (i=1; NULL != members[i]; i++) {
        nm = OBJ_NEW(opal_namelist_t);
        opal_convert_string_to_process_name(&nm->name, members[i]);
        opal_list_append(&mlist, &nm->super);
        /* see if this needs to be added to our ompi_proc_t array */
        proc = ompi_proc_find_and_add(&nm->name, &isnew);
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
    opal_argv_free(members);

    /* tell the host RTE to connect us - this will download
     * all known data for the nspace's of participating procs */
    rc = opal_pmix.connect(&mlist);
    OPAL_LIST_DESTRUCT(&mlist);

#if 0
        /* set the locality of the new procs - the required info should
         * have been included in the data exchange */
        for (j=0; j < new_proc_len; j++) {
            OBJ_CONSTRUCT(&myvals, opal_list_t);
            if (OMPI_SUCCESS != (rc = opal_dstore.fetch(opal_dstore_internal,
                                                         &new_proc_list[j]->super.proc_name,
                                                         OPAL_DSTORE_LOCALITY, &myvals))) {
                new_proc_list[j]->super.proc_flags = OPAL_PROC_NON_LOCAL;
            } else {
                kv = (opal_value_t*)opal_list_get_first(&myvals);
                new_proc_list[j]->super.proc_flags = kv->data.uint16;
            }
            OPAL_LIST_DESTRUCT(&myvals);
        }
#endif
    if (0 < opal_list_get_size(&ilist)) {
        /* convert the list of new procs to a proc_t array */
        new_proc_list = (ompi_proc_t**)calloc(opal_list_get_size(&ilist),
                                              sizeof(ompi_proc_t *));
        i = 0;
        OPAL_LIST_FOREACH(cd, &ilist, ompi_dpm_proct_caddy_t) {
            new_proc_list[i++] = cd->p;
        }
        /* call add_procs on the new ones */
        rc = MCA_PML_CALL(add_procs(new_proc_list, opal_list_get_size(&ilist)));
        free(new_proc_list);
        if (OMPI_SUCCESS != rc) {
            OMPI_ERROR_LOG(rc);
            OPAL_LIST_DESTRUCT(&ilist);
            goto exit;
        }
    }
    OPAL_LIST_DESTRUCT(&ilist);

    /* now deal with the remote group */
    rsize = opal_list_get_size(&rlist);
    new_group_pointer=ompi_group_allocate(rsize);
    if (NULL == new_group_pointer) {
        rc = OMPI_ERR_OUT_OF_RESOURCE;
        OPAL_LIST_DESTRUCT(&rlist);
        goto exit;
    }
    /* assign group elements */
    i=0;
    OPAL_LIST_FOREACH(cd, &rlist, ompi_dpm_proct_caddy_t) {
        new_group_pointer->grp_proc_pointers[i++] = cd->p;
    }
    OPAL_LIST_DESTRUCT(&rlist);

    /* increment proc reference counters */
    ompi_group_increment_proc_count(new_group_pointer);

    /* set up communicator structure */
    rc = ompi_comm_set ( &newcomp,                 /* new comm */
                         comm,                     /* old comm */
                         group->grp_proc_count,    /* local_size */
                         NULL,                     /* local_procs */
                         rsize,                    /* remote_size */
                         NULL  ,                   /* remote_procs */
                         NULL,                     /* attrs */
                         comm->error_handler,      /* error handler */
                         NULL,                     /* topo component */
                         group,                    /* local group */
                         new_group_pointer         /* remote group */
                         );
    if ( NULL == newcomp ) {
        rc = OMPI_ERR_OUT_OF_RESOURCE;
        goto exit;
    }

    ompi_group_decrement_proc_count (new_group_pointer);
    OBJ_RELEASE(new_group_pointer);
    new_group_pointer = MPI_GROUP_NULL;

    /* allocate comm_cid */
    rc = ompi_comm_nextcid ( newcomp,                 /* new communicator */
                             comm,                    /* old communicator */
                             NULL,                    /* bridge comm */
                             &root,                   /* local leader */
                             &pname,                  /* remote leader */
                             OMPI_COMM_CID_INTRA_OOB, /* mode */
                             send_first );            /* send or recv first */
    if (OMPI_SUCCESS != rc) {
        goto exit;
    }

    /* activate comm and init coll-component */
    rc = ompi_comm_activate ( &newcomp,                /* new communicator */
                              comm,                    /* old communicator */
                              NULL,                    /* bridge comm */
                              &root,                   /* local leader */
                              &pname,                  /* remote leader */
                              OMPI_COMM_CID_INTRA_OOB, /* mode */
                              send_first );            /* send or recv first */
    if (OMPI_SUCCESS != rc) {
        goto exit;
    }

    /* Question: do we have to re-start some low level stuff
       to enable the usage of fast communication devices
       between the two worlds ?
    */

 exit:
    if (NULL != proc_list) {
        free (proc_list);
    }
    if (NULL != new_proc_list) {
        free (new_proc_list);
    }
    if (OMPI_SUCCESS != rc) {
        if (MPI_COMM_NULL != newcomp && NULL != newcomp) {
            OBJ_RETAIN(newcomp);
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

    if (OMPI_GROUP_IS_DENSE(group)) {
        for (i=0; i < group->grp_proc_count; i++) {
            if (NULL == (proct = group->grp_proc_pointers[i])) {
                OMPI_ERROR_LOG(ORTE_ERR_NOT_FOUND);
                return OMPI_ERR_NOT_FOUND;
            }
            /* add to the list of peers */
            nm = OBJ_NEW(opal_namelist_t);
            nm->name = *(opal_process_name_t*)&proct->super.proc_name;
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
    } else {
        for (i=0; i < group->grp_proc_count; i++) {
            /* lookup this proc_t to get the process name */
            if (NULL == (proct = ompi_group_peer_lookup(group, i))) {
                OMPI_ERROR_LOG(OMPI_ERR_NOT_FOUND);
                return OMPI_ERR_NOT_FOUND;
            }
            /* add to the list of peers */
            nm = OBJ_NEW(opal_namelist_t);
            nm->name = *(opal_process_name_t*)&proct->super.proc_name;
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
    }
    return ORTE_SUCCESS;
}

int ompi_dpm_disconnect(ompi_communicator_t *comm)
{
    int ret;
    ompi_group_t *group;
    opal_list_t coll;

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

    opal_pmix.fence(&coll, false);
    OPAL_LIST_DESTRUCT(&coll);

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
    char cwd[OPAL_PATH_MAX];
    char host[OPAL_MAX_INFO_VAL];  /*** should define OMPI_HOST_MAX ***/
    char prefix[OPAL_MAX_INFO_VAL];
    char stdin_target[OPAL_MAX_INFO_VAL];
    char params[OPAL_MAX_INFO_VAL];
    char mapper[OPAL_MAX_INFO_VAL];
    char slot_list[OPAL_MAX_INFO_VAL];
    uint32_t ui32;
    bool personality = false;
    opal_jobid_t jobid;

    opal_list_t apps;
    opal_list_t job_info;
    opal_pmix_app_t *app;
    opal_pmix_info_t *info;
    bool local_spawn, non_mpi;
    char **envars;

    /* parse the info object */
    /* check potentially for:
       - "host": desired host where to spawn the processes
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
       - "arch": desired architecture
       - "wdir": directory, where executable can be found
       - "path": list of directories where to look for the executable
       - "file": filename, where additional information is provided.
       - "soft": see page 92 of MPI-2.
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
       - "env": newline (\n) delimited list of envar values to be passed to spawned procs
    */

    /* setup the job object */
    OBJ_CONSTRUCT(&job_info, opal_list_t);
    OBJ_CONSTRUCT(&apps, opal_list_t);

    /* Convert the list of commands to list of opal_pmix_app_t */
    for (i = 0; i < count; ++i) {
        app = OBJ_NEW(opal_pmix_app_t);
        if (NULL == app) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            OPAL_LIST_DESTRUCT(&apps);
            opal_progress_event_users_decrement();
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        /* add the app to the job data */
        opal_list_append(&apps, &app->super);

        /* copy over the name of the executable */
        app->cmd = strdup(array_of_commands[i]);
        opal_argv_append(&app->argc, &app->argv, app->cmd);

        /* record the number of procs to be generated */
        app->maxprocs = array_of_maxprocs[i];

        /* copy over the argv array */
        if (MPI_ARGVS_NULL != array_of_argv &&
            MPI_ARGV_NULL != array_of_argv[i]) {
            for (j=1; NULL != array_of_argv[i][j]; j++) {
                opal_argv_append(&app->argc, &app->argv, array_of_argv[i][j-1]);
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
            ompi_info_get (array_of_info[i], "personality", sizeof(host) - 1, host, &flag);
            if ( flag ) {
                personality = true;
                info = OBJ_NEW(opal_pmix_info_t);
                info->key = strdup(OPAL_PMIX_PERSONALITY);
                opal_value_load(&info->value, host, OPAL_STRING);
                opal_list_append(&job_info, &info->super);
            }

            /* check for 'host' */
            ompi_info_get (array_of_info[i], "host", sizeof(host) - 1, host, &flag);
            if ( flag ) {
                info = OBJ_NEW(opal_pmix_info_t);
                info->key = strdup(OPAL_PMIX_HOST);
                opal_value_load(&info->value, host, OPAL_STRING);
                opal_list_append(&app->info, &info->super);
            }

            /* check for 'hostfile' */
            ompi_info_get (array_of_info[i], "hostfile", sizeof(host) - 1, host, &flag);
            if ( flag ) {
                info = OBJ_NEW(opal_pmix_info_t);
                info->key = strdup(OPAL_PMIX_HOSTFILE);
                opal_value_load(&info->value, host, OPAL_STRING);
                opal_list_append(&app->info, &info->super);
            }

            /* check for 'add-hostfile' */
            ompi_info_get (array_of_info[i], "add-hostfile", sizeof(host) - 1, host, &flag);
            if ( flag ) {
                info = OBJ_NEW(opal_pmix_info_t);
                info->key = strdup(OPAL_PMIX_ADD_HOSTFILE);
                opal_value_load(&info->value, host, OPAL_STRING);
                opal_list_append(&app->info, &info->super);
            }

            /* check for 'add-host' */
            ompi_info_get (array_of_info[i], "add-host", sizeof(host) - 1, host, &flag);
            if ( flag ) {
                info = OBJ_NEW(opal_pmix_info_t);
                info->key = strdup(OPAL_PMIX_ADD_HOST);
                opal_value_load(&info->value, host, OPAL_STRING);
                opal_list_append(&app->info, &info->super);
            }

            /* check for env */
            ompi_info_get (array_of_info[i], "env", sizeof(host)-1, host, &flag);
            if ( flag ) {
                envars = opal_argv_split(host, '\n');
                for (j=0; NULL != envars[j]; j++) {
                    opal_argv_append_nosize(&app->env, envars[j]);
                }
                opal_argv_free(envars);
            }

            /* 'path', 'arch', 'file', 'soft'  -- to be implemented */

            /* check for 'ompi_prefix' (OMPI-specific -- to effect the same
             * behavior as --prefix option to orterun)
             *
             * This is a job-level key
             */
            ompi_info_get (array_of_info[i], "ompi_prefix", sizeof(prefix) - 1, prefix, &flag);
            if ( flag ) {
                info = OBJ_NEW(opal_pmix_info_t);
                info->key = strdup(OPAL_PMIX_PREFIX);
                opal_value_load(&info->value, prefix, OPAL_STRING);
                opal_list_append(&job_info, &info->super);
            }

            /* check for 'wdir' */
            ompi_info_get (array_of_info[i], "wdir", sizeof(cwd) - 1, cwd, &flag);
            if ( flag ) {
                info = OBJ_NEW(opal_pmix_info_t);
                info->key = strdup(OPAL_PMIX_WDIR);
                opal_value_load(&info->value, cwd, OPAL_STRING);
                opal_list_append(&app->info, &info->super);
                have_wdir = 1;
            }

            /* check for 'mapper' - a job-level key */
            ompi_info_get(array_of_info[i], "mapper", sizeof(mapper) - 1, mapper, &flag);
            if ( flag ) {
                info = OBJ_NEW(opal_pmix_info_t);
                info->key = strdup(OPAL_PMIX_MAPPER);
                opal_value_load(&info->value, mapper, OPAL_STRING);
                opal_list_append(&job_info, &info->super);
            }

            /* check for 'display_map' - a job-level key */
            ompi_info_get_bool(array_of_info[i], "display_map", &local_spawn, &flag);
            if ( flag ) {
                info = OBJ_NEW(opal_pmix_info_t);
                info->key = strdup(OPAL_PMIX_DISPLAY_MAP);
                opal_value_load(&info->value, &local_spawn, OPAL_BOOL);
                opal_list_append(&job_info, &info->super);
            }

            /* check for 'npernode' and 'ppr' - job-level key */
            ompi_info_get (array_of_info[i], "npernode", sizeof(slot_list) - 1, slot_list, &flag);
            if ( flag ) {
                info = OBJ_NEW(opal_pmix_info_t);
                info->key = strdup(OPAL_PMIX_PPR);
                info->value.type = OPAL_STRING;
                (void)asprintf(&(info->value.data.string), "%s:n", slot_list);
                opal_list_append(&job_info, &info->super);
            }
            ompi_info_get (array_of_info[i], "pernode", sizeof(slot_list) - 1, slot_list, &flag);
            if ( flag ) {
                info = OBJ_NEW(opal_pmix_info_t);
                info->key = strdup(OPAL_PMIX_PPR);
                opal_value_load(&info->value, "1:n", OPAL_STRING);
                opal_list_append(&job_info, &info->super);
            }
            ompi_info_get (array_of_info[i], "ppr", sizeof(slot_list) - 1, slot_list, &flag);
            if ( flag ) {
                info = OBJ_NEW(opal_pmix_info_t);
                info->key = strdup(OPAL_PMIX_PPR);
                opal_value_load(&info->value, slot_list, OPAL_STRING);
                opal_list_append(&job_info, &info->super);
            }

            /* check for 'map_by' - job-level key */
            ompi_info_get(array_of_info[i], "map_by", sizeof(slot_list) - 1, slot_list, &flag);
            if ( flag ) {
                info = OBJ_NEW(opal_pmix_info_t);
                info->key = strdup(OPAL_PMIX_MAPBY);
                opal_value_load(&info->value, slot_list, OPAL_STRING);
                opal_list_append(&job_info, &info->super);
            }

            /* check for 'rank_by' - job-level key */
            ompi_info_get(array_of_info[i], "rank_by", sizeof(slot_list) - 1, slot_list, &flag);
            if ( flag ) {
                info = OBJ_NEW(opal_pmix_info_t);
                info->key = strdup(OPAL_PMIX_RANKBY);
                opal_value_load(&info->value, slot_list, OPAL_STRING);
                opal_list_append(&job_info, &info->super);
            }

#if OPAL_HAVE_HWLOC
            /* check for 'bind_to' - job-level key */
            ompi_info_get(array_of_info[i], "bind_to", sizeof(slot_list) - 1, slot_list, &flag);
            if ( flag ) {
                info = OBJ_NEW(opal_pmix_info_t);
                info->key = strdup(OPAL_PMIX_BINDTO);
                opal_value_load(&info->value, slot_list, OPAL_STRING);
                opal_list_append(&job_info, &info->super);
            }
#endif

            /* check for 'preload_binary' - job-level key */
            ompi_info_get_bool(array_of_info[i], "ompi_preload_binary", &local_spawn, &flag);
            if ( flag ) {
                info = OBJ_NEW(opal_pmix_info_t);
                info->key = strdup(OPAL_PMIX_PRELOAD_BIN);
                opal_value_load(&info->value, &local_spawn, OPAL_BOOL);
                opal_list_append(&job_info, &info->super);
            }

            /* check for 'preload_files' - job-level key */
            ompi_info_get (array_of_info[i], "ompi_preload_files", sizeof(cwd) - 1, cwd, &flag);
            if ( flag ) {
                info = OBJ_NEW(opal_pmix_info_t);
                info->key = strdup(OPAL_PMIX_PRELOAD_FILES);
                opal_value_load(&info->value, cwd, OPAL_STRING);
                opal_list_append(&job_info, &info->super);
            }

            /* see if this is a non-mpi job - if so, then set the flag so ORTE
             * knows what to do - job-level key
             */
            ompi_info_get_bool(array_of_info[i], "ompi_non_mpi", &non_mpi, &flag);
            if (flag && non_mpi) {
                info = OBJ_NEW(opal_pmix_info_t);
                info->key = strdup(OPAL_PMIX_NON_PMI);
                opal_value_load(&info->value, &non_mpi, OPAL_BOOL);
                opal_list_append(&job_info, &info->super);
            }

            /* see if this is an MCA param that the user wants applied to the child job */
            ompi_info_get (array_of_info[i], "ompi_param", sizeof(params) - 1, params, &flag);
            if ( flag ) {
                opal_argv_append_unique_nosize(&app->env, params, true);
            }

            /* see if user specified what to do with stdin - defaults to
             * not forwarding stdin to child processes - job-level key
             */
            ompi_info_get (array_of_info[i], "ompi_stdin_target", sizeof(stdin_target) - 1, stdin_target, &flag);
            if ( flag ) {
                if (0 == strcmp(stdin_target, "all")) {
                    ui32 = ORTE_VPID_WILDCARD;
                } else if (0 == strcmp(stdin_target, "none")) {
                    ui32 = ORTE_VPID_INVALID;
                } else {
                    ui32 = strtoul(stdin_target, NULL, 10);
                }
                info = OBJ_NEW(opal_pmix_info_t);
                info->key = strdup(OPAL_PMIX_STDIN_TGT);
                opal_value_load(&info->value, &ui32, OPAL_UINT32);
                opal_list_append(&job_info, &info->super);
            }
        }

        /* default value: If the user did not tell us where to look for the
         * executable, we assume the current working directory
         */
        if ( !have_wdir ) {
            if (OMPI_SUCCESS != (rc = opal_getcwd(cwd, OPAL_PATH_MAX))) {
                ORTE_ERROR_LOG(rc);
                OPAL_LIST_DESTRUCT(&apps);
                opal_progress_event_users_decrement();
                return rc;
            }
            info = OBJ_NEW(opal_pmix_info_t);
            info->key = strdup(OPAL_PMIX_WDIR);
            opal_value_load(&info->value, cwd, OPAL_STRING);
            opal_list_append(&app->info, &info->super);
        }

        /* leave the map info alone - the launcher will
         * decide where to put things
         */
    } /* for (i = 0 ; i < count ; ++i) */

    /* default the personality - job-level key */
    if (!personality) {
        info = OBJ_NEW(opal_pmix_info_t);
        info->key = strdup(OPAL_PMIX_PERSONALITY);
        opal_value_load(&info->value, "ompi", OPAL_STRING);
        opal_list_append(&job_info, &info->super);
    }

    /* spawn procs */
    rc = opal_pmix.spawn(&job_info, &apps, &jobid);
    OPAL_LIST_DESTRUCT(&job_info);
    OPAL_LIST_DESTRUCT(&apps);

    if (OPAL_SUCCESS != rc) {
        OMPI_ERROR_LOG(rc);
        opal_progress_event_users_decrement();
        return MPI_ERR_SPAWN;
    }

    return OMPI_SUCCESS;
}

/* Create a rendezvous tag consisting of our name + a random number */
int ompi_dpm_open_port(char *port_name)
{
    uint32_t r;

    r = opal_rand(&rnd);
    snprintf(port_name, MPI_MAX_PORT_NAME, "%s:%u",
             OMPI_NAME_PRINT(OMPI_PROC_MY_NAME), r);
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


/*
 * finalize the module
 */
int ompi_dpm_finalize(void)
{
    return OMPI_SUCCESS;
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
        objs = (ompi_dpm_disconnect_obj**)malloc(ompi_comm_num_dyncomm*
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

        if (j != ompi_comm_num_dyncomm+1) {
            free(objs);
            return OMPI_ERROR;
        }

        disconnect_waitall(ompi_comm_num_dyncomm, objs);
        free(objs);
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

    /* Finally, free everything */
    for (i=0; i< count; i++ ) {
        if (NULL != objs[i]->reqs ) {
            free(objs[i]->reqs );
            free(objs[i]);
        }
    }

    free(reqs);

    return ret;
}

/**********************************************************************/
/**********************************************************************/
/**********************************************************************/
/* All we want to do in this function is determine if the number of
 * jobids in the local and/or remote group is > 1. This tells us to
 * set the disconnect flag. We don't actually care what the true
 * number -is-, only that it is > 1
 */
void ompi_dpm_mark_dyncomm(ompi_communicator_t *comm)
{
    int i;
    int size, rsize;
    bool found=false;
    ompi_jobid_t thisjobid;
    ompi_group_t *grp=NULL;
    ompi_proc_t *proc = NULL;

    /* special case for MPI_COMM_NULL */
    if (comm == MPI_COMM_NULL) {
        return;
    }

    size  = ompi_comm_size(comm);
    rsize = ompi_comm_remote_size(comm);

    /* loop over all processes in local group and check for
     * a different jobid
     */
    grp = comm->c_local_group;
    proc = ompi_group_peer_lookup(grp,0);
    thisjobid = ((ompi_process_name_t*)&proc->super.proc_name)->jobid;

    for (i=1; i< size; i++) {
        proc = ompi_group_peer_lookup(grp,i);
        if (thisjobid != ((ompi_process_name_t*)&proc->super.proc_name)->jobid) {
            /* at least one is different */
            found = true;
            goto complete;
        }
    }

    /* if inter-comm, loop over all processes in remote_group
     * and see if any are different from thisjobid
     */
    grp = comm->c_remote_group;
    for (i=0; i< rsize; i++) {
        proc = ompi_group_peer_lookup(grp,i);
        if (thisjobid != ((ompi_process_name_t*)&proc->super.proc_name)->jobid) {
            /* at least one is different */
            found = true;
            break;
        }
    }

 complete:
    /* if a different jobid was found, set the disconnect flag*/
    if (found) {
        ompi_comm_num_dyncomm++;
        OMPI_COMM_SET_DYNAMIC(comm);
    }

    return;
}
