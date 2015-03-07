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
 * Copyright (c) 2013-2014 Intel, Inc. All rights reserved
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
#if HAVE_TIME_H
#include <time.h>
#endif
#if HAVE_SYS_TIME_H
#include <sys/time.h>
#endif

#include "opal/util/argv.h"
#include "opal/util/opal_getcwd.h"
#include "opal/dss/dss.h"
#include "opal/mca/dstore/dstore.h"
#include "opal/mca/hwloc/base/base.h"
#include "opal/mca/pmix/pmix.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/grpcomm/base/base.h"
#include "orte/mca/plm/base/base.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/rml/rml_types.h"
#include "orte/mca/rmaps/rmaps.h"
#include "orte/mca/rmaps/rmaps_types.h"
#include "orte/mca/rmaps/base/base.h"
#include "orte/mca/rml/base/rml_contact.h"
#include "orte/mca/routed/routed.h"
#include "orte/util/name_fns.h"
#include "orte/util/show_help.h"
#include "orte/runtime/orte_globals.h"
#include "orte/runtime/orte_wait.h"

#include "ompi/communicator/communicator.h"
#include "ompi/group/group.h"
#include "ompi/proc/proc.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/rte/rte.h"
#include "ompi/info/info.h"

#include "ompi/mca/dpm/base/base.h"
#include "dpm_orte.h"

/* Local static variables */
static opal_mutex_t ompi_dpm_port_mutex;
static orte_rml_tag_t next_tag;
static opal_list_t orte_dpm_acceptors, orte_dpm_connectors, dynamics;
static uint32_t next_preq=0;

/* API functions */
static int init(void);
static int connect_accept (ompi_communicator_t *comm, int root,
                           const char *port_string, bool send_first,
                           ompi_communicator_t **newcomm);
static int disconnect(ompi_communicator_t *comm);
static int spawn(int count, const char *array_of_commands[],
                 char **array_of_argv[],
                 const int array_of_maxprocs[],
                 const MPI_Info array_of_info[],
                 const char *port_name);
static int dyn_init(void);
static int open_port(char *port_name, orte_rml_tag_t given_tag);
static int parse_port_name(const char *port_name, char **hnp_uri, char **rml_uri,
                           orte_rml_tag_t *tag);
static int route_to_port(char *rml_uri, orte_process_name_t *rproc);
static int close_port(const char *port_name);
static int finalize(void);
static int dpm_pconnect(char *port,
                        struct timeval *timeout,
                        ompi_dpm_base_paccept_connect_callback_fn_t cbfunc,
                        void *cbdata);
static int dpm_paccept(char *port,
                       ompi_dpm_base_paccept_connect_callback_fn_t cbfunc,
                       void *cbdata);
static void dpm_pclose(char *port);

/*
 * instantiate the module
 */
ompi_dpm_base_module_t ompi_dpm_orte_module = {
    init,
    connect_accept,
    disconnect,
    spawn,
    dyn_init,
    ompi_dpm_base_dyn_finalize,
    ompi_dpm_base_mark_dyncomm,
    open_port,
    parse_port_name,
    route_to_port, 
    close_port,
    finalize,
    dpm_pconnect,
    dpm_paccept,
    dpm_pclose
};

typedef struct {
    opal_list_item_t super;
    opal_event_t ev;
    bool event_active;
    uint32_t id;
    uint32_t cid;
    orte_rml_tag_t tag;
    ompi_dpm_base_paccept_connect_callback_fn_t cbfunc;
    void *cbdata;
} orte_dpm_prequest_t;
OBJ_CLASS_INSTANCE(orte_dpm_prequest_t,
                   opal_list_item_t,
                   NULL, NULL);


static void connect_complete(int status, orte_process_name_t* sender,
                             opal_buffer_t* buffer, orte_rml_tag_t tag,
                             void* cbdata);

/*
 * Init the module
 */
static int init(void)
{    
    OBJ_CONSTRUCT(&ompi_dpm_port_mutex, opal_mutex_t);
    next_tag = OMPI_RML_TAG_DYNAMIC;
    OBJ_CONSTRUCT(&orte_dpm_acceptors, opal_list_t);
    OBJ_CONSTRUCT(&orte_dpm_connectors, opal_list_t);
    OBJ_CONSTRUCT(&dynamics, opal_list_t);

    /* post a receive for pconnect request responses */
    orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD,
                            OMPI_RML_PCONNECT_TAG,
                            ORTE_RML_PERSISTENT,
                            connect_complete, NULL);


    return OMPI_SUCCESS;
}

static int connect_accept(ompi_communicator_t *comm, int root,
                          const char *port_string, bool send_first,
                          ompi_communicator_t **newcomm)
{
    int size, rsize, rank, rc;
    orte_std_cntr_t num_vals;
    orte_std_cntr_t rnamebuflen = 0;
    int rnamebuflen_int = 0;
    void *rnamebuf=NULL;

    ompi_communicator_t *newcomp=MPI_COMM_NULL;
    ompi_proc_t **rprocs=NULL;
    ompi_group_t *group=comm->c_local_group;
    orte_process_name_t port;
    orte_rml_tag_t tag=ORTE_RML_TAG_INVALID;
    opal_buffer_t *nbuf=NULL, *nrbuf=NULL;
    ompi_proc_t **proc_list=NULL, **new_proc_list = NULL;
    int32_t i,j, new_proc_len;
    ompi_group_t *new_group_pointer;

    orte_namelist_t *nm;
    orte_rml_recv_cb_t xfer;
    orte_process_name_t carport;

    OPAL_OUTPUT_VERBOSE((1, ompi_dpm_base_framework.framework_output,
                         "%s dpm:orte:connect_accept with port %s %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         port_string, send_first ? "sending first" : "recv first"));
    
    /* set default error return */
    *newcomm = MPI_COMM_NULL;
    
    size = ompi_comm_size ( comm );
    rank = ompi_comm_rank ( comm );

    /* extract the process name from the port string, if given, and
     * set us up to communicate with it
     */
    if (NULL != port_string && 0 < strlen(port_string)) {
        char *hnp_uri, *rml_uri;

        /* separate the string into the HNP and RML URI and tag */
        if (ORTE_SUCCESS != (rc = parse_port_name(port_string, &hnp_uri, &rml_uri, &tag))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        /* extract the originating proc's name */
        if (ORTE_SUCCESS != (rc = orte_rml_base_parse_uris(rml_uri, &port, NULL))) {
            ORTE_ERROR_LOG(rc);
            free(hnp_uri); free(rml_uri);
            return rc;
        }
        /* make sure we can route rml messages to the destination job */
        if (ORTE_SUCCESS != (rc = route_to_port(hnp_uri, &port))) {
            ORTE_ERROR_LOG(rc);
            free(hnp_uri); free(rml_uri);
            return rc;
        }
        free(hnp_uri); free(rml_uri);
    }
    
    if ( rank == root ) {
        /* Generate the message buffer containing the number of processes and the list of
           participating processes */
        nbuf = OBJ_NEW(opal_buffer_t);
        if (NULL == nbuf) {
            return OMPI_ERROR;
        }
        
        if (OPAL_SUCCESS != (rc = opal_dss.pack(nbuf, &size, 1, OPAL_INT))) {
            ORTE_ERROR_LOG(rc);
            goto exit;
        }
        
        if (OMPI_GROUP_IS_DENSE(group)) {
            ompi_proc_pack(group->grp_proc_pointers, size, false, nbuf);
        } else {
            proc_list = (ompi_proc_t **) calloc (group->grp_proc_count, 
                                                 sizeof (ompi_proc_t *));
            for (i=0 ; i<group->grp_proc_count ; i++) {
                if (NULL == (proc_list[i] = ompi_group_peer_lookup(group,i))) {
                    ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
                    rc = ORTE_ERR_NOT_FOUND;
                    goto exit;
                }
                
                OPAL_OUTPUT_VERBOSE((3, ompi_dpm_base_framework.framework_output,
                                     "%s dpm:orte:connect_accept adding %s to proc list",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     OMPI_NAME_PRINT(&proc_list[i]->super.proc_name)));
            }
            ompi_proc_pack(proc_list, size, false, nbuf);
        }
        
        /* pack wireup info - this is required so that all involved parties can
         * discover how to talk to each other. For example, consider the case
         * where we connect_accept to one independent job (B), and then connect_accept
         * to another one (C) to wire all three of us together. Job B will not know
         * how to talk to job C at the OOB level because the two of them didn't
         * directly connect_accept to each other. Hence, we include the required
         * wireup info at this first exchange
         */
        if (ORTE_SUCCESS != (rc = orte_routed.get_wireup_info(nbuf))) {
            ORTE_ERROR_LOG(rc);
            goto exit;
        }

        OBJ_CONSTRUCT(&xfer, orte_rml_recv_cb_t);
        /* Exchange the number and the list of processes in the groups */
        if ( send_first ) {
            OPAL_OUTPUT_VERBOSE((3, ompi_dpm_base_framework.framework_output,
                                 "%s dpm:orte:connect_accept sending first to %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(&port)));
            rc = orte_rml.send_buffer_nb(&port, nbuf, tag, orte_rml_send_callback, NULL);
            /* setup to recv */
            OPAL_OUTPUT_VERBOSE((3, ompi_dpm_base_framework.framework_output,
                                 "%s dpm:orte:connect_accept waiting for response",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
            xfer.active = true;
            orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, tag,
                                    ORTE_RML_NON_PERSISTENT,
                                    orte_rml_recv_callback, &xfer);
            /* wait for response */
            OMPI_WAIT_FOR_COMPLETION(xfer.active);
            OPAL_OUTPUT_VERBOSE((3, ompi_dpm_base_framework.framework_output,
                                 "%s dpm:orte:connect_accept got data from %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(&xfer.name)));
            
        } else {
            OPAL_OUTPUT_VERBOSE((3, ompi_dpm_base_framework.framework_output,
                                 "%s dpm:orte:connect_accept recving first",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
            /* setup to recv */
            xfer.active = true;
            orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, tag,
                                    ORTE_RML_NON_PERSISTENT,
                                    orte_rml_recv_callback, &xfer);
            /* wait for response */
            OMPI_WAIT_FOR_COMPLETION(xfer.active);
            /* now send our info */
            OPAL_OUTPUT_VERBOSE((3, ompi_dpm_base_framework.framework_output,
                                 "%s dpm:orte:connect_accept sending info to %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(&xfer.name)));
            rc = orte_rml.send_buffer_nb(&xfer.name, nbuf, tag, orte_rml_send_callback, NULL);
        }

        if (OPAL_SUCCESS != (rc = opal_dss.unload(&xfer.data, &rnamebuf, &rnamebuflen))) {
            ORTE_ERROR_LOG(rc);
            OBJ_DESTRUCT(&xfer.data);
            goto exit;
        }
        carport.jobid = xfer.name.jobid;
        carport.vpid = xfer.name.vpid;
        OBJ_DESTRUCT(&xfer);
    }

    /* First convert the size_t to an int so we can cast in the bcast to a void *
     * if we don't then we will get badness when using big vs little endian
     * THIS IS NO LONGER REQUIRED AS THE LENGTH IS NOW A STD_CNTR_T, WHICH
     * CORRELATES TO AN INT32
     */
    rnamebuflen_int = (int)rnamebuflen;

    /* bcast the buffer-length to all processes in the local comm */
    OPAL_OUTPUT_VERBOSE((3, ompi_dpm_base_framework.framework_output,
                         "%s dpm:orte:connect_accept bcast buffer length",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    rc = comm->c_coll.coll_bcast (&rnamebuflen_int, 1, MPI_INT, root, comm,
                                  comm->c_coll.coll_bcast_module);
    if ( OMPI_SUCCESS != rc ) {
        goto exit;
    }
    rnamebuflen = rnamebuflen_int;

    if ( rank != root ) {
        /* non root processes need to allocate the buffer manually */
        rnamebuf = (char *) malloc(rnamebuflen);
        if ( NULL == rnamebuf ) {
            rc = OMPI_ERR_OUT_OF_RESOURCE;
            goto exit;
        }
    }

    /* bcast list of processes to all procs in local group
       and reconstruct the data. Note that proc_get_proclist
       adds processes, which were not known yet to our
       process pool.
    */
    OPAL_OUTPUT_VERBOSE((3, ompi_dpm_base_framework.framework_output,
                         "%s dpm:orte:connect_accept bcast proc list",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    rc = comm->c_coll.coll_bcast (rnamebuf, rnamebuflen_int, MPI_BYTE, root, comm,
                                  comm->c_coll.coll_bcast_module);
    if ( OMPI_SUCCESS != rc ) {
        goto exit;
    }

    nrbuf = OBJ_NEW(opal_buffer_t);
    if (NULL == nrbuf) {
        goto exit;
    }
    if ( OPAL_SUCCESS != ( rc = opal_dss.load(nrbuf, rnamebuf, rnamebuflen))) {
        ORTE_ERROR_LOG(rc);
        goto exit;
    }
    num_vals = 1;
    if (OPAL_SUCCESS != (rc = opal_dss.unpack(nrbuf, &rsize, &num_vals, OPAL_INT))) {
        ORTE_ERROR_LOG(rc);
        goto exit;
    }
    rc = ompi_proc_unpack(nrbuf, rsize, &rprocs, false, &new_proc_len, &new_proc_list);
    if ( OMPI_SUCCESS != rc ) {
        goto exit;
    }

    OPAL_OUTPUT_VERBOSE((3, ompi_dpm_base_framework.framework_output,
                         "%s dpm:orte:connect_accept unpacked %d new procs",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), new_proc_len));
    
    /* If we added new procs, we need to do the modex and then call
       PML add_procs */
    if (new_proc_len > 0) {
        opal_list_t all_procs;
        orte_namelist_t *name;
        opal_process_name_t *ids;
        opal_list_t myvals;
        opal_value_t *kv;

        /* we first need to give the wireup info to our routed module.
         * Not every routed module will need it, but some do require
         * this info before we can do any comm
         */
        if (ORTE_SUCCESS != (rc = orte_routed.init_routes(OMPI_CAST_RTE_NAME(&rprocs[0]->super.proc_name)->jobid, nrbuf))) {
            ORTE_ERROR_LOG(rc);
            goto exit;
        }

        OBJ_CONSTRUCT(&all_procs, opal_list_t);

        if (send_first) {
            for (i = 0 ; i < rsize ; ++i) {
                name = OBJ_NEW(orte_namelist_t);
                name->name = *OMPI_CAST_RTE_NAME(&rprocs[i]->super.proc_name);
                opal_list_append(&all_procs, &name->super);
                OPAL_OUTPUT_VERBOSE((3, ompi_dpm_base_framework.framework_output,
                                     "%s dpm:orte:connect_accept send first adding %s to allgather list",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     ORTE_NAME_PRINT(&name->name)));
            }
            for (i = 0 ; i < group->grp_proc_count ; ++i) {
                name = OBJ_NEW(orte_namelist_t);
                name->name = *OMPI_CAST_RTE_NAME(&(ompi_group_peer_lookup(group, i)->super.proc_name));
                opal_list_append(&all_procs, &name->super);
                OPAL_OUTPUT_VERBOSE((3, ompi_dpm_base_framework.framework_output,
                                     "%s dpm:orte:connect_accept send first adding %s to allgather list",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     ORTE_NAME_PRINT(&name->name)));
            }

        } else {
            for (i = 0 ; i < group->grp_proc_count ; ++i) {
                name = OBJ_NEW(orte_namelist_t);
                name->name = *OMPI_CAST_RTE_NAME(&(ompi_group_peer_lookup(group, i)->super.proc_name));
                opal_list_append(&all_procs, &name->super);
                OPAL_OUTPUT_VERBOSE((3, ompi_dpm_base_framework.framework_output,
                                     "%s dpm:orte:connect_accept recv first adding %s to allgather list",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     ORTE_NAME_PRINT(&name->name)));
            }
            for (i = 0 ; i < rsize ; ++i) {
                name = OBJ_NEW(orte_namelist_t);
                name->name = *OMPI_CAST_RTE_NAME(&rprocs[i]->super.proc_name);
                opal_list_append(&all_procs, &name->super);
                OPAL_OUTPUT_VERBOSE((3, ompi_dpm_base_framework.framework_output,
                                     "%s dpm:orte:connect_accept recv first adding %s to allgather list",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     ORTE_NAME_PRINT(&name->name)));
            }

        }

        OPAL_OUTPUT_VERBOSE((3, ompi_dpm_base_framework.framework_output,
                             "%s dpm:orte:connect_accept executing modex",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

        /* setup the modex */
        ids = (opal_process_name_t*)malloc(opal_list_get_size(&all_procs) * sizeof(opal_process_name_t));
        /* copy across the list of participants */
        i=0;
        OPAL_LIST_FOREACH(nm, &all_procs, orte_namelist_t) {
            ids[i++] = nm->name;
        }
        OPAL_LIST_DESTRUCT(&all_procs);
        /* perform it */
        opal_pmix.fence(ids, i);
        free(ids);

        OPAL_OUTPUT_VERBOSE((3, ompi_dpm_base_framework.framework_output,
                             "%s dpm:orte:connect_accept adding procs",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

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

        if (OMPI_SUCCESS != (rc = MCA_PML_CALL(add_procs(new_proc_list, new_proc_len)))) {
            ORTE_ERROR_LOG(rc);
            goto exit;
        }

        OPAL_OUTPUT_VERBOSE((3, ompi_dpm_base_framework.framework_output,
                             "%s dpm:orte:connect_accept new procs added",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    }

    OBJ_RELEASE(nrbuf);

    OPAL_OUTPUT_VERBOSE((3, ompi_dpm_base_framework.framework_output,
                         "%s dpm:orte:connect_accept allocating group size %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), rsize));

    new_group_pointer=ompi_group_allocate(rsize);
    if( NULL == new_group_pointer ) {
        rc = OMPI_ERR_OUT_OF_RESOURCE;
        goto exit;
    }

    /* put group elements in the list */
    for (j = 0; j < rsize; j++) {
        new_group_pointer->grp_proc_pointers[j] = rprocs[j]; 
    }                           /* end proc loop */

    /* increment proc reference counters */
    ompi_group_increment_proc_count(new_group_pointer);
    
    OPAL_OUTPUT_VERBOSE((3, ompi_dpm_base_framework.framework_output,
                         "%s dpm:orte:connect_accept setting up communicator",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

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

    OPAL_OUTPUT_VERBOSE((3, ompi_dpm_base_framework.framework_output,
                         "%s dpm:orte:connect_accept allocate comm_cid",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    /* allocate comm_cid */
    rc = ompi_comm_nextcid ( newcomp,                 /* new communicator */
                             comm,                    /* old communicator */
                             NULL,                    /* bridge comm */
                             &root,                   /* local leader */
                             &carport,                /* remote leader */
                             OMPI_COMM_CID_INTRA_OOB, /* mode */
                             send_first );            /* send or recv first */
    if ( OMPI_SUCCESS != rc ) {
        goto exit;
    }

    OPAL_OUTPUT_VERBOSE((3, ompi_dpm_base_framework.framework_output,
                         "%s dpm:orte:connect_accept activate comm",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    /* activate comm and init coll-component */
    rc = ompi_comm_activate ( &newcomp,               /* new communicator */
                              comm,                    /* old communicator */
                              NULL,                    /* bridge comm */
                              &root,                   /* local leader */
                              &carport,                /* remote leader */
                              OMPI_COMM_CID_INTRA_OOB, /* mode */
                              send_first );            /* send or recv first */
    if ( OMPI_SUCCESS != rc ) {
        goto exit;
    }

    /* Question: do we have to re-start some low level stuff
       to enable the usage of fast communication devices
       between the two worlds ?
    */

 exit:
    if ( NULL != rprocs ) {
        free ( rprocs );
    }
    if ( NULL != proc_list ) {
        free ( proc_list );
    }
    if ( NULL != new_proc_list ) {
        free ( new_proc_list );
    }
    if ( OMPI_SUCCESS != rc ) {
        if ( MPI_COMM_NULL != newcomp && NULL != newcomp ) {
            OBJ_RETAIN(newcomp);
            newcomp = MPI_COMM_NULL;
        }
    }

    *newcomm = newcomp;
    OPAL_OUTPUT_VERBOSE((3, ompi_dpm_base_framework.framework_output,
                         "%s dpm:orte:connect_accept complete",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    return rc;
}

static int construct_peers(ompi_group_t *group, opal_list_t *peers)
{
    int i;
    orte_namelist_t *nm, *n2;
    ompi_proc_t *proct;

    if (OMPI_GROUP_IS_DENSE(group)) {
        OPAL_OUTPUT_VERBOSE((3, ompi_dpm_base_framework.framework_output,
                             "%s dpm:orte:disconnect group is dense",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        for (i=0; i < group->grp_proc_count; i++) {
            if (NULL == (proct = group->grp_proc_pointers[i])) {
                ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
                return ORTE_ERR_NOT_FOUND;
            }
            /* add to the list of peers */
            OPAL_OUTPUT_VERBOSE((3, ompi_dpm_base_framework.framework_output,
                                 "%s dpm:orte:disconnect adding participant %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT((const orte_process_name_t *)&proct->super.proc_name)));
            nm = OBJ_NEW(orte_namelist_t);
            nm->name = *(orte_process_name_t*)&proct->super.proc_name;
            /* need to maintain an ordered list to ensure the tracker signatures
             * match across all procs */
            OPAL_LIST_FOREACH(n2, peers, orte_namelist_t) {
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
                ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
                return ORTE_ERR_NOT_FOUND;
            }
            /* add to the list of peers */
            OPAL_OUTPUT_VERBOSE((3, ompi_dpm_base_framework.framework_output,
                                 "%s dpm:orte:disconnect adding participant %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT((const orte_process_name_t *)&proct->super.proc_name)));
            nm = OBJ_NEW(orte_namelist_t);
            nm->name = *(orte_process_name_t*)&proct->super.proc_name;
            /* need to maintain an ordered list to ensure the tracker signatures
             * match across all procs */
            OPAL_LIST_FOREACH(n2, peers, orte_namelist_t) {
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

static int disconnect(ompi_communicator_t *comm)
{
    int ret, i;
    ompi_group_t *group;
    opal_list_t coll;
    orte_namelist_t *nm;
    opal_process_name_t *ids;

    /* Note that we explicitly use an RTE-based barrier (vs. an MPI
       barrier).  See a lengthy comment in
       ompi/runtime/ompi_mpi_finalize.c for a much more detailed
       rationale. */

    OPAL_OUTPUT_VERBOSE((3, ompi_dpm_base_framework.framework_output,
                         "%s dpm:orte:disconnect comm_cid %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), comm->c_contextid));

    /* setup the collective */
    OBJ_CONSTRUCT(&coll, opal_list_t);
    /* RHC: assuming for now that this must flow across all
     * local and remote group members */
    group = comm->c_local_group;
    if (ORTE_SUCCESS != (ret = construct_peers(group, &coll))) {
        ORTE_ERROR_LOG(ret);
        OPAL_LIST_DESTRUCT(&coll);
        return ret;
    }
    /* do the same for the remote group */
    group = comm->c_remote_group;
    if (ORTE_SUCCESS != (ret = construct_peers(group, &coll))) {
        ORTE_ERROR_LOG(ret);
        OPAL_LIST_DESTRUCT(&coll);
        return ret;
    }

    /* setup the ids */
    ids = (opal_process_name_t*)malloc(opal_list_get_size(&coll) * sizeof(opal_process_name_t));
    i=0;
    OPAL_LIST_FOREACH(nm, &coll, orte_namelist_t) {
        ids[i++] = nm->name;
    }
    OPAL_LIST_DESTRUCT(&coll);

    OPAL_OUTPUT_VERBOSE((3, ompi_dpm_base_framework.framework_output,
                         "%s dpm:orte:disconnect calling barrier on comm_cid %d with %d participants",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), comm->c_contextid, i));
    opal_pmix.fence(ids, i);
    free(ids);

    OPAL_OUTPUT_VERBOSE((3, ompi_dpm_base_framework.framework_output,
                         "%s dpm:orte:disconnect barrier complete for comm_cid %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), comm->c_contextid));

    return OMPI_SUCCESS;
}

static int spawn(int count, const char *array_of_commands[],
                 char **array_of_argv[],
                 const int array_of_maxprocs[],
                 const MPI_Info array_of_info[],
                 const char *port_name)
{
    int rc, i, j, counter;
    int have_wdir=0;
    int flag=0;
    char cwd[OPAL_PATH_MAX];
    char host[OPAL_MAX_INFO_VAL];  /*** should define OMPI_HOST_MAX ***/
    char prefix[OPAL_MAX_INFO_VAL];
    char stdin_target[OPAL_MAX_INFO_VAL];
    char params[OPAL_MAX_INFO_VAL];
    char mapper[OPAL_MAX_INFO_VAL];
    int npernode;
    char slot_list[OPAL_MAX_INFO_VAL];

    orte_job_t *jdata;
    orte_app_context_t *app;
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
    jdata = OBJ_NEW(orte_job_t);
    
    /* Convert the list of commands to an array of orte_app_context_t
       pointers */
    for (i = 0; i < count; ++i) {
        app = OBJ_NEW(orte_app_context_t);
        if (NULL == app) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            OBJ_RELEASE(jdata);
            opal_progress_event_users_decrement();
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        /* add the app to the job data */
        opal_pointer_array_add(jdata->apps, app);
        app->idx = i;
        jdata->num_apps++;
        
        /* copy over the name of the executable */
        app->app = strdup(array_of_commands[i]);
        if (NULL == app->app) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            OBJ_RELEASE(jdata);
            opal_progress_event_users_decrement();
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        /* record the number of procs to be generated */
        app->num_procs = array_of_maxprocs[i];
        
        /* copy over the argv array */
        counter = 1;

        if (MPI_ARGVS_NULL != array_of_argv &&
            MPI_ARGV_NULL != array_of_argv[i]) {
            /* first need to find out how many entries there are */
            j=0;
            while (NULL != array_of_argv[i][j]) {
                j++;
            }
            counter += j;
        }

        /* now copy them over, ensuring to NULL terminate the array */
        app->argv = (char**)malloc((1 + counter) * sizeof(char*));
        if (NULL == app->argv) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            OBJ_RELEASE(jdata);
            opal_progress_event_users_decrement();
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        app->argv[0] = strdup(array_of_commands[i]);
        for (j=1; j < counter; j++) {
            app->argv[j] = strdup(array_of_argv[i][j-1]);
        }
        app->argv[counter] = NULL;


        /* the environment gets set by the launcher
         * all we need to do is add the specific values
         * needed for comm_spawn
         */
        /* Add environment variable with the contact information for the
           child processes.
        */
        app->env = (char**)malloc(2 * sizeof(char*));
        if (NULL == app->env) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            OBJ_RELEASE(jdata);
            opal_progress_event_users_decrement();
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        asprintf(&(app->env[0]), "OMPI_PARENT_PORT=%s", port_name);
        app->env[1] = NULL;
        for (j = 0; NULL != environ[j]; ++j) {
            if (0 == strncmp("OMPI_", environ[j], 5)) {
                opal_argv_append_nosize(&app->env, environ[j]);
            }
        }

        /* Check for well-known info keys */
        have_wdir = 0;
        if ( array_of_info != NULL && array_of_info[i] != MPI_INFO_NULL ) {

            /* check for personality */
            ompi_info_get (array_of_info[i], "personality", sizeof(host) - 1, host, &flag);
            if ( flag ) {
                jdata->personality = strdup(host);
            }
            
            /* check for 'host' */
            ompi_info_get (array_of_info[i], "host", sizeof(host) - 1, host, &flag);
            if ( flag ) {
                orte_set_attribute(&app->attributes, ORTE_APP_DASH_HOST, false, host, OPAL_STRING);
            }
 
            /* check for 'hostfile' */
            ompi_info_get (array_of_info[i], "hostfile", sizeof(host) - 1, host, &flag);
            if ( flag ) {
                orte_set_attribute(&app->attributes, ORTE_APP_HOSTFILE, false, host, OPAL_STRING);
            }
            
            /* check for 'add-hostfile' */
            ompi_info_get (array_of_info[i], "add-hostfile", sizeof(host) - 1, host, &flag);
            if ( flag ) {
                orte_set_attribute(&app->attributes, ORTE_APP_ADD_HOSTFILE, false, host, OPAL_STRING);
            }
            
            /* check for 'add-host' */
            ompi_info_get (array_of_info[i], "add-host", sizeof(host) - 1, host, &flag);
            if ( flag ) {
                orte_set_attribute(&app->attributes, ORTE_APP_ADD_HOST, false, host, OPAL_STRING);
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
             */
            ompi_info_get (array_of_info[i], "ompi_prefix", sizeof(prefix) - 1, prefix, &flag);
            if ( flag ) {
                orte_set_attribute(&app->attributes, ORTE_APP_PREFIX_DIR, false, prefix, OPAL_STRING);
            }

            /* check for 'wdir' */ 
            ompi_info_get (array_of_info[i], "wdir", sizeof(cwd) - 1, cwd, &flag);
            if ( flag ) {
                app->cwd = strdup(cwd);
                have_wdir = 1;
            }
            
            /* check for 'mapper' */ 
            ompi_info_get(array_of_info[i], "mapper", sizeof(mapper) - 1, mapper, &flag);
            if ( flag ) {
                if (NULL == jdata->map) {
                    jdata->map = OBJ_NEW(orte_job_map_t);
                    if (NULL == jdata->map) {
                        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                        return ORTE_ERR_OUT_OF_RESOURCE;
                    }
                }
                jdata->map->req_mapper = strdup(mapper);
            }

            /* check for 'display_map' */ 
            ompi_info_get_bool(array_of_info[i], "display_map", &local_spawn, &flag);
            if ( flag ) {
                if (NULL == jdata->map) {
                    jdata->map = OBJ_NEW(orte_job_map_t);
                    if (NULL == jdata->map) {
                        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                        return ORTE_ERR_OUT_OF_RESOURCE;
                    }
                }
                jdata->map->display_map = true;
            }

            /* check for 'npernode' and 'ppr' */ 
            ompi_info_get (array_of_info[i], "npernode", sizeof(slot_list) - 1, slot_list, &flag);
            if ( flag ) {
                if (ORTE_SUCCESS != ompi_info_value_to_int(slot_list, &npernode)) {
                    ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
                    return ORTE_ERR_BAD_PARAM;
                }
                if (NULL == jdata->map) {
                    jdata->map = OBJ_NEW(orte_job_map_t);
                    if (NULL == jdata->map) {
                        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                        return ORTE_ERR_OUT_OF_RESOURCE;
                    }
                }
                if (ORTE_MAPPING_POLICY_IS_SET(jdata->map->mapping)) {
                    /* not allowed to provide multiple mapping policies */
                    return OMPI_ERROR;
                }
                ORTE_SET_MAPPING_DIRECTIVE(jdata->map->mapping, ORTE_MAPPING_PPR);
                asprintf(&(jdata->map->ppr), "%d:n", npernode);
            }
            ompi_info_get (array_of_info[i], "pernode", sizeof(slot_list) - 1, slot_list, &flag);
            if ( flag ) {
                if (NULL == jdata->map) {
                    jdata->map = OBJ_NEW(orte_job_map_t);
                    if (NULL == jdata->map) {
                        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                        return ORTE_ERR_OUT_OF_RESOURCE;
                    }
                }
                if (ORTE_MAPPING_POLICY_IS_SET(jdata->map->mapping)) {
                    /* not allowed to provide multiple mapping policies */
                    return OMPI_ERROR;
                }
                ORTE_SET_MAPPING_DIRECTIVE(jdata->map->mapping, ORTE_MAPPING_PPR);
                jdata->map->ppr = strdup("1:n");
            }
            ompi_info_get (array_of_info[i], "ppr", sizeof(slot_list) - 1, slot_list, &flag);
            if ( flag ) {
                if (NULL == jdata->map) {
                    jdata->map = OBJ_NEW(orte_job_map_t);
                    if (NULL == jdata->map) {
                        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                        return ORTE_ERR_OUT_OF_RESOURCE;
                    }
                }
                if (ORTE_MAPPING_POLICY_IS_SET(jdata->map->mapping)) {
                    /* not allowed to provide multiple mapping policies */
                    return OMPI_ERROR;
                }
                ORTE_SET_MAPPING_DIRECTIVE(jdata->map->mapping, ORTE_MAPPING_PPR);
                jdata->map->ppr = strdup(slot_list);
            }

            /* check for 'map_by' */
            ompi_info_get(array_of_info[i], "map_by", sizeof(slot_list) - 1, slot_list, &flag);
            if ( flag ) {
                if (NULL == jdata->map) {
                    jdata->map = OBJ_NEW(orte_job_map_t);
                    if (NULL == jdata->map) {
                        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                        return ORTE_ERR_OUT_OF_RESOURCE;
                    }
                }
                if (ORTE_MAPPING_POLICY_IS_SET(jdata->map->mapping)) {
                    /* not allowed to provide multiple mapping policies */
                    return OMPI_ERROR;
                }
                if (ORTE_SUCCESS != (rc = orte_rmaps_base_set_mapping_policy(&jdata->map->mapping,
                                                                             NULL, slot_list))) {
                    return rc;
                }
            }

            /* check for 'rank_by' */
            ompi_info_get(array_of_info[i], "rank_by", sizeof(slot_list) - 1, slot_list, &flag);
            if ( flag ) {
                if (NULL == jdata->map) {
                    jdata->map = OBJ_NEW(orte_job_map_t);
                    if (NULL == jdata->map) {
                        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                        return ORTE_ERR_OUT_OF_RESOURCE;
                    }
                }
                if (ORTE_RANKING_POLICY_IS_SET(jdata->map->ranking)) {
                    /* not allowed to provide multiple ranking policies */
                    return OMPI_ERROR;
                }
                if (ORTE_SUCCESS != (rc = orte_rmaps_base_set_ranking_policy(&jdata->map->ranking, 
                                                                             jdata->map->mapping, slot_list))) {
                    return rc;
                }
            }

#if OPAL_HAVE_HWLOC
            /* check for 'bind_to' */
            ompi_info_get(array_of_info[i], "bind_to", sizeof(slot_list) - 1, slot_list, &flag);
            if ( flag ) {
                if (NULL == jdata->map) {
                    jdata->map = OBJ_NEW(orte_job_map_t);
                    if (NULL == jdata->map) {
                        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                        return ORTE_ERR_OUT_OF_RESOURCE;
                    }
                }
                if (OPAL_BINDING_POLICY_IS_SET(jdata->map->binding)) {
                    /* not allowed to provide multiple binding policies */
                    return OMPI_ERROR;
                }
                if (ORTE_SUCCESS != (rc = opal_hwloc_base_set_binding_policy(&jdata->map->binding, slot_list))) {
                    return rc;
                }
            }
#endif

            /* check for 'preload_binary' */
            ompi_info_get_bool(array_of_info[i], "ompi_preload_binary", &local_spawn, &flag);
            if ( flag ) {
                orte_set_attribute(&app->attributes, ORTE_APP_PRELOAD_BIN, false, NULL, OPAL_BOOL);
            }
            
            /* check for 'preload_files' */ 
            ompi_info_get (array_of_info[i], "ompi_preload_files", sizeof(cwd) - 1, cwd, &flag);
            if ( flag ) {
                orte_set_attribute(&app->attributes, ORTE_APP_PRELOAD_FILES, false, cwd, OPAL_STRING);
            }
            
            /* see if this is a non-mpi job - if so, then set the flag so ORTE
             * knows what to do
             */
            ompi_info_get_bool(array_of_info[i], "ompi_non_mpi", &non_mpi, &flag);
            if (flag && non_mpi) {
                orte_set_attribute(&jdata->attributes, ORTE_JOB_NON_ORTE_JOB, false, NULL, OPAL_BOOL);
            }
            
            /* see if this is an MCA param that the user wants applied to the child job */
            ompi_info_get (array_of_info[i], "ompi_param", sizeof(params) - 1, params, &flag);
            if ( flag ) {
                opal_argv_append_unique_nosize(&app->env, params, true);
            }
            
            /* see if user specified what to do with stdin - defaults to
             * not forwarding stdin to child processes
             */
            ompi_info_get (array_of_info[i], "ompi_stdin_target", sizeof(stdin_target) - 1, stdin_target, &flag);
            if ( flag ) {
                if (0 == strcmp(stdin_target, "all")) {
                    jdata->stdin_target = ORTE_VPID_WILDCARD;
                } else if (0 == strcmp(stdin_target, "none")) {
                    jdata->stdin_target = ORTE_VPID_INVALID;
                } else {
                    jdata->stdin_target = strtoul(stdin_target, NULL, 10);
                }
            }
        }

        /* default value: If the user did not tell us where to look for the
         * executable, we assume the current working directory
         */
        if ( !have_wdir ) {
            if (OMPI_SUCCESS != (rc = opal_getcwd(cwd, OPAL_PATH_MAX))) {
                ORTE_ERROR_LOG(rc);
                OBJ_RELEASE(jdata);
                opal_progress_event_users_decrement();
                return rc;
            }
            app->cwd = strdup(cwd);
        }
        
        /* leave the map info alone - the launcher will
         * decide where to put things
         */
    } /* for (i = 0 ; i < count ; ++i) */

    /* default the personality */
    if (NULL == jdata->personality) {
        jdata->personality = strdup("ompi");
    }
    
    /* spawn procs */
    rc = orte_plm.spawn(jdata);
    OBJ_RELEASE(jdata);

    if (ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        opal_progress_event_users_decrement();
        return MPI_ERR_SPAWN;
    }

    return OMPI_SUCCESS;
}

/*
 * The port_name is constructed to support the ability
 * to route messages between different jobs. Messages
 * between job families are routed via their respective HNPs
 * to reduce connection count and to support connect/accept.
 * Thus, the port_name consists of three fields:
 * (a) the contact info of the process opening the port. This
 *     is provided in case the routed module wants to communicate
 *     directly between the procs.
 * (b) the tag of the port. The reason for adding the tag is
 *     to make the port unique for multi-threaded scenarios.
 * (c) the contact info for the job's HNP. This will be
 *     used to route messages between job families
 *
 * Construction of the port name is done here - as opposed to
 * in the routed module itself - because two mpiruns using different
 * routed modules could exchange the port name (via pubsub). The
 * format of the port name must, therefore, be universal.
 *
 * Optionally can provide a tag to be used - otherwise, we supply the
 * next dynamically assigned tag
 */
static int open_port(char *port_name, orte_rml_tag_t given_tag)
{
    char *rml_uri=NULL;
    int rc, len;
    char tag[12];
    
    /* if we are a singleton and the supporting HNP hasn't
     * been spawned, then do so now
     */
    if ((orte_process_info.proc_type & ORTE_PROC_SINGLETON) &&
        !orte_routing_is_enabled) {
        if (ORTE_SUCCESS != orte_plm_base_fork_hnp()) {
            ORTE_ERROR_LOG(ORTE_ERR_FATAL);
            return ORTE_ERR_FATAL;
        }
        orte_routing_is_enabled = true;
        /* need to init_routes again to redirect messages
         * thru the HNP
         */
        orte_routed.init_routes(ORTE_PROC_MY_NAME->jobid, NULL);
    }

    if (NULL == orte_process_info.my_hnp_uri) {
        rc = OMPI_ERR_NOT_AVAILABLE;
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    
    if (NULL == (rml_uri = orte_rml.get_contact_info())) {
        rc = OMPI_ERROR;
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    
    if (ORTE_RML_TAG_INVALID == given_tag) {
        OPAL_THREAD_LOCK(&ompi_dpm_port_mutex);
        snprintf(tag, 12, "%d", next_tag);
        next_tag++;
        OPAL_THREAD_UNLOCK(&ompi_dpm_port_mutex);
    } else {
        snprintf(tag, 12, "%d", given_tag);
    }
    
    
    len = strlen(orte_process_info.my_hnp_uri) + strlen(rml_uri) + strlen(tag);
    
    /* if the overall port name is too long, we abort */
    if (len > (MPI_MAX_PORT_NAME-1)) {
        rc = OMPI_ERR_VALUE_OUT_OF_BOUNDS;
        goto cleanup;
    }
    
    /* assemble the port name */
    snprintf(port_name, MPI_MAX_PORT_NAME, "%s+%s:%s", orte_process_info.my_hnp_uri, rml_uri, tag);
    rc = OMPI_SUCCESS;

cleanup:
    if (NULL != rml_uri) {
        free(rml_uri);
    }
    
    return rc;
}


static int route_to_port(char *rml_uri, orte_process_name_t *rproc)
{    
    opal_buffer_t route;
    int rc;
    
    /* We need to ask the routed module to init_routes so it can do the
     * right thing. In most cases, it will route any messages to the
     * proc through our HNP - however, this is NOT the case in all
     * circumstances, so we need to let the routed module decide what
     * to do.
     */
    /* pack a cmd so the buffer can be unpacked correctly */
    OBJ_CONSTRUCT(&route, opal_buffer_t);
    
    /* pack the provided uri */
    opal_dss.pack(&route, &rml_uri, 1, OPAL_STRING);
    
    /* init the route */
    if (ORTE_SUCCESS != (rc = orte_routed.init_routes(rproc->jobid, &route))) {
        ORTE_ERROR_LOG(rc);
    }
    OBJ_DESTRUCT(&route);
    
    /* nothing more to do here */
    return rc;
}

static int parse_port_name(const char *port_name,
                           char **hnp_uri, 
                           char **rml_uri,
                           orte_rml_tag_t *ptag)
{
    char *tmpstring=NULL, *ptr;
    int tag;
    int rc;
    
    /* don't mangle the port name */
    tmpstring = strdup(port_name);
    if (NULL == tmpstring) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    
    /* find the ':' demarking the RML tag we added to the end */
    if (NULL == (ptr = strrchr(tmpstring, ':'))) {
        rc = OMPI_ERR_NOT_FOUND;
        goto cleanup;
    }
    
    /* terminate the port_name at that location */
    *ptr = '\0';
    ptr++;
    
    /* convert the RML tag */
    sscanf(ptr,"%d", &tag);
    
    /* now split out the second field - the uri of the remote proc */
    if (NULL == (ptr = strchr(tmpstring, '+'))) {
        rc = OMPI_ERR_NOT_FOUND;
        goto cleanup;
    }
    *ptr = '\0';
    ptr++;
    
    /* save that info */
    if(NULL != hnp_uri) *hnp_uri = tmpstring;
    else free(tmpstring);
    if(NULL != rml_uri) *rml_uri = strdup(ptr);
    if(NULL != ptag) *ptag = tag;
    
    return OMPI_SUCCESS;
    
cleanup:
    /* release the tmp storage */
    free(tmpstring);
    return rc;
}

static int close_port(const char *port_name)
{
    /* nothing to do here - user is responsible for the memory */
    return OMPI_SUCCESS;
}

static int dyn_init(void)
{
    char *port_name=NULL;
    int root=0, rc;
    bool send_first = true;
    ompi_communicator_t *newcomm=NULL;
    
    /* if env-variable is set, we are a dynamically spawned
        * child - parse port and call comm_connect_accept */
    if (NULL == (port_name = ompi_dpm_base_dyn_init())) {
        /* nothing to do */
        return OMPI_SUCCESS;
    }
    
    OPAL_OUTPUT_VERBOSE((1, ompi_dpm_base_framework.framework_output,
                         "%s dpm:orte:dyn_init with port %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         port_name));
    
    rc = connect_accept (MPI_COMM_WORLD, root, port_name, send_first, &newcomm);
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
static int finalize(void)
{
    OBJ_DESTRUCT(&ompi_dpm_port_mutex);
    OPAL_LIST_DESTRUCT(&orte_dpm_acceptors);
    OPAL_LIST_DESTRUCT(&orte_dpm_connectors);
    return OMPI_SUCCESS;
}

static void timeout_cb(int fd, short args, void *cbdata)
{
    orte_dpm_prequest_t *req = (orte_dpm_prequest_t*)cbdata;

    /* remove the request from the list */
    OPAL_THREAD_LOCK(&ompi_dpm_port_mutex);
    opal_list_remove_item(&orte_dpm_connectors, &req->super);
    OPAL_THREAD_UNLOCK(&ompi_dpm_port_mutex);

    /* this connection request failed - notify the caller */
    req->cbfunc(MPI_COMM_NULL, NULL, req->cbdata);

    /* cleanup */
    OBJ_RELEASE(req);
}

static void process_request(orte_process_name_t* sender,
                            opal_buffer_t *buffer,
                            bool connector,
                            ompi_communicator_t **newcomm,
                            ompi_proc_t **proct)
{
    ompi_communicator_t *newcomp=MPI_COMM_NULL;
    ompi_group_t *group=MPI_COMM_SELF->c_local_group;
    ompi_group_t *new_group_pointer;
    ompi_proc_t **rprocs=NULL;
    ompi_proc_t **new_proc_list=NULL;
    int new_proc_len;
    opal_buffer_t *xfer;
    int cnt, rc;
    uint32_t id;

    OPAL_OUTPUT_VERBOSE((2, ompi_dpm_base_framework.framework_output,
                         "%s dpm:pconprocess: PROCESS REQUEST: %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         connector ? "connector" : "acceptor"));

    /* if we are the acceptor, unpack the remote peer's request id */
    if (!connector) {
        cnt=1;
        if (OPAL_SUCCESS != (rc = opal_dss.unpack(buffer, &id, &cnt, OPAL_UINT32))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
        OPAL_OUTPUT_VERBOSE((2, ompi_dpm_base_framework.framework_output,
                             "%s dpm:pconprocess: PROCESS REQUEST ID: %d",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), id));
    }

    /* unpack the proc info */
    if (OMPI_SUCCESS != (rc = ompi_proc_unpack(buffer, 1, &rprocs, false, &new_proc_len, &new_proc_list))) {
        ORTE_ERROR_LOG(rc);
        return;
    }

    /* If we added new procs, we need to unpack the modex info
     * and then call PML add_procs
     */
    if (0 < new_proc_len) {
        OPAL_OUTPUT_VERBOSE((3, ompi_dpm_base_framework.framework_output,
                             "%s dpm:pconprocess: process modex",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

        OPAL_OUTPUT_VERBOSE((3, ompi_dpm_base_framework.framework_output,
                             "%s dpm:pconprocess: adding procs",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        if (OMPI_SUCCESS != (rc = MCA_PML_CALL(add_procs(new_proc_list, new_proc_len)))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
        
        OPAL_OUTPUT_VERBOSE((3, ompi_dpm_base_framework.framework_output,
                             "%s dpm:orte:pconnect new procs added",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    }

    /* if we are the acceptor, we now have to send the requestor our
     * info so we can collaborate on setup of the communicator - we
     * must wait until this point so the route can be initiated, if
     * required
     */
    if (!connector) {
        xfer = OBJ_NEW(opal_buffer_t);
        /* pack the request id */
        if (ORTE_SUCCESS != (rc = opal_dss.pack(xfer, &id, 1, OPAL_UINT32))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(xfer);
            goto cleanup;
        }
        /* pack the remaining info */
        if (ORTE_SUCCESS != ompi_proc_pack(group->grp_proc_pointers, 1, true, xfer)) {
            OBJ_RELEASE(xfer);
            goto cleanup;
        }
        /* send to requestor */
        if (ORTE_SUCCESS != (rc = orte_rml.send_buffer_nb(sender, xfer, OMPI_RML_PCONNECT_TAG,
                                                          orte_rml_send_callback, NULL))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(xfer);
            goto cleanup;
        }
    }

    /* allocate a new group */
    new_group_pointer=ompi_group_allocate(1);
    if( NULL == new_group_pointer ) {
        rc = OMPI_ERR_OUT_OF_RESOURCE;
        goto cleanup;
    }

    /* put group element in the list */
    new_group_pointer->grp_proc_pointers[0] = rprocs[0]; 

    /* increment proc reference counter */
    ompi_group_increment_proc_count(new_group_pointer);
    
    OPAL_OUTPUT_VERBOSE((3, ompi_dpm_base_framework.framework_output,
                         "%s dpm:pconprocess setting up communicator",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    /* set up communicator structure */
    rc = ompi_comm_set(&newcomp,                       /* new comm */
                       MPI_COMM_SELF,                  /* old comm */
                       1,                              /* local_size */
                       NULL,                           /* local_procs */
                       1,                              /* remote_size */
                       NULL,                           /* remote_procs */
                       NULL,                           /* attrs */
                       MPI_COMM_SELF->error_handler,   /* error handler */
                       NULL,                           /* topo component */
                       group,                          /* local group */
                       new_group_pointer               /* remote group */
                       );
    if (NULL == newcomp) {
        rc = OMPI_ERR_OUT_OF_RESOURCE;
        goto cleanup;
    }

    ompi_group_decrement_proc_count (new_group_pointer);
    OBJ_RELEASE(new_group_pointer);
    new_group_pointer = MPI_GROUP_NULL;

    /* return the communicator */
    *newcomm = newcomp;
    *proct = rprocs[0];
    rc = OMPI_SUCCESS;

 cleanup:
    if (NULL != rprocs) {
        free(rprocs);
    }
    if (NULL != new_proc_list) {
        free(new_proc_list);
    }
    if (OMPI_SUCCESS != rc && MPI_COMM_NULL != newcomp) {
        OBJ_RELEASE(newcomp);
    }
}

static void connect_complete(int status, orte_process_name_t* sender,
                             opal_buffer_t* buffer, orte_rml_tag_t tag,
                             void* cbdata)
{
    ompi_communicator_t *newcomm=MPI_COMM_NULL;
    ompi_proc_t *proct=NULL;
    orte_dpm_prequest_t *req=NULL, *rptr;
    int rc, cnt;
    uint32_t id;

    OPAL_OUTPUT_VERBOSE((3, ompi_dpm_base_framework.framework_output,
                         "%s dpm:pconnect: starting",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    /* unpack the request id */
    cnt=1;
    if (OPAL_SUCCESS != (rc = opal_dss.unpack(buffer, &id, &cnt, OPAL_UINT32))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }

    /* find this request on the list */
    req = NULL;
    OPAL_THREAD_LOCK(&ompi_dpm_port_mutex);
    OPAL_LIST_FOREACH(rptr, &orte_dpm_connectors, orte_dpm_prequest_t) {
        if (id == rptr->id) {
            req = rptr;
            break;
        }
    }
    if (NULL == req) {
        /* unknown request */
        opal_output(0, "%s dpm:pconnect: received unknown id %u from %s",
                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), id,
                    ORTE_NAME_PRINT(sender));
        OPAL_THREAD_UNLOCK(&ompi_dpm_port_mutex);
        return;
    }
    /* remove the request from the list */
    opal_list_remove_item(&orte_dpm_connectors, &req->super);
    OPAL_THREAD_UNLOCK(&ompi_dpm_port_mutex);

    OPAL_OUTPUT_VERBOSE((3, ompi_dpm_base_framework.framework_output,
                         "%s dpm:pconnect: found request %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), id));

    if (req->event_active) {
        /* release the timeout */
        opal_event_del(&req->ev);
    }

    /* process the request - as the initiator, we will send first
     * for communicator creation
     */
    process_request(sender, buffer, true, &newcomm, &proct);
    /* notify the MPI layer */
    req->cbfunc(newcomm, proct, req->cbdata);

 cleanup:
    if (NULL != req) {
        OBJ_RELEASE(req);
    }
}

static int dpm_pconnect(char *port,
                        struct timeval *timeout,
                        ompi_dpm_base_paccept_connect_callback_fn_t cbfunc,
                        void *cbdata)
{
    char *hnp_uri, *rml_uri;
    orte_rml_tag_t tag;
    int rc;
    orte_dpm_prequest_t *connector;
    orte_process_name_t peer;
    ompi_group_t *group=MPI_COMM_SELF->c_local_group;
    opal_buffer_t *buf;

    /* separate the string into the HNP and RML URI and tag */
    if (ORTE_SUCCESS != (rc = parse_port_name(port, &hnp_uri, &rml_uri, &tag))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    /* extract the originating proc's name */
    if (ORTE_SUCCESS != (rc = orte_rml_base_parse_uris(rml_uri, &peer, NULL))) {
        ORTE_ERROR_LOG(rc);
        free(hnp_uri); free(rml_uri);
        return rc;
    }
    /* make sure we can route rml messages to the destination job */
    if (ORTE_SUCCESS != (rc = route_to_port(hnp_uri, &peer))) {
        ORTE_ERROR_LOG(rc);
        free(hnp_uri); free(rml_uri);
        return rc;
    }
    opal_output(0, "dpm:pconnect requesting connect to %s on tag %d",
                ORTE_NAME_PRINT(&peer), tag);

    free(hnp_uri); free(rml_uri);

    /* create a message to the remote peer */
    buf = OBJ_NEW(opal_buffer_t);

    /* track the connection request */
    connector = OBJ_NEW(orte_dpm_prequest_t);
    connector->tag = tag;
    connector->cbfunc = cbfunc;
    connector->cbdata = cbdata;
    OPAL_THREAD_LOCK(&ompi_dpm_port_mutex);
    connector->id = next_preq++;
    opal_list_append(&orte_dpm_connectors, &connector->super);
    OPAL_THREAD_UNLOCK(&ompi_dpm_port_mutex);

    /* pack my request id */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(buf, &connector->id, 1, OPAL_UINT32))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(buf);
        OPAL_THREAD_LOCK(&ompi_dpm_port_mutex);
        opal_list_remove_item(&orte_dpm_connectors, &connector->super);
        OPAL_THREAD_UNLOCK(&ompi_dpm_port_mutex);
        OBJ_RELEASE(connector);
        return rc;
    }
    /* pack the request info */
    if (ORTE_SUCCESS != ompi_proc_pack(group->grp_proc_pointers, 1, true, buf)) {
        OBJ_RELEASE(buf);
        OPAL_THREAD_LOCK(&ompi_dpm_port_mutex);
        opal_list_remove_item(&orte_dpm_connectors, &connector->super);
        OPAL_THREAD_UNLOCK(&ompi_dpm_port_mutex);
        OBJ_RELEASE(connector);
        return rc;
    }

    /* setup the timeout, if requested */
    if (NULL != timeout) {
        opal_output(0, "dpm:pconnect setting timeout");
        opal_event_evtimer_set(orte_event_base,
                               &connector->ev, timeout_cb, connector);
        opal_event_set_priority(&connector->ev, ORTE_ERROR_PRI);
        opal_event_evtimer_add(&connector->ev, timeout);
        connector->event_active = true;
    } else {
        connector->event_active = false;
    }

    /* send it to our new friend */
    OPAL_OUTPUT_VERBOSE((2, ompi_dpm_base_framework.framework_output,
                         "%s dpm:pconnect sending connect to %s on tag %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(&peer), tag));

    if (ORTE_SUCCESS != (rc = orte_rml.send_buffer_nb(&peer, buf, tag,
                                                      orte_rml_send_callback, NULL))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(buf);
    }

    return rc;
}

static void paccept_recv(int status,
                         orte_process_name_t* peer,
                         struct opal_buffer_t* buffer,
                         orte_rml_tag_t tag,
                         void* cbdata)
{
    orte_dpm_prequest_t *acceptor = (orte_dpm_prequest_t*)cbdata;
    ompi_communicator_t *newcomm=MPI_COMM_NULL;
    ompi_proc_t *proct=NULL;

    OPAL_OUTPUT_VERBOSE((2, ompi_dpm_base_framework.framework_output,
                         "%s dpm:paccept recvd request from %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(peer)));

    /* process the request - as the acceptor, we will recv first
     * on communicator formation
     */
    process_request(peer, buffer, false, &newcomm, &proct);
    /* if we succeeded, notify the MPI layer */
    if (MPI_COMM_NULL != newcomm) {
        acceptor->cbfunc(newcomm, proct, acceptor->cbdata);
    }
}

static int dpm_paccept(char *port,
                       ompi_dpm_base_paccept_connect_callback_fn_t cbfunc,
                       void *cbdata)
{
    orte_rml_tag_t tag;
    int rc;
    orte_dpm_prequest_t *acceptor;

    /* extract the RML tag from the port name - it's the only part we need */
    if (OMPI_SUCCESS != (rc = parse_port_name(port, NULL, NULL, &tag))) {
        return rc;
    }

    /* track the accept request */
    acceptor = OBJ_NEW(orte_dpm_prequest_t);
    acceptor->tag = tag;
    acceptor->cbfunc = cbfunc;
    acceptor->cbdata = cbdata;
    OPAL_THREAD_LOCK(&ompi_dpm_port_mutex);
    opal_list_append(&orte_dpm_acceptors, &acceptor->super);
    OPAL_THREAD_UNLOCK(&ompi_dpm_port_mutex);

    /* register a recv for this tag */
    orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, tag,
                            ORTE_RML_PERSISTENT,
                            paccept_recv, acceptor);

    return OMPI_SUCCESS;
}

static void dpm_pclose(char *port)
{
    orte_rml_tag_t tag;
    orte_dpm_prequest_t *rptr;

    /* extract the RML tag from the port name - it's the only part we need */
    if (OMPI_SUCCESS != parse_port_name(port, NULL, NULL, &tag)) {
        return;
    }

    OPAL_THREAD_LOCK(&ompi_dpm_port_mutex);
    OPAL_LIST_FOREACH(rptr, &orte_dpm_acceptors, orte_dpm_prequest_t) {
        if (tag == rptr->tag) {
            /* found it */
            opal_list_remove_item(&orte_dpm_acceptors, &rptr->super);
            orte_rml.recv_cancel(ORTE_NAME_WILDCARD, tag);
            OBJ_RELEASE(rptr);
            break;
        }
    }
    OPAL_THREAD_UNLOCK(&ompi_dpm_port_mutex);
}
