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
 * Copyright (c) 2012      Los Alamos National Security, LLC.
 *                         All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
#include "orte_config.h"
#include "orte/types.h"
#include "orte/constants.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <fcntl.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SYS_SOCKET_H
#include <sys/socket.h>
#endif
#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif
#ifdef HAVE_ARPA_INET_H
#include <arpa/inet.h>
#endif
#ifdef HAVE_NETDB_H
#include <netdb.h>
#endif
#ifdef HAVE_IFADDRS_H
#include <ifaddrs.h>
#endif

#include "opal/dss/dss.h"
#include "opal/runtime/opal.h"
#include "opal/class/opal_pointer_array.h"
#include "opal/mca/hwloc/base/base.h"
#include "opal/util/output.h"
#include "opal/util/argv.h"
#include "opal/datatype/opal_datatype.h"

#include "orte/mca/db/db.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/odls/base/odls_private.h"
#include "orte/util/show_help.h"
#include "orte/util/proc_info.h"
#include "orte/util/name_fns.h"
#include "orte/util/regex.h"
#include "orte/runtime/orte_globals.h"
#include "orte/mca/rml/base/rml_contact.h"
#include "orte/mca/state/state.h"

#include "orte/util/nidmap.h"

static int orte_nidmap_verbose, orte_nidmap_output=-1;

int orte_util_nidmap_init(opal_buffer_t *buffer)
{
    int32_t cnt;
    int rc;
    opal_byte_object_t *bo;
    
    mca_base_param_reg_int_name("orte", "nidmap_verbose",
                                "Verbosity of the nidmap subsystem",
                                true, false, 0,  &orte_nidmap_verbose);
    if (0 < orte_nidmap_verbose) {
        orte_nidmap_output = opal_output_open(NULL);
        opal_output_set_verbosity(orte_nidmap_output, orte_nidmap_verbose);
    }

    /* it is okay if the buffer is empty */
    if (NULL == buffer || 0 == buffer->bytes_used) {
        return ORTE_SUCCESS;
    }
    
#if OPAL_HAVE_HWLOC
    {
        hwloc_topology_t topo;

        /* extract the topology */
        cnt=1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &topo, &cnt, OPAL_HWLOC_TOPO))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        if (NULL == opal_hwloc_topology) {
            opal_hwloc_topology = topo;
        } else {
            hwloc_topology_destroy(topo);
        }
    }
#endif

    /* extract the byte object holding the daemonmap */
    cnt=1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &bo, &cnt, OPAL_BYTE_OBJECT))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    /* unpack the node map */
    if (ORTE_SUCCESS != (rc = orte_util_decode_nodemap(bo))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    /* the bytes in the object were free'd by the decode */
    
    /* extract the byte object holding the process map */
    cnt=1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &bo, &cnt, OPAL_BYTE_OBJECT))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    /* unpack the process map */
    if (ORTE_SUCCESS != (rc = orte_util_decode_pidmap(bo))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    /* the bytes in the object were free'd by the decode */

    return ORTE_SUCCESS;
}

void orte_util_nidmap_finalize(void)
{
#if OPAL_HAVE_HWLOC
    /* destroy the topology */
    if (NULL != opal_hwloc_topology) {
        hwloc_topology_destroy(opal_hwloc_topology);
        opal_hwloc_topology = NULL;
    }
#endif
}

#if ORTE_ENABLE_STATIC_PORTS
int orte_util_build_daemon_nidmap(char **nodes)
{
    int i, num_nodes;
    int rc;
    struct hostent *h;
    opal_buffer_t buf;
    orte_process_name_t proc;
    char *uri, *addr;
    char *proc_name;
    
    num_nodes = opal_argv_count(nodes);
    
    OPAL_OUTPUT_VERBOSE((2, orte_nidmap_output,
                         "%s orte:util:build:daemon:nidmap found %d nodes",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), num_nodes));
    
    if (0 == num_nodes) {
        /* nothing to do */
        return ORTE_SUCCESS;
    }
    
    /* install the entry for the HNP */
    proc.jobid = ORTE_PROC_MY_NAME->jobid;
    proc.vpid = 0;
    if (ORTE_SUCCESS != (rc = orte_db.store(&proc, ORTE_DB_DAEMON_VPID, &proc.vpid, ORTE_VPID))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    addr = "HNP";
    if (ORTE_SUCCESS != (rc = orte_db.store(&proc, ORTE_DB_HOSTNAME, addr, OPAL_STRING))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    /* the daemon vpids will be assigned in order,
     * starting with vpid=1 for the first node in
     * the list
     */
    OBJ_CONSTRUCT(&buf, opal_buffer_t);
    for (i=0; i < num_nodes; i++) {
        /* define the vpid for this daemon */
        proc.vpid = i+1;
        /* store the hostname for the proc */
        if (ORTE_SUCCESS != (rc = orte_db.store(&proc, ORTE_DB_HOSTNAME, nodes[i], OPAL_STRING))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        /* the arch defaults to our arch so that non-hetero
         * case will yield correct behavior
         */
        if (ORTE_SUCCESS != (rc = orte_db.store(&proc, ORTE_DB_ARCH, &opal_local_arch, OPAL_UINT32))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* lookup the address of this node */
        if (NULL == (h = gethostbyname(nodes[i]))) {
            ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
            return ORTE_ERR_NOT_FOUND;
        }
        addr = inet_ntoa(*(struct in_addr*)h->h_addr_list[0]);
        
        /* since we are using static ports, all my fellow daemons will be on my
         * port. Setup the contact info for each daemon in my hash tables. Note
         * that this will -not- open a port to those daemons, but will only
         * define the info necessary for opening such a port if/when I communicate
         * to them
         */

        /* construct the URI */
        orte_util_convert_process_name_to_string(&proc_name, &proc);
        asprintf(&uri, "%s;tcp://%s:%d", proc_name, addr, (int)orte_process_info.my_port);
        OPAL_OUTPUT_VERBOSE((2, orte_nidmap_output,
                             "%s orte:util:build:daemon:nidmap node %s daemon %d addr %s uri %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             nodes[i], i+1, addr, uri));
        opal_dss.pack(&buf, &uri, 1, OPAL_STRING);
        free(proc_name);
        free(uri);
    }
    
    /* load the hash tables */
    if (ORTE_SUCCESS != (rc = orte_rml_base_update_contact_info(&buf))) {
        ORTE_ERROR_LOG(rc);
    }
    OBJ_DESTRUCT(&buf);

    return rc;
}
#endif

int orte_util_encode_nodemap(opal_byte_object_t *boptr)
{
    orte_vpid_t vpid;
    orte_node_t *node;
    int32_t i, num_nodes;
    int rc;
    opal_buffer_t buf;
    char *ptr, *nodename;

    /* setup a buffer for tmp use */
    OBJ_CONSTRUCT(&buf, opal_buffer_t);

    /* determine the number of nodes in the global node array */
    num_nodes = 0;
    for (i=0; i < orte_node_pool->size; i++) {
        if (NULL == opal_pointer_array_get_item(orte_node_pool, i)) {
            continue;
        }
        ++num_nodes;
    }

    /* pack number of nodes */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &num_nodes, 1, OPAL_INT32))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* pack the data for each node by daemon */
    for (i=0; i < orte_node_pool->size; i++) {
        if (NULL == (node = (orte_node_t*)opal_pointer_array_get_item(orte_node_pool, i))) {
            continue;
        }
        if (NULL == node->daemon) {
            /* some nodes may not have daemons on them */
            vpid = ORTE_VPID_INVALID;
        } else {
            vpid = node->daemon->name.vpid;
        }
        if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &vpid, 1, ORTE_VPID))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        /* pack the name of the node */
        if (!orte_keep_fqdn_hostnames) {
            nodename = strdup(node->name);
            if (NULL != (ptr = strchr(nodename, '.'))) {
                *ptr = '\0';
            }
            if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &nodename, 1, OPAL_STRING))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
            free(nodename);
        } else {
            if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &node->name, 1, OPAL_STRING))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
        }
        /* pack the oversubscribed flag */
        if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &node->oversubscribed, 1, OPAL_UINT8))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    }
    
    /* transfer the payload to the byte object */
    opal_dss.unload(&buf, (void**)&boptr->bytes, &boptr->size);
    OBJ_DESTRUCT(&buf);
    
    return ORTE_SUCCESS;
}

/* decode a nodemap for an application process */
int orte_util_decode_nodemap(opal_byte_object_t *bo)
{
    int n;
    int32_t num_nodes, i, num_daemons;
    orte_process_name_t daemon;
    opal_buffer_t buf;
    int rc;
    uint8_t oversub;
    char *nodename;

    OPAL_OUTPUT_VERBOSE((1, orte_nidmap_output,
                         "%s decode:nidmap decoding nodemap",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    /* xfer the byte object to a buffer for unpacking */
    OBJ_CONSTRUCT(&buf, opal_buffer_t);
    opal_dss.load(&buf, bo->bytes, bo->size);
    
    /* unpack number of nodes */
    n=1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(&buf, &num_nodes, &n, OPAL_INT32))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
 
    OPAL_OUTPUT_VERBOSE((1, orte_nidmap_output,
                         "%s decode:nidmap decoding %d nodes",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), num_nodes));
    
    /* set the daemon jobid */
    daemon.jobid = ORTE_DAEMON_JOBID(ORTE_PROC_MY_NAME->jobid);

    num_daemons = 0;
    for (i=0; i < num_nodes; i++) {
        /* unpack the daemon vpid */
        n=1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(&buf, &daemon.vpid, &n, ORTE_VPID))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        if (ORTE_VPID_INVALID != daemon.vpid) {
            ++num_daemons;
        }
        /* unpack and store the node's name */
        n=1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(&buf, &nodename, &n, OPAL_STRING))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        if (ORTE_SUCCESS != (rc = orte_db.store(&daemon, ORTE_DB_HOSTNAME, nodename, OPAL_STRING))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        OPAL_OUTPUT_VERBOSE((2, orte_nidmap_output,
                             "%s orte:util:decode:nidmap daemon %s node %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_VPID_PRINT(daemon.vpid), nodename));
        /* if this is my daemon, then store the data for me too */
        if (daemon.vpid == ORTE_PROC_MY_DAEMON->vpid) {
            if (ORTE_SUCCESS != (rc = orte_db.store(ORTE_PROC_MY_NAME, ORTE_DB_HOSTNAME, nodename, OPAL_STRING))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
            if (ORTE_SUCCESS != (rc = orte_db.store(ORTE_PROC_MY_NAME, ORTE_DB_DAEMON_VPID, &daemon.vpid, ORTE_VPID))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
        }
        /* unpack and discard the oversubscribed flag - procs don't need it */
        n=1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(&buf, &oversub, &n, OPAL_UINT8))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    }
    
    /* update num_daemons */
    orte_process_info.num_daemons = num_daemons;
    
    OBJ_DESTRUCT(&buf);
    return ORTE_SUCCESS;
}

/* decode a nodemap for a daemon */
int orte_util_decode_daemon_nodemap(opal_byte_object_t *bo)
{
    int n;
    int32_t num_nodes, i;
    orte_vpid_t vpid;
    orte_node_t *node;
    opal_buffer_t buf;
    int rc;
    uint8_t *oversub;
    char *name;
    orte_job_t *daemons;
    orte_proc_t *dptr;

    OPAL_OUTPUT_VERBOSE((1, orte_nidmap_output,
                         "%s decode:nidmap decoding daemon nodemap",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    /* xfer the byte object to a buffer for unpacking */
    OBJ_CONSTRUCT(&buf, opal_buffer_t);
    opal_dss.load(&buf, bo->bytes, bo->size);
    
    /* unpack number of nodes */
    n=1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(&buf, &num_nodes, &n, OPAL_INT32))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
 
    OPAL_OUTPUT_VERBOSE((1, orte_nidmap_output,
                         "%s decode:nidmap decoding %d nodes",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), num_nodes));
    
    /* set the size of the node pool storage so we minimize realloc's */
    if (ORTE_SUCCESS != (rc = opal_pointer_array_set_size(orte_node_pool, num_nodes))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* transfer the data to the nodes, counting the number of
     * daemons in the system
     */
    daemons = orte_get_job_data_object(ORTE_PROC_MY_NAME->jobid);
    for (i=0; i < num_nodes; i++) {
        /* unpack the daemon vpid */
        n=1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(&buf, &vpid, &n, ORTE_VPID))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        /* unpack and store the node's name */
        n=1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(&buf, &name, &n, OPAL_STRING))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        /* do we already have this node? */
        if (NULL == (node = (orte_node_t*)opal_pointer_array_get_item(orte_node_pool, vpid))) {
            node = OBJ_NEW(orte_node_t);
            node->name = name;
            opal_pointer_array_set_item(orte_node_pool, vpid, node);
        } else {
            free(name);
        }
        /* unpack the oversubscribed flag */
        n=1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(&buf, &oversub, &n, OPAL_UINT8))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        if (ORTE_VPID_INVALID == vpid) {
            /* no daemon on this node */
            continue;
        }
        if (NULL == (dptr = (orte_proc_t*)opal_pointer_array_get_item(daemons->procs, vpid))) {
            dptr = OBJ_NEW(orte_proc_t);
            dptr->name.jobid = ORTE_PROC_MY_NAME->jobid;
            dptr->name.vpid = vpid;
            opal_pointer_array_set_item(daemons->procs, vpid, dptr);
            daemons->num_procs++;
        }
        if (NULL != node->daemon) {
            OBJ_RELEASE(node->daemon);
        }
        OBJ_RETAIN(dptr);
        node->daemon = dptr;
        if (NULL != dptr->node) {
            OBJ_RELEASE(dptr->node);
        }
        OBJ_RETAIN(node);
        dptr->node = node;
        if (0 == oversub) {
            node->oversubscribed = false;
        } else {
            node->oversubscribed = true;
        }
    }
    
    orte_process_info.num_procs = daemons->num_procs;
    
    if (orte_process_info.max_procs < orte_process_info.num_procs) {
        orte_process_info.max_procs = orte_process_info.num_procs;
    }

    /* update num_daemons */
    orte_process_info.num_daemons = daemons->num_procs;
    
    if (0 < opal_output_get_verbosity(orte_nidmap_output)) {
        for (i=0; i < num_nodes; i++) {
            if (NULL == (node = (orte_node_t*)opal_pointer_array_get_item(orte_node_pool, i))) {
                continue;
            }
            opal_output(0, "%s node[%d].name %s daemon %s",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), i,
                        (NULL == node->name) ? "NULL" : node->name,
                        (NULL == node->daemon) ? "NONE" : ORTE_VPID_PRINT(node->daemon->name.vpid));
        }
    }

    OBJ_DESTRUCT(&buf);
    return ORTE_SUCCESS;
}

int orte_util_encode_pidmap(opal_byte_object_t *boptr)
{
    orte_proc_t *proc;
    opal_buffer_t buf;
    orte_local_rank_t *lrank = NULL;
    orte_node_rank_t *nrank = NULL;
    orte_job_t *jdata = NULL;
    orte_vpid_t *daemons = NULL;
    int i, j, k, rc = ORTE_SUCCESS;
#if OPAL_HAVE_HWLOC
    unsigned int *bind_idx=NULL;
#endif
    orte_proc_state_t *states=NULL;
    orte_app_idx_t *app_idx=NULL;
    int32_t *restarts=NULL;

    /* setup the working buffer */
    OBJ_CONSTRUCT(&buf, opal_buffer_t);
    
    for (j=1; j < orte_job_data->size; j++) {
        /* the job array is no longer left-justified and may
         * have holes in it as we recover resources at job
         * completion
         */
        if (NULL == (jdata = (orte_job_t*)opal_pointer_array_get_item(orte_job_data, j))) {
            continue;
        }
        /* if this job doesn't have a map, then it is a tool
         * and doesn't need to be included
         */
        if (NULL == jdata->map) {
            continue;
        }
        /* pack the jobid */
        if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &jdata->jobid, 1, ORTE_JOBID))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup_and_return;
        }
        /* pack the number of procs */
        if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &jdata->num_procs, 1, ORTE_VPID))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup_and_return;
        }
#if OPAL_HAVE_HWLOC
        /* pack the bind level */
        if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &(jdata->map->bind_level), 1, OPAL_HWLOC_LEVEL_T))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup_and_return;
        }
#endif

        /* allocate memory for the nodes, local ranks, node ranks, and bind_idx */
        daemons = (orte_vpid_t*)malloc(jdata->num_procs * sizeof(orte_vpid_t));
        lrank = (orte_local_rank_t*)malloc(jdata->num_procs*sizeof(orte_local_rank_t));
        nrank = (orte_node_rank_t*)malloc(jdata->num_procs*sizeof(orte_node_rank_t));
        states = (orte_proc_state_t*)malloc(jdata->num_procs*sizeof(orte_proc_state_t));
        app_idx = (orte_app_idx_t*)malloc(jdata->num_procs*sizeof(orte_app_idx_t));
        restarts = (int32_t*)malloc(jdata->num_procs*sizeof(int32_t));
#if OPAL_HAVE_HWLOC
        bind_idx = (unsigned int*)malloc(jdata->num_procs*sizeof(unsigned int));
#endif
        /* transfer and pack the node info in one pack */
        for (i=0, k=0; i < jdata->procs->size; i++) {
            if (NULL == (proc = (orte_proc_t *) opal_pointer_array_get_item(jdata->procs, i))) {
                continue;
            }
            if( k >= (int)jdata->num_procs ) {
                orte_show_help("help-orte-runtime.txt", "orte_nidmap:too_many_nodes",
                               true, jdata->num_procs);
                break;
            }
            daemons[k] = proc->node->daemon->name.vpid;
            lrank[k] = proc->local_rank;
            nrank[k] = proc->node_rank;
            states[k] = proc->state;
            app_idx[k] = proc->app_idx;
            restarts[k] = proc->restarts;
#if OPAL_HAVE_HWLOC
            bind_idx[k] = proc->bind_idx;
#endif
            ++k;
        }
        if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, daemons, jdata->num_procs, ORTE_VPID))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup_and_return;
        }
        /* transfer and pack the local_ranks in one pack */
        if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, lrank, jdata->num_procs, ORTE_LOCAL_RANK))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup_and_return;
        }
        /* transfer and pack the node ranks in one pack */
        if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, nrank, jdata->num_procs, ORTE_NODE_RANK))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup_and_return;
        }
#if OPAL_HAVE_HWLOC
        /* transfer and pack the bind_idx in one pack */
        if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, bind_idx, jdata->num_procs, OPAL_UINT))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup_and_return;
        }
#endif
        /* transfer and pack the states in one pack */
        if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, states, jdata->num_procs, ORTE_PROC_STATE))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup_and_return;
        }
        /* transfer and pack the app_idx's in one pack */
        if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, app_idx, jdata->num_procs, ORTE_APP_IDX))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup_and_return;
        }
        /* transfer and pack the restarts in one pack */
        if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, restarts, jdata->num_procs, OPAL_INT32))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup_and_return;
        }
    }
    
    /* transfer the payload to the byte object */
    opal_dss.unload(&buf, (void**)&boptr->bytes, &boptr->size);

 cleanup_and_return:

    if( NULL != lrank ) {
        free(lrank);
    }
    if( NULL != nrank ) {
        free(nrank);
    }
    if( NULL != daemons ) {
        free(daemons);
    }
#if OPAL_HAVE_HWLOC
    if( NULL != bind_idx ) {
        free(bind_idx);
    }
#endif
    if (NULL != states) {
        free(states);
    }
    if (NULL != app_idx) {
        free(app_idx);
    }
    if (NULL != restarts) {
        free(restarts);
    }
    OBJ_DESTRUCT(&buf);
    
    return rc;
}

/* only APPS call this function - daemons have their own */
int orte_util_decode_pidmap(opal_byte_object_t *bo)
{
    orte_vpid_t i, num_procs, *vptr, daemon;
    orte_vpid_t *daemons=NULL;
    orte_local_rank_t *local_rank=NULL;
    orte_node_rank_t *node_rank=NULL;
#if OPAL_HAVE_HWLOC
    opal_hwloc_level_t bind_level = OPAL_HWLOC_NODE_LEVEL, pbind, *lvptr;
    unsigned int *bind_idx=NULL, pbidx, *uiptr;
#endif
    opal_hwloc_locality_t locality;
    orte_std_cntr_t n;
    opal_buffer_t buf;
    int rc;
    orte_proc_state_t *states = NULL;
    orte_app_idx_t *app_idx = NULL;
    int32_t *restarts = NULL;
    orte_process_name_t proc, dmn;
    orte_namelist_t *nm;
    opal_list_t jobs;
    char *hostname;

    /* xfer the byte object to a buffer for unpacking */
    OBJ_CONSTRUCT(&buf, opal_buffer_t);
    if (ORTE_SUCCESS != (rc = opal_dss.load(&buf, bo->bytes, bo->size))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    
    n = 1;
    /* cycle through the buffer */
    OBJ_CONSTRUCT(&jobs, opal_list_t);
    while (ORTE_SUCCESS == (rc = opal_dss.unpack(&buf, &proc.jobid, &n, ORTE_JOBID))) {
        OPAL_OUTPUT_VERBOSE((2, orte_nidmap_output,
                             "%s orte:util:decode:pidmap working job %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_JOBID_PRINT(proc.jobid)));
        /* record the jobid */
        nm = OBJ_NEW(orte_namelist_t);
        nm->name.jobid = proc.jobid;
        opal_list_append(&jobs, &nm->super);

        /* unpack and store the number of procs */
        n=1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(&buf, &num_procs, &n, ORTE_VPID))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
        proc.vpid = ORTE_VPID_INVALID;
        if (ORTE_SUCCESS != (rc = orte_db.store(&proc, ORTE_DB_NPROCS, &num_procs, ORTE_VPID))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }

#if OPAL_HAVE_HWLOC
        /* unpack and store the binding level */
        n=1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(&buf, &bind_level, &n, OPAL_HWLOC_LEVEL_T))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
        /* store it */
        proc.vpid = ORTE_VPID_INVALID;
        if (ORTE_SUCCESS != (rc = orte_db.store(&proc, ORTE_DB_BIND_LEVEL, &bind_level, OPAL_HWLOC_LEVEL_T))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
        /* set mine */
        if (proc.jobid == ORTE_PROC_MY_NAME->jobid) {
            orte_process_info.bind_level = bind_level;
        }

        OPAL_OUTPUT_VERBOSE((2, orte_nidmap_output,
                             "%s orte:util:decode:pidmap nprocs %s bind level %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_VPID_PRINT(num_procs),
                             opal_hwloc_base_print_level(bind_level)));
#endif

        /* allocate memory for the daemon info */
        daemons = (orte_vpid_t*)malloc(num_procs * sizeof(orte_vpid_t));
        /* unpack it in one shot */
        n=num_procs;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(&buf, daemons, &n, ORTE_VPID))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }

        /* allocate memory for local ranks */
        local_rank = (orte_local_rank_t*)malloc(num_procs*sizeof(orte_local_rank_t));
        /* unpack them in one shot */
        n=num_procs;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(&buf, local_rank, &n, ORTE_LOCAL_RANK))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
        if (proc.jobid == ORTE_PROC_MY_NAME->jobid) {
            /* set mine */
            orte_process_info.my_local_rank = local_rank[ORTE_PROC_MY_NAME->vpid];
            if (ORTE_SUCCESS != (rc = orte_db.store(ORTE_PROC_MY_NAME, ORTE_DB_LOCALRANK,
                                                    &orte_process_info.my_local_rank, ORTE_LOCAL_RANK))) {
                ORTE_ERROR_LOG(rc);
                goto cleanup;
            }
        }
        
        /* allocate memory for node ranks */
        node_rank = (orte_node_rank_t*)malloc(num_procs*sizeof(orte_node_rank_t));
        /* unpack node ranks in one shot */
        n=num_procs;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(&buf, node_rank, &n, ORTE_NODE_RANK))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
        if (proc.jobid == ORTE_PROC_MY_NAME->jobid) {
            /* set mine */
            orte_process_info.my_node_rank = node_rank[ORTE_PROC_MY_NAME->vpid];
            if (ORTE_SUCCESS != (rc = orte_db.store(ORTE_PROC_MY_NAME, ORTE_DB_NODERANK,
                                                    &orte_process_info.my_node_rank, ORTE_NODE_RANK))) {
                ORTE_ERROR_LOG(rc);
                goto cleanup;
            }
        }
        
#if OPAL_HAVE_HWLOC
        /* allocate memory for bind_idx */
        bind_idx = (unsigned int*)malloc(num_procs*sizeof(unsigned int));
        /* unpack bind_idx in one shot */
        n=num_procs;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(&buf, bind_idx, &n, OPAL_UINT))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
        if (proc.jobid == ORTE_PROC_MY_NAME->jobid) {
            /* set mine */
            orte_process_info.bind_idx = bind_idx[ORTE_PROC_MY_NAME->vpid];
            if (ORTE_SUCCESS != (rc = orte_db.store(ORTE_PROC_MY_NAME, ORTE_DB_BIND_INDEX,
                                                    &orte_process_info.bind_idx, OPAL_UINT))) {
                ORTE_ERROR_LOG(rc);
                goto cleanup;
            }
        }
#endif

        /* allocate memory for states */
        states = (orte_proc_state_t*)malloc(num_procs*sizeof(orte_proc_state_t));
        /* unpack states in one shot */
        n=num_procs;
        if (OPAL_SUCCESS != (rc = opal_dss.unpack(&buf, states, &n, ORTE_PROC_STATE))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
        /* dump this info - apps don't need it */
        free(states);
        states = NULL;

        /* allocate memory for app_idx's */
        app_idx = (orte_app_idx_t*)malloc(num_procs*sizeof(orte_app_idx_t));
        /* unpack app_idx's in one shot */
        n=num_procs;
        if (OPAL_SUCCESS != (rc = opal_dss.unpack(&buf, app_idx, &n, ORTE_APP_IDX))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
        /* dump this info - apps don't need it */
        free(app_idx);
        app_idx = NULL;

        /* allocate memory for restarts */
        restarts = (int32_t*)malloc(num_procs*sizeof(int32_t));
        /* unpack restarts in one shot */
        n=num_procs;
        if (OPAL_SUCCESS != (rc = opal_dss.unpack(&buf, restarts, &n, OPAL_INT32))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
        /* dump this info - apps don't need it */
        free(restarts);
        restarts = NULL;

        /* set the daemon jobid */
        dmn.jobid = ORTE_DAEMON_JOBID(ORTE_PROC_MY_NAME->jobid);

        /* xfer the data */
        for (i=0; i < num_procs; i++) {
            if (proc.jobid == ORTE_PROC_MY_NAME->jobid &&
                i == ORTE_PROC_MY_NAME->vpid) {
                continue;
            }
            proc.vpid = i;
            if (ORTE_SUCCESS != (rc = orte_db.store(&proc, ORTE_DB_DAEMON_VPID, &daemons[i], ORTE_VPID))) {
                ORTE_ERROR_LOG(rc);
                goto cleanup;
            }
            /* lookup and store the hostname for this proc */
            dmn.vpid = daemons[i];
            if (ORTE_SUCCESS != (rc = orte_db.fetch_pointer(&dmn, ORTE_DB_HOSTNAME, (void**)&hostname, OPAL_STRING))) {
                ORTE_ERROR_LOG(rc);
                goto cleanup;
            }
            if (ORTE_SUCCESS != (rc = orte_db.store(&proc, ORTE_DB_HOSTNAME, hostname, OPAL_STRING))) {
                ORTE_ERROR_LOG(rc);
                goto cleanup;
            }
            if (ORTE_SUCCESS != (rc = orte_db.store(&proc, ORTE_DB_LOCALRANK, &local_rank[i], ORTE_LOCAL_RANK))) {
                ORTE_ERROR_LOG(rc);
                goto cleanup;
            }
            if (ORTE_SUCCESS != (rc = orte_db.store(&proc, ORTE_DB_NODERANK, &node_rank[i], ORTE_NODE_RANK))) {
                ORTE_ERROR_LOG(rc);
                goto cleanup;
            }
#if OPAL_HAVE_HWLOC
            if (ORTE_SUCCESS != (rc = orte_db.store(&proc, ORTE_DB_BIND_INDEX, &bind_idx[i], OPAL_UINT))) {
                ORTE_ERROR_LOG(rc);
                goto cleanup;
            }
            OPAL_OUTPUT_VERBOSE((10, orte_nidmap_output,
                                 "%s orte:util:decode:pidmap proc %s host %s lrank %d nrank %d bindidx %u",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(&proc), hostname,
                                 (int)local_rank[i], (int)node_rank[i], bind_idx[i]));
#endif
        }
        /* release data */
        free(daemons);
        daemons = NULL;
        free(local_rank);
        local_rank = NULL;
        free(node_rank);
        node_rank = NULL;
#if OPAL_HAVE_HWLOC
        free(bind_idx);
        bind_idx = NULL;
#endif
        /* setup for next cycle */
        n = 1;
    }
    if (ORTE_ERR_UNPACK_READ_PAST_END_OF_BUFFER != rc) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    rc = ORTE_SUCCESS;

    /* now that we have all the data, we are guaranteed
     * to know our own node, so go back and record the
     * locality of each proc relative to me
     */
    while (NULL != (nm = (orte_namelist_t*)opal_list_remove_first(&jobs))) {
        proc.jobid = nm->name.jobid;
        /* recover the number of procs in this job */
        vptr = &num_procs;
        proc.vpid = ORTE_VPID_INVALID;
        if (ORTE_SUCCESS != (rc = orte_db.fetch(&proc, ORTE_DB_NPROCS, (void**)&vptr, ORTE_VPID))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }

        for (i=0; i < num_procs; i++) {
            if (ORTE_PROC_MY_NAME->vpid == i &&
                ORTE_PROC_MY_NAME->jobid == proc.jobid) {
                /* this is me */
                continue;
            }
            proc.vpid = i;
            /* recover the daemon for this proc */
            vptr = &daemon;
            if (ORTE_SUCCESS != (rc = orte_db.fetch(&proc, ORTE_DB_DAEMON_VPID, (void**)&vptr, ORTE_VPID))) {
                ORTE_ERROR_LOG(rc);
                goto cleanup;
            }
            if (daemon == ORTE_PROC_MY_DAEMON->vpid) {
                OPAL_OUTPUT_VERBOSE((2, orte_nidmap_output,
                                     "%s orte:util:decode:pidmap proc %s shares node",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     ORTE_NAME_PRINT(&proc)));
                /* we share a node, so add them to the count of peers
                 * sharing the node with me
                 */
                orte_process_info.num_local_peers++;
#if OPAL_HAVE_HWLOC
                /* retrieve the bind level for the other proc's job */
                lvptr = &pbind;
                proc.vpid = ORTE_VPID_INVALID;
                if (ORTE_SUCCESS != (rc = orte_db.fetch(&proc, ORTE_DB_BIND_LEVEL, (void**)&lvptr, OPAL_HWLOC_LEVEL_T))) {
                    ORTE_ERROR_LOG(rc);
                    goto cleanup;
                }

                /* retrieve the other's proc's bind idx */
                uiptr = &pbidx;
                proc.vpid = i;
                if (ORTE_SUCCESS != (rc = orte_db.fetch(&proc, ORTE_DB_BIND_INDEX, (void**)&uiptr, OPAL_UINT))) {
                    ORTE_ERROR_LOG(rc);
                    goto cleanup;
                }

                /* we share a node - see what else we share */
                locality = opal_hwloc_base_get_relative_locality(opal_hwloc_topology,
                                                                 orte_process_info.bind_level,
                                                                 orte_process_info.bind_idx,
                                                                 pbind, pbidx);
#else
                locality = OPAL_PROC_ON_NODE;
#endif
            } else {
                /* we don't share a node */
                OPAL_OUTPUT_VERBOSE((2, orte_nidmap_output,
                                     "%s orte:util:decode:pidmap proc %s does NOT node [my daemon %s, their daemon %s]",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     ORTE_NAME_PRINT(&proc),
                                     ORTE_VPID_PRINT(ORTE_PROC_MY_DAEMON->vpid),
                                     ORTE_VPID_PRINT(daemon)));
                locality = OPAL_PROC_NON_LOCAL;
            }
            /* store the locality */
                OPAL_OUTPUT_VERBOSE((2, orte_nidmap_output,
                                     "%s orte:util:decode:pidmap set proc %s locality to %s",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     ORTE_NAME_PRINT(&proc),
                                     opal_hwloc_base_print_locality(locality)));
            if (ORTE_SUCCESS != (rc = orte_db.store(&proc, ORTE_DB_LOCALITY, &locality, OPAL_HWLOC_LOCALITY_T))) {
                ORTE_ERROR_LOG(rc);
                goto cleanup;
            }
        }
    }
        
    
 cleanup:
    if (NULL != daemons) {
        free(daemons);
    }
    if (NULL != local_rank) {
        free(local_rank);
    }
    if (NULL != node_rank) {
        free(node_rank);
    }
#if OPAL_HAVE_HWLOC
    if (NULL != bind_idx) {
        free(bind_idx);
    }
#endif
    if (NULL != states) {
        free(states);
    }
    if (NULL != app_idx) {
        free(app_idx);
    }
    if (NULL != restarts) {
        free(restarts);
    }
    OBJ_DESTRUCT(&buf);
    return rc;
}

int orte_util_decode_daemon_pidmap(opal_byte_object_t *bo)
{
    orte_jobid_t jobid;
    orte_vpid_t i, num_procs;
    orte_vpid_t *nodes=NULL;
    orte_local_rank_t *local_rank=NULL;
    orte_node_rank_t *node_rank=NULL;
#if OPAL_HAVE_HWLOC
    opal_hwloc_level_t bind_level = OPAL_HWLOC_NODE_LEVEL;
    unsigned int *bind_idx=NULL;
#endif
    orte_std_cntr_t n;
    opal_buffer_t buf;
    int rc, j, k;
    orte_job_t *jdata, *daemons;
    orte_proc_t *proc, *pptr;
    orte_node_t *node, *nptr;
    orte_proc_state_t *states=NULL;
    orte_app_idx_t *app_idx=NULL;
    int32_t *restarts=NULL;
    orte_job_map_t *map;
    bool found;

    /* xfer the byte object to a buffer for unpacking */
    OBJ_CONSTRUCT(&buf, opal_buffer_t);
    if (ORTE_SUCCESS != (rc = opal_dss.load(&buf, bo->bytes, bo->size))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    
    daemons = orte_get_job_data_object(ORTE_PROC_MY_NAME->jobid);

    n = 1;
    /* cycle through the buffer */
    while (ORTE_SUCCESS == (rc = opal_dss.unpack(&buf, &jobid, &n, ORTE_JOBID))) {
        /* see if we have this job object - could be a restart scenario */
        if (NULL == (jdata = orte_get_job_data_object(jobid))) {
            /* need to create this job */
            jdata = OBJ_NEW(orte_job_t);
            jdata->jobid = jobid;
            opal_pointer_array_set_item(orte_job_data, ORTE_LOCAL_JOBID(jobid), jdata);
        }
        
        /* unpack the number of procs */
        n=1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(&buf, &num_procs, &n, ORTE_VPID))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
        jdata->num_procs = num_procs;

#if OPAL_HAVE_HWLOC
        /* unpack the binding level */
        n=1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(&buf, &bind_level, &n, OPAL_HWLOC_LEVEL_T))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
#endif

        /* allocate memory for the node info */
        nodes = (orte_vpid_t*)malloc(num_procs * 4);
        /* unpack it in one shot */
        n=num_procs;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(&buf, nodes, &n, ORTE_VPID))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }

        /* allocate memory for local ranks */
        local_rank = (orte_local_rank_t*)malloc(num_procs*sizeof(orte_local_rank_t));
        /* unpack them in one shot */
        n=num_procs;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(&buf, local_rank, &n, ORTE_LOCAL_RANK))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
        
        /* allocate memory for node ranks */
        node_rank = (orte_node_rank_t*)malloc(num_procs*sizeof(orte_node_rank_t));
        /* unpack node ranks in one shot */
        n=num_procs;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(&buf, node_rank, &n, ORTE_NODE_RANK))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
        
#if OPAL_HAVE_HWLOC
        /* allocate memory for bind_idx */
        bind_idx = (unsigned int*)malloc(num_procs*sizeof(unsigned int));
        /* unpack bind_idx in one shot */
        n=num_procs;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(&buf, bind_idx, &n, OPAL_UINT))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
#endif

        /* allocate memory for states */
        states = (orte_proc_state_t*)malloc(num_procs*sizeof(orte_proc_state_t));
        /* unpack states in one shot */
        n=num_procs;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(&buf, states, &n, ORTE_PROC_STATE))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
        
        /* allocate memory for app_idx's */
        app_idx = (orte_app_idx_t*)malloc(num_procs*sizeof(orte_app_idx_t));
        /* unpack app_idx's in one shot */
        n=num_procs;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(&buf, app_idx, &n, ORTE_APP_IDX))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
        
        /* allocate memory for restarts */
        restarts = (int32_t*)malloc(num_procs*sizeof(int32_t));
        /* unpack restarts in one shot */
        n=num_procs;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(&buf, restarts, &n, OPAL_INT32))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
        
        /* xfer the data */
        map = jdata->map;
        if (NULL == map) {
            jdata->map = OBJ_NEW(orte_job_map_t);
            map = jdata->map;
        }
        for (i=0; i < num_procs; i++) {
            if (NULL == (proc = (orte_proc_t*)opal_pointer_array_get_item(jdata->procs, i))) {
                proc = OBJ_NEW(orte_proc_t);
                proc->name.jobid = jdata->jobid;
                proc->name.vpid = i;
                opal_pointer_array_set_item(jdata->procs, i, proc);
            }
            /* lookup the node - should always be present */
            if (NULL == (node = (orte_node_t*)opal_pointer_array_get_item(orte_node_pool, nodes[i]))) {
                /* this should never happen, but protect ourselves anyway */
                node = OBJ_NEW(orte_node_t);
                /* get the daemon */
                if (NULL == (pptr = (orte_proc_t*)opal_pointer_array_get_item(daemons->procs, nodes[i]))) {
                    pptr = OBJ_NEW(orte_proc_t);
                    pptr->name.jobid = ORTE_PROC_MY_NAME->jobid;
                    pptr->name.vpid = nodes[i];
                    opal_pointer_array_set_item(daemons->procs, nodes[i], pptr);
                }
                node->daemon = pptr;
                opal_pointer_array_add(orte_node_pool, node);
            }
            if (NULL != proc->node) {
                if (node != proc->node) {
                    /* proc has moved - cleanup the prior node proc array */
                    for (j=0; j < proc->node->procs->size; j++) {
                        if (NULL == (pptr = (orte_proc_t*)opal_pointer_array_get_item(proc->node->procs, j))) {
                            continue;
                        }
                        if (pptr == proc) {
                            /* maintain accounting */
                            OBJ_RELEASE(pptr);
                            opal_pointer_array_set_item(proc->node->procs, j, NULL);
                            proc->node->num_procs--;
                            if (0 == proc->node->num_procs) {
                                /* remove node from the map */
                                for (k=0; k < map->nodes->size; k++) {
                                    if (NULL == (nptr = (orte_node_t*)opal_pointer_array_get_item(map->nodes, k))) {
                                        continue;
                                    }
                                    if (nptr == proc->node) {
                                        /* maintain accounting */
                                        OBJ_RELEASE(nptr);
                                        opal_pointer_array_set_item(map->nodes, k, NULL);
                                        map->num_nodes--;
                                        break;
                                    }
                                }
                            }
                            break;
                        }
                    }
                }
                OBJ_RELEASE(proc->node);
            }
            /* see if this node is already in the map */
            found = false;
            for (j=0; j < map->nodes->size; j++) {
                if (NULL == (nptr = (orte_node_t*)opal_pointer_array_get_item(map->nodes, j))) {
                    continue;
                }
                if (nptr == node) {
                    found = true;
                    break;
                }
            }
            if (!found) {
                opal_pointer_array_add(map->nodes, node);
                map->num_nodes++;
            }
            /* add the node to the proc */
            OBJ_RETAIN(node);
            proc->node = node;
            /* add the proc to the node */
            OBJ_RETAIN(proc);
            opal_pointer_array_add(node->procs, proc);
            /* update proc values */
            proc->local_rank = local_rank[i];
            proc->node_rank = node_rank[i];
            proc->app_idx = app_idx[i];
            proc->restarts = restarts[i];
            proc->state = states[i];
        }
        
        /* release data */
        free(nodes);
        nodes = NULL;
        free(local_rank);
        local_rank = NULL;
        free(node_rank);
        node_rank = NULL;
#if OPAL_HAVE_HWLOC
        free(bind_idx);
        bind_idx = NULL;
#endif
        free(states);
        states = NULL;
        free(app_idx);
        app_idx = NULL;
        free(restarts);
        restarts = NULL;
        /* setup for next cycle */
        n = 1;
    }
    if (ORTE_ERR_UNPACK_READ_PAST_END_OF_BUFFER == rc) {
        rc = ORTE_SUCCESS;
    }
    
 cleanup:
    if (NULL != nodes) {
        free(nodes);
    }
    if (NULL != local_rank) {
        free(local_rank);
    }
    if (NULL != node_rank) {
        free(node_rank);
    }
#if OPAL_HAVE_HWLOC
    if (NULL != bind_idx) {
        free(bind_idx);
    }
#endif
    if (NULL != states) {
        free(states);
    }
    if (NULL != app_idx) {
        free(app_idx);
    }
    if (NULL != restarts) {
        free(restarts);
    }
    OBJ_DESTRUCT(&buf);
    return rc;
}
