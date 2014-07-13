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
 * Copyright (c) 2012-2014 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2013-2014 Intel, Inc. All rights reserved
 *
 * Copyright (c) 2014      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
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
#include "opal/mca/dstore/dstore.h"
#include "opal/mca/hwloc/base/base.h"
#include "opal/util/net.h"
#include "opal/util/output.h"
#include "opal/util/argv.h"
#include "opal/datatype/opal_datatype.h"

#include "orte/mca/dfs/dfs.h"
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
static int decode_app_nodemap(opal_byte_object_t *bo);
static int decode_app_pidmap(opal_byte_object_t *bo);

int orte_util_nidmap_init(opal_buffer_t *buffer)
{
    int32_t cnt;
    int rc;
    opal_byte_object_t *bo;

    orte_nidmap_verbose = 0;
    (void) mca_base_var_register ("orte", "orte", NULL, "nidmap_verbose",
                                  "Verbosity of the nidmap subsystem",
                                  MCA_BASE_VAR_TYPE_INT, NULL, 0,
                                  MCA_BASE_VAR_FLAG_INTERNAL,
                                  OPAL_INFO_LVL_9,
                                  MCA_BASE_VAR_SCOPE_ALL_EQ,
                                  &orte_nidmap_verbose);
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
    if (ORTE_SUCCESS != (rc = decode_app_nodemap(bo))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    /* the bytes in the object were free'd by the decode */
    free(bo);
    
    /* extract the byte object holding the process map */
    cnt=1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &bo, &cnt, OPAL_BYTE_OBJECT))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    /* unpack the process map */
    if (ORTE_SUCCESS != (rc = decode_app_pidmap(bo))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    /* the bytes in the object were free'd by the decode */
    free(bo);

    return ORTE_SUCCESS;
}

void orte_util_nidmap_finalize(void)
{
#if OPAL_HAVE_HWLOC
    /* destroy the topology */
    if (NULL != opal_hwloc_topology) {
        hwloc_obj_t root;
        root = hwloc_get_root_obj(opal_hwloc_topology);
        if (NULL != root->userdata) {
            OBJ_RELEASE(root->userdata);
        }
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
    opal_value_t kv;

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
    OBJ_CONSTRUCT(&kv, opal_value_t);
    kv.key = strdup(ORTE_DB_DAEMON_VPID);
    kv.data.uint32 = proc.vpid;
    kv.type = OPAL_UINT32;
    if (OPAL_SUCCESS != (rc = opal_dstore.store(opal_dstore_internal,
                                                (opal_identifier_t*)&proc,
                                                &kv))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&kv);
        return rc;
    }
    OBJ_DESTRUCT(&kv);

    OBJ_CONSTRUCT(&kv, opal_value_t);
    kv.key = strdup(ORTE_DB_HOSTNAME);
    kv.data.string = strdup("HNP");
    kv.type = OPAL_STRING;
    if (OPAL_SUCCESS != (rc = opal_dstore.store(opal_dstore_internal,
                                                (opal_identifier_t*)&proc,
                                                &kv))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&kv);
        return rc;
    }
    OBJ_DESTRUCT(&kv);

    /* the daemon vpids will be assigned in order,
     * starting with vpid=1 for the first node in
     * the list
     */
    OBJ_CONSTRUCT(&buf, opal_buffer_t);
    for (i=0; i < num_nodes; i++) {
        /* define the vpid for this daemon */
        proc.vpid = i+1;
        /* store the hostname for the proc */
        OBJ_CONSTRUCT(&kv, opal_value_t);
        kv.key = strdup(ORTE_DB_HOSTNAME);
        kv.data.string = strdup(nodes[i]);
        kv.type = OPAL_STRING;
        if (OPAL_SUCCESS != (rc = opal_dstore.store(opal_dstore_internal,
                                                    (opal_identifier_t*)&proc,
                                                    &kv))) {
            ORTE_ERROR_LOG(rc);
            OBJ_DESTRUCT(&kv);
            return rc;
        }
        OBJ_DESTRUCT(&kv);

        /* the arch defaults to our arch so that non-hetero
         * case will yield correct behavior
         */
        OBJ_CONSTRUCT(&kv, opal_value_t);
        kv.key = strdup(ORTE_DB_ARCH);
        kv.data.uint32 = opal_local_arch;
        kv.type = OPAL_UINT32;
        if (OPAL_SUCCESS != (rc = opal_dstore.store(opal_dstore_internal,
                                                    (opal_identifier_t*)&proc,
                                                    &kv))) {
            ORTE_ERROR_LOG(rc);
            OBJ_DESTRUCT(&kv);
            return rc;
        }
        OBJ_DESTRUCT(&kv);

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

int orte_util_encode_nodemap(opal_byte_object_t *boptr, bool update)
{
    orte_node_t *node;
    int32_t i;
    int rc;
    opal_buffer_t buf;
    orte_job_t *daemons;
    orte_proc_t *dmn;

    OPAL_OUTPUT_VERBOSE((2, orte_nidmap_output,
                         "%s orte:util:encode_nidmap",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    /* if the daemon job has not been updated, then there is
     * nothing to send
     */
    daemons = orte_get_job_data_object(ORTE_PROC_MY_NAME->jobid);
    if (update && !ORTE_FLAG_TEST(daemons, ORTE_JOB_FLAG_UPDATED)) {
        boptr->bytes = NULL;
        boptr->size = 0;
        return ORTE_SUCCESS;
    }

    /* setup a buffer for tmp use */
    OBJ_CONSTRUCT(&buf, opal_buffer_t);

    /* send the number of nodes */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &daemons->num_procs, 1, ORTE_VPID))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    for (i=0; i < daemons->procs->size; i++) {
        if (NULL == (dmn = (orte_proc_t*)opal_pointer_array_get_item(daemons->procs, i))) {
            continue;
        }
        /* if the daemon doesn't have a node, that's an error */
        if (NULL == (node = dmn->node)) {
            opal_output(0, "DAEMON %s HAS NO NODE", ORTE_NAME_PRINT(&dmn->name));
            ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
            return ORTE_ERR_NOT_FOUND;
        }
        if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &dmn->name.vpid, 1, ORTE_VPID))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        /* pack the node */
        if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &node, 1, ORTE_NODE))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    }
    
    /* transfer the payload to the byte object */
    opal_dss.unload(&buf, (void**)&boptr->bytes, &boptr->size);
    OBJ_DESTRUCT(&buf);
    
    OPAL_OUTPUT_VERBOSE((2, orte_nidmap_output,
                         "%s orte:util:build:daemon:nidmap packed %d bytes",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), boptr->size));
    
    return ORTE_SUCCESS;
}

/* decode a nodemap for an application process */
static int decode_app_nodemap(opal_byte_object_t *bo)
{
    int n;
    orte_vpid_t num_daemons;
    orte_process_name_t daemon;
    opal_buffer_t buf;
    int rc=ORTE_SUCCESS;
    orte_node_t *node;
    orte_vpid_t hostid, *hostidptr;
    opal_value_t kv;

    OPAL_OUTPUT_VERBOSE((1, orte_nidmap_output,
                         "%s decode:nidmap decoding nodemap",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    /* should never happen, but... */
    if (NULL == bo->bytes || 0 == bo->size) {
        return ORTE_SUCCESS;
    }

    /* xfer the byte object to a buffer for unpacking */
    OBJ_CONSTRUCT(&buf, opal_buffer_t);
    opal_dss.load(&buf, bo->bytes, bo->size);
    
    /* unpack the number of daemons */
    n=1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(&buf, &num_daemons, &n, ORTE_VPID))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    /* set the daemon jobid */
    daemon.jobid = ORTE_DAEMON_JOBID(ORTE_PROC_MY_NAME->jobid);

    n=1;
    while (OPAL_SUCCESS == (rc = opal_dss.unpack(&buf, &daemon.vpid, &n, ORTE_VPID))) {
        /* unpack and store the node */
        n=1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(&buf, &node, &n, ORTE_NODE))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        /* we only need the hostname for our own error messages, so mark it as internal */
        OBJ_CONSTRUCT(&kv, opal_value_t);
        kv.key = strdup(ORTE_DB_HOSTNAME);
        kv.type = OPAL_STRING;
        kv.data.string = strdup(node->name);
        if (ORTE_SUCCESS != (rc = opal_dstore.store(opal_dstore_internal,
                                                    (opal_identifier_t*)&daemon, &kv))) {
            ORTE_ERROR_LOG(rc);
            OBJ_DESTRUCT(&kv);
            return rc;
        }
        OBJ_DESTRUCT(&kv);
        /* now store a direct reference so we can quickly lookup the daemon from a hostname */
        opal_output_verbose(2, orte_nidmap_output,
                            "%s storing nodename %s for daemon %s",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                            node->name, ORTE_VPID_PRINT(daemon.vpid));
        OBJ_CONSTRUCT(&kv, opal_value_t);
        kv.key = strdup(node->name);
        kv.type = OPAL_UINT32;
        kv.data.uint32 = daemon.vpid;
        if (ORTE_SUCCESS != (rc = opal_dstore.store(opal_dstore_internal,
                                                    (opal_identifier_t*)ORTE_NAME_WILDCARD, &kv))) {
            ORTE_ERROR_LOG(rc);
            OBJ_DESTRUCT(&kv);
            return rc;
        }
        OBJ_DESTRUCT(&kv);

        OPAL_OUTPUT_VERBOSE((2, orte_nidmap_output,
                             "%s orte:util:decode:nidmap daemon %s node %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_VPID_PRINT(daemon.vpid), node->name));

        /* if this is my daemon, then store the data for me too */
        if (daemon.vpid == ORTE_PROC_MY_DAEMON->vpid) {
            OBJ_CONSTRUCT(&kv, opal_value_t);
            kv.key = strdup(ORTE_DB_HOSTNAME);
            kv.type = OPAL_STRING;
            kv.data.string = strdup(node->name);
            if (ORTE_SUCCESS != (rc = opal_dstore.store(opal_dstore_nonpeer,
                                                        (opal_identifier_t*)ORTE_PROC_MY_NAME, &kv))) {
                ORTE_ERROR_LOG(rc);
                OBJ_DESTRUCT(&kv);
                return rc;
            }
            OBJ_DESTRUCT(&kv);
            /* we may need our daemon vpid to be shared with non-peers */
            OBJ_CONSTRUCT(&kv, opal_value_t);
            kv.key = strdup(ORTE_DB_DAEMON_VPID);
            kv.type = OPAL_UINT32;
            kv.data.uint32 = daemon.vpid;
            if (ORTE_SUCCESS != (rc = opal_dstore.store(opal_dstore_nonpeer,
                                                        (opal_identifier_t*)ORTE_PROC_MY_NAME, &kv))) {
                ORTE_ERROR_LOG(rc);
                OBJ_DESTRUCT(&kv);
                return rc;
            }
            OBJ_DESTRUCT(&kv);
        }

        /* if coprocessors were detected, unpack the hostid for the node - this
         * value is associate with this daemon, not with any application process
         */
        hostid = 0;
        hostidptr = &hostid;
        if (orte_get_attribute(&node->attributes, ORTE_NODE_HOSTID, (void**)&hostidptr, ORTE_VPID)) {
            orte_coprocessors_detected = true;  // obviously, coprocessors were detected
            OBJ_CONSTRUCT(&kv, opal_value_t);
            kv.key = strdup(ORTE_DB_HOSTID);
            kv.type = OPAL_UINT32;
            kv.data.uint32 = hostid;
            if (ORTE_SUCCESS != (rc = opal_dstore.store(opal_dstore_nonpeer,
                                                        (opal_identifier_t*)&daemon, &kv))) {
                ORTE_ERROR_LOG(rc);
                OBJ_DESTRUCT(&kv);
                return rc;
            }
            /* if this is my daemon, then store it as my hostid as well */
            if (daemon.vpid == ORTE_PROC_MY_DAEMON->vpid) {
                if (ORTE_SUCCESS != (rc = opal_dstore.store(opal_dstore_nonpeer,
                                                            (opal_identifier_t*)ORTE_PROC_MY_NAME, &kv))) {
                    ORTE_ERROR_LOG(rc);
                    OBJ_DESTRUCT(&kv);
                    return rc;
                }
                /* and record it */
                orte_process_info.my_hostid = hostid;
            }
            OBJ_DESTRUCT(&kv);
        }
        /* we don't need the node object itself */
        OBJ_RELEASE(node);
    }
    if (ORTE_ERR_UNPACK_READ_PAST_END_OF_BUFFER != rc) {
        ORTE_ERROR_LOG(rc);
    } else {
        rc = ORTE_SUCCESS;
    }

    /* update num_daemons */
    orte_process_info.num_daemons = num_daemons;
    
    OBJ_DESTRUCT(&buf);
    return rc;
}

/* decode a nodemap for a daemon */
int orte_util_decode_daemon_nodemap(opal_byte_object_t *bo)
{
    int n;
    orte_vpid_t vpid;
    orte_node_t *node, *nptr;
    opal_buffer_t buf;
    int rc=ORTE_SUCCESS;
    orte_job_t *daemons;
    orte_proc_t *dptr;
    orte_vpid_t num_daemons;

    OPAL_OUTPUT_VERBOSE((0, orte_nidmap_output,
                         "%s decode:nidmap decoding daemon nodemap",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    if (NULL == bo->bytes || 0 == bo->size) {
        /* nothing to unpack */
        return ORTE_SUCCESS;
    }

    /* xfer the byte object to a buffer for unpacking */
    OBJ_CONSTRUCT(&buf, opal_buffer_t);
    opal_dss.load(&buf, bo->bytes, bo->size);

    /* unpack the number of procs */
    n=1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(&buf, &num_daemons, &n, ORTE_VPID))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    /* transfer the data to the nodes */
    daemons = orte_get_job_data_object(ORTE_PROC_MY_NAME->jobid);
    daemons->num_procs = num_daemons;
    n=1;
    while (OPAL_SUCCESS == (rc = opal_dss.unpack(&buf, &vpid, &n, ORTE_VPID))) {
        /* unpack and store the node */
        n=1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(&buf, &node, &n, ORTE_NODE))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        /* do we already have this node? */
        nptr = (orte_node_t*)opal_pointer_array_get_item(orte_node_pool, vpid);
        /* set the new node object into the array */
        opal_pointer_array_set_item(orte_node_pool, vpid, node);
        if (NULL == (dptr = (orte_proc_t*)opal_pointer_array_get_item(daemons->procs, vpid))) {
            dptr = OBJ_NEW(orte_proc_t);
            dptr->name.jobid = ORTE_PROC_MY_NAME->jobid;
            dptr->name.vpid = vpid;
            opal_pointer_array_set_item(daemons->procs, vpid, dptr);
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
        if (NULL != nptr) {
            OBJ_RELEASE(nptr);
        }
        dptr->node = node;
    }
    if (ORTE_ERR_UNPACK_READ_PAST_END_OF_BUFFER != rc) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&buf);
        return rc;
    }
    rc = ORTE_SUCCESS;

    orte_process_info.num_procs = daemons->num_procs;
    
    if (orte_process_info.max_procs < orte_process_info.num_procs) {
        orte_process_info.max_procs = orte_process_info.num_procs;
    }

    /* update num_daemons */
    orte_process_info.num_daemons = daemons->num_procs;
    
    if (0 < opal_output_get_verbosity(orte_nidmap_output)) {
        int i;
        for (i=0; i < orte_node_pool->size; i++) {
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
    return rc;
}

/* encode ONLY the minimum amount of info that a proc needs in
 * its local store. Since anything sent to an app is going
 * to be considered an "update", just send what we know about */
int orte_util_encode_app_pidmap(opal_byte_object_t *boptr)
{
    orte_proc_t *proc;
    opal_buffer_t buf;
    int i, j, rc = ORTE_SUCCESS;
    orte_job_t *jdata;

    /* setup the working buffer */
    OBJ_CONSTRUCT(&buf, opal_buffer_t);
    
    /* we don't care about the daemon job, so don't include j=0 */
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
        /* if this job has already terminated, then ignore it */
        if (ORTE_JOB_STATE_TERMINATED < jdata->state) {
            continue;
        }
        /* pack the jobid */
        if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &jdata->jobid, 1, ORTE_JOBID))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup_and_return;
        }
        /* pack the total number of procs */
        if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &jdata->num_procs, 1, ORTE_VPID))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup_and_return;
        }
        /* pack the offset */
        if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &jdata->offset, 1, ORTE_VPID))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup_and_return;
        }
        /* cycle thru the job's procs */
        for (i=0; i < jdata->procs->size; i++) {
            if (NULL == (proc = (orte_proc_t *) opal_pointer_array_get_item(jdata->procs, i))) {
                continue;
            }
            if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &proc, 1, ORTE_PROC))) {
                ORTE_ERROR_LOG(rc);
                goto cleanup_and_return;
            }
        }
    }
    
    /* transfer the payload to the byte object */
    opal_dss.unload(&buf, (void**)&boptr->bytes, &boptr->size);

 cleanup_and_return:
    OBJ_DESTRUCT(&buf);
    
    return rc;
}

/* only APPS call this function - daemons have their own */
static int decode_app_pidmap(opal_byte_object_t *bo)
{
    orte_vpid_t num_procs, offset, j;
    char *cpu_bitmap;
    orte_std_cntr_t n;
    opal_buffer_t buf;
    int rc;
    orte_process_name_t proc, dmn;
    opal_list_t myvals;
    opal_value_t kv, *kvp;
    orte_proc_t *pptr;

    /* xfer the byte object to a buffer for unpacking */
    OBJ_CONSTRUCT(&buf, opal_buffer_t);
    if (ORTE_SUCCESS != (rc = opal_dss.load(&buf, bo->bytes, bo->size))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }

    /* set the daemon jobid */
    dmn.jobid = ORTE_DAEMON_JOBID(ORTE_PROC_MY_NAME->jobid);

    n = 1;
    /* cycle through the buffer */
    orte_process_info.num_local_peers = 0;
    while (ORTE_SUCCESS == (rc = opal_dss.unpack(&buf, &proc.jobid, &n, ORTE_JOBID))) {
        OPAL_OUTPUT_VERBOSE((2, orte_nidmap_output,
                             "%s orte:util:decode:pidmap working job %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_JOBID_PRINT(proc.jobid)));

        /* unpack and store the number of procs */
        n=1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(&buf, &num_procs, &n, ORTE_VPID))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
        proc.vpid = ORTE_VPID_INVALID;
        /* only useful to ourselves */
        OBJ_CONSTRUCT(&kv, opal_value_t);
        kv.key = strdup(ORTE_DB_NPROCS);
        kv.type = OPAL_UINT32;
        kv.data.uint32 = num_procs;
        if (ORTE_SUCCESS != (rc = opal_dstore.store(opal_dstore_internal,
                                                    (opal_identifier_t*)&proc, &kv))) {
            ORTE_ERROR_LOG(rc);
            OBJ_DESTRUCT(&kv);
            goto cleanup;
        }
        OBJ_DESTRUCT(&kv);
        /* unpack and store the offset */
        n=1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(&buf, &offset, &n, ORTE_VPID))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
        /* only of possible use to ourselves */
        OBJ_CONSTRUCT(&kv, opal_value_t);
        kv.key = strdup(ORTE_DB_NPROC_OFFSET);
        kv.type = OPAL_UINT32;
        kv.data.uint32 = offset;
        if (ORTE_SUCCESS != (rc = opal_dstore.store(opal_dstore_internal,
                                                    (opal_identifier_t*)&proc, &kv))) {
            ORTE_ERROR_LOG(rc);
            OBJ_DESTRUCT(&kv);
            goto cleanup;
        }
        OBJ_DESTRUCT(&kv);
        /* cycle thru the data until we hit an INVALID vpid indicating
         * all data for this job has been read
         */
        for (j=0; j < num_procs; j++) {
            n=1;
            if (ORTE_SUCCESS != (rc = opal_dss.unpack(&buf, &pptr, &n, ORTE_PROC))) {
                ORTE_ERROR_LOG(rc);
                goto cleanup;
            }
            dmn.vpid = pptr->parent;  // complete the id of the daemon for this proc
            if (pptr->name.jobid == ORTE_PROC_MY_NAME->jobid &&
                pptr->name.vpid == ORTE_PROC_MY_NAME->vpid) {
                /* set mine */
                orte_process_info.my_local_rank = pptr->local_rank;
                orte_process_info.my_node_rank = pptr->node_rank;
                /* if we are the local leader (i.e., local_rank=0), then record it */
                if (0 == pptr->local_rank) {
                    OBJ_CONSTRUCT(&kv, opal_value_t);
                    kv.key = strdup(OPAL_DSTORE_LOCALLDR);
                    kv.type = OPAL_UINT64;
                    kv.data.uint64 = *(opal_identifier_t*)&(pptr->name);
                    if (ORTE_SUCCESS != (rc = opal_dstore.store(opal_dstore_internal,
                                                                (opal_identifier_t*)ORTE_PROC_MY_NAME, &kv))) {
                        ORTE_ERROR_LOG(rc);
                        OBJ_DESTRUCT(&kv);
                        goto cleanup;
                    }
                    OBJ_DESTRUCT(&kv);
                }
                cpu_bitmap = NULL;
#if OPAL_HAVE_HWLOC
                if (orte_get_attribute(&pptr->attributes, ORTE_PROC_CPU_BITMAP, (void**)&cpu_bitmap, OPAL_STRING) &&
                    NULL != cpu_bitmap) {
                    orte_process_info.cpuset = cpu_bitmap;
                }
#endif
            } else if (pptr->name.jobid == ORTE_PROC_MY_NAME->jobid &&
                       pptr->parent == ORTE_PROC_MY_DAEMON->vpid) {
                /* if we share a daemon, then add to my local peers */
                orte_process_info.num_local_peers++;
                /* if this is the local leader (i.e., local_rank=0), then record it */
                if (0 == pptr->local_rank) {
                    OBJ_CONSTRUCT(&kv, opal_value_t);
                    kv.key = strdup(OPAL_DSTORE_LOCALLDR);
                    kv.type = OPAL_UINT64;
                    kv.data.uint64 = *(opal_identifier_t*)&(pptr->name);
                    if (ORTE_SUCCESS != (rc = opal_dstore.store(opal_dstore_internal,
                                                                (opal_identifier_t*)ORTE_PROC_MY_NAME, &kv))) {
                        ORTE_ERROR_LOG(rc);
                        OBJ_DESTRUCT(&kv);
                        goto cleanup;
                    }
                    OBJ_DESTRUCT(&kv);
                }
            }
            /* store the values in the database - again, these are for our own internal use */
            OBJ_CONSTRUCT(&kv, opal_value_t);
            kv.key = strdup(OPAL_DSTORE_LOCALRANK);
            kv.type = OPAL_UINT16;
            kv.data.uint16 = pptr->local_rank;
            if (ORTE_SUCCESS != (rc = opal_dstore.store(opal_dstore_internal,
                                                        (opal_identifier_t*)&(pptr->name), &kv))) {
                ORTE_ERROR_LOG(rc);
                OBJ_DESTRUCT(&kv);
                goto cleanup;
            }
            OBJ_DESTRUCT(&kv);
            OBJ_CONSTRUCT(&kv, opal_value_t);
            kv.key = strdup(ORTE_DB_NODERANK);
            kv.type = OPAL_UINT16;
            kv.data.uint16 = pptr->node_rank;
            if (ORTE_SUCCESS != (rc = opal_dstore.store(opal_dstore_internal,
                                                        (opal_identifier_t*)&(pptr->name), &kv))) {
                ORTE_ERROR_LOG(rc);
                OBJ_DESTRUCT(&kv);
                goto cleanup;
            }
            OBJ_DESTRUCT(&kv);
            cpu_bitmap = NULL;
            if (orte_get_attribute(&pptr->attributes, ORTE_PROC_CPU_BITMAP, (void**)&cpu_bitmap, OPAL_STRING) &&
                NULL != cpu_bitmap) {
                OBJ_CONSTRUCT(&kv, opal_value_t);
                kv.key = strdup(OPAL_DSTORE_CPUSET);
                kv.type = OPAL_STRING;
                kv.data.string = cpu_bitmap;  // will free the storage when kv is destructed
                if (ORTE_SUCCESS != (rc = opal_dstore.store(opal_dstore_internal,
                                                            (opal_identifier_t*)&(pptr->name), &kv))) {
                    ORTE_ERROR_LOG(rc);
                    OBJ_DESTRUCT(&kv);
                    goto cleanup;
                }
                /* also need a copy in nonpeer to support dynamic spawns */
                if (ORTE_SUCCESS != (rc = opal_dstore.store(opal_dstore_nonpeer,
                                                            (opal_identifier_t*)&(pptr->name), &kv))) {
                    ORTE_ERROR_LOG(rc);
                    OBJ_DESTRUCT(&kv);
                    goto cleanup;
                }
                OBJ_DESTRUCT(&kv);
            }
            /* we don't need to store the rest of the values
             * for ourself in the database
             * as we already did so during startup
             */
            if (pptr->name.jobid != ORTE_PROC_MY_NAME->jobid ||
                pptr->name.vpid != ORTE_PROC_MY_NAME->vpid) {
                /* store the data for this proc - the location of a proc is something
                 * we would potentially need to share with a non-peer
                 */
                OBJ_CONSTRUCT(&kv, opal_value_t);
                kv.key = strdup(ORTE_DB_DAEMON_VPID);
                kv.type = OPAL_UINT32;
                kv.data.uint32 = pptr->parent;
                if (ORTE_SUCCESS != (rc = opal_dstore.store(opal_dstore_nonpeer,
                                                            (opal_identifier_t*)&(pptr->name), &kv))) {
                    ORTE_ERROR_LOG(rc);
                    OBJ_DESTRUCT(&kv);
                    goto cleanup;
                }
                OBJ_DESTRUCT(&kv);
                /* in a singleton comm_spawn, we can be passed the name of a daemon, which
                 * means that the proc's parent is invalid - check and avoid the rest of
                 * this logic in that case */
                if (ORTE_VPID_INVALID != dmn.vpid) {
                    /* if coprocessors were detected, lookup and store the hostid for this proc */
                    if (orte_coprocessors_detected) {
                        /* lookup the hostid for this daemon */
                        OBJ_CONSTRUCT(&myvals, opal_list_t);
                        if (ORTE_SUCCESS != (rc = opal_dstore.fetch(opal_dstore_nonpeer,
                                                                    (opal_identifier_t*)&dmn,
                                                                    ORTE_DB_HOSTID, &myvals))) {
                            ORTE_ERROR_LOG(rc);
                            OPAL_LIST_DESTRUCT(&myvals);
                            goto cleanup;
                        }
                        kvp = (opal_value_t*)opal_list_get_first(&myvals);
                        OPAL_OUTPUT_VERBOSE((2, orte_nidmap_output,
                                             "%s FOUND HOSTID %s FOR DAEMON %s",
                                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                             ORTE_VPID_PRINT(kvp->data.uint32), ORTE_VPID_PRINT(dmn.vpid)));
                        /* store it as hostid for this proc */
                        if (ORTE_SUCCESS != (rc = opal_dstore.store(opal_dstore_nonpeer,
                                                                    (opal_identifier_t*)&(pptr->name), kvp))) {
                            ORTE_ERROR_LOG(rc);
                            OPAL_LIST_DESTRUCT(&myvals);
                            goto cleanup;
                        }
                        OPAL_LIST_DESTRUCT(&myvals);
                    }
                    /* lookup and store the hostname for this proc */
                    OBJ_CONSTRUCT(&myvals, opal_list_t);
                    if (ORTE_SUCCESS != (rc = opal_dstore.fetch(opal_dstore_internal,
                                                                (opal_identifier_t*)&dmn,
                                                                ORTE_DB_HOSTNAME, &myvals))) {
                        ORTE_ERROR_LOG(rc);
                        OPAL_LIST_DESTRUCT(&myvals);
                        goto cleanup;
                    }
                    kvp = (opal_value_t*)opal_list_get_first(&myvals);
                    if (ORTE_SUCCESS != (rc = opal_dstore.store(opal_dstore_nonpeer,
                                                                (opal_identifier_t*)&(pptr->name), kvp))) {
                        ORTE_ERROR_LOG(rc);
                        OPAL_LIST_DESTRUCT(&myvals);
                        goto cleanup;
                    }
                    OPAL_LIST_DESTRUCT(&myvals);
                }
                /* store this procs global rank - only used by us */
                OBJ_CONSTRUCT(&kv, opal_value_t);
                kv.key = strdup(ORTE_DB_GLOBAL_RANK);
                kv.type = OPAL_UINT32;
                kv.data.uint32 = pptr->name.vpid + offset;
                if (ORTE_SUCCESS != (rc = opal_dstore.store(opal_dstore_internal,
                                                            (opal_identifier_t*)&(pptr->name), &kv))) {
                    ORTE_ERROR_LOG(rc);
                    OBJ_DESTRUCT(&kv);
                    goto cleanup;
                }
                OBJ_DESTRUCT(&kv);
            } else {
                /* update our own global rank - this is something we will need
                 * to share with non-peers
                 */
                OBJ_CONSTRUCT(&kv, opal_value_t);
                kv.key = strdup(ORTE_DB_GLOBAL_RANK);
                kv.type = OPAL_UINT32;
                kv.data.uint32 = pptr->name.vpid + offset;
                if (ORTE_SUCCESS != (rc = opal_dstore.store(opal_dstore_nonpeer,
                                                            (opal_identifier_t*)&(pptr->name), &kv))) {
                    ORTE_ERROR_LOG(rc);
                    OBJ_DESTRUCT(&kv);
                    goto cleanup;
                }
                OBJ_DESTRUCT(&kv);
            }
        }
        /* setup for next cycle */
        n = 1;
    }
    if (ORTE_ERR_UNPACK_READ_PAST_END_OF_BUFFER != rc) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    rc = ORTE_SUCCESS;        

 cleanup:
    OBJ_DESTRUCT(&buf);
    return rc;
}
