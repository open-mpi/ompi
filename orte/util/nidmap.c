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
 * Copyright (c) 2013      Intel, Inc. All rights reserved
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
#include "opal/mca/db/db.h"
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
    if (ORTE_SUCCESS != (rc = orte_util_decode_nodemap(bo))) {
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
    if (ORTE_SUCCESS != (rc = orte_util_decode_pidmap(bo))) {
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

    num_nodes = opal_argv_count(nodes);
    
    OPAL_OUTPUT_VERBOSE((2, orte_nidmap_output,
                         "%s orte:util:build:daemon:nidmap found %d nodes",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), num_nodes));
    
    if (0 == num_nodes) {
        /* nothing to do */
        return ORTE_SUCCESS;
    }
    
    /* the values computed here do not need to be published
     * as each proc will compute them separately, so mark the
     * storage scope as INTERNAL
     */

    /* install the entry for the HNP */
    proc.jobid = ORTE_PROC_MY_NAME->jobid;
    proc.vpid = 0;
    if (ORTE_SUCCESS != (rc = opal_db.store((opal_identifier_t*)&proc, OPAL_SCOPE_INTERNAL,
                                            ORTE_DB_DAEMON_VPID, &proc.vpid, OPAL_UINT32))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    addr = "HNP";
    if (ORTE_SUCCESS != (rc = opal_db.store((opal_identifier_t*)&proc, OPAL_SCOPE_INTERNAL,
                                            ORTE_DB_HOSTNAME, addr, OPAL_STRING))) {
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
        if (ORTE_SUCCESS != (rc = opal_db.store((opal_identifier_t*)&proc, OPAL_SCOPE_INTERNAL,
                                                ORTE_DB_HOSTNAME, nodes[i], OPAL_STRING))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        /* the arch defaults to our arch so that non-hetero
         * case will yield correct behavior
         */
        if (ORTE_SUCCESS != (rc = opal_db.store((opal_identifier_t*)&proc, OPAL_SCOPE_INTERNAL,
                                                ORTE_DB_ARCH, &opal_local_arch, OPAL_UINT32))) {
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

int orte_util_encode_nodemap(opal_byte_object_t *boptr, bool update)
{
    orte_node_t *node;
    int32_t i;
    int rc;
    opal_buffer_t buf;
    char *ptr, *nodename;
    orte_job_t *daemons;
    orte_proc_t *dmn;
    uint8_t flag;

    OPAL_OUTPUT_VERBOSE((2, orte_nidmap_output,
                         "%s orte:util:encode_nidmap",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    /* if the daemon job has not been updated, then there is
     * nothing to send
     */
    daemons = orte_get_job_data_object(ORTE_PROC_MY_NAME->jobid);
    if (update && !daemons->updated) {
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

    /* flag if coprocessors were detected */
    if (orte_coprocessors_detected) {
        flag = 1;
    } else {
        flag = 0;
    }
    if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &flag, 1, OPAL_UINT8))) {
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
        /* pack the name of the node */
        if (!orte_keep_fqdn_hostnames) {
            nodename = strdup(node->name);
            /* if the nodename is an IP address, do not mess with it! */
            if (!opal_net_isaddr(nodename)) {
                /* not an IP address */
                if (NULL != (ptr = strchr(nodename, '.'))) {
                    *ptr = '\0';
                }
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
        /* if requested, pack any aliases */
        if (orte_retain_aliases) {
            uint8_t naliases, ni;
            naliases = opal_argv_count(node->alias);
            if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &naliases, 1, OPAL_UINT8))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
            for (ni=0; ni < naliases; ni++) {
                if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &node->alias[ni], 1, OPAL_STRING))) {
                    ORTE_ERROR_LOG(rc);
                    return rc;
                }
            }
        }

        /* pack the oversubscribed flag */
        if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &node->oversubscribed, 1, OPAL_UINT8))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* if coprocessors were detected, send the hostid for this node */
        if (orte_coprocessors_detected) {
            if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &node->hostid, 1, ORTE_VPID))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
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
int orte_util_decode_nodemap(opal_byte_object_t *bo)
{
    int n;
    orte_vpid_t num_daemons;
    orte_process_name_t daemon;
    opal_buffer_t buf;
    int rc=ORTE_SUCCESS;
    uint8_t oversub;
    char *nodename;
    orte_vpid_t hostid;

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

    /* see if coprocessors were detected */
    n=1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(&buf, &oversub, &n, OPAL_UINT8))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    if (0 == oversub) {
        orte_coprocessors_detected = false;
    } else {
        orte_coprocessors_detected = true;
    }

    /* set the daemon jobid */
    daemon.jobid = ORTE_DAEMON_JOBID(ORTE_PROC_MY_NAME->jobid);

    n=1;
    while (OPAL_SUCCESS == (rc = opal_dss.unpack(&buf, &daemon.vpid, &n, ORTE_VPID))) {
        /* unpack and store the node's name */
        n=1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(&buf, &nodename, &n, OPAL_STRING))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        /* we only need the hostname for our own error messages, so mark it as internal */
        if (ORTE_SUCCESS != (rc = opal_db.store((opal_identifier_t*)&daemon, OPAL_SCOPE_INTERNAL,
                                                ORTE_DB_HOSTNAME, nodename, OPAL_STRING))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        /* now store a direct reference so we can quickly lookup the daemon from a hostname */
        opal_output_verbose(2, orte_nidmap_output,
                            "%s storing nodename %s for daemon %s",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                            nodename, ORTE_VPID_PRINT(daemon.vpid));
        if (ORTE_SUCCESS != (rc = opal_db.store((opal_identifier_t*)ORTE_NAME_WILDCARD, OPAL_SCOPE_INTERNAL,
                                                nodename, &daemon.vpid, OPAL_UINT32))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        OPAL_OUTPUT_VERBOSE((2, orte_nidmap_output,
                             "%s orte:util:decode:nidmap daemon %s node %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_VPID_PRINT(daemon.vpid), nodename));

        /* if this is my daemon, then store the data for me too */
        if (daemon.vpid == ORTE_PROC_MY_DAEMON->vpid) {
            if (ORTE_SUCCESS != (rc = opal_db.store((opal_identifier_t*)ORTE_PROC_MY_NAME, OPAL_SCOPE_NON_PEER,
                                                    ORTE_DB_HOSTNAME, nodename, OPAL_STRING))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
            /* we may need our daemon vpid to be shared with non-peers */
            if (ORTE_SUCCESS != (rc = opal_db.store((opal_identifier_t*)ORTE_PROC_MY_NAME, OPAL_SCOPE_NON_PEER,
                                                    ORTE_DB_DAEMON_VPID, &daemon.vpid, OPAL_UINT32))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
        }

        /* if requested, unpack any aliases */
        if (orte_retain_aliases) {
            char *alias;
            uint8_t naliases, ni;
            n=1;
            if (ORTE_SUCCESS != (rc = opal_dss.unpack(&buf, &naliases, &n, OPAL_UINT8))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
            for (ni=0; ni < naliases; ni++) {
                n=1;
                if (ORTE_SUCCESS != (rc = opal_dss.unpack(&buf, &alias, &n, OPAL_STRING))) {
                    ORTE_ERROR_LOG(rc);
                    return rc;
                }
                /* store a cross-reference to the daemon for this nodename */
                opal_output_verbose(2, orte_nidmap_output,
                                    "%s storing alias %s for daemon %s",
                                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                    alias, ORTE_VPID_PRINT(daemon.vpid));
                if (ORTE_SUCCESS != (rc = opal_db.store((opal_identifier_t*)ORTE_NAME_WILDCARD, OPAL_SCOPE_INTERNAL,
                                                        alias, &daemon.vpid, OPAL_UINT32))) {
                    ORTE_ERROR_LOG(rc);
                    return rc;
                }
                free(alias);
            }
        }

        /* unpack and discard the oversubscribed flag - procs don't need it */
        n=1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(&buf, &oversub, &n, OPAL_UINT8))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* if coprocessors were detected, unpack the hostid for the node - this
         * value is associate with this daemon, not with any application process
         */
        if (orte_coprocessors_detected) {
            n=1;
            if (ORTE_SUCCESS != (rc = opal_dss.unpack(&buf, &hostid, &n, ORTE_VPID))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
            if (ORTE_SUCCESS != (rc = opal_db.store((opal_identifier_t*)&daemon, OPAL_SCOPE_NON_PEER,
                                                    ORTE_DB_HOSTID, &hostid, OPAL_UINT32))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
            /* if this is my daemon, then store it as my hostid as well */
            if (daemon.vpid == ORTE_PROC_MY_DAEMON->vpid) {
                if (ORTE_SUCCESS != (rc = opal_db.store((opal_identifier_t*)ORTE_PROC_MY_NAME, OPAL_SCOPE_NON_PEER,
                                                        ORTE_DB_HOSTID, &hostid, OPAL_UINT32))) {
                    ORTE_ERROR_LOG(rc);
                    return rc;
                }
                /* and record it */
                orte_process_info.my_hostid = hostid;
            }
        }
        free (nodename);
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
    orte_node_t *node;
    opal_buffer_t buf;
    int rc=ORTE_SUCCESS;
    uint8_t oversub;
    char *name;
    orte_job_t *daemons;
    orte_proc_t *dptr;
    orte_vpid_t num_daemons;

    OPAL_OUTPUT_VERBOSE((1, orte_nidmap_output,
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

    /* see if coprocessors were detected */
    n=1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(&buf, &oversub, &n, OPAL_UINT8))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    if (0 == oversub) {
        orte_coprocessors_detected = false;
    } else {
        orte_coprocessors_detected = true;
    }

    /* transfer the data to the nodes */
    daemons = orte_get_job_data_object(ORTE_PROC_MY_NAME->jobid);
    daemons->num_procs = num_daemons;
    n=1;
    while (OPAL_SUCCESS == (rc = opal_dss.unpack(&buf, &vpid, &n, ORTE_VPID))) {
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
        /* if requested, unpack any aliases */
        if (orte_retain_aliases) {
            char *alias;
            uint8_t naliases, ni;
            n=1;
            if (ORTE_SUCCESS != (rc = opal_dss.unpack(&buf, &naliases, &n, OPAL_UINT8))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
            for (ni=0; ni < naliases; ni++) {
                n=1;
                if (ORTE_SUCCESS != (rc = opal_dss.unpack(&buf, &alias, &n, OPAL_STRING))) {
                    ORTE_ERROR_LOG(rc);
                    return rc;
                }
                opal_argv_append_nosize(&node->alias, alias);
                free(alias);
            }
        }
        /* unpack the oversubscribed flag */
        n=1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(&buf, &oversub, &n, OPAL_UINT8))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
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
        dptr->node = node;
        if (0 == oversub) {
            node->oversubscribed = false;
        } else {
            node->oversubscribed = true;
        }

        /* if coprocessors were detected, unpack the hostid */
        if (orte_coprocessors_detected) {
            n=1;
            if (ORTE_SUCCESS != (rc = opal_dss.unpack(&buf, &node->hostid, &n, ORTE_VPID))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
        }

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
    
    /* update the global nidmap object for sending to
     * application procs
     */
    if (NULL != orte_nidmap.bytes) {
        free(orte_nidmap.bytes);
        orte_nidmap.bytes = NULL;
    }
    if (ORTE_SUCCESS != (rc = orte_util_encode_nodemap(&orte_nidmap, false))) {
        ORTE_ERROR_LOG(rc);
    }

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

int orte_util_encode_pidmap(opal_byte_object_t *boptr, bool update)
{
    orte_proc_t *proc;
    opal_buffer_t buf;
    int i, j, rc = ORTE_SUCCESS;
    orte_job_t *jdata;
    bool include_all;
    uint8_t flag;

    /* setup the working buffer */
    OBJ_CONSTRUCT(&buf, opal_buffer_t);
    
    /* check the daemon job to see if it has changed - perhaps
     * new daemons were added as the result of a comm_spawn
     */
    jdata = orte_get_job_data_object(ORTE_PROC_MY_NAME->jobid);
    /* if it did change, then the pidmap will be going
     * to new daemons - so we need to include everything.
     * also include everything if we were asked to do so
     */
    if (jdata->updated || !update) {
        include_all = true;
    } else {
        include_all = false;
    }

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
        /* if we want an update version and there is nothing to update, ignore it */
        if (!include_all && !jdata->updated) {
            continue;
        }
        /* flag that we included it so we don't do so again */
        jdata->updated = false;
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
        /* pack the offset */
        if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &jdata->offset, 1, ORTE_VPID))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup_and_return;
        }
        /* cycle thru the job's procs, including only those that have
         * been updated so we minimize the amount of info being sent
         */
        for (i=0; i < jdata->procs->size; i++) {
            if (NULL == (proc = (orte_proc_t *) opal_pointer_array_get_item(jdata->procs, i))) {
                continue;
            }
            if (!proc->updated) {
                continue;
            }
            if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &proc->name.vpid, 1, ORTE_VPID))) {
                ORTE_ERROR_LOG(rc);
                goto cleanup_and_return;
            }

            if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &(proc->node->daemon->name.vpid), 1, ORTE_VPID))) {
                ORTE_ERROR_LOG(rc);
                goto cleanup_and_return;
            }
            if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &proc->local_rank, 1, ORTE_LOCAL_RANK))) {
                ORTE_ERROR_LOG(rc);
                goto cleanup_and_return;
            }
            if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &proc->node_rank, 1, ORTE_NODE_RANK))) {
                ORTE_ERROR_LOG(rc);
                goto cleanup_and_return;
            }
#if OPAL_HAVE_HWLOC
            if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &proc->cpu_bitmap, 1, OPAL_STRING))) {
                ORTE_ERROR_LOG(rc);
                goto cleanup_and_return;
            }
#endif
            if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &proc->state, 1, ORTE_PROC_STATE))) {
                ORTE_ERROR_LOG(rc);
                goto cleanup_and_return;
            }
            if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &proc->app_idx, 1, ORTE_APP_IDX))) {
                ORTE_ERROR_LOG(rc);
                goto cleanup_and_return;
            }
            if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &proc->do_not_barrier, 1, OPAL_BOOL))) {
                ORTE_ERROR_LOG(rc);
                goto cleanup_and_return;
            }
            if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &proc->restarts, 1, OPAL_INT32))) {
                ORTE_ERROR_LOG(rc);
                goto cleanup_and_return;
            }
        }
        /* pack an invalid vpid to flag the end of this job data */
        if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &ORTE_NAME_INVALID->vpid, 1, ORTE_VPID))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup_and_return;
        }
        /* if there is a file map, then include it */
        if (NULL != jdata->file_maps) {
            flag = 1;
            if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &flag, 1, OPAL_UINT8))) {
                ORTE_ERROR_LOG(rc);
                goto cleanup_and_return;
            }
            if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &jdata->file_maps, 1, OPAL_BUFFER))) {
                ORTE_ERROR_LOG(rc);
                goto cleanup_and_return;
            }
        } else {
            flag = 0;
            if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &flag, 1, OPAL_UINT8))) {
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
int orte_util_decode_pidmap(opal_byte_object_t *bo)
{
    orte_vpid_t num_procs, offset, hostid, *vptr, global_rank;
    orte_local_rank_t local_rank;
    orte_node_rank_t node_rank;
#if OPAL_HAVE_HWLOC
    char *cpu_bitmap;
#endif
    orte_std_cntr_t n;
    opal_buffer_t buf;
    int rc;
    orte_proc_state_t state;
    orte_app_idx_t app_idx;
    int32_t restarts;
    orte_process_name_t proc, dmn;
    char *hostname;
    uint8_t flag;
    opal_buffer_t *bptr;
    bool barrier;

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
        if (ORTE_SUCCESS != (rc = opal_db.store((opal_identifier_t*)&proc, OPAL_SCOPE_INTERNAL,
                                                ORTE_DB_NPROCS, &num_procs, OPAL_UINT32))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
        /* unpack and store the offset */
        n=1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(&buf, &offset, &n, ORTE_VPID))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
        /* only of possible use to ourselves */
        if (ORTE_SUCCESS != (rc = opal_db.store((opal_identifier_t*)&proc, OPAL_SCOPE_INTERNAL,
                                                ORTE_DB_NPROC_OFFSET, &offset, OPAL_UINT32))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
        /* cycle thru the data until we hit an INVALID vpid indicating
         * all data for this job has been read
         */
        n=1;
        while (OPAL_SUCCESS == (rc = opal_dss.unpack(&buf, &proc.vpid, &n, ORTE_VPID))) {
            if (ORTE_VPID_INVALID == proc.vpid) {
                break;
            }
            n=1;
            if (ORTE_SUCCESS != (rc = opal_dss.unpack(&buf, &dmn.vpid, &n, ORTE_VPID))) {
                ORTE_ERROR_LOG(rc);
                goto cleanup;
            }
            n=1;
            if (ORTE_SUCCESS != (rc = opal_dss.unpack(&buf, &local_rank, &n, ORTE_LOCAL_RANK))) {
                ORTE_ERROR_LOG(rc);
                goto cleanup;
            }
            n=1;
            if (ORTE_SUCCESS != (rc = opal_dss.unpack(&buf, &node_rank, &n, ORTE_NODE_RANK))) {
                ORTE_ERROR_LOG(rc);
                goto cleanup;
            }
#if OPAL_HAVE_HWLOC
            n=1;
            if (ORTE_SUCCESS != (rc = opal_dss.unpack(&buf, &cpu_bitmap, &n, OPAL_STRING))) {
                ORTE_ERROR_LOG(rc);
                goto cleanup;
            }
#endif
            if (proc.jobid == ORTE_PROC_MY_NAME->jobid &&
                proc.vpid == ORTE_PROC_MY_NAME->vpid) {
                /* set mine */
                orte_process_info.my_local_rank = local_rank;
                orte_process_info.my_node_rank = node_rank;
                /* if we are the local leader (i.e., local_rank=0), then record it */
                if (0 == local_rank) {
                    if (ORTE_SUCCESS != (rc = opal_db.store((opal_identifier_t*)ORTE_PROC_MY_NAME, OPAL_SCOPE_INTERNAL,
                                                            OPAL_DB_LOCALLDR, (opal_identifier_t*)&proc, OPAL_ID_T))) {
                        ORTE_ERROR_LOG(rc);
                        goto cleanup;
                    }
                }
#if OPAL_HAVE_HWLOC
                if (NULL != cpu_bitmap) {
                    orte_process_info.cpuset = strdup(cpu_bitmap);
                }
#endif
            } else if (proc.jobid == ORTE_PROC_MY_NAME->jobid &&
                       dmn.vpid == ORTE_PROC_MY_DAEMON->vpid) {
                /* if we share a daemon, then add to my local peers */
                orte_process_info.num_local_peers++;
                /* if this is the local leader (i.e., local_rank=0), then record it */
                if (0 == local_rank) {
                    if (ORTE_SUCCESS != (rc = opal_db.store((opal_identifier_t*)ORTE_PROC_MY_NAME, OPAL_SCOPE_INTERNAL,
                                                            OPAL_DB_LOCALLDR, (opal_identifier_t*)&proc, OPAL_ID_T))) {
                        ORTE_ERROR_LOG(rc);
                        goto cleanup;
                    }
                }
            }
            /* apps don't need the rest of the data in the buffer for this proc,
             * but we have to unpack it anyway to stay in sync
             */
            n=1;
            if (OPAL_SUCCESS != (rc = opal_dss.unpack(&buf, &state, &n, ORTE_PROC_STATE))) {
                ORTE_ERROR_LOG(rc);
                goto cleanup;
            }
            n=1;
            if (OPAL_SUCCESS != (rc = opal_dss.unpack(&buf, &app_idx, &n, ORTE_APP_IDX))) {
                ORTE_ERROR_LOG(rc);
                goto cleanup;
            }
            n=1;
            if (OPAL_SUCCESS != (rc = opal_dss.unpack(&buf, &barrier, &n, OPAL_BOOL))) {
                ORTE_ERROR_LOG(rc);
                goto cleanup;
            }
            n=1;
            if (OPAL_SUCCESS != (rc = opal_dss.unpack(&buf, &restarts, &n, OPAL_INT32))) {
                ORTE_ERROR_LOG(rc);
                goto cleanup;
            }
            /* store the values in the database - again, these are for our own internal use */
            if (ORTE_SUCCESS != (rc = opal_db.store((opal_identifier_t*)&proc, OPAL_SCOPE_INTERNAL,
                                                    ORTE_DB_LOCALRANK, &local_rank, ORTE_LOCAL_RANK))) {
                ORTE_ERROR_LOG(rc);
                goto cleanup;
            }
            if (ORTE_SUCCESS != (rc = opal_db.store((opal_identifier_t*)&proc, OPAL_SCOPE_INTERNAL,
                                                    ORTE_DB_NODERANK, &node_rank, ORTE_NODE_RANK))) {
                ORTE_ERROR_LOG(rc);
                goto cleanup;
            }
#if OPAL_HAVE_HWLOC
            if (ORTE_SUCCESS != (rc = opal_db.store((opal_identifier_t*)&proc, OPAL_SCOPE_INTERNAL,
                                                    OPAL_DB_CPUSET, cpu_bitmap, OPAL_STRING))) {
                ORTE_ERROR_LOG(rc);
                goto cleanup;
            }
            if (NULL != cpu_bitmap) {
                free(cpu_bitmap);
            }
#endif
            /* we don't need to store the rest of the values
             * for ourself in the database
             * as we already did so during startup
             */
            if (proc.jobid != ORTE_PROC_MY_NAME->jobid ||
                proc.vpid != ORTE_PROC_MY_NAME->vpid) {
                /* store the data for this proc - the location of a proc is something
                 * we would potentially need to share with a non-peer
                 */
                if (ORTE_SUCCESS != (rc = opal_db.store((opal_identifier_t*)&proc, OPAL_SCOPE_NON_PEER,
                                                        ORTE_DB_DAEMON_VPID, &dmn.vpid, OPAL_UINT32))) {
                    ORTE_ERROR_LOG(rc);
                    goto cleanup;
                }
                /* in a singleton comm_spawn, we can be passed the name of a daemon, which
                 * means that the proc's parent is invalid - check and avoid the rest of
                 * this logic in that case */
                if (ORTE_VPID_INVALID != dmn.vpid) {
                    /* if coprocessors were detected, lookup and store the hostid for this proc */
                    if (orte_coprocessors_detected) {
                        /* lookup the hostid for this daemon */
                        vptr = &hostid;
                        if (ORTE_SUCCESS != (rc = opal_db.fetch((opal_identifier_t*)&dmn, ORTE_DB_HOSTID,
                                                                (void**)&vptr, OPAL_UINT32))) {
                            ORTE_ERROR_LOG(rc);
                            goto cleanup;
                        }
                        OPAL_OUTPUT_VERBOSE((2, orte_nidmap_output,
                                             "%s FOUND HOSTID %s FOR DAEMON %s",
                                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                             ORTE_VPID_PRINT(hostid), ORTE_VPID_PRINT(dmn.vpid)));
                        /* store it as hostid for this proc */
                        if (ORTE_SUCCESS != (rc = opal_db.store((opal_identifier_t*)&proc, OPAL_SCOPE_NON_PEER,
                                                                ORTE_DB_HOSTID, &hostid, OPAL_UINT32))) {
                            ORTE_ERROR_LOG(rc);
                            goto cleanup;
                        }
                    }
                    /* lookup and store the hostname for this proc */
                    if (ORTE_SUCCESS != (rc = opal_db.fetch_pointer((opal_identifier_t*)&dmn, ORTE_DB_HOSTNAME,
                                                                    (void**)&hostname, OPAL_STRING))) {
                        ORTE_ERROR_LOG(rc);
                        goto cleanup;
                    }
                    if (ORTE_SUCCESS != (rc = opal_db.store((opal_identifier_t*)&proc, OPAL_SCOPE_NON_PEER,
                                                            ORTE_DB_HOSTNAME, hostname, OPAL_STRING))) {
                        ORTE_ERROR_LOG(rc);
                        goto cleanup;
                    }
                }
                /* store this procs global rank - only used by us */
                global_rank = proc.vpid + offset;
                if (ORTE_SUCCESS != (rc = opal_db.store((opal_identifier_t*)&proc, OPAL_SCOPE_INTERNAL,
                                                        ORTE_DB_GLOBAL_RANK, &global_rank, OPAL_UINT32))) {
                    ORTE_ERROR_LOG(rc);
                    goto cleanup;
                }
            } else {
                /* update our own global rank - this is something we will need
                 * to share with non-peers
                 */
                global_rank = proc.vpid + offset;
                if (ORTE_SUCCESS != (rc = opal_db.store((opal_identifier_t*)&proc, OPAL_SCOPE_NON_PEER,
                                                        ORTE_DB_GLOBAL_RANK, &global_rank, OPAL_UINT32))) {
                    ORTE_ERROR_LOG(rc);
                    goto cleanup;
                }
            }
        }
        /* see if there is a file map */
        n=1;
        if (OPAL_SUCCESS != (rc = opal_dss.unpack(&buf, &flag, &n, OPAL_UINT8))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
        if (0 != flag) {
            /* unpack it and discard */
            n=1;
            if (OPAL_SUCCESS != (rc = opal_dss.unpack(&buf, &bptr, &n, OPAL_BUFFER))) {
                ORTE_ERROR_LOG(rc);
                goto cleanup;
            }
            OBJ_RELEASE(bptr);
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

static void fm_release(void *cbdata)
{
    opal_buffer_t *bptr = (opal_buffer_t*)cbdata;

    OBJ_RELEASE(bptr);
}

int orte_util_decode_daemon_pidmap(opal_byte_object_t *bo)
{
    orte_jobid_t jobid;
    orte_vpid_t vpid, num_procs, dmn;
    orte_local_rank_t local_rank;
    orte_node_rank_t node_rank;
#if OPAL_HAVE_HWLOC
    char *cpu_bitmap;
#endif
    orte_std_cntr_t n;
    opal_buffer_t buf;
    int rc, j, k;
    orte_job_t *jdata, *daemons;
    orte_proc_t *proc, *pptr;
    orte_node_t *node, *nptr;
    orte_proc_state_t state;
    orte_app_idx_t app_idx;
    int32_t restarts;
    orte_job_map_t *map;
    bool found;
    uint8_t flag;
    opal_buffer_t *bptr;
    bool barrier;

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
        
        /* setup the map */
        map = jdata->map;
        if (NULL == map) {
            jdata->map = OBJ_NEW(orte_job_map_t);
            map = jdata->map;
        }

        /* unpack the number of procs */
        n=1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(&buf, &num_procs, &n, ORTE_VPID))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
        jdata->num_procs = num_procs;

        /* unpack the offset */
        n=1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(&buf, &num_procs, &n, ORTE_VPID))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
        jdata->offset = num_procs;

        /* cycle thru the data until we hit an INVALID vpid indicating
         * all data for this job has been read
         */
        n=1;
        while (OPAL_SUCCESS == (rc = opal_dss.unpack(&buf, &vpid, &n, ORTE_VPID))) {
            if (ORTE_VPID_INVALID == vpid) {
                break;
            }
            n=1;
            if (ORTE_SUCCESS != (rc = opal_dss.unpack(&buf, &dmn, &n, ORTE_VPID))) {
                ORTE_ERROR_LOG(rc);
                goto cleanup;
            }
            n=1;
            if (ORTE_SUCCESS != (rc = opal_dss.unpack(&buf, &local_rank, &n, ORTE_LOCAL_RANK))) {
                ORTE_ERROR_LOG(rc);
                goto cleanup;
            }
            n=1;
            if (ORTE_SUCCESS != (rc = opal_dss.unpack(&buf, &node_rank, &n, ORTE_NODE_RANK))) {
                ORTE_ERROR_LOG(rc);
                goto cleanup;
            }
#if OPAL_HAVE_HWLOC
            n=1;
            if (ORTE_SUCCESS != (rc = opal_dss.unpack(&buf, &cpu_bitmap, &n, OPAL_STRING))) {
                ORTE_ERROR_LOG(rc);
                goto cleanup;
            }
#endif
            n=1;
            if (OPAL_SUCCESS != (rc = opal_dss.unpack(&buf, &state, &n, ORTE_PROC_STATE))) {
                ORTE_ERROR_LOG(rc);
                goto cleanup;
            }
            n=1;
            if (OPAL_SUCCESS != (rc = opal_dss.unpack(&buf, &app_idx, &n, ORTE_APP_IDX))) {
                ORTE_ERROR_LOG(rc);
                goto cleanup;
            }
            n=1;
            if (OPAL_SUCCESS != (rc = opal_dss.unpack(&buf, &barrier, &n, OPAL_BOOL))) {
                ORTE_ERROR_LOG(rc);
                goto cleanup;
            }
            n=1;
            if (OPAL_SUCCESS != (rc = opal_dss.unpack(&buf, &restarts, &n, OPAL_INT32))) {
                ORTE_ERROR_LOG(rc);
                goto cleanup;
            }
            /* store the data for this proc */
            if (NULL == (proc = (orte_proc_t*)opal_pointer_array_get_item(jdata->procs, vpid))) {
                proc = OBJ_NEW(orte_proc_t);
                proc->name.jobid = jdata->jobid;
                proc->name.vpid = vpid;
                opal_pointer_array_set_item(jdata->procs, vpid, proc);
            }
            /* lookup the node - should always be present */
            if (NULL == (node = (orte_node_t*)opal_pointer_array_get_item(orte_node_pool, dmn))) {
                /* this should never happen, but protect ourselves anyway */
                node = OBJ_NEW(orte_node_t);
                /* get the daemon */
                if (NULL == (pptr = (orte_proc_t*)opal_pointer_array_get_item(daemons->procs, dmn))) {
                    pptr = OBJ_NEW(orte_proc_t);
                    pptr->name.jobid = ORTE_PROC_MY_NAME->jobid;
                    pptr->name.vpid = dmn;
                    opal_pointer_array_set_item(daemons->procs, dmn, pptr);
                }
                node->daemon = pptr;
                opal_pointer_array_set_item(orte_node_pool, dmn, node);
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
            proc->local_rank = local_rank;
            proc->node_rank = node_rank;
            proc->app_idx = app_idx;
            proc->do_not_barrier = barrier;
            proc->restarts = restarts;
            proc->state = state;
#if OPAL_HAVE_HWLOC
            proc->cpu_bitmap = cpu_bitmap;
#endif
        }
        /* see if we have a file map for this job */
        n=1;
        if (OPAL_SUCCESS != (rc = opal_dss.unpack(&buf, &flag, &n, OPAL_UINT8))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
        if (0 != flag) {
            /* yep - retrieve and load it */
            n=1;
            if (OPAL_SUCCESS != (rc = opal_dss.unpack(&buf, &bptr, &n, OPAL_BUFFER))) {
                ORTE_ERROR_LOG(rc);
                goto cleanup;
            }
            if (NULL != orte_dfs.load_file_maps) {
                orte_dfs.load_file_maps(jdata->jobid, bptr, fm_release, bptr);
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
    
    /* update our global pidmap object for sending
     * to procs
     */
    if (NULL != orte_pidmap.bytes) {
        free(orte_pidmap.bytes);
    }
    if (ORTE_SUCCESS != (rc = orte_util_encode_pidmap(&orte_pidmap, false))) {
        ORTE_ERROR_LOG(rc);
    }

 cleanup:
    OBJ_DESTRUCT(&buf);
    return rc;
}
