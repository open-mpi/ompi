/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
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
#include "orte_config.h"
#include "orte/types.h"
#include "orte/constants.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "opal/dss/dss.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/util/output.h"
#include "orte/util/proc_info.h"
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"

#include "orte/util/nidmap.h"

int orte_util_encode_nodemap(opal_byte_object_t *boptr)
{
    orte_vpid_t *vpids;
    orte_node_t **nodes;
    char prefix[ORTE_MAX_NODE_PREFIX], *tmp;
    int32_t i, len, firstnode, lastnode, nodenum, num_nodes;
    uint8_t command = ORTE_CONTIG_NODE_CMD;
    uint8_t num_digs;
    uint8_t incdec;
    int rc;
    char *nodename;
    opal_buffer_t buf;
    int step;
#if OMPI_ENABLE_HETEROGENEOUS_SUPPORT
    int32_t *arch;
#endif
    
    /* setup a buffer for tmp use */
    OBJ_CONSTRUCT(&buf, opal_buffer_t);

    /* determine the number of nodes in the global node array */
    num_nodes = 0;
    nodes = (orte_node_t**)orte_node_pool->addr;
    while (NULL != nodes[num_nodes]) {
        ++num_nodes;
    }
    /* pack number of nodes */
    opal_dss.pack(&buf, &num_nodes, 1, OPAL_INT32);
    
    /* pack the HNP's node name - don't mess with
     * trying to encode it - it could be different
     */
    /* if we are not keeping FQDN hostnames, abbreviate
     * the nodename as required
     */
    if (!orte_keep_fqdn_hostnames) {
        char *ptr;
        nodename = strdup(nodes[0]->name);
        if (NULL != (ptr = strchr(nodename, '.'))) {
            *ptr = '\0';
        }
        opal_dss.pack(&buf, &nodename, 1, OPAL_STRING);
        free(nodename);
    } else {
        opal_dss.pack(&buf, &nodes[0]->name, 1, OPAL_STRING);
    }
    
    /* see if the cluster is configured with contiguous
     * node names and we have more than the HNP
     */
    if (orte_contiguous_nodes < num_nodes) {
        /* discover the prefix - find first non-alpha character */
        len = strlen(nodes[1]->name);
        memset(prefix, 0, ORTE_MAX_NODE_PREFIX);
        prefix[0] = nodes[1]->name[0];  /* must start with alpha */
        for (i=1; i < len; i++) {
            if (!isalpha(nodes[1]->name[i])) {
                /* found a non-alpha char */
                if (!isdigit(nodes[1]->name[i])) {
                    /* if it is anything but a digit,
                     * then that's not good
                     */
                    orte_output(0, "%s encode:nidmap Nodename pattern is nonstandard",
                                ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
                    return ORTE_ERROR;
                }
                /* okay, this defines end of the prefix.
                 * convert rest of name to an offset
                 */
                firstnode = strtol(&(nodes[1]->name[i]), NULL, 10);
                /* figure out how many digits are in the index */
                for (num_digs=0; isdigit(nodes[1]->name[i+num_digs]); num_digs++);
                goto PACK;
            }
            prefix[i] = nodes[1]->name[i];
        }

    PACK:
       /* begin encoding rest of map by indicating that this will
        * be a contiguous node map
        */
        opal_dss.pack(&buf, &command, 1, OPAL_UINT8);
        
        /* pack the prefix */
        tmp = &prefix[0];
        opal_dss.pack(&buf, &tmp, 1, OPAL_STRING);
        len = strlen(prefix);
        
        /* pack the number of digits in the index */
        opal_dss.pack(&buf, &num_digs, 1, OPAL_UINT8);
        
        /* and the starting offset */
        opal_dss.pack(&buf, &firstnode, 1, OPAL_INT32);
        
        ORTE_OUTPUT_VERBOSE((2, orte_debug_output,
                             "%s encode:nidmap:contig_nodes prefix %s num_digits %d offset %d",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), prefix, num_digs, firstnode));
        
        lastnode = strtol(&(nodes[2]->name[i]), NULL, 10);
        if ((lastnode - firstnode) < 0) {
            /* we are decrementing */
            incdec = 0;
            opal_dss.pack(&buf, &incdec, 1, OPAL_INT8);
        } else {
            /* we are incrementing */
            incdec = 1;
            opal_dss.pack(&buf, &incdec, 1, OPAL_INT8);
        }
        
        lastnode = firstnode;
        /* cycle through the nodes - pack the starting offset
         * and total number of nodes in each contiguous range
         */
        for (i=2; i < num_nodes; i++) {
            nodenum = strtol(&(nodes[i]->name[len]), NULL, 10);
            step = nodenum -lastnode;
            if (step < 0) {
                /* we are decrementing */
                step = lastnode - nodenum;
            }
            if (step > 1) {
                /* have a break - indicate end of range */
                opal_dss.pack(&buf, &lastnode, 1, OPAL_INT32);
                /* indicate start of new range */
                opal_dss.pack(&buf, &nodenum, 1, OPAL_INT32);
                ORTE_OUTPUT_VERBOSE((2, orte_debug_output,
                                     "%s encode:nidmap:contig_nodes end range %d start next range %d",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), lastnode, nodenum));
            }
            lastnode = nodenum;
        }
        /* pack end of range */
        opal_dss.pack(&buf, &lastnode, 1, OPAL_INT32);
        ORTE_OUTPUT_VERBOSE((2, orte_debug_output,
                             "%s encode:nidmap:contig_nodes end range %d",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), lastnode));
        /* pack flag end of ranges */
        lastnode = -1;
        opal_dss.pack(&buf, &lastnode, 1, OPAL_INT32);
   } else {
        /* if the nodes aren't contiguous, then we need
         * to simply pack every nodename individually
         */
        ORTE_OUTPUT_VERBOSE((2, orte_debug_output,
                             "%s encode:nidmap non_contig_nodes - packing all names",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        /* indicate that this will not be a contiguous node map */
        command = ORTE_NON_CONTIG_NODE_CMD;
        opal_dss.pack(&buf, &command, 1, OPAL_UINT8);
        for (i=1; i < num_nodes; i++) {
            if (!orte_keep_fqdn_hostnames) {
                char *ptr;
                nodename = strdup(nodes[i]->name);
                if (NULL != (ptr = strchr(nodename, '.'))) {
                    *ptr = '\0';
                }
                if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &nodename, 1, OPAL_STRING))) {
                    ORTE_ERROR_LOG(rc);
                    return rc;
                }
                free(nodename);
            } else {
                if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &nodes[i]->name, 1, OPAL_STRING))) {
                    ORTE_ERROR_LOG(rc);
                    return rc;
                }
            }
        }
    }
    
    /* since the daemon vpids may not correspond to the node
     * index, we need to also pack the vpid array for all
     * daemons. This scenario can happen when the user is
     * employing a mapping algo that doesn't use all allocated
     * nodes, and sprinkles procs across them in some non-contig
     * manner. For example, use of the seq mapper where only
     * some nodes are used, and where the usage leaves "holes"
     * in the node array, will cause the daemon vpids to not
     * match their node array index
     */
    
    /* allocate space for the daemon vpids */
    vpids = (orte_vpid_t*)malloc(num_nodes * sizeof(orte_vpid_t));
    for (i=0; i < num_nodes; i++) {
        if (NULL == nodes[i]->daemon) {
            /* some nodes may not have daemons on them */
            vpids[i] = ORTE_VPID_INVALID;
            continue;
        }
        vpids[i] = nodes[i]->daemon->name.vpid;
    }
    opal_dss.pack(&buf, vpids, num_nodes, ORTE_VPID);
    free(vpids);
    
#if OMPI_ENABLE_HETEROGENEOUS_SUPPORT
    /* allocate space for the node arch */
    arch = (int32_t*)malloc(num_nodes * 4);
    /* transfer the data from the nodes */
    for (i=0; i < num_nodes; i++) {
        arch[i] = nodes[i]->arch;
    }
    /* pack the values */
    opal_dss.pack(&buf, arch, num_nodes, OPAL_INT32);
    free(arch);
#endif
    
    /* transfer the payload to the byte object */
    opal_dss.unload(&buf, (void**)&boptr->bytes, &boptr->size);
    OBJ_DESTRUCT(&buf);
    
    return ORTE_SUCCESS;
}

int orte_util_decode_nodemap(opal_byte_object_t *bo, opal_pointer_array_t *nodes)
{
    int n, loc, k, diglen, namelen;
    char *prefix, digits[10];
    int32_t num_nodes, lastnode, endrange, i, num_daemons;
    orte_nid_t *node;
    orte_vpid_t *vpids;
    uint8_t command, num_digs;
    orte_nid_t **nd;
    uint8_t incdec;
    int32_t index, step;
#if OMPI_ENABLE_HETEROGENEOUS_SUPPORT
    int32_t *arch;
#endif
    opal_buffer_t buf;

    ORTE_OUTPUT_VERBOSE((2, orte_debug_output,
                         "%s decode:nidmap decoding nodemap",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    /* xfer the byte object to a buffer for unpacking */
    /* load it into a buffer */
    OBJ_CONSTRUCT(&buf, opal_buffer_t);
    opal_dss.load(&buf, bo->bytes, bo->size);
    
    /* unpack number of nodes */
    n=1;
    opal_dss.unpack(&buf, &num_nodes, &n, OPAL_INT32);
 
    ORTE_OUTPUT_VERBOSE((2, orte_debug_output,
                         "%s decode:nidmap decoding %d nodes with %d already loaded",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), num_nodes, nodes->lowest_free));
    
    /* is this greater than the number of entries in nodes? if so, then
     * we will update the node array. if not, then we can return now
     */
    if (num_nodes <= nodes->lowest_free) {
        /* nothing more to do */
        return ORTE_SUCCESS;
    }
    
    /* set the size of the nidmap storage so we minimize
     * realloc's
     */
    opal_pointer_array_set_size(nodes, num_nodes);
    
    /* create the struct for the HNP's node */
    node = (orte_nid_t*)malloc(sizeof(orte_nid_t));
    node->name = NULL;
    /* default the arch to our arch so that non-hetero
     * case will yield correct behavior
     */
    node->arch = orte_process_info.arch;
    opal_pointer_array_set_item(nodes, 0, node);
    
    /* unpack the name of the HNP's node */
    n=1;
    opal_dss.unpack(&buf, &(node->name), &n, OPAL_STRING);
    
    /* unpack flag to see if this is a contiguous node map or not */
    n=1;
    opal_dss.unpack(&buf, &command, &n, OPAL_UINT8);
    
    if (ORTE_CONTIG_NODE_CMD == command) {
        /* unpack the prefix */
        n=1;
        opal_dss.unpack(&buf, &prefix, &n, OPAL_STRING);
        
        /* the number of digits in the index */
        n=1;
        opal_dss.unpack(&buf, &num_digs, &n, OPAL_UINT8);
        
        /* and the starting offset */
        n=1;
        opal_dss.unpack(&buf, &lastnode, &n, OPAL_INT32);
        
        /* unpack increment/decrement flag */
        n=1;
        opal_dss.unpack(&buf, &incdec, &n, OPAL_INT8);

        /* unpack the end of the range */
        n=1;
        opal_dss.unpack(&buf, &endrange, &n, OPAL_INT32);

        /* setup loop params */
        if (0 == incdec) {
            endrange -= 1;
            step = -1;
        } else {
            endrange += 1;
            step = 1;
        }
        
        ORTE_OUTPUT_VERBOSE((2, orte_debug_output,
                             "%s decode:nidmap:contig_nodes prefix %s num_digits %d offset %d endrange %d",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), prefix, num_digs, lastnode, endrange));
        
        namelen = strlen(prefix) + num_digs + 1;
        /* cycle through the ranges */
        index = 1;
        while (1) {
            for (i=lastnode; i != endrange; i += step) {
                node = (orte_nid_t*)malloc(sizeof(orte_nid_t));
                /* allocate space for the nodename */
                node->name = (char*)malloc(namelen);
                memset(node->name, 0, namelen);
                loc = snprintf(node->name, namelen, "%s", prefix);
                diglen = num_digs - snprintf(digits, 10, "%d", i);
                for (k=0; k < diglen && loc < namelen; k++) {
                    node->name[loc] = '0';
                    loc++;
                }
                strncat(node->name, digits, num_digs);
                node->daemon = ORTE_VPID_INVALID;
                /* default the arch to our arch so that non-hetero
                 * case will yield correct behavior
                 */
                node->arch = orte_process_info.arch;
                opal_pointer_array_set_item(nodes, index, node);
                index++;
            }
            /* unpack start of new range */
            n=1;
            opal_dss.unpack(&buf, &lastnode, &n, OPAL_INT32);
            /* if that is -1, then it flags no more ranges */
            if (-1 == lastnode) {
                goto vpids;
            }
            n=1;
            opal_dss.unpack(&buf, &endrange, &n, OPAL_INT32);
            if (0 == incdec) {
                endrange -= 1;
            } else {
                endrange += 1;
            }
        }
    } else {
        /* not contiguous - just loop over nodes and
         * unpack the raw nodename
         */
        for (i=1; i < num_nodes; i++) {
            node = (orte_nid_t*)malloc(sizeof(orte_nid_t));
            node->name = NULL;
            node->daemon = ORTE_VPID_INVALID;
            /* default the arch to our arch so that non-hetero
             * case will yield correct behavior
             */
            node->arch = orte_process_info.arch;
            opal_pointer_array_set_item(nodes, i, node);
            
            /* unpack the node's name */
            n=1;
            opal_dss.unpack(&buf, &(node->name), &n, OPAL_STRING);
        }
    }
    
vpids:
    /* unpack the daemon vpids */
    vpids = (orte_vpid_t*)malloc(num_nodes * sizeof(orte_vpid_t));
    n=num_nodes;
    opal_dss.unpack(&buf, vpids, &n, ORTE_VPID);
    nd = (orte_nid_t**)nodes->addr;
    /* transfer the data to the nidmap, counting the number of
     * daemons in the system
     */
    num_daemons = 0;
    for (i=0; i < num_nodes; i++) {
        nd[i]->daemon = vpids[i];
        if (ORTE_VPID_INVALID != vpids[i]) {
            ++num_daemons;
        }
    }
    free(vpids);
    
    /* if we are a daemon or the HNP, update our num_procs */
    if (orte_process_info.hnp || orte_process_info.daemon) {
        orte_process_info.num_procs = num_daemons;
    }
    
    
#if OMPI_ENABLE_HETEROGENEOUS_SUPPORT
    /* allocate space for the node arch */
    arch = (int32_t*)malloc(num_nodes * 4);
    /* unpack the values */
    n=num_nodes;
    opal_dss.unpack(&buf, arch, &n, OPAL_INT32);
    /* transfer the data to the nodes */
    nd = (orte_nid_t**)nodes->addr;
    for (i=0; i < num_nodes; i++) {
        nd[i]->arch = arch[i];
    }
    free(arch);
#endif
 
    if (0 < orte_output_get_verbosity(orte_debug_output)) {
        nd = (orte_nid_t**)nodes->addr;
        for (i=0; i < num_nodes; i++) {
            orte_output(0, "%s node[%d].name %s daemon %s arch %0x",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), i,
                        (NULL == nd[i]) ? "NULL" : nd[i]->name,
                        ORTE_VPID_PRINT(nd[i]->daemon),
                        (NULL == nd[i]) ? 0 : nd[i]->arch);
        }
    }

    OBJ_DESTRUCT(&buf);
    return ORTE_SUCCESS;
}

int orte_util_encode_pidmap(orte_job_t *jdata, opal_byte_object_t *boptr)
{
    int32_t *nodes;
    orte_proc_t **procs;
    orte_vpid_t i;
    int8_t *tmp, flag;
    opal_buffer_t buf;

    /* setup the working buffer */
    OBJ_CONSTRUCT(&buf, opal_buffer_t);
    
    /* pack the number of procs */
    opal_dss.pack(&buf, &jdata->num_procs, 1, ORTE_VPID);
    
    /* allocate memory for the nodes */
    nodes = (int32_t*)malloc(jdata->num_procs * 4);
    
    /* transfer and pack the node info in one pack */
    procs = (orte_proc_t**)jdata->procs->addr;
    for (i=0; i < jdata->num_procs; i++) {
        nodes[i] = procs[i]->node->index;
    }
    opal_dss.pack(&buf, nodes, jdata->num_procs, OPAL_INT32);
    
    /* free node storage */
    free(nodes);
    
    /* allocate memory for the local_ranks */
    tmp = (int8_t*)malloc(jdata->num_procs);
    
   /* transfer and pack them in one pack */
    for (i=0; i < jdata->num_procs; i++) {
        tmp[i] = procs[i]->local_rank;
    }
    opal_dss.pack(&buf, tmp, jdata->num_procs, OPAL_UINT8);
    
    /* transfer and pack the node ranks in one pack */
    for (i=0; i < jdata->num_procs; i++) {
        tmp[i] = procs[i]->node_rank;
    }
    opal_dss.pack(&buf, tmp, jdata->num_procs, OPAL_UINT8);
    
    /* transfer and pack the app_idx in one pack */
    for (i=0; i < jdata->num_procs; i++) {
        tmp[i] = procs[i]->app_idx;
    }
    opal_dss.pack(&buf, tmp, jdata->num_procs, OPAL_INT8);

    /* free the storage */
    free(tmp);
    
    /* are there cpu_list strings? */
    if (jdata->map->cpu_lists) {
        flag = (int)true;
        opal_dss.pack(&buf, &flag, 1, OPAL_INT8);
        for (i=0; i < jdata->num_procs; i++) {
            opal_dss.pack(&buf, &procs[i]->slot_list, 1, OPAL_STRING);
        }
    } else {
        flag = (int)false;
        opal_dss.pack(&buf, &flag, 1, OPAL_INT8);
    }

    /* transfer the payload to the byte object */
    opal_dss.unload(&buf, (void**)&boptr->bytes, &boptr->size);
    OBJ_DESTRUCT(&buf);
    
    return ORTE_SUCCESS;
}


int orte_util_decode_pidmap(opal_byte_object_t *bo, orte_vpid_t *nprocs,
                            orte_pmap_t **procs, int8_t **app_idx,
                            char ***slot_str)
{
    orte_vpid_t i, num_procs;
    orte_pmap_t *pmap;
    int32_t *nodes;
    int8_t *tmp;
    int8_t flag;
    char **slots;
    orte_std_cntr_t n;
    opal_buffer_t buf;
    
    /* xfer the byte object to a buffer for unpacking */
    /* load it into a buffer */
    OBJ_CONSTRUCT(&buf, opal_buffer_t);
    opal_dss.load(&buf, bo->bytes, bo->size);
    
    /* unpack the number of procs */
    n=1;
    opal_dss.unpack(&buf, &num_procs, &n, ORTE_VPID);
    *nprocs = num_procs;
    
    /* allocate memory for the procs array */
    pmap = (orte_pmap_t*)malloc(num_procs * sizeof(orte_pmap_t));
    *procs = pmap;
    
    /* allocate memory for the node info */
    nodes = (int32_t*)malloc(num_procs * 4);

    /* unpack it in one shot */
    n=num_procs;
    opal_dss.unpack(&buf, nodes, &n, OPAL_INT32);
    /* store it */
    for (i=0; i < num_procs; i++) {
        pmap[i].node = nodes[i];
    }
    free(nodes);

    /* allocate memory for local ranks */
    tmp = (int8_t*)malloc(num_procs);

    /* unpack them in one shot */
    n=num_procs;
    opal_dss.unpack(&buf, tmp, &n, OPAL_UINT8);
    /* store them */
    for (i=0; i < num_procs; i++) {
        pmap[i].local_rank = tmp[i];
    }

    /* unpack node ranks in one shot */
    n=num_procs;
    opal_dss.unpack(&buf, tmp, &n, OPAL_UINT8);
    /* store it */
    for (i=0; i < num_procs; i++) {
        pmap[i].node_rank = tmp[i];
    }
    
    /* only daemons/HNPs need the rest of the data, so if
     * we aren't one of those, we are done!
     */
    if (!orte_process_info.hnp &&
        !orte_process_info.daemon) {
        OBJ_DESTRUCT(&buf);
        return ORTE_SUCCESS;
    }
    
    /* unpack app_idx in one shot */
    n=num_procs;
    opal_dss.unpack(&buf, tmp, &n, OPAL_INT8);
    /* hand the array back to the caller */
    *app_idx = tmp;
    
    /* unpack flag to indicate if slot_strings are present */
    n=1;
    opal_dss.unpack(&buf, &flag, &n, OPAL_INT8);

    if (flag) {
        /* allocate space */
        slots = (char**)malloc(num_procs * sizeof(char*));
        for (i=0; i < num_procs; i++) {
            n=1;
            opal_dss.unpack(&buf, &slots[i], &n, OPAL_STRING);
        }
        *slot_str = slots;
    }
    
    OBJ_DESTRUCT(&buf);
    return ORTE_SUCCESS;
}

