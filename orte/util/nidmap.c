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
#include "orte/util/show_help.h"
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
    int32_t *arch;
    
    /* setup a buffer for tmp use */
    OBJ_CONSTRUCT(&buf, opal_buffer_t);

    /* determine the number of nodes in the global node array */
    num_nodes = 0;
    nodes = (orte_node_t**)orte_node_pool->addr;
    while (num_nodes < orte_node_pool->size &&
           NULL != nodes[num_nodes]) {
        ++num_nodes;
    }
    /* pack number of nodes */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &num_nodes, 1, OPAL_INT32))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
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
        if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &nodename, 1, OPAL_STRING))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        free(nodename);
    } else {
        if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &nodes[0]->name, 1, OPAL_STRING))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
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
                    opal_output(0, "%s encode:nidmap Nodename pattern is nonstandard",
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
        if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &command, 1, OPAL_UINT8))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* pack the prefix */
        tmp = &prefix[0];
        if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &tmp, 1, OPAL_STRING))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        len = strlen(prefix);
        
        /* pack the number of digits in the index */
        if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &num_digs, 1, OPAL_UINT8))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* and the starting offset */
        if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &firstnode, 1, OPAL_INT32))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        OPAL_OUTPUT_VERBOSE((2, orte_debug_output,
                             "%s encode:nidmap:contig_nodes prefix %s num_digits %d offset %d",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), prefix, num_digs, firstnode));
        
        lastnode = strtol(&(nodes[2]->name[i]), NULL, 10);
        if ((lastnode - firstnode) < 0) {
            /* we are decrementing */
            incdec = 0;
            if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &incdec, 1, OPAL_INT8))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
        } else {
            /* we are incrementing */
            incdec = 1;
            if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &incdec, 1, OPAL_INT8))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
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
                if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &lastnode, 1, OPAL_INT32))) {
                    ORTE_ERROR_LOG(rc);
                    return rc;
                }
                /* indicate start of new range */
                if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &nodenum, 1, OPAL_INT32))) {
                    ORTE_ERROR_LOG(rc);
                    return rc;
                }
                OPAL_OUTPUT_VERBOSE((2, orte_debug_output,
                                     "%s encode:nidmap:contig_nodes end range %d start next range %d",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), lastnode, nodenum));
            }
            lastnode = nodenum;
        }
        /* pack end of range */
        if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &lastnode, 1, OPAL_INT32))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        OPAL_OUTPUT_VERBOSE((2, orte_debug_output,
                             "%s encode:nidmap:contig_nodes end range %d",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), lastnode));
        /* pack flag end of ranges */
        lastnode = -1;
        if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &lastnode, 1, OPAL_INT32))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
   } else {
        /* if the nodes aren't contiguous, then we need
         * to simply pack every nodename individually
         */
        OPAL_OUTPUT_VERBOSE((2, orte_debug_output,
                             "%s encode:nidmap non_contig_nodes - packing all names",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        /* indicate that this will not be a contiguous node map */
        command = ORTE_NON_CONTIG_NODE_CMD;
       if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &command, 1, OPAL_UINT8))) {
           ORTE_ERROR_LOG(rc);
           return rc;
       }
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
    if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, vpids, num_nodes, ORTE_VPID))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    free(vpids);
    
    if (OMPI_ENABLE_HETEROGENEOUS_SUPPORT) {
        /* check to see if all reported archs are the same */
        orte_homogeneous_nodes = true;
        for (i=1; i < num_nodes; i++) {
            if (nodes[i]->arch != nodes[0]->arch) {
                orte_homogeneous_nodes = false;
                break;
            }
        }
        if (orte_homogeneous_nodes) {
            /* if everything is homo, just set that
             * flag - no need to send everything
             */
            num_digs = 0;
            if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &num_digs, 1, OPAL_UINT8))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
        } else {
            /* it isn't homo, so we have to pass the
             * archs to the daemons
             */
            num_digs = 1;
            if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &num_digs, 1, OPAL_UINT8))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
           /* allocate space for the node arch */
            arch = (int32_t*)malloc(num_nodes * 4);
            /* transfer the data from the nodes */
            for (i=0; i < num_nodes; i++) {
                arch[i] = nodes[i]->arch;
            }
            /* pack the values */
            if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, arch, num_nodes, OPAL_INT32))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
            free(arch);
        }
    } else {
        /* pack a flag indicating that the archs are the same */
        num_digs = 0;
        if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &num_digs, 1, OPAL_UINT8))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    }
    
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
    int32_t *arch;
    opal_buffer_t buf;
    int rc;

    OPAL_OUTPUT_VERBOSE((2, orte_debug_output,
                         "%s decode:nidmap decoding nodemap",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    /* if there are any entries already in the node array, clear it out */
    if (0 < nodes->size) {
        /* unfortunately, the opal function "remove_all" doesn't release
         * the memory pointed to by the elements in the array, so we need
         * to release those first
         */
        nd = (orte_nid_t**)nodes->addr;
        for (i=0; i < nodes->size && NULL != nd[i]; i++) {
            OBJ_RELEASE(nd[i]);
        }
        /* now use the opal function to reset the internal pointers */
        opal_pointer_array_remove_all(nodes);
    }
    
    /* xfer the byte object to a buffer for unpacking */
    OBJ_CONSTRUCT(&buf, opal_buffer_t);
    opal_dss.load(&buf, bo->bytes, bo->size);
    
    /* unpack number of nodes */
    n=1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(&buf, &num_nodes, &n, OPAL_INT32))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
 
    OPAL_OUTPUT_VERBOSE((2, orte_debug_output,
                         "%s decode:nidmap decoding %d nodes with %d already loaded",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), num_nodes, nodes->lowest_free));
    
    /* set the size of the nidmap storage so we minimize realloc's */
    if (ORTE_SUCCESS != (rc = opal_pointer_array_set_size(nodes, num_nodes))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* create the struct for the HNP's node */
    node = OBJ_NEW(orte_nid_t);
    /* the arch defaults to our arch so that non-hetero
     * case will yield correct behavior
     */
    opal_pointer_array_set_item(nodes, 0, node);
    
    /* unpack the name of the HNP's node */
    n=1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(&buf, &(node->name), &n, OPAL_STRING))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* unpack flag to see if this is a contiguous node map or not */
    n=1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(&buf, &command, &n, OPAL_UINT8))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    if (ORTE_CONTIG_NODE_CMD == command) {
        /* unpack the prefix */
        n=1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(&buf, &prefix, &n, OPAL_STRING))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* the number of digits in the index */
        n=1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(&buf, &num_digs, &n, OPAL_UINT8))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* and the starting offset */
        n=1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(&buf, &lastnode, &n, OPAL_INT32))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* unpack increment/decrement flag */
        n=1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(&buf, &incdec, &n, OPAL_INT8))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* unpack the end of the range */
        n=1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(&buf, &endrange, &n, OPAL_INT32))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* setup loop params */
        if (0 == incdec) {
            endrange -= 1;
            step = -1;
        } else {
            endrange += 1;
            step = 1;
        }
        
        OPAL_OUTPUT_VERBOSE((2, orte_debug_output,
                             "%s decode:nidmap:contig_nodes prefix %s num_digits %d offset %d endrange %d",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), prefix, num_digs, lastnode, endrange));
        
        namelen = strlen(prefix) + num_digs + 1;
        /* cycle through the ranges */
        index = 1;
        while (1) {
            for (i=lastnode; i != endrange; i += step) {
                node = OBJ_NEW(orte_nid_t);
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
                /* the arch defaults to our arch so that non-hetero
                 * case will yield correct behavior
                 */
                opal_pointer_array_set_item(nodes, index, node);
                index++;
            }
            /* unpack start of new range */
            n=1;
            opal_dss.unpack(&buf, &lastnode, &n, OPAL_INT32);
            /* if that is -1, then it flags no more ranges */
            if (-1 == lastnode) {
                goto process_daemons;
            }
            n=1;
            if (ORTE_SUCCESS != (rc = opal_dss.unpack(&buf, &endrange, &n, OPAL_INT32))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
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
            node = OBJ_NEW(orte_nid_t);
            /* the arch defaults to our arch so that non-hetero
             * case will yield correct behavior
             */
            opal_pointer_array_set_item(nodes, i, node);
            
            /* unpack the node's name */
            n=1;
            if (ORTE_SUCCESS != (rc = opal_dss.unpack(&buf, &(node->name), &n, OPAL_STRING))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
        }
    }
    
process_daemons:
    /* unpack the daemon names */
    vpids = (orte_vpid_t*)malloc(num_nodes * sizeof(orte_vpid_t));
    n=num_nodes;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(&buf, vpids, &n, ORTE_VPID))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    /* transfer the data to the nidmap, counting the number of
     * daemons in the system
     */
    num_daemons = 0;
    nd = (orte_nid_t**)nodes->addr;
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
    
    /* unpack a flag to see if we are in a homogeneous
     * scenario - could be that no hetero is supported,
     * or could be that things just are homo anyway
     */
    n=1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(&buf, &num_digs, &n, OPAL_UINT8))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    if (0 == num_digs) {
        /* homo situation */
        orte_homogeneous_nodes = true;
    } else {
        /* hetero situation */
        orte_homogeneous_nodes = false;
        /* get the archs */
        arch = (int32_t*)malloc(num_nodes * 4);
        /* unpack the values */
        n=num_nodes;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(&buf, arch, &n, OPAL_INT32))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        /* transfer the data to the nodes */
        nd = (orte_nid_t**)nodes->addr;
        for (i=0; i < num_nodes; i++) {
            nd[i]->arch = arch[i];
        }
        free(arch);
    }
 
    if (0 < opal_output_get_verbosity(orte_debug_output)) {
        nd = (orte_nid_t**)nodes->addr;
        for (i=0; i < num_nodes; i++) {
            opal_output(0, "%s node[%d].name %s daemon %s arch %0x",
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
    int8_t *tmp;
    opal_buffer_t buf;
    orte_local_rank_t *lrank;
    orte_node_rank_t *nrank;
    int rc;

    /* setup the working buffer */
    OBJ_CONSTRUCT(&buf, opal_buffer_t);
    
    /* pack the number of procs */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &jdata->num_procs, 1, ORTE_VPID))) {
        ORTE_ERROR_LOG(rc);
        return rc; 
    }
    
    /* allocate memory for the nodes */
    nodes = (int32_t*)malloc(jdata->num_procs * 4);
    
    /* transfer and pack the node info in one pack */
    procs = (orte_proc_t**)jdata->procs->addr;
    for (i=0; i < jdata->num_procs; i++) {
        nodes[i] = procs[i]->node->index;
    }
    if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, nodes, jdata->num_procs, OPAL_INT32))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* free node storage */
    free(nodes);
    
    /* allocate memory for the local_ranks */
    lrank = (orte_local_rank_t*)malloc(jdata->num_procs*sizeof(orte_local_rank_t));
    
   /* transfer and pack them in one pack */
    for (i=0; i < jdata->num_procs; i++) {
        lrank[i] = procs[i]->local_rank;
    }
    if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, lrank, jdata->num_procs, ORTE_LOCAL_RANK))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    free(lrank);
    
    /* transfer and pack the node ranks in one pack */
    nrank = (orte_node_rank_t*)malloc(jdata->num_procs*sizeof(orte_node_rank_t));
    for (i=0; i < jdata->num_procs; i++) {
        nrank[i] = procs[i]->node_rank;
    }
    if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, nrank, jdata->num_procs, ORTE_NODE_RANK))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    free(nrank);
    
    /* transfer and pack the app_idx in one pack */
    tmp = (int8_t*)malloc(jdata->num_procs);
    for (i=0; i < jdata->num_procs; i++) {
        tmp[i] = procs[i]->app_idx;
    }
    if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, tmp, jdata->num_procs, OPAL_INT8))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    free(tmp);
   
    /* transfer the payload to the byte object */
    opal_dss.unload(&buf, (void**)&boptr->bytes, &boptr->size);
    OBJ_DESTRUCT(&buf);
    
    return ORTE_SUCCESS;
}


int orte_util_decode_pidmap(opal_byte_object_t *bo, orte_vpid_t *nprocs,
                            opal_value_array_t *procs, int8_t **app_idx,
                            char ***slot_str)
{
    orte_vpid_t i, num_procs;
    orte_pmap_t pmap;
    int32_t *nodes;
    orte_local_rank_t *local_rank;
    orte_node_rank_t *node_rank;
    int8_t *idx;
    orte_std_cntr_t n;
    opal_buffer_t buf;
    int rc;
    
    /* xfer the byte object to a buffer for unpacking */
    /* load it into a buffer */
    OBJ_CONSTRUCT(&buf, opal_buffer_t);
    if (ORTE_SUCCESS != (rc = opal_dss.load(&buf, bo->bytes, bo->size))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* unpack the number of procs */
    n=1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(&buf, &num_procs, &n, ORTE_VPID))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    *nprocs = num_procs;
    
    /* allocate memory for the procs array */
    opal_value_array_set_size(procs, num_procs);

    /* allocate memory for the node info */
    nodes = (int32_t*)malloc(num_procs * 4);
    /* unpack it in one shot */
    n=num_procs;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(&buf, nodes, &n, OPAL_INT32))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    /* allocate memory for local ranks */
    local_rank = (orte_local_rank_t*)malloc(num_procs*sizeof(orte_local_rank_t));
    /* unpack them in one shot */
    n=num_procs;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(&buf, local_rank, &n, ORTE_LOCAL_RANK))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    /* allocate memory for node ranks */
    node_rank = (orte_node_rank_t*)malloc(num_procs*sizeof(orte_node_rank_t));
    /* unpack node ranks in one shot */
    n=num_procs;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(&buf, node_rank, &n, ORTE_NODE_RANK))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* store the data */
    for (i=0; i < num_procs; i++) {
        pmap.node = nodes[i];
        pmap.local_rank = local_rank[i];
        pmap.node_rank = node_rank[i];
        opal_value_array_set_item(procs, i, &pmap);
    }
    
    /* release data */
    free(nodes);
    free(local_rank);
    free(node_rank);
    
    /* only daemons/HNPs need the rest of the data, so if
     * we aren't one of those, we are done!
     */
    if (!orte_process_info.hnp &&
        !orte_process_info.daemon) {
        OBJ_DESTRUCT(&buf);
        return ORTE_SUCCESS;
    }
    
    /* allocate memory for app_idx */
    idx = (int8_t*)malloc(num_procs);
    /* unpack app_idx in one shot */
    n=num_procs;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(&buf, idx, &n, OPAL_INT8))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    /* hand the array back to the caller */
    *app_idx = idx;

    OBJ_DESTRUCT(&buf);
    return ORTE_SUCCESS;
}

