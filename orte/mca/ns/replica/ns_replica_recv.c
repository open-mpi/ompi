/* -*- C -*-
*
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
/** @file:
*
* The Open MPI Name Server
*
*/

/*
 * includes
 */
#include "orte_config.h"

#include "orte/orte_constants.h"
#include "orte/orte_types.h"

#include "opal/threads/mutex.h"
#include "opal/class/opal_list.h"
#include "opal/util/output.h"

#include "opal/mca/mca.h"
#include "opal/mca/base/mca_base_param.h"

#include "orte/util/proc_info.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/rml/rml.h"

#include "orte/mca/ns/base/ns_private.h"
#include "ns_replica.h"


/*
 * handle message from proxies
 * NOTE: The incoming buffer "buffer" is OBJ_RELEASED by the calling program.
 * DO NOT RELEASE THIS BUFFER IN THIS CODE
 */

void orte_ns_replica_recv(int status, orte_process_name_t* sender,
                          orte_buffer_t* buffer, orte_rml_tag_t tag,
                          void* cbdata)
{
    orte_buffer_t answer, error_answer;
    orte_ns_cmd_flag_t command;
    opal_list_t attrs;
    orte_cellid_t cell;
    orte_jobid_t job, root, *descendants;
    orte_vpid_t startvpid, range;
    char *tagname, *site, *resource;
    orte_rml_tag_t oob_tag;
    orte_data_type_t type;
    orte_std_cntr_t count, nprocs, nret;
    orte_process_name_t *procs;
    int rc=ORTE_SUCCESS, ret;
    
    count = 1;
    if (ORTE_SUCCESS != (rc = orte_dss.unpack(buffer, &command, &count, ORTE_NS_CMD))) {
        ORTE_ERROR_LOG(rc);
        rc = ORTE_ERR_BAD_PARAM;
        goto RETURN_ERROR;
    }
    
    OBJ_CONSTRUCT(&answer, orte_buffer_t);
    if (ORTE_SUCCESS != (rc = orte_dss.pack(&answer, &command, 1, ORTE_NS_CMD))) {
        ORTE_ERROR_LOG(rc);
        goto RETURN_ERROR;
    }
        
    switch (command) {
        case ORTE_NS_CREATE_CELLID_CMD:
            count = 1;
            if (ORTE_SUCCESS != (rc = orte_dss.unpack(buffer, &site, &count, ORTE_STRING))) {
                ORTE_ERROR_LOG(rc);
                rc = ORTE_ERR_BAD_PARAM;
                goto RETURN_ERROR;
            }
                
                count = 1;
            if (ORTE_SUCCESS != (rc = orte_dss.unpack(buffer, &resource, &count, ORTE_STRING))) {
                ORTE_ERROR_LOG(rc);
                rc = ORTE_ERR_BAD_PARAM;
                goto RETURN_ERROR;
            }
                
                rc = orte_ns_replica_create_cellid(&cell, site, resource);
            
            if (ORTE_SUCCESS != (ret = orte_dss.pack(&answer, &cell, 1, ORTE_CELLID))) {
                ORTE_ERROR_LOG(ret);
                goto RETURN_ERROR;
            }
               
            if (0 > orte_rml.send_buffer(sender, &answer, tag, 0)) {
                ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
                goto RETURN_ERROR;
            }
            break;
            
        case ORTE_NS_GET_CELL_INFO_CMD:
           count = 1;
            if (ORTE_SUCCESS != (rc = orte_dss.unpack(buffer, &cell, &count, ORTE_CELLID))) {
                ORTE_ERROR_LOG(rc);
                rc = ORTE_ERR_BAD_PARAM;
                goto RETURN_ERROR;
            }
                
            site = NULL;
            resource = NULL;
            rc = orte_ns_replica_get_cell_info(cell, &site, &resource);
            
            if (ORTE_SUCCESS != (ret = orte_dss.pack(&answer, &site, 1, ORTE_STRING))) {
                ORTE_ERROR_LOG(ret);
                goto RETURN_ERROR;
            }
                
            if (ORTE_SUCCESS != (ret = orte_dss.pack(&answer, &resource, 1, ORTE_STRING))) {
                ORTE_ERROR_LOG(ret);
                goto RETURN_ERROR;
            }
            
            if (0 > orte_rml.send_buffer(sender, &answer, tag, 0)) {
                ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
                goto RETURN_ERROR;
            }
            break;
            
        case ORTE_NS_CREATE_NODEID_CMD:
        case ORTE_NS_GET_NODE_INFO_CMD:
            ORTE_ERROR_LOG(ORTE_ERR_NOT_IMPLEMENTED);
            goto RETURN_ERROR;
            break;
            
        case ORTE_NS_CREATE_JOBID_CMD:
            /* get the list of attributes */
            OBJ_CONSTRUCT(&attrs, opal_list_t);
            count = 1;
            if(ORTE_SUCCESS != (rc = orte_dss.unpack(buffer, &attrs, &count, ORTE_ATTR_LIST))) {
                ORTE_ERROR_LOG(rc);
                goto RETURN_ERROR;
            }
                
            if (ORTE_SUCCESS != (rc = orte_ns_replica_create_jobid(&job, &attrs))) {
                ORTE_ERROR_LOG(rc);
                OBJ_DESTRUCT(&attrs);
                goto RETURN_ERROR;
            }
            OBJ_DESTRUCT(&attrs);
            
            if (ORTE_SUCCESS != (rc = orte_dss.pack(&answer, (void*)&job, 1, ORTE_JOBID))) {
                ORTE_ERROR_LOG(rc);
                goto RETURN_ERROR;
            }
            
            if (0 > orte_rml.send_buffer(sender, &answer, tag, 0)) {
                ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
                goto RETURN_ERROR;
            }
            break;
            
        case ORTE_NS_GET_JOB_DESC_CMD:
            count = 1;
            if (ORTE_SUCCESS != (rc = orte_dss.unpack(buffer, (void*)&job, &count, ORTE_JOBID))) {
                ORTE_ERROR_LOG(rc);
                goto RETURN_ERROR;
            }
            
            if (ORTE_SUCCESS != (rc = orte_ns_replica_get_job_descendants(&descendants, &nret, job))) {
                ORTE_ERROR_LOG(rc);
                goto RETURN_ERROR;
            }
            
            if (ORTE_SUCCESS != (rc = orte_dss.pack(&answer, (void*)&nret, 1, ORTE_STD_CNTR))) {
                ORTE_ERROR_LOG(rc);
                goto RETURN_ERROR;
            }
            
            if (0 < nret) {
                if (ORTE_SUCCESS != (rc = orte_dss.pack(&answer, (void*)descendants, nret, ORTE_JOBID))) {
                    ORTE_ERROR_LOG(rc);
                    goto RETURN_ERROR;
                }
            }
                
            if (0 > (rc = orte_rml.send_buffer(sender, &answer, tag, 0))) {
                ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
                goto RETURN_ERROR;
            }
            break;
            
        case ORTE_NS_GET_JOB_CHILD_CMD:
            count = 1;
            if (ORTE_SUCCESS != (rc = orte_dss.unpack(buffer, (void*)&job, &count, ORTE_JOBID))) {
                ORTE_ERROR_LOG(rc);
                goto RETURN_ERROR;
            }
                
                if (ORTE_SUCCESS != (rc = orte_ns_replica_get_job_children(&descendants, &nret, job))) {
                    ORTE_ERROR_LOG(rc);
                    goto RETURN_ERROR;
                }
                
                if (ORTE_SUCCESS != (rc = orte_dss.pack(&answer, (void*)&nret, 1, ORTE_STD_CNTR))) {
                    ORTE_ERROR_LOG(rc);
                    goto RETURN_ERROR;
                }
                
                if (0 < nret) {
                    if (ORTE_SUCCESS != (rc = orte_dss.pack(&answer, (void*)descendants, nret, ORTE_JOBID))) {
                        ORTE_ERROR_LOG(rc);
                        goto RETURN_ERROR;
                    }
                }
                
                if (0 > (rc = orte_rml.send_buffer(sender, &answer, tag, 0))) {
                    ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
                    goto RETURN_ERROR;
                }
                break;
            
        case ORTE_NS_GET_ROOT_JOB_CMD:
            count = 1;
            if (ORTE_SUCCESS != (rc = orte_dss.unpack(buffer, (void*)&job, &count, ORTE_JOBID))) {
                ORTE_ERROR_LOG(rc);
                goto RETURN_ERROR;
            }

            if (ORTE_SUCCESS != (rc = orte_ns_replica_get_root_job(&root, job))) {
                ORTE_ERROR_LOG(rc);
                goto RETURN_ERROR;
            }
            
            if (ORTE_SUCCESS != (rc = orte_dss.pack(&answer, (void*)&root, 1, ORTE_JOBID))) {
                ORTE_ERROR_LOG(rc);
                goto RETURN_ERROR;
            }

            if (0 > (rc = orte_rml.send_buffer(sender, &answer, tag, 0))) {
                ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
                goto RETURN_ERROR;
            }
            break;
            
        case ORTE_NS_GET_PARENT_JOB_CMD:
            count = 1;
            if (ORTE_SUCCESS != (rc = orte_dss.unpack(buffer, (void*)&job, &count, ORTE_JOBID))) {
                ORTE_ERROR_LOG(rc);
                goto RETURN_ERROR;
            }
                
            if (ORTE_SUCCESS != (rc = orte_ns_replica_get_parent_job(&root, job))) {
                ORTE_ERROR_LOG(rc);
                goto RETURN_ERROR;
            }
            
            if (ORTE_SUCCESS != (rc = orte_dss.pack(&answer, (void*)&root, 1, ORTE_JOBID))) {
                ORTE_ERROR_LOG(rc);
                goto RETURN_ERROR;
            }
            
            if (0 > (rc = orte_rml.send_buffer(sender, &answer, tag, 0))) {
                ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
                goto RETURN_ERROR;
            }
            break;
                        
        case ORTE_NS_RESERVE_RANGE_CMD:
            count = 1;
            if (ORTE_SUCCESS != (rc = orte_dss.unpack(buffer, (void*)&job, &count, ORTE_JOBID))) {
                ORTE_ERROR_LOG(rc);
                goto RETURN_ERROR;
            }
                
            count = 1;
            if (ORTE_SUCCESS != (rc = orte_dss.unpack(buffer, (void*)&range, &count, ORTE_VPID))) {
                ORTE_ERROR_LOG(rc);
                goto RETURN_ERROR;
            }
                
            if (ORTE_SUCCESS != (rc = orte_ns_replica_reserve_range(job, range, &startvpid))) {
                ORTE_ERROR_LOG(rc);
                goto RETURN_ERROR;
            }
            
           if (ORTE_SUCCESS != (rc = orte_dss.pack(&answer, (void*)&startvpid, 1, ORTE_VPID))) {
                ORTE_ERROR_LOG(rc);
                goto RETURN_ERROR;
            }
            
            if (0 > (rc = orte_rml.send_buffer(sender, &answer, tag, 0))) {
                ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
                goto RETURN_ERROR;
            }
            break;
            
        case ORTE_NS_ASSIGN_OOB_TAG_CMD:
            count = 1;
            if (0 > orte_dss.unpack(buffer, &tagname, &count, ORTE_STRING)) {
                rc = ORTE_ERR_UNPACK_FAILURE;
                goto RETURN_ERROR;
            }
                
            if (0 == strncmp(tagname, "NULL", 4)) {
                if (ORTE_SUCCESS != (rc = orte_ns_replica_assign_rml_tag(&oob_tag, NULL))) {
                    goto RETURN_ERROR;
                }
            } else {
                if (ORTE_SUCCESS != (rc = orte_ns_replica_assign_rml_tag(&oob_tag, tagname))) {
                    goto RETURN_ERROR;
                }
            }
            
            if (ORTE_SUCCESS != (rc = orte_dss.pack(&answer, (void*)&oob_tag, 1, ORTE_RML_TAG))) {
                goto RETURN_ERROR;
            }
            
            if (0 > orte_rml.send_buffer(sender, &answer, tag, 0)) {
                ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
                goto RETURN_ERROR;
            }
            break;
            
        case ORTE_NS_DEFINE_DATA_TYPE_CMD:
            count = 1;
            if (0 > orte_dss.unpack(buffer, &tagname, &count, ORTE_STRING)) {
                rc = ORTE_ERR_UNPACK_FAILURE;
                goto RETURN_ERROR;
            }
                
            if (ORTE_SUCCESS != (rc = orte_ns_replica_define_data_type(tagname, &type))) {
                goto RETURN_ERROR;
            }
            
            if (ORTE_SUCCESS != (rc = orte_dss.pack(&answer, (void*)&type, 1, ORTE_DATA_TYPE))) {
                goto RETURN_ERROR;
            }
            
            if (0 > orte_rml.send_buffer(sender, &answer, tag, 0)) {
                ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
                goto RETURN_ERROR;
            }
            break;
            
        case ORTE_NS_CREATE_MY_NAME_CMD:
            /* ignore this command */
            break;
            
        case ORTE_NS_GET_PEERS_CMD:
            /* get the list of attributes */
            OBJ_CONSTRUCT(&attrs, opal_list_t);
            count = 1;
            if(ORTE_SUCCESS != (rc = orte_dss.unpack(buffer, &attrs, &count, ORTE_ATTR_LIST))) {
                ORTE_ERROR_LOG(rc);
                goto RETURN_ERROR;
            }
                
            /* process the request */
            if (ORTE_SUCCESS != (rc = orte_ns_replica_get_peers(&procs, &nprocs, &attrs))) {
                ORTE_ERROR_LOG(rc);
                OBJ_DESTRUCT(&attrs);
                goto RETURN_ERROR;
            }
            OBJ_DESTRUCT(&attrs);
            
            /* pack the answer */
            if (ORTE_SUCCESS != (rc = orte_dss.pack(&answer, &nprocs, 1, ORTE_STD_CNTR))) {
                ORTE_ERROR_LOG(rc);
                goto RETURN_ERROR;
            }
            
            if (nprocs > 0) {
                if (ORTE_SUCCESS != (rc = orte_dss.pack(&answer, procs, nprocs, ORTE_NAME))) {
                    ORTE_ERROR_LOG(rc);
                    goto RETURN_ERROR;
                }
            }
                
            if (0 > orte_rml.send_buffer(sender, &answer, tag, 0)) {
                ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
                goto RETURN_ERROR;
            }
            break;
            
        case ORTE_NS_DUMP_CELLS_CMD:
            if (ORTE_SUCCESS != (rc = orte_ns_replica_dump_cells_fn(&answer))) {
                ORTE_ERROR_LOG(rc);
                goto RETURN_ERROR;
            }
            if (0 > orte_rml.send_buffer(sender, &answer, tag, 0)) {
                ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
                goto RETURN_ERROR;
            }
            break;
            
        case ORTE_NS_DUMP_JOBIDS_CMD:
            if (ORTE_SUCCESS != (rc = orte_ns_replica_dump_jobs_fn(&answer))) {
                ORTE_ERROR_LOG(rc);
                goto RETURN_ERROR;
            }
            if (0 > orte_rml.send_buffer(sender, &answer, tag, 0)) {
                ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
                goto RETURN_ERROR;
            }
            break;
            
        case ORTE_NS_DUMP_TAGS_CMD:
            if (ORTE_SUCCESS != (rc = orte_ns_replica_dump_tags_fn(&answer))) {
                ORTE_ERROR_LOG(rc);
                goto RETURN_ERROR;
            }
            if (0 > orte_rml.send_buffer(sender, &answer, tag, 0)) {
                ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
                goto RETURN_ERROR;
            }
            break;
            
        case ORTE_NS_DUMP_DATATYPES_CMD:
            if (ORTE_SUCCESS != (rc = orte_ns_replica_dump_datatypes_fn(&answer))) {
                ORTE_ERROR_LOG(rc);
                goto RETURN_ERROR;
            }
            if (0 > orte_rml.send_buffer(sender, &answer, tag, 0)) {
                ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
                goto RETURN_ERROR;
            }
            break;
            
        default:
            goto RETURN_ERROR;
    }
    goto CLEANUP;
    
RETURN_ERROR:
    OBJ_CONSTRUCT(&error_answer, orte_buffer_t);
    orte_dss.pack(&error_answer, (void*)&command, 1, ORTE_NS_CMD);
    orte_dss.pack(&error_answer, (void*)&rc, 1, ORTE_INT32);
    orte_rml.send_buffer(sender, &error_answer, tag, 0);
    OBJ_DESTRUCT(&error_answer);
    
CLEANUP:
        /* cleanup */
        OBJ_DESTRUCT(&answer);
}

