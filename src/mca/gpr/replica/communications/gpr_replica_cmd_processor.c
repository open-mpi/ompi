/* -*- C -*-
 *
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/** @file:
 *
 * The Open MPI General Purpose Registry - Replica component
 *
 */

/*
 * includes
 */
#include "orte_config.h"

#include "dps/dps.h"
#include "mca/errmgr/errmgr.h"

#include "gpr_replica_comm.h"

/* 
 * handle message from proxies
 */

int orte_gpr_replica_process_command_buffer(orte_buffer_t *input_buffer,
                            orte_process_name_t *sender,
                            orte_buffer_t **output_buffer)
{
    orte_buffer_t *answer;
    orte_gpr_cmd_flag_t command;
    int rc, ret, rc2;
    size_t n;


    *output_buffer = OBJ_NEW(orte_buffer_t);
    if (NULL == *output_buffer) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    answer = *output_buffer;  /* for convenience */
    
    n = 1;
    rc = ORTE_SUCCESS;
    ret = ORTE_SUCCESS;
    
    while (ORTE_SUCCESS == orte_dps.unpack(input_buffer, &command, &n, ORTE_GPR_CMD)) {

        	switch(command) {
        
            	case ORTE_GPR_COMPOUND_CMD:  /*****     COMPOUND COMMAND     ******/
            	    if (orte_gpr_replica_globals.debug) {
            		    ompi_output(0, "\tcompound cmd");
            	    }
            	    break;
                   
            	
            	case ORTE_GPR_DELETE_SEGMENT_CMD:  /******    DELETE SEGMENT    *****/
            
            	    if (orte_gpr_replica_globals.debug) {
            		    ompi_output(0, "\tdelete segment cmd");
            	    }
            
            	    if (ORTE_SUCCESS != (ret =
                        orte_gpr_replica_recv_delete_segment_cmd(input_buffer, answer))) {
                     ORTE_ERROR_LOG(ret);
                     goto RETURN_ERROR;
            	    }
            	    break;
            
            	    
            	case ORTE_GPR_PUT_CMD:  /*****    PUT    *****/
            
            	    if (orte_gpr_replica_globals.debug) {
            		    ompi_output(0, "\tput cmd");
            	    }
            
            	    if (ORTE_SUCCESS != (ret = orte_gpr_replica_recv_put_cmd(input_buffer, answer))) {
                     ORTE_ERROR_LOG(ret);
            		    goto RETURN_ERROR;
            	    }
            
            	    break;
            
            	    
            	case ORTE_GPR_GET_CMD:  /*****    GET    *****/
            
            	    if (orte_gpr_replica_globals.debug) {
            		    ompi_output(0, "\tget cmd");
            	    }
            
            	    if (ORTE_SUCCESS != (ret = orte_gpr_replica_recv_get_cmd(input_buffer, answer))) {
                     ORTE_ERROR_LOG(ret);
            		    goto RETURN_ERROR;
            	    }            
            	    break;
            
            	    
            	case ORTE_GPR_DELETE_ENTRIES_CMD:  /*****     DELETE ENTRIES     *****/
            
            	    if (orte_gpr_replica_globals.debug) {
            		    ompi_output(0, "\tdelete object cmd");
            	    }
            
            	    if (ORTE_SUCCESS != (ret =
                        orte_gpr_replica_recv_delete_entries_cmd(input_buffer, answer))) {
                     ORTE_ERROR_LOG(ret);
            		    goto RETURN_ERROR;
            	    }
            	    break;
            
            	    
            	case ORTE_GPR_INDEX_CMD:  /*****     INDEX     *****/
            
            	    if (orte_gpr_replica_globals.debug) {
            		    ompi_output(0, "\tindex cmd");
            	    }
            
            	    if (ORTE_SUCCESS != (ret = orte_gpr_replica_recv_index_cmd(input_buffer, answer))) {
                     ORTE_ERROR_LOG(ret);
            		    goto RETURN_ERROR;
            	    }
            	    break;
            
            	    
            	case ORTE_GPR_SUBSCRIBE_CMD:  /*****     SUBSCRIBE     *****/
            
            	    if (orte_gpr_replica_globals.debug) {
            		ompi_output(0, "\tsubscribe cmd");
            	    }
            
            	    if (ORTE_SUCCESS != (ret =
                        orte_gpr_replica_recv_subscribe_cmd(sender, input_buffer, answer))) {
                     ORTE_ERROR_LOG(ret);
            		    goto RETURN_ERROR;
            	    }
            	    break;
            
            	   
            	case ORTE_GPR_UNSUBSCRIBE_CMD:  /*****     UNSUBSCRIBE     *****/
            
            	    if (orte_gpr_replica_globals.debug) {
            		    ompi_output(0, "\tunsubscribe cmd");
            	    }
            
            	    if (ORTE_SUCCESS != (ret =
                        orte_gpr_replica_recv_unsubscribe_cmd(input_buffer, answer))) {
                     ORTE_ERROR_LOG(ret);
            		    goto RETURN_ERROR;
            	    }
            	    break;
            
            
            	    
            	case ORTE_GPR_DUMP_ALL_CMD:  /*****     DUMP     *****/
            
            	    if (orte_gpr_replica_globals.debug) {
            		    ompi_output(0, "\tdump cmd");
            	    }
            
            	    if (ORTE_SUCCESS != (ret = orte_gpr_replica_recv_dump_all_cmd(answer))) {
                     ORTE_ERROR_LOG(ret);
                     goto RETURN_ERROR;
                 }
            	    break;
            
            
            	    
                case ORTE_GPR_DUMP_SEGMENTS_CMD:  /*****     DUMP     *****/
            
                   if (orte_gpr_replica_globals.debug) {
                      ompi_output(0, "\tdump cmd");
                  }
            
                 if (ORTE_SUCCESS != (ret = orte_gpr_replica_recv_dump_segments_cmd(answer))) {
                     ORTE_ERROR_LOG(ret);
                     goto RETURN_ERROR;
                 }
                 break;
            
            
                   
                case ORTE_GPR_DUMP_TRIGGERS_CMD:  /*****     DUMP     *****/
            
                   if (orte_gpr_replica_globals.debug) {
                      ompi_output(0, "\tdump cmd");
                  }
            
                 if (ORTE_SUCCESS != (ret = orte_gpr_replica_recv_dump_triggers_cmd(answer))) {
                     ORTE_ERROR_LOG(ret);
                     goto RETURN_ERROR;
                 }
                 break;
            
            
                   
            	case ORTE_GPR_INCREMENT_VALUE_CMD:  /*****     INCREMENT_VALUE     *****/
            
            	    if (orte_gpr_replica_globals.debug) {
            		    ompi_output(0, "\tincrement_value cmd");
            	    }
            
            	    if (ORTE_SUCCESS != (ret =
                        orte_gpr_replica_recv_increment_value_cmd(input_buffer, answer))) {
                     ORTE_ERROR_LOG(ret);
                     goto RETURN_ERROR;
                 }
            	    break;
            
            
            	    
            	case ORTE_GPR_DECREMENT_VALUE_CMD:  /*****     DECREMENT_VALUE     ******/
            
            	    if (orte_gpr_replica_globals.debug) {
            		    ompi_output(0, "\tdecrement_value cmd");
            	    }
            
                 if (ORTE_SUCCESS != (ret =
                        orte_gpr_replica_recv_decrement_value_cmd(input_buffer, answer))) {
                     ORTE_ERROR_LOG(ret);
                     goto RETURN_ERROR;
                 }
            	    break;
            
            
            	    
            	case ORTE_GPR_CLEANUP_JOB_CMD:  /*****     CLEANUP JOB    *****/
            
            	    if (orte_gpr_replica_globals.debug) {
            		    ompi_output(0, "\tcleanup job cmd");
            	    }
            
            	    if (ORTE_SUCCESS != (ret =
                        orte_gpr_replica_recv_cleanup_job_cmd(input_buffer, answer))) {
                     ORTE_ERROR_LOG(ret);
                     goto RETURN_ERROR;
                 }
            	    break;
            
            
            	    
            	case ORTE_GPR_CLEANUP_PROC_CMD:  /*****     CLEANUP PROCESS     *****/
            
            	    if (orte_gpr_replica_globals.debug) {
            		    ompi_output(0, "\tcleanup proc cmd");
            	    }
            
            	    if (ORTE_SUCCESS != (ret =
                        orte_gpr_replica_recv_cleanup_proc_cmd(input_buffer, answer))) {
                     ORTE_ERROR_LOG(ret);
                     goto RETURN_ERROR;
                 }
            	    break;
            
            
            	    
            	default:  /****    UNRECOGNIZED COMMAND   ****/
                command = ORTE_GPR_ERROR;
                if (ORTE_SUCCESS != (rc = orte_dps.pack(answer, (void*)&command, 1, ORTE_GPR_CMD))) {
                     ORTE_ERROR_LOG(rc);
                     goto RETURN_ERROR;
                 }
        } /* end switch command */

        n = 1;  /* unpack a single command */
    } /* end while */

    return ORTE_SUCCESS;
    
RETURN_ERROR:
	if (orte_gpr_replica_globals.debug) {
		ompi_output(0, "unrecognized command");
	}
    OBJ_RELEASE(*output_buffer);
    *output_buffer = answer = OBJ_NEW(orte_buffer_t);
    if (NULL == *output_buffer) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    if (ORTE_SUCCESS != (rc2 = orte_dps.pack(answer, (void*)&command, 1, ORTE_GPR_CMD))) {
         ORTE_ERROR_LOG(rc2);
    }
    if (ORTE_SUCCESS != ret) {
        orte_dps.pack(answer, &ret, 1, ORTE_INT);
        return rc;
    }
    orte_dps.pack(answer, &rc, 1, ORTE_INT);
    return rc;
}

