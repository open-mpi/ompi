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
#include "orte/orte_constants.h"

#include <stdio.h>
#include <ctype.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_NETDB_H
#include <netdb.h>
#endif
#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif
#include <fcntl.h>
#include <errno.h>
#include <signal.h>


#include "opal/event/event.h"
#include "opal/mca/base/base.h"
#include "opal/threads/mutex.h"
#include "opal/threads/condition.h"
#include "opal/util/output.h"
#include "opal/util/bit_ops.h"
#include "opal/util/trace.h"

#include "orte/dss/dss.h"
#include "orte/util/sys_info.h"
#include "orte/util/proc_info.h"
#include "orte/util/univ_info.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/ns/ns.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/odls/odls.h"
#include "orte/mca/rmgr/rmgr.h"
#include "orte/mca/pls/pls.h"
#include "orte/runtime/params.h"

#include "orte/tools/orted/orted.h"

static int binomial_route_msg(orte_buffer_t *buf, orte_jobid_t job, orte_rml_tag_t target_tag);

void orte_daemon_recv_pls(int status, orte_process_name_t* sender,
                          orte_buffer_t *buffer, orte_rml_tag_t tag,
                          void* cbdata)
{
    orte_daemon_cmd_flag_t command, routing_mode;
    orte_buffer_t answer, *relay;
    int ret;
    orte_std_cntr_t n;
    int32_t signal;
    orte_gpr_notify_data_t *ndat;
    orte_jobid_t *jobs, job;
    orte_std_cntr_t num_jobs;
    orte_rml_tag_t target_tag;

    OPAL_TRACE(1);

    OPAL_THREAD_LOCK(&orted_globals.mutex);

    if (orted_globals.debug_daemons) {
       opal_output(0, "[%lu,%lu,%lu] orted_recv_pls: received message from [%ld,%ld,%ld]",
                   ORTE_NAME_ARGS(orte_process_info.my_name),
                   ORTE_NAME_ARGS(sender));
    }

    /* unpack the command */
    n = 1;
    if (ORTE_SUCCESS != (ret = orte_dss.unpack(buffer, &command, &n, ORTE_DAEMON_CMD))) {
        ORTE_ERROR_LOG(ret);
        goto CLEANUP;
    }
    
    switch(command) {

        /****    KILL_LOCAL_PROCS   ****/
        case ORTE_DAEMON_KILL_LOCAL_PROCS:
            /* unpack the number of jobids */
            n = 1;
            if (ORTE_SUCCESS != (ret = orte_dss.unpack(buffer, &num_jobs, &n, ORTE_STD_CNTR))) {
                ORTE_ERROR_LOG(ret);
                goto CLEANUP;
            }
            /* unpack the array of jobids */
            jobs = (orte_jobid_t*)malloc(num_jobs * sizeof(orte_jobid_t));
            if (ORTE_SUCCESS != (ret = orte_dss.unpack(buffer, jobs, &num_jobs, ORTE_JOBID))) {
                ORTE_ERROR_LOG(ret);
                goto CLEANUP;
            }

            for (n=0; n < num_jobs; n++) {
                if (orted_globals.debug_daemons) {
                    opal_output(0, "[%lu,%lu,%lu] orted_recv_pls: received kill_local_procs for job %ld",
                                ORTE_NAME_ARGS(orte_process_info.my_name), (long)jobs[n]);
                }
                
                if (ORTE_SUCCESS != (ret = orte_odls.kill_local_procs(jobs[n], true))) {
                    ORTE_ERROR_LOG(ret);
                }
            }
            free(jobs);
            break;
            
        /****    SIGNAL_LOCAL_PROCS   ****/
        case ORTE_DAEMON_SIGNAL_LOCAL_PROCS:
            if (orted_globals.debug_daemons) {
                opal_output(0, "[%lu,%lu,%lu] orted_recv_pls: received signal_local_procs",
                            ORTE_NAME_ARGS(orte_process_info.my_name));
            }
            /* unpack the number of jobids */
            n = 1;
            if (ORTE_SUCCESS != (ret = orte_dss.unpack(buffer, &num_jobs, &n, ORTE_STD_CNTR))) {
                ORTE_ERROR_LOG(ret);
                goto CLEANUP;
            }
            /* unpack the array of jobids */
            jobs = (orte_jobid_t*)malloc(num_jobs * sizeof(orte_jobid_t));
            if (ORTE_SUCCESS != (ret = orte_dss.unpack(buffer, jobs, &num_jobs, ORTE_JOBID))) {
                ORTE_ERROR_LOG(ret);
                goto CLEANUP;
            }
                
            /* get the signal */
            n = 1;
            if (ORTE_SUCCESS != (ret = orte_dss.unpack(buffer, &signal, &n, ORTE_INT32))) {
                ORTE_ERROR_LOG(ret);
                goto CLEANUP;
            }
                
            /* signal them */
            if (ORTE_SUCCESS != (ret = orte_odls.signal_local_procs(NULL, signal))) {
                ORTE_ERROR_LOG(ret);
            }
            free(jobs);
            break;

            /****    ADD_LOCAL_PROCS   ****/
        case ORTE_DAEMON_ADD_LOCAL_PROCS:
            if (orted_globals.debug_daemons) {
                opal_output(0, "[%lu,%lu,%lu] orted_recv_pls: received add_local_procs",
                            ORTE_NAME_ARGS(orte_process_info.my_name));
            }
            /* unpack the notify data object */
            n = 1;
            if (ORTE_SUCCESS != (ret = orte_dss.unpack(buffer, &ndat, &n, ORTE_GPR_NOTIFY_DATA))) {
                ORTE_ERROR_LOG(ret);
                goto CLEANUP;
            }
            
            /* launch the processes */
            if (ORTE_SUCCESS != (ret = orte_odls.launch_local_procs(ndat, orted_globals.saved_environ))) {
                ORTE_ERROR_LOG(ret);
            }

            /* cleanup the memory */
            OBJ_RELEASE(ndat);
            break;
           
            /****    DELIVER A MESSAGE TO THE LOCAL PROCS    ****/
        case ORTE_DAEMON_MESSAGE_LOCAL_PROCS:
            if (orted_globals.debug_daemons) {
                opal_output(0, "[%lu,%lu,%lu] orted_recv_pls: received message_local_procs",
                            ORTE_NAME_ARGS(orte_process_info.my_name));
            }
            
            /* unpack the routing algorithm */
            n = 1;
            if (ORTE_SUCCESS != (ret = orte_dss.unpack(buffer, &routing_mode, &n, ORTE_DAEMON_CMD))) {
                ORTE_ERROR_LOG(ret);
                goto CLEANUP;
            }
            
            /* unpack the jobid of the procs that are to receive the message */
            n = 1;
            if (ORTE_SUCCESS != (ret = orte_dss.unpack(buffer, &job, &n, ORTE_JOBID))) {
                ORTE_ERROR_LOG(ret);
                goto CLEANUP;
            }
                
            /* unpack the tag where we are to deliver the message */
            n = 1;
            if (ORTE_SUCCESS != (ret = orte_dss.unpack(buffer, &target_tag, &n, ORTE_RML_TAG))) {
                ORTE_ERROR_LOG(ret);
                goto CLEANUP;
            }
                
            /* if the mode is BINOMIAL, then relay it on before doing anything else */
            if (ORTE_DAEMON_ROUTE_BINOMIAL == routing_mode) {
                if (ORTE_SUCCESS != (ret = binomial_route_msg(buffer, job, target_tag))) {
                    ORTE_ERROR_LOG(ret);
                    goto CLEANUP;
                }
            }
                
            relay = OBJ_NEW(orte_buffer_t);
            orte_dss.copy_payload(relay, buffer);
            
            /* now deliver the message to our children */
            if (ORTE_SUCCESS != (ret = orte_odls.deliver_message(job, relay, target_tag))) {
                ORTE_ERROR_LOG(ret);
                goto CLEANUP;
            }
            OBJ_RELEASE(relay);
            break;
    
            /****    EXIT COMMAND    ****/
        case ORTE_DAEMON_EXIT_CMD:
            /* eventually, we need to revise this so we only
             * exit if all our children are dead. For now, treat
             * the same as an exit_vm "hard kill" command
             */
            if (orted_globals.debug_daemons) {
                opal_output(0, "[%lu,%lu,%lu] orted_recv_pls: received exit",
                            ORTE_NAME_ARGS(orte_process_info.my_name));
            }
            /* no response to send here - we'll send it when nearly exit'd */
            orted_globals.exit_condition = true;
            opal_condition_signal(&orted_globals.condition);
            OPAL_THREAD_UNLOCK(&orted_globals.mutex);
            return;
            break;

            /****    HALT VM COMMAND    ****/
        case ORTE_DAEMON_HALT_VM_CMD:
            if (orted_globals.debug_daemons) {
                opal_output(0, "[%lu,%lu,%lu] orted_recv_pls: received halt vm",
                            ORTE_NAME_ARGS(orte_process_info.my_name));
            }
            /* no response to send here - we'll send it when nearly exit'd */
            orted_globals.exit_condition = true;
            opal_condition_signal(&orted_globals.condition);
            OPAL_THREAD_UNLOCK(&orted_globals.mutex);
            return;
            break;
            
            /****    WARMUP CONNECTION TO LOCAL PROC    ****/
        case ORTE_DAEMON_WARMUP_LOCAL_CONN:
            /* nothing to do here - just ignore it */
            if (orted_globals.debug_daemons) {
                opal_output(0, "[%lu,%lu,%lu] orted_recv_pls: received connection from local proc",
                            ORTE_NAME_ARGS(orte_process_info.my_name));
            }
            break;
            
        default:
            ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
            break;
    }

 CLEANUP:
    /* send an ack that command is done */
    OBJ_CONSTRUCT(&answer, orte_buffer_t);
    if (0 > orte_rml.send_buffer(sender, &answer, ORTE_RML_TAG_PLS_ORTED_ACK, 0)) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
    }
    OBJ_DESTRUCT(&answer);
    
    OPAL_THREAD_UNLOCK(&orted_globals.mutex);

    /* reissue the non-blocking receive */
    ret = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, ORTE_RML_TAG_PLS_ORTED, ORTE_RML_NON_PERSISTENT, orte_daemon_recv_pls, NULL);
    if (ret != ORTE_SUCCESS && ret != ORTE_ERR_NOT_IMPLEMENTED) {
        ORTE_ERROR_LOG(ret);
    }

    return;
}

static void halt_vm(void)
{
    int ret;
    opal_list_t attrs;
    opal_list_item_t *item;
    
    /* terminate the vm - this will also wake us up so we can exit */
    OBJ_CONSTRUCT(&attrs, opal_list_t);
    orte_rmgr.add_attribute(&attrs, ORTE_DAEMON_HARD_KILL, ORTE_UNDEF, NULL, ORTE_RMGR_ATTR_OVERRIDE);
    ret = orte_pls.terminate_orteds(&orte_abort_timeout, &attrs);
    while (NULL != (item = opal_list_remove_first(&attrs))) OBJ_RELEASE(item);
    OBJ_DESTRUCT(&attrs);
    
    /* Trigger the normal exit conditions */
    orted_globals.exit_condition = true;
    opal_condition_signal(&orted_globals.condition);
}

void orte_daemon_recv(int status, orte_process_name_t* sender,
                             orte_buffer_t *buffer, orte_rml_tag_t tag,
                             void* cbdata)
{
    orte_buffer_t *answer;
    orte_daemon_cmd_flag_t command;
    int ret;
    orte_std_cntr_t n;
    char *contact_info;
    
    OPAL_TRACE(1);
    
    OPAL_THREAD_LOCK(&orted_globals.mutex);
    
    if (orted_globals.debug_daemons) {
        opal_output(0, "[%lu,%lu,%lu] orted_recv: received message from [%ld,%ld,%ld]",
                    ORTE_NAME_ARGS(orte_process_info.my_name),
                    ORTE_NAME_ARGS(sender));
    }
    
    n = 1;
    if (ORTE_SUCCESS != (ret = orte_dss.unpack(buffer, &command, &n, ORTE_DAEMON_CMD))) {
        ORTE_ERROR_LOG(ret);
        OPAL_THREAD_UNLOCK(&orted_globals.mutex);
        return;
    }
    
    answer = OBJ_NEW(orte_buffer_t);
    if (NULL == answer) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        goto DONE;
    }
    
    switch(command) {
        /****    EXIT COMMAND    ****/
        case ORTE_DAEMON_EXIT_CMD:
            if (orted_globals.debug_daemons) {
                opal_output(0, "[%lu,%lu,%lu] orted_recv: received exit",
                            ORTE_NAME_ARGS(orte_process_info.my_name));
            }
            
            orted_globals.exit_condition = true;
            opal_condition_signal(&orted_globals.condition);
            break;

        /****    HALT VM COMMAND    ****/
        case ORTE_DAEMON_HALT_VM_CMD:
            if (orted_globals.debug_daemons) {
                opal_output(0, "[%lu,%lu,%lu] orted_recv: received halt vm",
                            ORTE_NAME_ARGS(orte_process_info.my_name));
            }
            halt_vm();
            break;
            
        /****     CONTACT QUERY COMMAND    ****/
        case ORTE_DAEMON_CONTACT_QUERY_CMD:
            /* send back contact info */
            contact_info = orte_rml.get_uri();
            
            if (NULL == contact_info) {
                ORTE_ERROR_LOG(ORTE_ERROR);
                goto CLEANUP;
            }
            
            if (ORTE_SUCCESS != (ret = orte_dss.pack(answer, &contact_info, 1, ORTE_STRING))) {
                ORTE_ERROR_LOG(ret);
                goto CLEANUP;
            }
            
            if (0 > orte_rml.send_buffer(sender, answer, tag, 0)) {
                ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
            }
            break;
        
        /****     HOSTFILE COMMAND    ****/
        case ORTE_DAEMON_HOSTFILE_CMD:
            ORTE_ERROR_LOG(ORTE_ERR_NOT_IMPLEMENTED);
            break;
        
        /****     SCRIPTFILE COMMAND    ****/
        case ORTE_DAEMON_SCRIPTFILE_CMD:
            ORTE_ERROR_LOG(ORTE_ERR_NOT_IMPLEMENTED);
            break;
        
        /****     HEARTBEAT COMMAND    ****/
        case ORTE_DAEMON_HEARTBEAT_CMD:
            ORTE_ERROR_LOG(ORTE_ERR_NOT_IMPLEMENTED);
            break;
            
        default:
            ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
    }
    
CLEANUP:
    OBJ_RELEASE(answer);
    
DONE:
    OPAL_THREAD_UNLOCK(&orted_globals.mutex);
    
    /* reissue the non-blocking receive */
    ret = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, ORTE_RML_TAG_DAEMON, ORTE_RML_NON_PERSISTENT, orte_daemon_recv, NULL);
    if (ret != ORTE_SUCCESS && ret != ORTE_ERR_NOT_IMPLEMENTED) {
        ORTE_ERROR_LOG(ret);
    }
    
    return;
}

static int binomial_route_msg(orte_buffer_t *buf, orte_jobid_t job, orte_rml_tag_t target_tag)
{
    orte_std_cntr_t n, num_daemons;
    int i, bitmap, peer, size, rank, hibit, mask;
    orte_process_name_t target;
    orte_daemon_cmd_flag_t command;
    orte_buffer_t *relay=NULL;
    int ret;
    
    /* initialize the relay buffer */
    relay = OBJ_NEW(orte_buffer_t);
    if (NULL == relay) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    /* tell the downstream daemons this is a message for their local procs */
    command=ORTE_DAEMON_MESSAGE_LOCAL_PROCS;
    if (ORTE_SUCCESS != (ret = orte_dss.pack(relay, &command, 1, ORTE_DAEMON_CMD))) {
        ORTE_ERROR_LOG(ret);
        goto CLEANUP;
    }
    
    /* tell the downstream daemons the routing algorithm is binomial */
    command = ORTE_DAEMON_ROUTE_BINOMIAL;
    if (ORTE_SUCCESS != (ret = orte_dss.pack(relay, &command, 1, ORTE_DAEMON_CMD))) {
        ORTE_ERROR_LOG(ret);
        goto CLEANUP;
    }
    
    /* tell the downstream daemons the jobid of the procs that are to receive the message */
    if (ORTE_SUCCESS != (ret = orte_dss.pack(relay, &job, 1, ORTE_JOBID))) {
        ORTE_ERROR_LOG(ret);
        goto CLEANUP;
    }
    
    /* tell the downstream daemons the tag where the message is to be delivered */
    if (ORTE_SUCCESS != (ret = orte_dss.pack(relay, &target_tag, 1, ORTE_RML_TAG))) {
        ORTE_ERROR_LOG(ret);
        goto CLEANUP;
    }
    
    /* unpack the current number of daemons */
    n = 1;
    if (ORTE_SUCCESS != (ret = orte_dss.unpack(buf, &num_daemons, &n, ORTE_STD_CNTR))) {
        ORTE_ERROR_LOG(ret);
        goto CLEANUP;
    }
    
    /* pass that value to the downstream daemons */
    if (ORTE_SUCCESS != (ret = orte_dss.pack(relay, &num_daemons, 1, ORTE_STD_CNTR))) {
        ORTE_ERROR_LOG(ret);
        goto CLEANUP;
    }
        
    /* copy the message payload to the relay buffer - this is non-destructive */
    if (ORTE_SUCCESS != (ret = orte_dss.copy_payload(relay, buf))) {
        ORTE_ERROR_LOG(ret);
        goto CLEANUP;
    }
        
    /* compute the bitmap */
    bitmap = opal_cube_dim((int)num_daemons);
    rank = (int)ORTE_PROC_MY_NAME->vpid;
    size = (int)num_daemons;
    
    hibit = opal_hibit(rank, bitmap);
    --bitmap;
    
    target.cellid = ORTE_PROC_MY_NAME->cellid;
    target.jobid = 0;
    for (i = hibit + 1, mask = 1 << i; i <= bitmap; ++i, mask <<= 1) {
        peer = rank | mask;
        if (peer < size) {
            target.vpid = (orte_vpid_t)peer;
            opal_output(0, "[%ld,%ld,%ld] relaying to [%ld,%ld,%ld]", ORTE_NAME_ARGS(ORTE_PROC_MY_NAME), ORTE_NAME_ARGS(&target));
            if (0 > (ret = orte_rml.send_buffer(&target, relay, ORTE_RML_TAG_PLS_ORTED, 0))) {
                ORTE_ERROR_LOG(ret);
                goto CLEANUP;
            }
        }
    }

CLEANUP:
    if (NULL != relay) OBJ_RELEASE(relay);
    
    return ORTE_SUCCESS;
}
