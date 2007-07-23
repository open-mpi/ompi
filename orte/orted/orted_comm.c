/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Cisco, Inc.  All rights reserved.
 * Copyright (c) 2007      Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "orte_config.h"

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

#include "orte/orte_constants.h"

#include "opal/event/event.h"
#include "opal/mca/base/base.h"
#include "opal/threads/mutex.h"
#include "opal/threads/condition.h"
#include "opal/util/bit_ops.h"
#include "opal/util/cmd_line.h"
#include "opal/util/daemon_init.h"
#include "opal/util/opal_environ.h"
#include "opal/util/os_path.h"
#include "opal/util/output.h"
#include "opal/util/printf.h"
#include "opal/util/show_help.h"
#include "opal/util/trace.h"
#include "opal/util/argv.h"
#include "opal/runtime/opal.h"
#include "opal/mca/base/mca_base_param.h"


#include "orte/dss/dss.h"
#include "orte/class/orte_value_array.h"
#include "orte/util/sys_info.h"
#include "orte/util/proc_info.h"
#include "orte/util/univ_info.h"
#include "orte/util/session_dir.h"
#include "orte/util/universe_setup_file_io.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/ns/ns.h"
#include "orte/mca/ras/ras.h"
#include "orte/mca/rds/rds.h"
#include "orte/mca/rmaps/rmaps.h"
#include "orte/mca/gpr/gpr.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/rml/base/rml_contact.h"
#include "orte/mca/smr/smr.h"
#include "orte/mca/rmgr/rmgr.h"
#include "orte/mca/rmgr/base/rmgr_private.h"
#include "orte/mca/odls/odls.h"
#include "orte/mca/pls/pls.h"


#include "orte/runtime/runtime.h"
#include "orte/runtime/params.h"

#include "orte/orted/orted.h"

/*
 * Globals
 */

static int binomial_route_msg(orte_process_name_t *sender,
                              orte_buffer_t *buf,
                              orte_rml_tag_t tag);

static int process_commands(orte_process_name_t* sender,
                            orte_buffer_t *buffer,
                            orte_rml_tag_t tag);


void orte_daemon_recv_routed(int status, orte_process_name_t* sender,
                             orte_buffer_t *buffer, orte_rml_tag_t tag,
                             void* cbdata)
{
    orte_daemon_cmd_flag_t routing_mode;
    int ret;
    orte_std_cntr_t n;

    OPAL_TRACE(1);

    OPAL_THREAD_LOCK(&orted_comm_mutex);

    if (orte_debug_daemons_flag) {
       opal_output(0, "%s orted_recv_routed: received message from %s",
                   ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                   ORTE_NAME_PRINT(sender));
    }

    /* unpack the routing algorithm */
    n = 1;
    if (ORTE_SUCCESS != (ret = orte_dss.unpack(buffer, &routing_mode, &n, ORTE_DAEMON_CMD))) {
        ORTE_ERROR_LOG(ret);
        goto CLEANUP;
    }

    /* if the mode is BINOMIAL, then handle that elsewhere */
    if (ORTE_DAEMON_ROUTE_BINOMIAL == routing_mode) {
        if (ORTE_SUCCESS != (ret = binomial_route_msg(sender, buffer, tag))) {
            ORTE_ERROR_LOG(ret);
            goto CLEANUP;
        }
    } else {
        /* process the command locally */
        if (ORTE_SUCCESS != (ret = process_commands(sender, buffer, tag))) {
            ORTE_ERROR_LOG(ret);
        }
    }

CLEANUP:
    OPAL_THREAD_UNLOCK(&orted_comm_mutex);

    /* reissue the non-blocking receive */
    ret = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, ORTE_RML_TAG_ORTED_ROUTED,
                                  ORTE_RML_NON_PERSISTENT, orte_daemon_recv_routed, NULL);
    if (ret != ORTE_SUCCESS && ret != ORTE_ERR_NOT_IMPLEMENTED) {
        ORTE_ERROR_LOG(ret);
    }
}    
    
void orte_daemon_recv(int status, orte_process_name_t* sender,
                      orte_buffer_t *buffer, orte_rml_tag_t tag,
                      void* cbdata)
{
    int ret;
    
    OPAL_TRACE(1);
    
    OPAL_THREAD_LOCK(&orted_comm_mutex);
    
    if (orte_debug_daemons_flag) {
        opal_output(0, "%s orted_recv_cmd: received message from %s",
                    ORTE_NAME_PRINT(orte_process_info.my_name),
                    ORTE_NAME_PRINT(sender));
    }
    
    /* process the command */
    if (ORTE_SUCCESS != (ret = process_commands(sender, buffer, tag))) {
        ORTE_ERROR_LOG(ret);
    }
    
    OPAL_THREAD_UNLOCK(&orted_comm_mutex);
    
    /* reissue the non-blocking receive */
    ret = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, ORTE_RML_TAG_DAEMON,
                                  ORTE_RML_NON_PERSISTENT, orte_daemon_recv, NULL);
    if (ret != ORTE_SUCCESS && ret != ORTE_ERR_NOT_IMPLEMENTED) {
        ORTE_ERROR_LOG(ret);
    }
}    

void orte_daemon_recv_gate(int status, orte_process_name_t* sender,
                           orte_buffer_t *buffer, orte_rml_tag_t tag,
                           void* cbdata)
{
    int rc;
    orte_std_cntr_t i;
    orte_gpr_notify_message_t *mesg;

    mesg = OBJ_NEW(orte_gpr_notify_message_t);
    if (NULL == mesg) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return;
    }
    i=1;
    if (ORTE_SUCCESS != (rc = orte_dss.unpack(buffer, &mesg, &i, ORTE_GPR_NOTIFY_MSG))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(mesg);
        return;
    }
    
    if (ORTE_SUCCESS != (rc = orte_gpr.deliver_notify_msg(mesg))) {
        ORTE_ERROR_LOG(rc);
    }
    OBJ_RELEASE(mesg);
    
    /* reissue the non-blocking receive */
    rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, ORTE_RML_TAG_XCAST_BARRIER,
                                 ORTE_RML_NON_PERSISTENT, orte_daemon_recv_gate, NULL);
    if (rc != ORTE_SUCCESS && rc != ORTE_ERR_NOT_IMPLEMENTED) {
        ORTE_ERROR_LOG(rc);
    }
}


static int process_commands(orte_process_name_t* sender,
                            orte_buffer_t *buffer,
                            orte_rml_tag_t tag)
{
    orte_daemon_cmd_flag_t command;
    orte_buffer_t *relay;
    int ret;
    orte_std_cntr_t n;
    int32_t signal;
    orte_gpr_notify_data_t *ndat;
    orte_jobid_t *jobs, job;
    orte_std_cntr_t num_jobs;
    orte_rml_tag_t target_tag;
    opal_list_t attrs;
    opal_list_item_t *item;
    char *contact_info;
    orte_buffer_t *answer;
    orte_rml_cmd_flag_t rml_cmd;
    orte_gpr_notify_message_t *mesg;
    char *unpack_ptr;

    /* unpack the command */
    n = 1;
    if (ORTE_SUCCESS != (ret = orte_dss.unpack(buffer, &command, &n, ORTE_DAEMON_CMD))) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }
    
    /* now process the command locally */
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
                free(jobs);
                goto CLEANUP;
            }

            for (n=0; n < num_jobs; n++) {
                if (orte_debug_daemons_flag) {
                    opal_output(0, "%s orted_cmd: received kill_local_procs for job %ld",
                                ORTE_NAME_PRINT(orte_process_info.my_name), (long)jobs[n]);
                }
                
                if (ORTE_SUCCESS != (ret = orte_odls.kill_local_procs(jobs[n], true))) {
                    ORTE_ERROR_LOG(ret);
                }
            }
            free(jobs);
            break;
            
        /****    SIGNAL_LOCAL_PROCS   ****/
        case ORTE_DAEMON_SIGNAL_LOCAL_PROCS:
            if (orte_debug_daemons_flag) {
                opal_output(0, "%s orted_cmd: received signal_local_procs",
                            ORTE_NAME_PRINT(orte_process_info.my_name));
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
                free(jobs);
                goto CLEANUP;
            }
                
            /* get the signal */
            n = 1;
            if (ORTE_SUCCESS != (ret = orte_dss.unpack(buffer, &signal, &n, ORTE_INT32))) {
                ORTE_ERROR_LOG(ret);
                free(jobs);
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
            if (orte_debug_daemons_flag) {
                opal_output(0, "%s orted_cmd: received add_local_procs",
                            ORTE_NAME_PRINT(orte_process_info.my_name));
            }
            /* unpack the notify data object */
            n = 1;
            if (ORTE_SUCCESS != (ret = orte_dss.unpack(buffer, &ndat, &n, ORTE_GPR_NOTIFY_DATA))) {
                ORTE_ERROR_LOG(ret);
                goto CLEANUP;
            }
            
            /* launch the processes */
            if (ORTE_SUCCESS != (ret = orte_odls.launch_local_procs(ndat))) {
                ORTE_ERROR_LOG(ret);
            }

            /* cleanup the memory */
            OBJ_RELEASE(ndat);
            break;
           
            /****    DELIVER A MESSAGE TO THE LOCAL PROCS    ****/
        case ORTE_DAEMON_MESSAGE_LOCAL_PROCS:
            if (orte_debug_daemons_flag) {
                opal_output(0, "%s orted_cmd: received message_local_procs",
                            ORTE_NAME_PRINT(orte_process_info.my_name));
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
                
            relay = OBJ_NEW(orte_buffer_t);
            orte_dss.copy_payload(relay, buffer);
            
            /* if job=0, then this message is for us and not for our children */
            if (0 == job) {
                /* if the target tag is our xcast_barrier or rml_update, then we have
                 * to handle the message as a special case. The RML has logic in it
                 * intended to make it easier to use. This special logic mandates that
                 * any message we "send" actually only goes into the queue for later
                 * transmission. Thus, since we are already in a recv when we enter
                 * the "process_commands" function, any attempt to "send" the relay
                 * buffer to ourselves will only be added to the queue - it won't
                 * actually be delivered until *after* we conclude the processing
                 * of the current recv.
                 *
                 * The problem here is that, for messages where we need to relay
                 * them along the orted chain, the xcast_barrier and rml_update
                 * messages contain contact info we may well need in order to do
                 * the relay! So we need to process those messages immediately.
                 * The only way to accomplish that is to (a) detect that the
                 * buffer is intended for those tags, and then (b) process
                 * those buffers here.
                 *
                 * NOTE: in the case of xcast_barrier, we *also* must send the
                 * message along anyway so that it will release us from the
                 * barrier! So we will process that info twice - can't be helped
                 * and won't harm anything
                 */
                if (ORTE_RML_TAG_XCAST_BARRIER == target_tag) {
                    /* need to preserve the relay buffer's pointers so it can be
                     * unpacked again at the barrier
                     */
                    unpack_ptr = relay->unpack_ptr;
                    mesg = OBJ_NEW(orte_gpr_notify_message_t);
                    n = 1;
                    if (ORTE_SUCCESS != (ret = orte_dss.unpack(relay, &mesg, &n, ORTE_GPR_NOTIFY_MSG))) {
                        ORTE_ERROR_LOG(ret);
                        OBJ_RELEASE(mesg);
                        goto CLEANUP;
                    }
                    orte_gpr.deliver_notify_msg(mesg);
                    OBJ_RELEASE(mesg);
                    /* restore the unpack ptr in the buffer */
                    relay->unpack_ptr = unpack_ptr;
                    /* make sure we queue this up for later delivery to release us from the barrier */
                    if ((ret = orte_rml.send_buffer(ORTE_PROC_MY_NAME, relay, target_tag, 0)) < 0) {
                        ORTE_ERROR_LOG(ret);
                    } else {
                        ret = ORTE_SUCCESS;
                    }
                } else if (ORTE_RML_TAG_RML_INFO_UPDATE == target_tag) {
                    n = 1;
                    if (ORTE_SUCCESS != (ret = orte_dss.unpack(relay, &rml_cmd, &n, ORTE_RML_CMD))) {
                        ORTE_ERROR_LOG(ret);
                        goto CLEANUP;
                    }
                    if (ORTE_SUCCESS != (ret = orte_dss.unpack(relay, &ndat, &n, ORTE_GPR_NOTIFY_DATA))) {
                        ORTE_ERROR_LOG(ret);
                        goto CLEANUP;
                    }
                    orte_rml_base_contact_info_notify(ndat, NULL);                    
                } else {
                    /* just deliver it to ourselves */
                    if ((ret = orte_rml.send_buffer(ORTE_PROC_MY_NAME, relay, target_tag, 0)) < 0) {
                        ORTE_ERROR_LOG(ret);
                    } else {
                        ret = ORTE_SUCCESS;
                    }
                }
            } else {
                /* must be for our children - deliver the message */
                if (ORTE_SUCCESS != (ret = orte_odls.deliver_message(job, relay, target_tag))) {
                    ORTE_ERROR_LOG(ret);
                }
            }
            OBJ_RELEASE(relay);
            break;
    
            /****    EXIT COMMAND    ****/
        case ORTE_DAEMON_EXIT_CMD:
            if (orte_orterun) {
                /* if we are mpirun, do nothing - we will
                 * exit at our own sweet time
                 */
                OPAL_THREAD_UNLOCK(&orted_comm_mutex);
                return ORTE_SUCCESS;
            }
            /* eventually, we need to revise this so we only
             * exit if all our children are dead. For now, treat
             * the same as an exit_vm "hard kill" command
             */
            if (orte_debug_daemons_flag) {
                opal_output(0, "%s orted_cmd: received exit",
                            ORTE_NAME_PRINT(orte_process_info.my_name));
            }
            /* no response to send here - we'll send it when nearly exit'd */
            orted_comm_exit_cond = true;
            opal_condition_signal(&orted_comm_cond);
            /* have to unlock here as we are waking up and will
             * do things inside the orted
             */
            OPAL_THREAD_UNLOCK(&orted_comm_mutex);
            return ORTE_SUCCESS;
            break;

            /****    HALT VM COMMAND    ****/
        case ORTE_DAEMON_HALT_VM_CMD:
            if (orte_orterun) {
                /* if we are mpirun, do nothing - we will
                * exit at our own sweet time
                */
                OPAL_THREAD_UNLOCK(&orted_comm_mutex);
                return ORTE_SUCCESS;
            }
            if (orte_debug_daemons_flag) {
                opal_output(0, "%s orted_cmd: received halt vm",
                            ORTE_NAME_PRINT(orte_process_info.my_name));
            }
            /* if we are the HNP, then terminate all orteds reporting to us */
            if (orte_process_info.seed) {
                OBJ_CONSTRUCT(&attrs, opal_list_t);
                orte_rmgr.add_attribute(&attrs, ORTE_DAEMON_HARD_KILL, ORTE_UNDEF, NULL, ORTE_RMGR_ATTR_OVERRIDE);
                ret = orte_pls.terminate_orteds(&orte_abort_timeout, &attrs);
                while (NULL != (item = opal_list_remove_first(&attrs))) OBJ_RELEASE(item);
                OBJ_DESTRUCT(&attrs);                
            }
            /* wake up so we can exit too */
            orted_comm_exit_cond = true;
            opal_condition_signal(&orted_comm_cond);
            /* have to unlock here as we are waking up and will
            * do things inside the orted
            */
            OPAL_THREAD_UNLOCK(&orted_comm_mutex);
            return ORTE_SUCCESS;
            break;
            
            /****     CONTACT QUERY COMMAND    ****/
        case ORTE_DAEMON_CONTACT_QUERY_CMD:
            if (orte_debug_daemons_flag) {
                opal_output(0, "%s orted_cmd: received contact query",
                            ORTE_NAME_PRINT(orte_process_info.my_name));
            }
            /* send back contact info */
            contact_info = orte_rml.get_contact_info();
            
            if (NULL == contact_info) {
                ORTE_ERROR_LOG(ORTE_ERROR);
                ret = ORTE_ERROR;
                goto CLEANUP;
            }
            
                /* setup buffer with answer */
            answer = OBJ_NEW(orte_buffer_t);
            if (ORTE_SUCCESS != (ret = orte_dss.pack(answer, &contact_info, 1, ORTE_STRING))) {
                ORTE_ERROR_LOG(ret);
                OBJ_RELEASE(answer);
                goto CLEANUP;
            }
            
            if (0 > orte_rml.send_buffer(sender, answer, tag, 0)) {
                ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
                ret = ORTE_ERR_COMM_FAILURE;
            }
            OBJ_RELEASE(answer);
            break;
            
            /****     HOSTFILE COMMAND    ****/
        case ORTE_DAEMON_HOSTFILE_CMD:
            ORTE_ERROR_LOG(ORTE_ERR_NOT_IMPLEMENTED);
            ret = ORTE_ERR_NOT_IMPLEMENTED;
            break;
            
            /****     SCRIPTFILE COMMAND    ****/
        case ORTE_DAEMON_SCRIPTFILE_CMD:
            ORTE_ERROR_LOG(ORTE_ERR_NOT_IMPLEMENTED);
            ret = ORTE_ERR_NOT_IMPLEMENTED;
            break;
            
            /****     HEARTBEAT COMMAND    ****/
        case ORTE_DAEMON_HEARTBEAT_CMD:
            ORTE_ERROR_LOG(ORTE_ERR_NOT_IMPLEMENTED);
            ret = ORTE_ERR_NOT_IMPLEMENTED;
            break;
            
            /****    WARMUP CONNECTION TO LOCAL PROC    ****/
        case ORTE_DAEMON_WARMUP_LOCAL_CONN:
            /* nothing to do here - just ignore it */
            if (orte_debug_daemons_flag) {
                opal_output(0, "%s orted_recv: received connection from local proc",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
            }
            ret = ORTE_SUCCESS;
            break;
            
        default:
            ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
            ret = ORTE_ERR_BAD_PARAM;
    }

CLEANUP:
    return ret;
}


static int binomial_route_msg(orte_process_name_t *sender,
                              orte_buffer_t *buf,
                              orte_rml_tag_t tag)
{
    orte_daemon_cmd_flag_t mode;
    orte_std_cntr_t n, num_daemons;
    int i, bitmap, peer, size, rank, hibit, mask;
    orte_process_name_t target;
    orte_buffer_t *relay;
    int ret;
    
    /* initialize the relay buffer */
    relay = OBJ_NEW(orte_buffer_t);
    if (NULL == relay) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    /* tell the downstream daemons the routing algorithm is binomial */
    mode = ORTE_DAEMON_ROUTE_BINOMIAL;
    if (ORTE_SUCCESS != (ret = orte_dss.pack(relay, &mode, 1, ORTE_DAEMON_CMD))) {
        ORTE_ERROR_LOG(ret);
        goto CLEANUP;
    }
    
    /* unpack the current number of daemons - we need it here! */
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
        
    /* copy the message payload to the relay buffer - this is non-destructive
     * Note that this still includes the target job and target tag data
     * required for eventual delivery of the payload
     */
    if (ORTE_SUCCESS != (ret = orte_dss.copy_payload(relay, buf))) {
        ORTE_ERROR_LOG(ret);
        goto CLEANUP;
    }
        
    /* process the command locally - we need to do this prior to attempting
     * to send the message to the next recipient in case this message
     * contains address information for that recipient. If we don't, then
     * the send will fail
     */
    if (ORTE_SUCCESS != (ret = process_commands(sender, buf, tag))) {
        ORTE_ERROR_LOG(ret);
    }

    /* compute the bitmap */
    bitmap = opal_cube_dim((int)num_daemons);
    rank = (int)ORTE_PROC_MY_NAME->vpid;
    size = (int)num_daemons;
    
    hibit = opal_hibit(rank, bitmap);
    --bitmap;
    
    target.jobid = 0;
    for (i = hibit + 1, mask = 1 << i; i <= bitmap; ++i, mask <<= 1) {
        peer = rank | mask;
        if (peer < size) {
            target.vpid = (orte_vpid_t)peer;
            if (0 > (ret = orte_rml.send_buffer(&target, relay, ORTE_RML_TAG_ORTED_ROUTED, 0))) {
                ORTE_ERROR_LOG(ret);
                goto CLEANUP;
            }
        }
    }

CLEANUP:
    OBJ_RELEASE(relay);
    
    return ORTE_SUCCESS;
}
