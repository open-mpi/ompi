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
#include "orte/constants.h"

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
#include "opal/runtime/opal_progress.h"
#include "opal/mca/base/mca_base_param.h"


#include "opal/dss/dss.h"
#include "orte/util/sys_info.h"
#include "orte/util/proc_info.h"
#include "orte/util/session_dir.h"
#include "orte/util/name_fns.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/grpcomm/grpcomm.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/rml/base/rml_contact.h"
#include "orte/mca/odls/odls.h"
#include "orte/mca/plm/plm.h"
#include "orte/mca/routed/routed.h"

#include "orte/runtime/runtime.h"
#include "orte/runtime/orte_globals.h"
#include "orte/runtime/orte_wait.h"
#include "orte/runtime/orte_wakeup.h"

#include "orte/orted/orted.h"

/*
 * Globals
 */
static int process_commands(orte_process_name_t* sender,
                            opal_buffer_t *buffer,
                            orte_rml_tag_t tag);


/* local callback function for non-blocking sends */
static void send_callback(int status, orte_process_name_t *peer,
                          opal_buffer_t *buf, orte_rml_tag_t tag,
                          void *cbdata)
{
    /* nothing to do here - just release buffer and return */
    OBJ_RELEASE(buf);
}

static void send_relay(int fd, short event, void *data)
{
    orte_message_event_t *mev = (orte_message_event_t*)data;
    opal_buffer_t *buffer = mev->buffer;
    orte_rml_tag_t tag = mev->tag;
    orte_grpcomm_mode_t relay_mode;
    opal_list_t recips;
    opal_list_item_t *item;
    int ret;
    
    OPAL_OUTPUT_VERBOSE((1, orte_debug_output,
                         "%s orte:daemon:send_relay",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    /* we pass the relay_mode in the mev "tag" field. This is a bit
     * of a hack as the two sizes may not exactly match. However, since
     * the rml_tag is an int32, it is doubtful we will ever see a
     * truncation error
     */
    relay_mode = (orte_grpcomm_mode_t)tag;
    
    /* setup a list of next recipients */
    OBJ_CONSTRUCT(&recips, opal_list_t);
    /* ask the active grpcomm module for the next recipients */
    if (ORTE_SUCCESS != (ret = orte_grpcomm.next_recipients(&recips, relay_mode))) {
        ORTE_ERROR_LOG(ret);
        goto CLEANUP;
    }
    /* send the message - if we are at the end of the chain, then there
     * will be nothing on the list, so remove_first will return NULL
     */
    while (NULL != (item = opal_list_remove_first(&recips))) {
        orte_namelist_t *target = (orte_namelist_t*)item;
        
        OPAL_OUTPUT_VERBOSE((1, orte_debug_output,
                             "%s orte:daemon:send_relay sending relay msg to %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(&target->name)));
        
        if (0 > (ret = orte_rml.send_buffer(&target->name, buffer, ORTE_RML_TAG_DAEMON, 0))) {
            ORTE_ERROR_LOG(ret);
            goto CLEANUP;
        }
        OBJ_RELEASE(item);
    }
    
CLEANUP:
    /* cleanup */
    OBJ_DESTRUCT(&recips);
    OBJ_RELEASE(mev);
}

void orte_daemon_recv(int status, orte_process_name_t* sender,
                      opal_buffer_t *buffer, orte_rml_tag_t tag,
                      void* cbdata)
{
    int ret;
    
    OPAL_OUTPUT_VERBOSE((1, orte_debug_output,
                         "%s orted_recv_cmd: received message from %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(sender)));

    /* don't process this right away - we need to get out of the recv before
     * we process the message as it may ask us to do something that involves
     * more messaging! Instead, setup an event so that the message gets processed
     * as soon as we leave the recv.
     *
     * The macro makes a copy of the buffer, which we release when processed - the incoming
     * buffer, however, is NOT released here, although its payload IS transferred
     * to the message buffer for later processing
     */
    ORTE_MESSAGE_EVENT(sender, buffer, tag, orte_daemon_cmd_processor);
    
    /* reissue the non-blocking receive */
    ret = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, ORTE_RML_TAG_DAEMON,
                                  ORTE_RML_NON_PERSISTENT, orte_daemon_recv, NULL);
    if (ret != ORTE_SUCCESS && ret != ORTE_ERR_NOT_IMPLEMENTED) {
        ORTE_ERROR_LOG(ret);
    }
    
    OPAL_OUTPUT_VERBOSE((1, orte_debug_output,
                         "%s orted_recv_cmd: reissued recv",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
}    
    
void orte_daemon_cmd_processor(int fd, short event, void *data)
{
    orte_message_event_t *mev = (orte_message_event_t*)data;
    orte_process_name_t *sender = &(mev->sender);
    opal_buffer_t *buffer = mev->buffer;
    orte_rml_tag_t tag = mev->tag;
    int ret;
    char *unpack_ptr;
    orte_std_cntr_t n;
    orte_daemon_cmd_flag_t command;

    OPAL_OUTPUT_VERBOSE((1, orte_debug_output,
                         "%s orte:daemon:cmd:processor called by %s for tag %ld",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(sender),
                        (long)(tag)));

    /* save the original buffer pointers */
    unpack_ptr = buffer->unpack_ptr;
    
    /* unpack the initial command */
    n = 1;
    if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &command, &n, ORTE_DAEMON_CMD))) {
        ORTE_ERROR_LOG(ret);
#if OMPI_ENABLE_DEBUG
        opal_output(0, "%s got message buffer from file %s line %d\n",
                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), mev->file, mev->line);
#endif
        goto CLEANUP;
    }
    
    /* see if this is a "process-and-relay" command */
    if (ORTE_DAEMON_PROCESS_AND_RELAY_CMD == command) {
        opal_buffer_t relay_buf;
        orte_grpcomm_mode_t relay_mode;
        orte_rml_tag_t relay_tag;
        
        /* unpack the routing mode in use */
        n = 1;
        if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &relay_mode, &n, ORTE_GRPCOMM_MODE))) {
            ORTE_ERROR_LOG(ret);
            goto CLEANUP;
        }
        /* we are going to use the message event's tag field to pass the relay mode
         * see comment in send_relay
         */
        relay_tag = (orte_rml_tag_t)relay_mode;
        /* initialize the relay buffer */
        OBJ_CONSTRUCT(&relay_buf, opal_buffer_t);
        /* tell the downstream daemons to process-and-relay */
        if (ORTE_SUCCESS != (ret = opal_dss.pack(&relay_buf, &command, 1, ORTE_DAEMON_CMD))) {
            ORTE_ERROR_LOG(ret);
            goto PROCESS;
        }
        /* tell the downstream daemons the routing algo in use */
        if (ORTE_SUCCESS != (ret = opal_dss.pack(&relay_buf, &relay_mode, 1, ORTE_GRPCOMM_MODE))) {
            ORTE_ERROR_LOG(ret);
            goto CLEANUP;
        }
        /* copy the message payload to the relay buffer - this is non-destructive */
        if (ORTE_SUCCESS != (ret = opal_dss.copy_payload(&relay_buf, buffer))) {
            ORTE_ERROR_LOG(ret);
            goto PROCESS;
        }
        /* setup an event to actually perform the relay */
        ORTE_MESSAGE_EVENT(sender, &relay_buf, relay_tag, send_relay);
    } else {
        /* rewind the buffer so we can process it correctly */
        buffer->unpack_ptr = unpack_ptr;
    }
    
PROCESS:
    /* process the command */
    if (ORTE_SUCCESS != (ret = process_commands(sender, buffer, tag))) {
        OPAL_OUTPUT_VERBOSE((1, orte_debug_output,
                             "%s orte:daemon:cmd:processor failed on error %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), ORTE_ERROR_NAME(ret)));
    }
    
    OPAL_OUTPUT_VERBOSE((1, orte_debug_output,
                         "%s orte:daemon:cmd:processor: processing commands completed",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

CLEANUP:
    OBJ_RELEASE(mev);
    return;
}    

static int process_commands(orte_process_name_t* sender,
                            opal_buffer_t *buffer,
                            orte_rml_tag_t tag)
{
    orte_daemon_cmd_flag_t command;
    opal_buffer_t *relay_msg;
    int ret;
    orte_std_cntr_t n;
    int32_t signal;
    orte_jobid_t job;
    orte_rml_tag_t target_tag;
    char *contact_info;
    opal_buffer_t *answer;
    orte_rml_cmd_flag_t rml_cmd;
    orte_job_t *jdata;

    /* unpack the command */
    n = 1;
    if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &command, &n, ORTE_DAEMON_CMD))) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }
    
    /* now process the command locally */
    switch(command) {

        /****    KILL_LOCAL_PROCS   ****/
        case ORTE_DAEMON_KILL_LOCAL_PROCS:
            /* unpack the jobid */
            n = 1;
            if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &job, &n, ORTE_JOBID))) {
                ORTE_ERROR_LOG(ret);
                goto CLEANUP;
            }

            if (ORTE_SUCCESS != (ret = orte_odls.kill_local_procs(job, true))) {
                ORTE_ERROR_LOG(ret);
            }
            break;
            
        /****    SIGNAL_LOCAL_PROCS   ****/
        case ORTE_DAEMON_SIGNAL_LOCAL_PROCS:
            if (orte_debug_daemons_flag) {
                opal_output(0, "%s orted_cmd: received signal_local_procs",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
            }
            /* unpack the jobid */
            n = 1;
            if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &job, &n, ORTE_JOBID))) {
                ORTE_ERROR_LOG(ret);
                goto CLEANUP;
            }
                
            /* get the signal */
            n = 1;
            if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &signal, &n, OPAL_INT32))) {
                ORTE_ERROR_LOG(ret);
                goto CLEANUP;
            }
                
            /* signal them */
            if (ORTE_SUCCESS != (ret = orte_odls.signal_local_procs(NULL, signal))) {
                ORTE_ERROR_LOG(ret);
            }
            break;

            /****    ADD_LOCAL_PROCS   ****/
        case ORTE_DAEMON_ADD_LOCAL_PROCS:
            if (orte_debug_daemons_flag) {
                opal_output(0, "%s orted_cmd: received add_local_procs",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
            }
            /* launch the processes */
            if (ORTE_SUCCESS != (ret = orte_odls.launch_local_procs(buffer))) {
                OPAL_OUTPUT_VERBOSE((1, orte_debug_output,
                                     "%s orted:comm:add_procs failed to launch on error %s",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), ORTE_ERROR_NAME(ret)));
            }
            break;
           
            /****    DELIVER A MESSAGE TO THE LOCAL PROCS    ****/
        case ORTE_DAEMON_MESSAGE_LOCAL_PROCS:
            if (orte_debug_daemons_flag) {
                opal_output(0, "%s orted_cmd: received message_local_procs",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
            }
                        
            /* unpack the jobid of the procs that are to receive the message */
            n = 1;
            if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &job, &n, ORTE_JOBID))) {
                ORTE_ERROR_LOG(ret);
                goto CLEANUP;
            }
                
            /* unpack the tag where we are to deliver the message */
            n = 1;
            if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &target_tag, &n, ORTE_RML_TAG))) {
                ORTE_ERROR_LOG(ret);
                goto CLEANUP;
            }
                
            relay_msg = OBJ_NEW(opal_buffer_t);
            opal_dss.copy_payload(relay_msg, buffer);
            
            /* if job=my_jobid, then this message is for us and not for our children */
            if (ORTE_PROC_MY_NAME->jobid == job) {
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
                 * them along the orted chain, the rml_update
                 * message contains contact info we may well need in order to do
                 * the relay! So we need to process those messages immediately.
                 * The only way to accomplish that is to (a) detect that the
                 * buffer is intended for those tags, and then (b) process
                 * those buffers here.
                 *
                 */
                if (ORTE_RML_TAG_RML_INFO_UPDATE == target_tag) {
                    n = 1;
                    if (ORTE_SUCCESS != (ret = opal_dss.unpack(relay_msg, &rml_cmd, &n, ORTE_RML_CMD))) {
                        ORTE_ERROR_LOG(ret);
                        goto CLEANUP;
                    }
                    /* initialize the routes to my peers - this will update the number
                     * of daemons in the system (i.e., orte_process_info.num_procs) as
                     * this might have changed
                     */
                    if (ORTE_SUCCESS != (ret = orte_routed.init_routes(ORTE_PROC_MY_NAME->jobid, relay_msg))) {
                        ORTE_ERROR_LOG(ret);
                        goto CLEANUP;
                    }
                } else {
                    /* just deliver it to ourselves */
                    if ((ret = orte_rml.send_buffer(ORTE_PROC_MY_NAME, relay_msg, target_tag, 0)) < 0) {
                        ORTE_ERROR_LOG(ret);
                    } else {
                        ret = ORTE_SUCCESS;
                        opal_progress(); /* give us a chance to move the message along */
                    }
                }
            } else {
                /* must be for our children - deliver the message */
                if (ORTE_SUCCESS != (ret = orte_odls.deliver_message(job, relay_msg, target_tag))) {
                    ORTE_ERROR_LOG(ret);
                }
            }
            OBJ_RELEASE(relay_msg);
            break;
    
            /****    EXIT COMMAND    ****/
        case ORTE_DAEMON_EXIT_CMD:
            if (orte_process_info.hnp) {
                /* if we are the HNP, do nothing - we will
                 * exit at our own sweet time
                 */
                return ORTE_SUCCESS;
            }
            /* eventually, we need to revise this so we only
             * exit if all our children are dead. For now, treat
             * the same as a "hard kill" command
             */
            if (orte_debug_daemons_flag) {
                opal_output(0, "%s orted_cmd: received exit",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
            }
            /* trigger our appropriate exit procedure
             * NOTE: this event will fire -after- any zero-time events
             * so any pending relays -do- get sent first
             */
            orte_wakeup(0);
            return ORTE_SUCCESS;
            break;

            /****    HALT VM COMMAND    ****/
        case ORTE_DAEMON_HALT_VM_CMD:
            if (orte_debug_daemons_flag) {
                opal_output(0, "%s orted_cmd: received halt vm",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
            }
            /* trigger our appropriate exit procedure
             * NOTE: this event will fire -after- any zero-time events
             * so any pending relays -do- get sent first
             */
            orte_wakeup(0);
            return ORTE_SUCCESS;
            break;
            
            /****    SPAWN JOB COMMAND    ****/
        case ORTE_DAEMON_SPAWN_JOB_CMD:
            if (orte_debug_daemons_flag) {
                opal_output(0, "%s orted_cmd: received spawn job",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
            }
            answer = OBJ_NEW(opal_buffer_t);
            job = ORTE_JOBID_INVALID;
            /* can only process this if we are the HNP */
            if (orte_process_info.hnp) {
                /* unpack the job data */
                n = 1;
                if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &jdata, &n, ORTE_JOB))) {
                    ORTE_ERROR_LOG(ret);
                    goto ANSWER_LAUNCH;
                }
                    
                /* launch it */
                if (ORTE_SUCCESS != (ret = orte_plm.spawn(jdata))) {
                    ORTE_ERROR_LOG(ret);
                    goto ANSWER_LAUNCH;
                }
                job = jdata->jobid;
            }
    ANSWER_LAUNCH:
            /* pack the jobid to be returned */
            if (ORTE_SUCCESS != (ret = opal_dss.pack(answer, &job, 1, ORTE_JOBID))) {
                ORTE_ERROR_LOG(ret);
                OBJ_RELEASE(answer);
                goto CLEANUP;
            }
            /* return response */
            if (0 > orte_rml.send_buffer_nb(sender, answer, ORTE_RML_TAG_TOOL, 0,
                                            send_callback, NULL)) {
                ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
                ret = ORTE_ERR_COMM_FAILURE;
            }
            return ORTE_SUCCESS;
            break;
            
            /****     CONTACT QUERY COMMAND    ****/
        case ORTE_DAEMON_CONTACT_QUERY_CMD:
            if (orte_debug_daemons_flag) {
                opal_output(0, "%s orted_cmd: received contact query",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
            }
            /* send back contact info */
            contact_info = orte_rml.get_contact_info();
            
            if (NULL == contact_info) {
                ORTE_ERROR_LOG(ORTE_ERROR);
                ret = ORTE_ERROR;
                goto CLEANUP;
            }
            
            /* setup buffer with answer */
            answer = OBJ_NEW(opal_buffer_t);
            if (ORTE_SUCCESS != (ret = opal_dss.pack(answer, &contact_info, 1, OPAL_STRING))) {
                ORTE_ERROR_LOG(ret);
                OBJ_RELEASE(answer);
                goto CLEANUP;
            }
            
            if (0 > orte_rml.send_buffer_nb(sender, answer, tag, 0,
                                            send_callback, NULL)) {
                ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
                ret = ORTE_ERR_COMM_FAILURE;
            }
            break;
            
            /****     REPORT_JOB_INFO_CMD COMMAND    ****/
        case ORTE_DAEMON_REPORT_JOB_INFO_CMD:
            if (orte_debug_daemons_flag) {
                opal_output(0, "%s orted_cmd: received job info query",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
            }
            /* if we are not the HNP, we can do nothing - report
             * back 0 procs so the tool won't hang
             */
            if (!orte_process_info.hnp) {
                orte_std_cntr_t zero=0;
                
                answer = OBJ_NEW(opal_buffer_t);
                if (ORTE_SUCCESS != (ret = opal_dss.pack(answer, &zero, 1, ORTE_STD_CNTR))) {
                    ORTE_ERROR_LOG(ret);
                    OBJ_RELEASE(answer);
                    goto CLEANUP;
                }
                /* callback function will release buffer */
                if (0 > orte_rml.send_buffer_nb(sender, answer, ORTE_RML_TAG_TOOL, 0,
                                                send_callback, NULL)) {
                    ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
                    ret = ORTE_ERR_COMM_FAILURE;
                }
            } else {
                /* if we are the HNP, process the request */
                orte_std_cntr_t i, num_jobs=0;
                orte_job_t **jobs=NULL, *jobdat;
                
                /* unpack the jobid */
                n = 1;
                if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &job, &n, ORTE_JOBID))) {
                    ORTE_ERROR_LOG(ret);
                    goto CLEANUP;
                }
                
                /* setup return */
                answer = OBJ_NEW(opal_buffer_t);
                
                /* if they asked for a specific job, then just get that info */
                if (ORTE_JOBID_WILDCARD != job) {
                    if (NULL != (jobdat = orte_get_job_data_object(job))) {
                        num_jobs = 1;
                        jobs = &jobdat;
                    }
                } else {
                    /* count number of jobs */
                    for (i=0; i < orte_job_data->size; i++) {
                        if (NULL == orte_job_data->addr[i]) break;
                        num_jobs++;
                    }
                    jobs = (orte_job_t**)orte_job_data->addr;
                }
                
                /* pack the answer */
                if (ORTE_SUCCESS != (ret = opal_dss.pack(answer, &num_jobs, 1, ORTE_STD_CNTR))) {
                    ORTE_ERROR_LOG(ret);
                    OBJ_RELEASE(answer);
                    goto CLEANUP;
                }
                if (0 < num_jobs) {
                    if (ORTE_SUCCESS != (ret = opal_dss.pack(answer, jobs, num_jobs, ORTE_JOB))) {
                        ORTE_ERROR_LOG(ret);
                        OBJ_RELEASE(answer);
                        goto CLEANUP;
                    }
                }
                /* callback function will release buffer */
                if (0 > orte_rml.send_buffer_nb(sender, answer, ORTE_RML_TAG_TOOL, 0,
                                                send_callback, NULL)) {
                    ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
                    ret = ORTE_ERR_COMM_FAILURE;
                }
           }
            break;
            
            /****     REPORT_NODE_INFO_CMD COMMAND    ****/
        case ORTE_DAEMON_REPORT_NODE_INFO_CMD:
            if (orte_debug_daemons_flag) {
                opal_output(0, "%s orted_cmd: received node info query",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
            }
            /* if we are not the HNP, we can do nothing - report
             * back 0 nodes so the tool won't hang
             */
            if (!orte_process_info.hnp) {
                orte_std_cntr_t zero=0;
                
                answer = OBJ_NEW(opal_buffer_t);
                if (ORTE_SUCCESS != (ret = opal_dss.pack(answer, &zero, 1, ORTE_STD_CNTR))) {
                    ORTE_ERROR_LOG(ret);
                    OBJ_RELEASE(answer);
                    goto CLEANUP;
                }
                /* callback function will release buffer */
                if (0 > orte_rml.send_buffer_nb(sender, answer, ORTE_RML_TAG_TOOL, 0,
                                                send_callback, NULL)) {
                    ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
                    ret = ORTE_ERR_COMM_FAILURE;
                }
            } else {
                /* if we are the HNP, process the request */
                orte_std_cntr_t i, num_nodes=0;
                orte_node_t **nodes;
                orte_nodeid_t nid;
                
                /* unpack the nodeid */
                n = 1;
                if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &nid, &n, ORTE_NODEID))) {
                    ORTE_ERROR_LOG(ret);
                    goto CLEANUP;
                }
                
                /* setup return */
                answer = OBJ_NEW(opal_buffer_t);
                
                /* if they asked for a specific node, then just get that info */
                if (ORTE_NODEID_WILDCARD != nid) {
                    /* find this node */
                    nodes = (orte_node_t**)orte_node_pool->addr;
                    for (i=0; i < orte_node_pool->size; i++) {
                        if (NULL == nodes[i]) break; /* stop when we get past the end of data */
                        if (nid == nodes[i]->nodeid) {
                            nodes = &nodes[i];
                            num_nodes = 1;
                            break;
                        }
                    }
                } else {
                    /* count number of nodes */
                    for (i=0; i < orte_node_pool->size; i++) {
                        if (NULL == orte_node_pool->addr[i]) break;
                        num_nodes++;
                    }
                    nodes = (orte_node_t**)orte_node_pool->addr;
                }
                
                /* pack the answer */
                if (ORTE_SUCCESS != (ret = opal_dss.pack(answer, &num_nodes, 1, ORTE_STD_CNTR))) {
                    ORTE_ERROR_LOG(ret);
                    OBJ_RELEASE(answer);
                    goto CLEANUP;
                }
                if (0 < num_nodes) {
                    if (ORTE_SUCCESS != (ret = opal_dss.pack(answer, nodes, num_nodes, ORTE_NODE))) {
                        ORTE_ERROR_LOG(ret);
                        OBJ_RELEASE(answer);
                        goto CLEANUP;
                    }
                }
                /* callback function will release buffer */
                if (0 > orte_rml.send_buffer_nb(sender, answer, ORTE_RML_TAG_TOOL, 0,
                                                send_callback, NULL)) {
                    ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
                    ret = ORTE_ERR_COMM_FAILURE;
                }
            }
            break;
            
            /****     REPORT_PROC_INFO_CMD COMMAND    ****/
        case ORTE_DAEMON_REPORT_PROC_INFO_CMD:
            if (orte_debug_daemons_flag) {
                opal_output(0, "%s orted_cmd: received proc info query",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
            }
            /* if we are not the HNP, we can do nothing - report
             * back 0 procs so the tool won't hang
             */
            if (!orte_process_info.hnp) {
                orte_std_cntr_t zero=0;
                
                answer = OBJ_NEW(opal_buffer_t);
                if (ORTE_SUCCESS != (ret = opal_dss.pack(answer, &zero, 1, ORTE_STD_CNTR))) {
                    ORTE_ERROR_LOG(ret);
                    OBJ_RELEASE(answer);
                    goto CLEANUP;
                }
                /* callback function will release buffer */
                if (0 > orte_rml.send_buffer_nb(sender, answer, ORTE_RML_TAG_TOOL, 0,
                                                send_callback, NULL)) {
                    ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
                    ret = ORTE_ERR_COMM_FAILURE;
                }
            } else {
                /* if we are the HNP, process the request */
                orte_job_t *jdata;
                orte_proc_t **procs=NULL;
                orte_vpid_t num_procs=0, vpid;
                orte_std_cntr_t i;
                
                /* setup the answer */
                answer = OBJ_NEW(opal_buffer_t);
                
                /* unpack the jobid */
                n = 1;
                if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &job, &n, ORTE_JOBID))) {
                    ORTE_ERROR_LOG(ret);
                    goto CLEANUP;
                }
                
                /* look up job data object */
                if (NULL == (jdata = orte_get_job_data_object(job))) {
                    ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
                    goto PACK_ANSWER;
                }
                
                /* unpack the vpid */
                n = 1;
                if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &vpid, &n, ORTE_VPID))) {
                    ORTE_ERROR_LOG(ret);
                    goto PACK_ANSWER;
                }

                /* if they asked for a specific proc, then just get that info */
                if (ORTE_VPID_WILDCARD != vpid) {
                    /* find this proc */
                    procs = (orte_proc_t**)jdata->procs->addr;
                    for (i=0; i < jdata->procs->size; i++) {
                        if (NULL == procs[i]) break; /* stop when we get past the end of data */
                        if (vpid == procs[i]->name.vpid) {
                            procs = &procs[i];
                            num_procs = 1;
                            break;
                        }
                    }
                } else {
                    procs = (orte_proc_t**)jdata->procs->addr;
                    num_procs = jdata->num_procs;
                }
                
PACK_ANSWER:
                /* pack number of procs */
                if (ORTE_SUCCESS != (ret = opal_dss.pack(answer, &num_procs, 1, ORTE_VPID))) {
                    ORTE_ERROR_LOG(ret);
                    goto SEND_ANSWER;
                }
                if (0 < num_procs) {
                    if (ORTE_SUCCESS != (ret = opal_dss.pack(answer, procs, jdata->num_procs, ORTE_PROC))) {
                        ORTE_ERROR_LOG(ret);
                        goto SEND_ANSWER;
                    }
                }
SEND_ANSWER:
                /* callback function will release buffer */
                if (0 > orte_rml.send_buffer_nb(sender, answer, ORTE_RML_TAG_TOOL, 0,
                                                send_callback, NULL)) {
                    ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
                    ret = ORTE_ERR_COMM_FAILURE;
                }
            }
            break;
            
            /****     ATTACH_STDIO COMMAND    ****/
        case ORTE_DAEMON_ATTACH_STDOUT_CMD:
            if (orte_debug_daemons_flag) {
                opal_output(0, "%s orted_cmd: received attach stdio cmd",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
            }
#if 0
            /* if we are not the HNP, we can do nothing - report
             * back error so the tool won't hang
             */
            if (!orte_process_info.hnp) {
                int status=ORTE_ERR_NOT_SUPPORTED;
                
                answer = OBJ_NEW(opal_buffer_t);
                if (ORTE_SUCCESS != (ret = opal_dss.pack(answer, &status, 1, OPAL_INT))) {
                    ORTE_ERROR_LOG(ret);
                    OBJ_RELEASE(answer);
                    goto CLEANUP;
                }
                /* callback function will release buffer */
                if (0 > orte_rml.send_buffer_nb(sender, answer, ORTE_RML_TAG_TOOL, 0,
                                                send_callback, NULL)) {
                    ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
                    ret = ORTE_ERR_COMM_FAILURE;
                }
            } else {
                /* if we are the HNP, process the request */
                int fd, status;
                orte_vpid_t vpid;
                orte_process_name_t source;
                
                /* setup the answer */
                answer = OBJ_NEW(opal_buffer_t);
                
                /* unpack the jobid */
                n = 1;
                if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &job, &n, ORTE_JOBID))) {
                    ORTE_ERROR_LOG(ret);
                    goto CLEANUP;
                }
                
                /* unpack the vpid */
                n = 1;
                if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &vpid, &n, ORTE_VPID))) {
                    ORTE_ERROR_LOG(ret);
                    goto PACK_ANSWER;
                }
                
                /* unpack the file descriptor */
                n = 1;
                if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &fd, &n, OPAL_INT))) {
                    ORTE_ERROR_LOG(ret);
                    goto PACK_ANSWER;
                }
                
                /* tell the iof to attach it */
                status = orte_iof.pull(
                /* if they asked for a specific proc, then just get that info */
                if (ORTE_VPID_WILDCARD != vpid) {
                    /* find this proc */
                    procs = (orte_proc_t**)jdata->procs->addr;
                    for (i=0; i < jdata->procs->size; i++) {
                        if (NULL == procs[i]) break; /* stop when we get past the end of data */
                        if (vpid == procs[i]->name.vpid) {
                            procs = &procs[i];
                            num_procs = 1;
                            break;
                        }
                    }
                } else {
                    procs = (orte_proc_t**)jdata->procs->addr;
                    num_procs = jdata->num_procs;
                }
                
PACK_ANSWER:
                /* pack number of procs */
                if (ORTE_SUCCESS != (ret = opal_dss.pack(answer, &num_procs, 1, ORTE_VPID))) {
                    ORTE_ERROR_LOG(ret);
                    goto SEND_ANSWER;
                }
                if (0 < num_procs) {
                    if (ORTE_SUCCESS != (ret = opal_dss.pack(answer, procs, jdata->num_procs, ORTE_PROC))) {
                        ORTE_ERROR_LOG(ret);
                        goto SEND_ANSWER;
                    }
                }
SEND_ANSWER:
                /* callback function will release buffer */
                if (0 > orte_rml.send_buffer_nb(sender, answer, ORTE_RML_TAG_TOOL, 0,
                                                send_callback, NULL)) {
                    ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
                    ret = ORTE_ERR_COMM_FAILURE;
                }
            }
#endif
            break;

            /****     HEARTBEAT COMMAND    ****/
        case ORTE_DAEMON_HEARTBEAT_CMD:
            ORTE_ERROR_LOG(ORTE_ERR_NOT_IMPLEMENTED);
            ret = ORTE_ERR_NOT_IMPLEMENTED;
            break;
            
            /****    SYNC FROM LOCAL PROC    ****/
        case ORTE_DAEMON_SYNC_BY_PROC:
            if (orte_debug_daemons_flag) {
                opal_output(0, "%s orted_recv: received sync from local proc %s",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                            ORTE_NAME_PRINT(sender));
            }
            if (ORTE_SUCCESS != (ret = orte_odls.require_sync(sender, buffer))) {
                ORTE_ERROR_LOG(ret);
                goto CLEANUP;
            }
            break;
            
        default:
            ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
            ret = ORTE_ERR_BAD_PARAM;
    }

CLEANUP:
    return ret;
}
