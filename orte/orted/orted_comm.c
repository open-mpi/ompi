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
 * Copyright (c) 2009      Sun Microsystems, Inc. All rights reserved.
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


#include "opal/class/opal_pointer_array.h"
#include "opal/event/event.h"
#include "opal/mca/base/base.h"
#include "opal/threads/mutex.h"
#include "opal/threads/condition.h"
#include "opal/util/bit_ops.h"
#include "opal/util/cmd_line.h"
#include "opal/util/daemon_init.h"
#include "opal/util/opal_environ.h"
#include "opal/util/os_path.h"
#include "opal/util/printf.h"
#include "opal/util/trace.h"
#include "opal/util/argv.h"
#include "opal/runtime/opal.h"
#include "opal/runtime/opal_progress.h"
#include "opal/mca/base/mca_base_param.h"


#include "opal/dss/dss.h"
#include "orte/util/show_help.h"
#include "orte/util/proc_info.h"
#include "orte/util/session_dir.h"
#include "orte/util/name_fns.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/grpcomm/grpcomm.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/rml/base/rml_contact.h"
#include "orte/mca/odls/odls.h"
#include "orte/mca/odls/base/base.h"
#include "orte/mca/odls/base/odls_private.h"
#include "orte/mca/plm/plm.h"
#include "orte/mca/plm/base/plm_private.h"
#include "orte/mca/routed/routed.h"

#include "orte/runtime/runtime.h"
#include "orte/runtime/orte_globals.h"
#include "orte/runtime/orte_wait.h"

#include "orte/orted/orted.h"

/*
 * Globals
 */
static int process_commands(orte_process_name_t* sender,
                            opal_buffer_t *buffer,
                            orte_rml_tag_t tag);

static bool exit_reqd;

static void send_relay(opal_buffer_t *buf)
{
    opal_list_t recips;
    opal_list_item_t *item;
    orte_namelist_t *nm;
    orte_process_name_t target;
    int ret;
    
    OPAL_OUTPUT_VERBOSE((1, orte_debug_output,
                         "%s orte:daemon:send_relay",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    /* get the list of next recipients from the routed module */
    OBJ_CONSTRUCT(&recips, opal_list_t);
    /* ignore returned parent vpid - we don't care here */
    orte_routed.get_routing_tree(ORTE_PROC_MY_NAME->jobid, &recips);
    
    /* if list is empty, nothing for us to do */
    if (opal_list_is_empty(&recips)) {
        OPAL_OUTPUT_VERBOSE((1, orte_debug_output,
                             "%s orte:daemon:send_relay - recipient list is empty!",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        goto CLEANUP;
    }
    
    /* send the message to each recipient on list, deconstructing it as we go */
    target.jobid = ORTE_PROC_MY_NAME->jobid;
    while (NULL != (item = opal_list_remove_first(&recips))) {
        nm = (orte_namelist_t*)item;
        
        OPAL_OUTPUT_VERBOSE((1, orte_debug_output,
                             "%s orte:daemon:send_relay sending relay msg to %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_VPID_PRINT(nm->name.vpid)));
        
        /* retain buffer so callback function can release it */
        target.vpid = nm->name.vpid;
        if (0 > (ret = orte_rml.send_buffer(&target, buf, ORTE_RML_TAG_DAEMON, 0))) {
            ORTE_ERROR_LOG(ret);
            goto CLEANUP;
        }
        OBJ_RELEASE(nm);
    }
    
CLEANUP:
    /* cleanup */
    OBJ_DESTRUCT(&recips);
}

void orte_daemon_recv(int status, orte_process_name_t* sender,
                      opal_buffer_t *buffer, orte_rml_tag_t tag,
                      void* cbdata)
{
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
    
    OPAL_OUTPUT_VERBOSE((1, orte_debug_output,
                         "%s orted_recv_cmd: reissued recv",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
}    

static int num_recursions=0;
static int wait_time=1;
#define MAX_RECURSIONS 24

void orte_daemon_cmd_processor(int fd, short event, void *data)
{
    orte_message_event_t *mev = (orte_message_event_t*)data;
    orte_process_name_t *sender = &(mev->sender);
    opal_buffer_t *buffer = mev->buffer;
    orte_rml_tag_t tag = mev->tag, target_tag;
    orte_jobid_t job;
    int ret;
    char *unpack_ptr, *save;
    orte_std_cntr_t n;
    orte_daemon_cmd_flag_t command;

    /* check to see if we are in a progress recursion */
    if (orte_process_info.daemon && 1 < (ret = opal_progress_recursion_depth())) {
        /* if we are in a recursion, we want to repost the message event
         * so the progress engine can work its way back up to the top
         * of the stack. Given that this could happen multiple times,
         * we have to be careful to increase the time we wait so that
         * we provide enough time - but not more time than necessary - for
         * the stack to clear
         */
        OPAL_OUTPUT_VERBOSE((1, orte_debug_output,
                             "%s orte:daemon:cmd:processor in recursion depth %d\n\treposting %s for tag %ld",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ret,
                             ORTE_NAME_PRINT(sender),
                             (long)(tag)));
        if (MAX_RECURSIONS < num_recursions) {
            /* we need to abort if we get too far down this path */
            opal_output(0, "%s ORTED_CMD_PROCESSOR: STUCK IN INFINITE LOOP - ABORTING",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
            OBJ_RELEASE(mev);
            /* make sure our local procs are dead - but don't update their state
             * on the HNP as this may be redundant
             */
            orte_odls.kill_local_procs(ORTE_JOBID_WILDCARD, false);
            
            /* do -not- call finalize as this will send a message to the HNP
             * indicating clean termination! Instead, just forcibly cleanup
             * the local session_dir tree and abort
             */
            orte_session_dir_cleanup(ORTE_JOBID_WILDCARD);
            
            abort();
        }
        wait_time = wait_time * 2;
        ++num_recursions;
        ORTE_MESSAGE_EVENT_DELAY(wait_time, mev);
        return;
    }
    wait_time = 1;
    num_recursions = 0;
    
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
    
    /* see if this is a "process-and-relay" command - i.e., an xcast is underway */
    if (ORTE_DAEMON_PROCESS_AND_RELAY_CMD == command) {
        /* get the target jobid and tag */
        n = 1;
        if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &job, &n, ORTE_JOBID))) {
            ORTE_ERROR_LOG(ret);
            goto CLEANUP;
        }
        n = 1;
        if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &target_tag, &n, ORTE_RML_TAG))) {
            ORTE_ERROR_LOG(ret);
            goto CLEANUP;
        }
        /* save this buffer location */
        save = buffer->unpack_ptr;
        /* unpack the command that will actually be executed */
        n = 1;
        if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &command, &n, ORTE_DAEMON_CMD))) {
            ORTE_ERROR_LOG(ret);
            goto CLEANUP;
        }
        /* is this an add-procs cmd? */
        if (ORTE_DAEMON_ADD_LOCAL_PROCS == command) {
            /* yes - then it contains daemon update info - process it */
            if (ORTE_SUCCESS != (ret = orte_odls_base_default_update_daemon_info(buffer))) {
                ORTE_ERROR_LOG(ret);
                goto CLEANUP;
            }
            /* flag this location */
            save = buffer->unpack_ptr;
        }
        
        /* rewind the buffer to the right place for processing the cmd */
        buffer->unpack_ptr = save;
        
        /* init flag */
        exit_reqd = false;
        
        /* process the command */
        if (ORTE_SUCCESS != (ret = process_commands(sender, buffer, tag))) {
            OPAL_OUTPUT_VERBOSE((1, orte_debug_output,
                                 "%s orte:daemon:cmd:processor failed on error %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), ORTE_ERROR_NAME(ret)));
        }
        
        /* rewind the buffer to the beginning */
        buffer->unpack_ptr = unpack_ptr;
        /* do the relay */
        send_relay(buffer);
        
        /* if we need to exit, do so now */
        if (exit_reqd) {
            orte_trigger_event(&orte_exit);
        }
        
        /* done */
        goto CLEANUP;
        
    } else {
        /* rewind the buffer so we can process it correctly */
        buffer->unpack_ptr = unpack_ptr;
    }
    
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
    /* reissue the non-blocking receive */
    ret = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, ORTE_RML_TAG_DAEMON,
                                  ORTE_RML_NON_PERSISTENT, orte_daemon_recv, NULL);
    if (ret != ORTE_SUCCESS && ret != ORTE_ERR_NOT_IMPLEMENTED) {
        ORTE_ERROR_LOG(ret);
    }
    
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
    orte_process_name_t proc;
    int32_t status;

    /* unpack the command */
    n = 1;
    if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &command, &n, ORTE_DAEMON_CMD))) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }
    
    /* now process the command locally */
    switch(command) {

        /****    NULL    ****/
        case ORTE_DAEMON_NULL_CMD:
            ret = ORTE_SUCCESS;
            break;
            
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

            /* Convert SIGTSTP to SIGSTOP so we can suspend a.out */
            if (SIGTSTP == signal) {
                if (orte_debug_daemons_flag) {
                    opal_output(0, "%s orted_cmd: converted SIGTSTP to SIGSTOP before delivering",
                                ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
                }
                signal = SIGSTOP;
            }

            if (orte_debug_daemons_flag) {
                opal_output(0, "%s orted_cmd: received signal_local_procs, delivering signal %d",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                            signal);
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
           
            /****    TREE_SPAWN   ****/
        case ORTE_DAEMON_TREE_SPAWN:
            if (orte_debug_daemons_flag) {
                opal_output(0, "%s orted_cmd: received tree_spawn",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
            }
            /* if the PLM supports remote spawn, pass it all along */
            if (NULL != orte_plm.remote_spawn) {
                if (ORTE_SUCCESS != (ret = orte_plm.remote_spawn(buffer))) {
                    ORTE_ERROR_LOG(ret);
                }
            } else {
                opal_output(0, "%s remote spawn is NULL!", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
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
                
            OPAL_OUTPUT_VERBOSE((1, orte_debug_output,
                                 "%s orted:comm:message_local_procs delivering message to job %s tag %d",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_JOBID_PRINT(job), (int)target_tag));

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
    
            /****    COLLECTIVE DATA COMMAND     ****/
        case ORTE_DAEMON_COLL_CMD:
            if (orte_debug_daemons_flag) {
                opal_output(0, "%s orted_cmd: received collective data cmd",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
            }
            if (ORTE_SUCCESS != (ret = orte_odls.collect_data(sender, buffer))) {
                ORTE_ERROR_LOG(ret);
            }
            break;
            
            /****    WAITPID_FIRED COMMAND     ****/
        case ORTE_DAEMON_WAITPID_FIRED:
            if (orte_debug_daemons_flag) {
                opal_output(0, "%s orted_cmd: received waitpid_fired cmd",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
            }
            /* unpack the name of the proc that terminated */
            n = 1;
            if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &proc, &n, ORTE_NAME))) {
                ORTE_ERROR_LOG(ret);
                goto CLEANUP;
            }
            /* unpack the termination status */
            n = 1;
            if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &status, &n, OPAL_INT32))) {
                ORTE_ERROR_LOG(ret);
                goto CLEANUP;
            }
            /* pass it down for processing */
            orte_base_default_waitpid_fired(&proc, status);
            break;
            
            
            /****    IOF_COMPLETE COMMAND     ****/
        case ORTE_DAEMON_IOF_COMPLETE:
            if (orte_debug_daemons_flag) {
                opal_output(0, "%s orted_cmd: received iof_complete cmd",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
            }
            /* unpack the name of the proc that completed */
            n = 1;
            if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &proc, &n, ORTE_NAME))) {
                ORTE_ERROR_LOG(ret);
                goto CLEANUP;
            }
            /* pass it down for processing */
            orte_odls_base_notify_iof_complete(&proc);
            break;

            /****    EXIT COMMAND    ****/
        case ORTE_DAEMON_EXIT_WITH_REPLY_CMD:
            if (orte_debug_daemons_flag) {
                opal_output(0, "%s orted_cmd: received exit",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
            }
            /* if we are the HNP, kill our local procs and
             * flag we are exited - but don't yet exit
             */
            if (orte_process_info.hnp) {
                orte_job_t *daemons;
                orte_proc_t **procs;
                /* if we are the HNP, ensure our local procs are terminated */
                orte_odls.kill_local_procs(ORTE_JOBID_WILDCARD, false);
                /* now lookup the daemon job object */
                if (NULL == (daemons = orte_get_job_data_object(ORTE_PROC_MY_NAME->jobid))) {
                    ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
                    return ORTE_ERR_NOT_FOUND;
                }
                procs = (orte_proc_t**)daemons->procs->addr;
                /* declare us terminated so things can exit cleanly */
                procs[0]->state = ORTE_PROC_STATE_TERMINATED;
                daemons->num_terminated++;
                /* need to check for job complete as otherwise this doesn't
                 * get triggered in single-daemon systems
                 */
                orte_plm_base_check_job_completed(daemons);
               /* all done! */
                return ORTE_SUCCESS;
            }
            /* if we are not the HNP, send a message to the HNP telling
             * it we are leaving - and then trigger our exit
             */
            {
                opal_buffer_t ack;
                orte_proc_state_t state=ORTE_PROC_STATE_TERMINATED;
                orte_exit_code_t exit_code=0;
                orte_plm_cmd_flag_t cmd = ORTE_PLM_UPDATE_PROC_STATE;
                
                OBJ_CONSTRUCT(&ack, opal_buffer_t);
                opal_dss.pack(&ack, &cmd, 1, ORTE_PLM_CMD);
                opal_dss.pack(&ack, &(ORTE_PROC_MY_NAME->jobid), 1, ORTE_JOBID);        
                opal_dss.pack(&ack, &(ORTE_PROC_MY_NAME->vpid), 1, ORTE_VPID);        
                opal_dss.pack(&ack, &state, 1, ORTE_PROC_STATE);        
                opal_dss.pack(&ack, &exit_code, 1, ORTE_EXIT_CODE);
                orte_rml.send_buffer(ORTE_PROC_MY_HNP, &ack, ORTE_RML_TAG_PLM, 0);
                OBJ_DESTRUCT(&ack);
            }
            exit_reqd = true;
            return ORTE_SUCCESS;
            break;
            
            /****    EXIT_NO_REPLY COMMAND    ****/
        case ORTE_DAEMON_EXIT_NO_REPLY_CMD:
            if (orte_debug_daemons_flag) {
                opal_output(0, "%s orted_cmd: received exit_no_reply",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
            }
            /* if we are the HNP, kill our local procs and
             * flag we are exited - but don't yet exit
             */
            if (orte_process_info.hnp) {
                orte_job_t *daemons;
                orte_proc_t **procs;
                /* if we are the HNP, ensure our local procs are terminated */
                orte_odls.kill_local_procs(ORTE_JOBID_WILDCARD, false);
                /* now lookup the daemon job object */
                if (NULL == (daemons = orte_get_job_data_object(ORTE_PROC_MY_NAME->jobid))) {
                    ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
                    return ORTE_ERR_NOT_FOUND;
                }
                procs = (orte_proc_t**)daemons->procs->addr;
                /* declare us terminated so things can exit cleanly */
                procs[0]->state = ORTE_PROC_STATE_TERMINATED;
                daemons->num_terminated++;
                /* There is nothing more to do here - actual exit will be
                 * accomplished by the plm
                 */
                return ORTE_SUCCESS;
            }
            exit_reqd = true;
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
            orte_trigger_event(&orte_exit);
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
            if (0 > orte_rml.send_buffer(sender, answer, ORTE_RML_TAG_TOOL, 0)) {
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
            
            if (0 > orte_rml.send_buffer(sender, answer, tag, 0)) {
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
                if (0 > orte_rml.send_buffer(sender, answer, ORTE_RML_TAG_TOOL, 0)) {
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
                if (0 > orte_rml.send_buffer(sender, answer, ORTE_RML_TAG_TOOL, 0)) {
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
                if (0 > orte_rml.send_buffer(sender, answer, ORTE_RML_TAG_TOOL, 0)) {
                    ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
                    ret = ORTE_ERR_COMM_FAILURE;
                }
            } else {
                /* if we are the HNP, process the request */
                orte_std_cntr_t i, num_nodes=0;
                orte_node_t **nodes;
                char *nid;
                
                /* unpack the nodename */
                n = 1;
                if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &nid, &n, OPAL_STRING))) {
                    ORTE_ERROR_LOG(ret);
                    goto CLEANUP;
                }
                
                /* setup return */
                answer = OBJ_NEW(opal_buffer_t);
                
                /* if they asked for a specific node, then just get that info */
                if (NULL != nid) {
                    /* find this node */
                    nodes = (orte_node_t**)orte_node_pool->addr;
                    for (i=0; i < orte_node_pool->size; i++) {
                        if (NULL == nodes[i]) break; /* stop when we get past the end of data */
                        if (0 == strcmp(nid, nodes[i]->name)) {
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
                if (0 > orte_rml.send_buffer(sender, answer, ORTE_RML_TAG_TOOL, 0)) {
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
                if (0 > orte_rml.send_buffer(sender, answer, ORTE_RML_TAG_TOOL, 0)) {
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
                if (0 > orte_rml.send_buffer(sender, answer, ORTE_RML_TAG_TOOL, 0)) {
                    ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
                    ret = ORTE_ERR_COMM_FAILURE;
                }
            }
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
            if (ORTE_SUCCESS != (ret = orte_odls.require_sync(sender, buffer, false))) {
                ORTE_ERROR_LOG(ret);
                goto CLEANUP;
            }
            break;
            
        case ORTE_DAEMON_SYNC_WANT_NIDMAP:
            if (orte_debug_daemons_flag) {
                opal_output(0, "%s orted_recv: received sync+nidmap from local proc %s",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                            ORTE_NAME_PRINT(sender));
            }
            if (ORTE_SUCCESS != (ret = orte_odls.require_sync(sender, buffer, true))) {
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
