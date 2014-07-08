/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2007-2012 Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * Copyright (c) 2009      Sun Microsystems, Inc. All rights reserved.
 * Copyright (c) 2010-2011 Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2014      Intel, Inc. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"

#include <stdio.h>
#include <stddef.h>
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
#ifdef HAVE_TIME_H
#include <time.h>
#endif


#include "opal/mca/event/event.h"
#include "opal/mca/base/base.h"
#include "opal/util/output.h"
#include "opal/util/opal_environ.h"
#include "opal/runtime/opal.h"
#include "opal/runtime/opal_progress.h"
#include "opal/dss/dss.h"

#include "orte/util/proc_info.h"
#include "orte/util/session_dir.h"
#include "orte/util/name_fns.h"
#include "orte/util/nidmap.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/grpcomm/grpcomm.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/rml/rml_types.h"
#include "orte/mca/odls/odls.h"
#include "orte/mca/odls/base/base.h"
#include "orte/mca/plm/plm.h"
#include "orte/mca/plm/base/plm_private.h"
#include "orte/mca/routed/routed.h"
#include "orte/mca/ess/ess.h"
#include "orte/mca/state/state.h"

#include "orte/mca/odls/base/odls_private.h"

#include "orte/runtime/runtime.h"
#include "orte/runtime/orte_globals.h"
#include "orte/runtime/orte_wait.h"
#include "orte/runtime/orte_quit.h"

#include "orte/orted/orted.h"

/*
 * Globals
 */
static char *get_orted_comm_cmd_str(int command);

static opal_pointer_array_t *procs_prev_ordered_to_terminate = NULL;

void orte_daemon_recv(int status, orte_process_name_t* sender,
                      opal_buffer_t *buffer, orte_rml_tag_t tag,
                      void* cbdata)
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
    orte_process_name_t proc, proc2;
    orte_process_name_t *return_addr;
    int32_t i, num_replies;
    bool hnp_accounted_for;
    opal_pointer_array_t procarray;
    orte_proc_t *proct;
    char *cmd_str = NULL;
    opal_pointer_array_t *procs_to_kill = NULL;
    orte_std_cntr_t num_procs, num_new_procs = 0, p;
    orte_proc_t *cur_proc = NULL, *prev_proc = NULL;
    bool found = false;

    /* unpack the command */
    n = 1;
    if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &command, &n, ORTE_DAEMON_CMD))) {
        ORTE_ERROR_LOG(ret);
        return;
    }

    cmd_str = get_orted_comm_cmd_str(command);
    OPAL_OUTPUT_VERBOSE((1, orte_debug_output,
                         "%s orted:comm:process_commands() Processing Command: %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), cmd_str));
    free(cmd_str);
    cmd_str = NULL;

    /* now process the command locally */
    switch(command) {

        /****    NULL    ****/
    case ORTE_DAEMON_NULL_CMD:
        ret = ORTE_SUCCESS;
        break;
            
        /****    KILL_LOCAL_PROCS   ****/
    case ORTE_DAEMON_KILL_LOCAL_PROCS:
        num_replies = 0;

        /* construct the pointer array */
        OBJ_CONSTRUCT(&procarray, opal_pointer_array_t);
        opal_pointer_array_init(&procarray, num_replies, ORTE_GLOBAL_ARRAY_MAX_SIZE, 16);
        
        /* unpack the proc names into the array */
        while (ORTE_SUCCESS == (ret = opal_dss.unpack(buffer, &proc, &n, ORTE_NAME))) {
            proct = OBJ_NEW(orte_proc_t);
            proct->name.jobid = proc.jobid;
            proct->name.vpid = proc.vpid;

            opal_pointer_array_add(&procarray, proct);
            num_replies++;
        }
        if (ORTE_ERR_UNPACK_READ_PAST_END_OF_BUFFER != ret) {
            ORTE_ERROR_LOG(ret);
            goto KILL_PROC_CLEANUP;
        }
            
        if (0 == num_replies) {
            /* kill everything */
            if (ORTE_SUCCESS != (ret = orte_odls.kill_local_procs(NULL))) {
                ORTE_ERROR_LOG(ret);
            }
            break;
        } else {
            /* kill the procs */
            if (ORTE_SUCCESS != (ret = orte_odls.kill_local_procs(&procarray))) {
                ORTE_ERROR_LOG(ret);
            }
        }

        /* cleanup */
    KILL_PROC_CLEANUP:
        for (i=0; i < procarray.size; i++) {
            if (NULL != (proct = (orte_proc_t*)opal_pointer_array_get_item(&procarray, i))) {
                free(proct);
            }
        }
        OBJ_DESTRUCT(&procarray);
        break;
            
        /****    SIGNAL_LOCAL_PROCS   ****/
    case ORTE_DAEMON_SIGNAL_LOCAL_PROCS:
        /* unpack the jobid */
        n = 1;
        if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &job, &n, ORTE_JOBID))) {
            ORTE_ERROR_LOG(ret);
            goto CLEANUP;
        }
                
        /* look up job data object */
        jdata = orte_get_job_data_object(job);

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
            if (NULL != jdata) {
                jdata->state |= ORTE_JOB_STATE_SUSPENDED;
            }
        } else if (SIGCONT == signal && NULL != jdata) {
            jdata->state &= ~ORTE_JOB_STATE_SUSPENDED;
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
           
    case ORTE_DAEMON_ABORT_PROCS_CALLED:
        if (orte_debug_daemons_flag) {
            opal_output(0, "%s orted_cmd: received abort_procs report",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
        }

        /* Number of processes */
        n = 1;
        if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &num_procs, &n, ORTE_STD_CNTR)) ) {
            ORTE_ERROR_LOG(ret);
            goto CLEANUP;
        }

        /* Retrieve list of processes */
        procs_to_kill = OBJ_NEW(opal_pointer_array_t);
        opal_pointer_array_init(procs_to_kill, num_procs, INT32_MAX, 2);

        /* Keep track of previously terminated, so we don't keep ordering the
         * same processes to die.
         */
        if( NULL == procs_prev_ordered_to_terminate ) {
            procs_prev_ordered_to_terminate = OBJ_NEW(opal_pointer_array_t);
            opal_pointer_array_init(procs_prev_ordered_to_terminate, num_procs+1, INT32_MAX, 8);
        }

        num_new_procs = 0;
        for( i = 0; i < num_procs; ++i) {
            cur_proc = OBJ_NEW(orte_proc_t);

            n = 1;
            if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &(cur_proc->name), &n, ORTE_NAME)) ) {
                ORTE_ERROR_LOG(ret);
                goto CLEANUP;
            }

            /* See if duplicate */
            found = false;
            for( p = 0; p < procs_prev_ordered_to_terminate->size; ++p) {
                if( NULL == (prev_proc = (orte_proc_t*)opal_pointer_array_get_item(procs_prev_ordered_to_terminate, p))) {
                    continue;
                }
                if(OPAL_EQUAL == orte_util_compare_name_fields(ORTE_NS_CMP_ALL,
                                                               &cur_proc->name,
                                                               &prev_proc->name) ) {
                    found = true;
                    break;
                }
            }

            OPAL_OUTPUT_VERBOSE((2, orte_debug_output,
                                 "%s orted:comm:abort_procs Application %s requests term. of %s (%2d of %2d) %3s.",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(sender),
                                 ORTE_NAME_PRINT(&(cur_proc->name)), i, num_procs,
                                 (found ? "Dup" : "New") ));

            /* If not a duplicate, then add to the to_kill list */
            if( !found ) {
                opal_pointer_array_add(procs_to_kill, (void*)cur_proc);
                OBJ_RETAIN(cur_proc);
                opal_pointer_array_add(procs_prev_ordered_to_terminate, (void*)cur_proc);
                num_new_procs++;
            }
        }

        /*
         * Send the request to termiante
         */
        if( num_new_procs > 0 ) {
            OPAL_OUTPUT_VERBOSE((2, orte_debug_output,
                                 "%s orted:comm:abort_procs Terminating application requested processes (%2d / %2d).",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 num_new_procs, num_procs));
            orte_plm.terminate_procs(procs_to_kill);
        } else {
            OPAL_OUTPUT_VERBOSE((2, orte_debug_output,
                                 "%s orted:comm:abort_procs No new application processes to terminating from request (%2d / %2d).",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 num_new_procs, num_procs));
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
                if ((ret = orte_rml.send_buffer_nb(ORTE_PROC_MY_NAME, relay_msg, target_tag,
                                                   orte_rml_send_callback, NULL)) < 0) {
                    ORTE_ERROR_LOG(ret);
                    OBJ_RELEASE(relay_msg);
                }
            }
        } else {
            /* must be for our children - deliver the message */
            if (ORTE_SUCCESS != (ret = orte_odls.deliver_message(job, relay_msg, target_tag))) {
                ORTE_ERROR_LOG(ret);
            }
            OBJ_RELEASE(relay_msg);
        }
        break;
    
        /****    EXIT COMMAND    ****/
    case ORTE_DAEMON_EXIT_CMD:
        if (orte_debug_daemons_flag) {
            opal_output(0, "%s orted_cmd: received exit cmd",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
        }
        /* kill the local procs */
        orte_odls.kill_local_procs(NULL);
        /* flag that orteds were ordered to terminate */
        orte_orteds_term_ordered = true;
        /* if all my routes and local children are gone, then terminate ourselves */
        if (0 == orte_routed.num_routes()) {
            for (i=0; i < orte_local_children->size; i++) {
                if (NULL != (proct = (orte_proc_t*)opal_pointer_array_get_item(orte_local_children, i)) &&
                    proct->alive) {
                    /* at least one is still alive */
                    return;
                }
            }
            /* call our appropriate exit procedure */
            if (orte_debug_daemons_flag) {
                opal_output(0, "%s orted_cmd: all routes and children gone - exiting",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
            }
            ORTE_ACTIVATE_JOB_STATE(NULL, ORTE_JOB_STATE_DAEMONS_TERMINATED);
        }
        return;
        break;
            
    case ORTE_DAEMON_HALT_VM_CMD:
        if (orte_debug_daemons_flag) {
            opal_output(0, "%s orted_cmd: received halt_vm cmd",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
        }
        /* kill the local procs */
        orte_odls.kill_local_procs(NULL);
        /* flag that orteds were ordered to terminate */
        orte_orteds_term_ordered = true;
        if (ORTE_PROC_IS_HNP) {
            /* if all my routes and local children are gone, then terminate ourselves */
            if (0 == orte_routed.num_routes()) {
                for (i=0; i < orte_local_children->size; i++) {
                    if (NULL != (proct = (orte_proc_t*)opal_pointer_array_get_item(orte_local_children, i)) &&
                        proct->alive) {
                        /* at least one is still alive */
                        return;
                    }
                }
                /* call our appropriate exit procedure */
                if (orte_debug_daemons_flag) {
                    opal_output(0, "%s orted_cmd: all routes and children gone - exiting",
                                ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
                }
                ORTE_ACTIVATE_JOB_STATE(NULL, ORTE_JOB_STATE_DAEMONS_TERMINATED);
            }
        } else {
            ORTE_ACTIVATE_JOB_STATE(NULL, ORTE_JOB_STATE_DAEMONS_TERMINATED);
        }
        return;
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
        if (ORTE_PROC_IS_HNP) {
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
        if (0 > (ret = orte_rml.send_buffer_nb(sender, answer, ORTE_RML_TAG_TOOL,
                                               orte_rml_send_callback, NULL))) {
            ORTE_ERROR_LOG(ret);
            OBJ_RELEASE(answer);
        }
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
            
        if (0 > (ret = orte_rml.send_buffer_nb(sender, answer, ORTE_RML_TAG_TOOL,
                                               orte_rml_send_callback, NULL))) {
            ORTE_ERROR_LOG(ret);
            OBJ_RELEASE(answer);
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
        if (!ORTE_PROC_IS_HNP) {
            int32_t zero=0;
                
            answer = OBJ_NEW(opal_buffer_t);
            if (ORTE_SUCCESS != (ret = opal_dss.pack(answer, &zero, 1, OPAL_INT32))) {
                ORTE_ERROR_LOG(ret);
                OBJ_RELEASE(answer);
                goto CLEANUP;
            }
            if (0 > (ret = orte_rml.send_buffer_nb(sender, answer, ORTE_RML_TAG_TOOL,
                                                   orte_rml_send_callback, NULL))) {
                ORTE_ERROR_LOG(ret);
                OBJ_RELEASE(answer);
            }
        } else {
            /* if we are the HNP, process the request */
            int32_t i, num_jobs;
            orte_job_t *jobdat;
                
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
                job = ORTE_CONSTRUCT_LOCAL_JOBID(ORTE_PROC_MY_NAME->jobid, job);
                if (NULL != (jobdat = orte_get_job_data_object(job))) {
                    num_jobs = 1;
                    if (ORTE_SUCCESS != (ret = opal_dss.pack(answer, &num_jobs, 1, OPAL_INT32))) {
                        ORTE_ERROR_LOG(ret);
                        OBJ_RELEASE(answer);
                        goto CLEANUP;
                    }
                    if (ORTE_SUCCESS != (ret = opal_dss.pack(answer, &jobdat, 1, ORTE_JOB))) {
                        ORTE_ERROR_LOG(ret);
                        OBJ_RELEASE(answer);
                        goto CLEANUP;
                    }
                } else {
                    /* if we get here, then send a zero answer */
                    num_jobs = 0;
                    if (ORTE_SUCCESS != (ret = opal_dss.pack(answer, &num_jobs, 1, OPAL_INT32))) {
                        ORTE_ERROR_LOG(ret);
                        OBJ_RELEASE(answer);
                        goto CLEANUP;
                    }
                }
            } else {
                /* since the job array is no longer
                 * left-justified and may have holes, we have
                 * to cnt the number of jobs. Be sure to include the daemon
                 * job - the user can slice that info out if they don't care
                 */
                num_jobs = 0;
                for (i=0; i < orte_job_data->size; i++) {
                    if (NULL != opal_pointer_array_get_item(orte_job_data, i)) {
                        num_jobs++;
                    }
                }
                /* pack the number of jobs */
                if (ORTE_SUCCESS != (ret = opal_dss.pack(answer, &num_jobs, 1, OPAL_INT32))) {
                    ORTE_ERROR_LOG(ret);
                    OBJ_RELEASE(answer);
                    goto CLEANUP;
                }
                /* now pack the data, one at a time */
                for (i=0; i < orte_job_data->size; i++) {
                    if (NULL != (jobdat = (orte_job_t*)opal_pointer_array_get_item(orte_job_data, i))) {
                        if (ORTE_SUCCESS != (ret = opal_dss.pack(answer, &jobdat, 1, ORTE_JOB))) {
                            ORTE_ERROR_LOG(ret);
                            OBJ_RELEASE(answer);
                            goto CLEANUP;
                        }
                    }
                }
            }
            if (0 > (ret = orte_rml.send_buffer_nb(sender, answer, ORTE_RML_TAG_TOOL,
                                                   orte_rml_send_callback, NULL))) {
                ORTE_ERROR_LOG(ret);
                OBJ_RELEASE(answer);
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
        if (!ORTE_PROC_IS_HNP) {
            int32_t zero=0;
                
            answer = OBJ_NEW(opal_buffer_t);
            if (ORTE_SUCCESS != (ret = opal_dss.pack(answer, &zero, 1, OPAL_INT32))) {
                ORTE_ERROR_LOG(ret);
                OBJ_RELEASE(answer);
                goto CLEANUP;
            }
            if (0 > (ret = orte_rml.send_buffer_nb(sender, answer, ORTE_RML_TAG_TOOL,
                                                   orte_rml_send_callback, NULL))) {
                ORTE_ERROR_LOG(ret);
                OBJ_RELEASE(answer);
            }
        } else {
            /* if we are the HNP, process the request */
            int32_t i, num_nodes;
            orte_node_t *node;
            char *nid;
                
            /* unpack the nodename */
            n = 1;
            if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &nid, &n, OPAL_STRING))) {
                ORTE_ERROR_LOG(ret);
                goto CLEANUP;
            }
                
            /* setup return */
            answer = OBJ_NEW(opal_buffer_t);
            num_nodes = 0;
                
            /* if they asked for a specific node, then just get that info */
            if (NULL != nid) {
                /* find this node */
                for (i=0; i < orte_node_pool->size; i++) {
                    if (NULL == (node = (orte_node_t*)opal_pointer_array_get_item(orte_node_pool, i))) {
                        continue;
                    }
                    if (0 == strcmp(nid, node->name)) {
                        num_nodes = 1;
                        break;
                    }
                }
                if (ORTE_SUCCESS != (ret = opal_dss.pack(answer, &num_nodes, 1, OPAL_INT32))) {
                    ORTE_ERROR_LOG(ret);
                    OBJ_RELEASE(answer);
                    goto CLEANUP;
                }
                if (ORTE_SUCCESS != (ret = opal_dss.pack(answer, &node, 1, ORTE_NODE))) {
                    ORTE_ERROR_LOG(ret);
                    OBJ_RELEASE(answer);
                    goto CLEANUP;
                }
            } else {
                /* count number of nodes */
                for (i=0; i < orte_node_pool->size; i++) {
                    if (NULL != opal_pointer_array_get_item(orte_node_pool, i)) {
                        num_nodes++;
                    }
                }
                /* pack the answer */
                if (ORTE_SUCCESS != (ret = opal_dss.pack(answer, &num_nodes, 1, OPAL_INT32))) {
                    ORTE_ERROR_LOG(ret);
                    OBJ_RELEASE(answer);
                    goto CLEANUP;
                }
                /* pack each node separately */
                for (i=0; i < orte_node_pool->size; i++) {
                    if (NULL != (node = (orte_node_t*)opal_pointer_array_get_item(orte_node_pool, i))) {
                        if (ORTE_SUCCESS != (ret = opal_dss.pack(answer, &node, 1, ORTE_NODE))) {
                            ORTE_ERROR_LOG(ret);
                            OBJ_RELEASE(answer);
                            goto CLEANUP;
                        }
                    }
                }
            }
            /* send the info */
            if (0 > (ret = orte_rml.send_buffer_nb(sender, answer, ORTE_RML_TAG_TOOL,
                                                   orte_rml_send_callback, NULL))) {
                ORTE_ERROR_LOG(ret);
                OBJ_RELEASE(answer);
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
        if (!ORTE_PROC_IS_HNP) {
            int32_t zero=0;
                
            answer = OBJ_NEW(opal_buffer_t);
            if (ORTE_SUCCESS != (ret = opal_dss.pack(answer, &zero, 1, OPAL_INT32))) {
                ORTE_ERROR_LOG(ret);
                OBJ_RELEASE(answer);
                goto CLEANUP;
            }
            if (0 > (ret = orte_rml.send_buffer_nb(sender, answer, ORTE_RML_TAG_TOOL,
                                                   orte_rml_send_callback, NULL))) {
                ORTE_ERROR_LOG(ret);
                OBJ_RELEASE(answer);
            }
        } else {
            /* if we are the HNP, process the request */
            orte_job_t *jdata;
            orte_proc_t *proc;
            orte_vpid_t vpid;
            int32_t i, num_procs;
            char *nid;

            /* setup the answer */
            answer = OBJ_NEW(opal_buffer_t);
                
            /* unpack the jobid */
            n = 1;
            if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &job, &n, ORTE_JOBID))) {
                ORTE_ERROR_LOG(ret);
                goto CLEANUP;
            }
                
            /* look up job data object */
            job = ORTE_CONSTRUCT_LOCAL_JOBID(ORTE_PROC_MY_NAME->jobid, job);
            if (NULL == (jdata = orte_get_job_data_object(job))) {
                ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
                goto CLEANUP;
            }
                
            /* unpack the vpid */
            n = 1;
            if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &vpid, &n, ORTE_VPID))) {
                ORTE_ERROR_LOG(ret);
                goto CLEANUP;
            }


            /* if they asked for a specific proc, then just get that info */
            if (ORTE_VPID_WILDCARD != vpid) {
                /* find this proc */
                for (i=0; i < jdata->procs->size; i++) {
                    if (NULL == (proc = (orte_proc_t*)opal_pointer_array_get_item(jdata->procs, i))) {
                        continue;
                    }
                    if (vpid == proc->name.vpid) {
                        num_procs = 1;
                        if (ORTE_SUCCESS != (ret = opal_dss.pack(answer, &num_procs, 1, OPAL_INT32))) {
                            ORTE_ERROR_LOG(ret);
                            goto CLEANUP;
                        }
                        if (ORTE_SUCCESS != (ret = opal_dss.pack(answer, &proc, 1, ORTE_PROC))) {
                            ORTE_ERROR_LOG(ret);
                            goto CLEANUP;
                        }
                        /* the vpid and nodename for this proc are no longer packed
                         * in the ORTE_PROC packing routines to save space for other
                         * uses, so we have to pack them separately
                         */
                        if (ORTE_SUCCESS != (ret = opal_dss.pack(answer, &proc->pid, 1, OPAL_PID))) {
                            ORTE_ERROR_LOG(ret);
                            goto CLEANUP;
                        }
                        if (NULL == proc->node) {
                            nid = "UNKNOWN";
                        } else {
                            nid = proc->node->name;
                        }
                        if (ORTE_SUCCESS != (ret = opal_dss.pack(answer, &nid, 1, OPAL_STRING))) {
                            ORTE_ERROR_LOG(ret);
                            goto CLEANUP;
                        }
                        break;
                    }                    
                }
            } else {
                /* count number of procs */
                num_procs = 0;
                for (i=0; i < jdata->procs->size; i++) {
                    if (NULL != opal_pointer_array_get_item(jdata->procs, i)) {
                        num_procs++;
                    }
                }
                /* pack the answer */
                if (ORTE_SUCCESS != (ret = opal_dss.pack(answer, &num_procs, 1, OPAL_INT32))) {
                    ORTE_ERROR_LOG(ret);
                    OBJ_RELEASE(answer);
                    goto CLEANUP;
                }
                /* pack each proc separately */
                for (i=0; i < jdata->procs->size; i++) {
                    if (NULL != (proc = (orte_proc_t*)opal_pointer_array_get_item(jdata->procs, i))) {
                        if (ORTE_SUCCESS != (ret = opal_dss.pack(answer, &proc, 1, ORTE_PROC))) {
                            ORTE_ERROR_LOG(ret);
                            OBJ_RELEASE(answer);
                            goto CLEANUP;
                        }
                        /* the vpid and nodename for this proc are no longer packed
                         * in the ORTE_PROC packing routines to save space for other
                         * uses, so we have to pack them separately
                         */
                        if (ORTE_SUCCESS != (ret = opal_dss.pack(answer, &proc->pid, 1, OPAL_PID))) {
                            ORTE_ERROR_LOG(ret);
                            goto CLEANUP;
                        }
                        if (NULL == proc->node) {
                            nid = "UNKNOWN";
                        } else {
                            nid = proc->node->name;
                        }
                        if (ORTE_SUCCESS != (ret = opal_dss.pack(answer, &nid, 1, OPAL_STRING))) {
                            ORTE_ERROR_LOG(ret);
                            goto CLEANUP;
                        }
                    }
                }
            }
            /* send the info */
            if (0 > (ret = orte_rml.send_buffer_nb(sender, answer, ORTE_RML_TAG_TOOL,
                                                   orte_rml_send_callback, NULL))) {
                ORTE_ERROR_LOG(ret);
                OBJ_RELEASE(answer);
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
        
        /****     TOP COMMAND     ****/
    case ORTE_DAEMON_TOP_CMD:
        /* setup the answer */
        answer = OBJ_NEW(opal_buffer_t);
        num_replies = 0;
        hnp_accounted_for = false;
            
        n = 1;
        return_addr = NULL;
        while (ORTE_SUCCESS == opal_dss.unpack(buffer, &proc, &n, ORTE_NAME)) {
            /* the jobid provided will, of course, have the job family of
             * the requestor. We need to convert that to our own job family
             */
            proc.jobid = ORTE_CONSTRUCT_LOCAL_JOBID(ORTE_PROC_MY_NAME->jobid, proc.jobid);
            if (ORTE_PROC_IS_HNP) {
                return_addr = sender;
                proc2.jobid = ORTE_PROC_MY_NAME->jobid;
                /* if the request is for a wildcard vpid, then it goes to every
                 * daemon. For scalability, we should probably xcast this some
                 * day - but for now, we just loop
                 */
                if (ORTE_VPID_WILDCARD == proc.vpid) {
                    /* loop across all daemons */
                    for (proc2.vpid=1; proc2.vpid < orte_process_info.num_procs; proc2.vpid++) {

                        /* setup the cmd */
                        relay_msg = OBJ_NEW(opal_buffer_t);
                        command = ORTE_DAEMON_TOP_CMD;
                        if (ORTE_SUCCESS != (ret = opal_dss.pack(relay_msg, &command, 1, ORTE_DAEMON_CMD))) {
                            ORTE_ERROR_LOG(ret);
                            OBJ_RELEASE(relay_msg);
                            goto SEND_TOP_ANSWER;
                        }
                        if (ORTE_SUCCESS != (ret = opal_dss.pack(relay_msg, &proc, 1, ORTE_NAME))) {
                            ORTE_ERROR_LOG(ret);
                            OBJ_RELEASE(relay_msg);
                            goto SEND_TOP_ANSWER;
                        }
                        if (ORTE_SUCCESS != (ret = opal_dss.pack(relay_msg, sender, 1, ORTE_NAME))) {
                            ORTE_ERROR_LOG(ret);
                            OBJ_RELEASE(relay_msg);
                            goto SEND_TOP_ANSWER;
                        }
                        /* the callback function will release relay_msg buffer */
                        if (0 > orte_rml.send_buffer_nb(&proc2, relay_msg,
                                                        ORTE_RML_TAG_DAEMON,
                                                        orte_rml_send_callback, NULL)) {
                            ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
                            OBJ_RELEASE(relay_msg);
                            ret = ORTE_ERR_COMM_FAILURE;
                        }
                        num_replies++;
                    }
                    /* account for our own reply */
                    if (!hnp_accounted_for) {
                        hnp_accounted_for = true;
                        num_replies++;
                    }
                    /* now get the data for my own procs */
                    goto GET_TOP;
                } else {
                    /* this is for a single proc - see which daemon
                     * this rank is on
                     */
                    if (ORTE_VPID_INVALID == (proc2.vpid = orte_get_proc_daemon_vpid(&proc))) {
                        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
                        goto SEND_TOP_ANSWER;
                    }
                    /* if the vpid is me, then just handle this myself */
                    if (proc2.vpid == ORTE_PROC_MY_NAME->vpid) {
                        if (!hnp_accounted_for) {
                            hnp_accounted_for = true;
                            num_replies++;
                        }
                        goto GET_TOP;
                    }
                    /* otherwise, forward the cmd on to the appropriate daemon */
                    relay_msg = OBJ_NEW(opal_buffer_t);
                    command = ORTE_DAEMON_TOP_CMD;
                    if (ORTE_SUCCESS != (ret = opal_dss.pack(relay_msg, &command, 1, ORTE_DAEMON_CMD))) {
                        ORTE_ERROR_LOG(ret);
                        OBJ_RELEASE(relay_msg);
                        goto SEND_TOP_ANSWER;
                    }
                    if (ORTE_SUCCESS != (ret = opal_dss.pack(relay_msg, &proc, 1, ORTE_NAME))) {
                        ORTE_ERROR_LOG(ret);
                        OBJ_RELEASE(relay_msg);
                        goto SEND_TOP_ANSWER;
                    }
                    if (ORTE_SUCCESS != (ret = opal_dss.pack(relay_msg, sender, 1, ORTE_NAME))) {
                        ORTE_ERROR_LOG(ret);
                        OBJ_RELEASE(relay_msg);
                        goto SEND_TOP_ANSWER;
                    }
                    /* the callback function will release relay_msg buffer */
                    if (0 > orte_rml.send_buffer_nb(&proc2, relay_msg,
                                                    ORTE_RML_TAG_DAEMON,
                                                    orte_rml_send_callback, NULL)) {
                        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
                        OBJ_RELEASE(relay_msg);
                        ret = ORTE_ERR_COMM_FAILURE;
                    }
                }
                /* end if HNP */
            } else {
                /* this came from the HNP, but needs to go back to the original
                 * requestor. Unpack the name of that entity first
                 */
                n = 1;
                if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &proc2, &n, ORTE_NAME))) {
                    ORTE_ERROR_LOG(ret);
                    /* in this case, we are helpless - we have no idea who to send an
                     * error message TO! All we can do is return - the tool that sent
                     * this request is going to hang, but there isn't anything we can
                     * do about it
                     */
                    goto CLEANUP;
                }
                return_addr = &proc2;
            GET_TOP:
                /* this rank must be local to me, or the HNP wouldn't
                 * have sent it to me - process the request
                 */
                if (ORTE_SUCCESS != (ret = orte_odls_base_get_proc_stats(answer, &proc))) {
                    ORTE_ERROR_LOG(ret);
                    goto SEND_TOP_ANSWER;
                }
            }
        }
    SEND_TOP_ANSWER:
        /* send the answer back to requester */
        if (ORTE_PROC_IS_HNP) {
            /* if I am the HNP, I need to also provide the number of
             * replies the caller should recv and the sample time
             */
            time_t mytime;
            char *cptr;
                
            relay_msg = OBJ_NEW(opal_buffer_t);
            if (ORTE_SUCCESS != (ret = opal_dss.pack(relay_msg, &num_replies, 1, OPAL_INT32))) {
                ORTE_ERROR_LOG(ret);
            }
            time(&mytime);
            cptr = ctime(&mytime);
            cptr[strlen(cptr)-1] = '\0';  /* remove trailing newline */
            if (ORTE_SUCCESS != (ret = opal_dss.pack(relay_msg, &cptr, 1, OPAL_STRING))) {
                ORTE_ERROR_LOG(ret);
            }
            /* copy the stats payload */
            opal_dss.copy_payload(relay_msg, answer);
            OBJ_RELEASE(answer);
            answer = relay_msg;
        }
        /* if we don't have a return address, then we are helpless */
        if (NULL == return_addr) {
            ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
            ret = ORTE_ERR_COMM_FAILURE;
            break;
        }
        if (0 > (ret = orte_rml.send_buffer_nb(return_addr, answer, ORTE_RML_TAG_TOOL,
                                               orte_rml_send_callback, NULL))) {
            ORTE_ERROR_LOG(ret);
            OBJ_RELEASE(answer);
        }
        break;
        
    default:
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
    }
    
 CLEANUP:
    return;
}

static char *get_orted_comm_cmd_str(int command)
{
    switch(command) {
    case ORTE_DAEMON_CONTACT_QUERY_CMD:
        return strdup("ORTE_DAEMON_CONTACT_QUERY_CMD");
    case ORTE_DAEMON_KILL_LOCAL_PROCS:
        return strdup("ORTE_DAEMON_KILL_LOCAL_PROCS");
    case ORTE_DAEMON_SIGNAL_LOCAL_PROCS:
        return strdup("ORTE_DAEMON_SIGNAL_LOCAL_PROCS");
    case ORTE_DAEMON_ADD_LOCAL_PROCS:
        return strdup("ORTE_DAEMON_ADD_LOCAL_PROCS");
    case ORTE_DAEMON_TREE_SPAWN:
        return strdup("ORTE_DAEMON_TREE_SPAWN");
    case ORTE_DAEMON_HEARTBEAT_CMD:
        return strdup("ORTE_DAEMON_HEARTBEAT_CMD");
     case ORTE_DAEMON_EXIT_CMD:
        return strdup("ORTE_DAEMON_EXIT_CMD");
     case ORTE_DAEMON_PROCESS_AND_RELAY_CMD:
        return strdup("ORTE_DAEMON_PROCESS_AND_RELAY_CMD");
    case ORTE_DAEMON_MESSAGE_LOCAL_PROCS:
        return strdup("ORTE_DAEMON_MESSAGE_LOCAL_PROCS");
    case ORTE_DAEMON_NULL_CMD:
        return strdup("NULL");
    case ORTE_DAEMON_SYNC_BY_PROC:
        return strdup("ORTE_DAEMON_SYNC_BY_PROC");
    case ORTE_DAEMON_SYNC_WANT_NIDMAP:
        return strdup("ORTE_DAEMON_SYNC_WANT_NIDMAP");

    case ORTE_DAEMON_REPORT_JOB_INFO_CMD:
        return strdup("ORTE_DAEMON_REPORT_JOB_INFO_CMD");
    case ORTE_DAEMON_REPORT_NODE_INFO_CMD:
        return strdup("ORTE_DAEMON_REPORT_NODE_INFO_CMD");
    case ORTE_DAEMON_REPORT_PROC_INFO_CMD:
        return strdup("ORTE_DAEMON_REPORT_PROC_INFO_CMD");
    case ORTE_DAEMON_SPAWN_JOB_CMD:
        return strdup("ORTE_DAEMON_SPAWN_JOB_CMD");
    case ORTE_DAEMON_TERMINATE_JOB_CMD:
        return strdup("ORTE_DAEMON_TERMINATE_JOB_CMD");
    case ORTE_DAEMON_HALT_VM_CMD:
        return strdup("ORTE_DAEMON_HALT_VM_CMD");

    case ORTE_DAEMON_TOP_CMD:
        return strdup("ORTE_DAEMON_TOP_CMD");

    case ORTE_DAEMON_NAME_REQ_CMD:
        return strdup("ORTE_DAEMON_NAME_REQ_CMD");
    case ORTE_DAEMON_CHECKIN_CMD:
        return strdup("ORTE_DAEMON_CHECKIN_CMD");
    case ORTE_TOOL_CHECKIN_CMD:
        return strdup("ORTE_TOOL_CHECKIN_CMD");

    case ORTE_DAEMON_ABORT_PROCS_CALLED:
        return strdup("ORTE_DAEMON_ABORT_PROCS_CALLED");

    default:
        return strdup("Unknown Command!");
    }
}
