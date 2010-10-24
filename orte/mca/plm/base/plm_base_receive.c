/* -*- C -*-
 *
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
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
 */

/*
 * includes
 */
#include "orte_config.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif

#include "opal/mca/mca.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/util/opal_sos.h"
#include "opal/dss/dss.h"
#include "opal/threads/threads.h"

#include "orte/constants.h"
#include "orte/types.h"
#include "orte/util/proc_info.h"
#include "orte/util/error_strings.h"
#include "orte/mca/debugger/base/base.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/rml/rml_types.h"
#include "orte/mca/routed/routed.h"
#include "orte/mca/ras/base/base.h"
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"
#include "orte/runtime/orte_quit.h"

#include "orte/mca/plm/plm_types.h"
#include "orte/mca/plm/plm.h"
#include "orte/mca/plm/base/plm_private.h"
#include "orte/mca/plm/base/base.h"

static bool recv_issued=false;
static opal_mutex_t lock;
static opal_condition_t cond;
static opal_list_t recvs;
static opal_event_t ready;
static int ready_fd[2];
static bool processing;

static void process_msg(int fd, short event, void *data);

int orte_plm_base_comm_start(void)
{
    int rc;

    if (recv_issued) {
        return ORTE_SUCCESS;
    }
    
    OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                         "%s plm:base:receive start comm",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    processing = false;
    OBJ_CONSTRUCT(&lock, opal_mutex_t);
    OBJ_CONSTRUCT(&cond, opal_condition_t);
    OBJ_CONSTRUCT(&recvs, opal_list_t);
#ifndef __WINDOWS__
    pipe(ready_fd);
#else
    if (evutil_socketpair(AF_UNIX, SOCK_STREAM, 0, ready_fd) == -1) {
        return ORTE_ERROR;
    }
#endif

    OBJ_CONSTRUCT(&ready, opal_event_t);
    opal_event.set(&ready, ready_fd[0], OPAL_EV_READ, process_msg, NULL);
    opal_event.add(&ready, 0);
    
    if (ORTE_SUCCESS != (rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD,
                                                      ORTE_RML_TAG_PLM,
                                                      ORTE_RML_NON_PERSISTENT,
                                                      orte_plm_base_recv,
                                                      NULL))) {
        ORTE_ERROR_LOG(rc);
    }
    recv_issued = true;
    
    return rc;
}


int orte_plm_base_comm_stop(void)
{
    if (!recv_issued) {
        return ORTE_SUCCESS;
    }
    
    OBJ_DESTRUCT(&recvs);
    opal_event.del(&ready);
    OBJ_DESTRUCT(&ready);
#ifndef __WINDOWS__
    close(ready_fd[0]);
#else
    closesocket(ready_fd[0]);
#endif
    processing = false;
    OBJ_DESTRUCT(&lock);
    
    OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                         "%s plm:base:receive stop comm",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    orte_rml.recv_cancel(ORTE_NAME_WILDCARD, ORTE_RML_TAG_PLM);
    recv_issued = false;
    
    return ORTE_SUCCESS;
}


/* process incoming messages in order of receipt */
static void process_msg(int fd, short event, void *data)
{
    orte_msg_packet_t *msgpkt;
    orte_plm_cmd_flag_t command;
    orte_std_cntr_t count;
    orte_jobid_t job;
    orte_job_t *jdata, *parent;
    opal_buffer_t answer;
    orte_vpid_t vpid;
    orte_proc_t *proc;
    orte_proc_state_t state;
    orte_exit_code_t exit_code;
    int rc=ORTE_SUCCESS, ret;
    orte_app_context_t *app, *child_app;
    opal_list_item_t *item;
    int dump[128];
    orte_process_name_t name;
    pid_t pid;
    bool running;
    
    OPAL_ACQUIRE_THREAD(&lock, &cond, &processing);
    
    OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                         "%s plm:base:receive processing msg",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    /* clear the file descriptor to stop the event from refiring */
#ifndef __WINDOWS__
    read(fd, &dump, sizeof(dump));
#else
    recv(fd, (char *) &dump, sizeof(dump), 0);
#endif
    
    /* reset the event for the next message */
    opal_event.add(&ready, 0);
    
    while (NULL != (item = opal_list_remove_first(&recvs))) {
        msgpkt = (orte_msg_packet_t*)item;

        /* setup a default response */
        OBJ_CONSTRUCT(&answer, opal_buffer_t);
        job = ORTE_JOBID_INVALID;
        
        count = 1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(msgpkt->buffer, &command, &count, ORTE_PLM_CMD))) {
            ORTE_ERROR_LOG(rc);
            goto CLEANUP;
        }
        
        switch (command) {
        case ORTE_PLM_LAUNCH_JOB_CMD:
            OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                                 "%s plm:base:receive job launch command",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
                
            /* unpack the job object */
            count = 1;
            if (ORTE_SUCCESS != (rc = opal_dss.unpack(msgpkt->buffer, &jdata, &count, ORTE_JOB))) {
                ORTE_ERROR_LOG(rc);
                goto ANSWER_LAUNCH;
            }
            
            /* flag that this is a dynamic spawn */
            jdata->dyn_spawn_active = true;

            /* if is a LOCAL slave cmd */
            if (jdata->controls & ORTE_JOB_CONTROL_LOCAL_SLAVE) {
                OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                                     "%s plm:base:receive local launch",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
                /* In this case, I cannot lookup job info. All I do is pass
                 * this along to the local launcher, IF it is available
                 */
                if (NULL == orte_plm.spawn) {
                    /* can't do this operation */
                    ORTE_ERROR_LOG(ORTE_ERR_NOT_SUPPORTED);
                    rc = ORTE_ERR_NOT_SUPPORTED;
                    goto ANSWER_LAUNCH;
                }
                if (ORTE_SUCCESS != (rc = orte_plm.spawn(jdata))) {
                    ORTE_ERROR_LOG(rc);
                    goto ANSWER_LAUNCH;
                }
                job = jdata->jobid;
            } else {  /* this is a GLOBAL launch cmd */
                /* get the parent's job object */
                if (NULL == (parent = orte_get_job_data_object(msgpkt->sender.jobid))) {
                    ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
                    goto ANSWER_LAUNCH;
                }
                    
                /* if the prefix was set in the parent's job, we need to transfer
                 * that prefix to the child's app_context so any further launch of
                 * orteds can find the correct binary. There always has to be at
                 * least one app_context in both parent and child, so we don't
                 * need to check that here. However, be sure not to overwrite
                 * the prefix if the user already provided it!
                 */
                app = (orte_app_context_t*)opal_pointer_array_get_item(parent->apps, 0);
                child_app = (orte_app_context_t*)opal_pointer_array_get_item(jdata->apps, 0);
                if (NULL != app->prefix_dir &&
                    NULL == child_app->prefix_dir) {
                    child_app->prefix_dir = strdup(app->prefix_dir);
                }
                    
                OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                                     "%s plm:base:receive adding hosts",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
                    
                /* process any add-hostfile and add-host options that were provided */
                if (ORTE_SUCCESS != (rc = orte_ras_base_add_hosts(jdata))) {
                    ORTE_ERROR_LOG(rc);
                    goto ANSWER_LAUNCH;
                }
                    
                if( NULL == parent->bookmark ) {
                    /* find the sender's node in the job map */
                    if (NULL != (proc = (orte_proc_t*)opal_pointer_array_get_item(parent->procs, msgpkt->sender.vpid))) {
                        /* set the bookmark so the child starts from that place - this means
                         * that the first child process could be co-located with the proc
                         * that called comm_spawn, assuming slots remain on that node. Otherwise,
                         * the procs will start on the next available node
                         */
                        jdata->bookmark = proc->node;
                    }
                } else {
                    jdata->bookmark = parent->bookmark;
                }
                    
                /* launch it */
                OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                                     "%s plm:base:receive calling spawn",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
                OPAL_RELEASE_THREAD(&lock, &cond, &processing);
                if (ORTE_SUCCESS != (rc = orte_plm.spawn(jdata))) {
                    ORTE_ERROR_LOG(rc);
                    goto ANSWER_LAUNCH;
                }
                OPAL_ACQUIRE_THREAD(&lock, &cond, &processing);

                job = jdata->jobid;
                    
                /* output debugger proctable, if requested */
                if (orte_debugger_base.dump_proctable) {
                    char *output;
                    opal_dss.print(&output, NULL, jdata->map, ORTE_JOB_MAP);
                    if (orte_xml_output) {
                        fprintf(orte_xml_fp, "%s\n", output);
                        fflush(orte_xml_fp);
                    } else {
                        opal_output(orte_clean_output, "%s", output);
                    }
                    free(output);
                }

                /* return the favor so that any repetitive comm_spawns track each other */
                parent->bookmark = jdata->bookmark;
            }
                
            /* if the child is an ORTE job, wait for the procs to report they are alive */
            if (!(jdata->controls & ORTE_JOB_CONTROL_NON_ORTE_JOB)) {
                OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                                     "%s plm:base:receive waiting for procs to report",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
                OPAL_RELEASE_THREAD(&lock, &cond, &processing);
                /* we will wait here until the thread is released,
                 * indicating that all procs have reported
                 */
                OPAL_ACQUIRE_THREAD(&jdata->dyn_spawn_lock,
                                    &jdata->dyn_spawn_cond,
                                    &jdata->dyn_spawn_active);
                OPAL_THREAD_UNLOCK(&jdata->dyn_spawn_lock);
                OPAL_ACQUIRE_THREAD(&lock, &cond, &processing);
            }
                
        ANSWER_LAUNCH:
            OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                                 "%s plm:base:receive job %s launched",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_JOBID_PRINT(job)));
                
            /* pack the jobid to be returned */
            if (ORTE_SUCCESS != (ret = opal_dss.pack(&answer, &job, 1, ORTE_JOBID))) {
                ORTE_ERROR_LOG(ret);
            }
                
            /* send the response back to the sender */
            if (0 > (ret = orte_rml.send_buffer(&msgpkt->sender, &answer, ORTE_RML_TAG_PLM_PROXY, 0))) {
                ORTE_ERROR_LOG(ret);
            }
            break;
                
        case ORTE_PLM_UPDATE_PROC_STATE:
            OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                                 "%s plm:base:receive update proc state command from %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(&(msgpkt->sender)) ));
            count = 1;
            running = false;
            while (ORTE_SUCCESS == (rc = opal_dss.unpack(msgpkt->buffer, &job, &count, ORTE_JOBID))) {
                    
                OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                                     "%s plm:base:receive got update_proc_state for job %s",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     ORTE_JOBID_PRINT(job)));
                    
                name.jobid = job;
                running = false;
                /* get the job object */
                if (NULL == (jdata = orte_get_job_data_object(job))) {
                    ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
                    goto CLEANUP;
                }
                /* if we are timing, the daemon will have included the time it
                 * recvd the launch msg - the maximum time between when we sent
                 * that message and a daemon recvd it tells us the time reqd
                 * to wireup the daemon comm network
                 */
                if (orte_timing) {
                    int64_t tmpsec, tmpusec;
                    count = 1;
                    if (ORTE_SUCCESS != (rc = opal_dss.unpack(msgpkt->buffer, &tmpsec, &count, OPAL_INT64))) {
                        ORTE_ERROR_LOG(rc);
                        goto CLEANUP;
                    }
                    count = 1;
                    if (ORTE_SUCCESS != (rc = opal_dss.unpack(msgpkt->buffer, &tmpusec, &count, OPAL_INT64))) {
                        ORTE_ERROR_LOG(rc);
                        goto CLEANUP;
                    }
                    /* keep the maximum time */
                    if (tmpsec > jdata->max_launch_msg_recvd.tv_sec) {
                        jdata->max_launch_msg_recvd.tv_sec = tmpsec;
                        jdata->max_launch_msg_recvd.tv_usec = tmpusec;
                    } else if (tmpsec == jdata->max_launch_msg_recvd.tv_sec &&
                               tmpusec > jdata->max_launch_msg_recvd.tv_usec) {
                        jdata->max_launch_msg_recvd.tv_usec = tmpusec;
                    }
                    if (orte_timing_details) {
                        int64_t sec, usec;
                        char *timestr;
                        ORTE_COMPUTE_TIME_DIFF(sec, usec, jdata->launch_msg_sent.tv_sec, jdata->launch_msg_sent.tv_usec,
                                               tmpsec, tmpusec);
                        timestr = orte_pretty_print_timing(sec, usec);
                        fprintf(orte_timing_output, "Time for launch msg to reach daemon %s: %s\n",
                                ORTE_VPID_PRINT(msgpkt->sender.vpid), timestr);
                        free(timestr);
                    }
                }
                count = 1;
                while (ORTE_SUCCESS == (rc = opal_dss.unpack(msgpkt->buffer, &vpid, &count, ORTE_VPID))) {
                    if (ORTE_VPID_INVALID == vpid) {
                        /* flag indicates that this job is complete - move on */
                        break;
                    }
                    name.vpid = vpid;
                    /* unpack the pid */
                    count = 1;
                    if (ORTE_SUCCESS != (rc = opal_dss.unpack(msgpkt->buffer, &pid, &count, OPAL_PID))) {
                        ORTE_ERROR_LOG(rc);
                        goto CLEANUP;
                    }
                    /* if we are timing things, unpack the time this proc was started */
                    if (orte_timing) {
                        int64_t tmpsec, tmpusec;
                        count = 1;
                        if (ORTE_SUCCESS != (rc = opal_dss.unpack(msgpkt->buffer, &tmpsec, &count, OPAL_INT64))) {
                            ORTE_ERROR_LOG(rc);
                            goto CLEANUP;
                        }
                        count = 1;
                        if (ORTE_SUCCESS != (rc = opal_dss.unpack(msgpkt->buffer, &tmpusec, &count, OPAL_INT64))) {
                            ORTE_ERROR_LOG(rc);
                            goto CLEANUP;
                        }
                        if (orte_timing_details) {
                            time_t tmptime;
                            char *tmpstr;
                            tmptime = tmpsec;
                            tmpstr = ctime(&tmptime);
                            /* remove the newline and the year at the end */
                            tmpstr[strlen(tmpstr)-6] = '\0';
                            fprintf(orte_timing_output, "Time rank %s was launched: %s.%3lu\n",
                                    ORTE_VPID_PRINT(vpid), tmpstr, (unsigned long)(tmpusec/1000));
                        }
                    }
                    /* unpack the state */
                    count = 1;
                    if (ORTE_SUCCESS != (rc = opal_dss.unpack(msgpkt->buffer, &state, &count, ORTE_PROC_STATE))) {
                        ORTE_ERROR_LOG(rc);
                        goto CLEANUP;
                    }
                    if (ORTE_PROC_STATE_RUNNING == state) {
                        running = true;
                    }
                    /* unpack the exit code */
                    count = 1;
                    if (ORTE_SUCCESS != (rc = opal_dss.unpack(msgpkt->buffer, &exit_code, &count, ORTE_EXIT_CODE))) {
                        ORTE_ERROR_LOG(rc);
                        goto CLEANUP;
                    }
                        
                    OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                                         "%s plm:base:receive got update_proc_state for vpid %lu state %s exit_code %d",
                                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                         (unsigned long)vpid, orte_proc_state_to_str(state), (int)exit_code));
                        
                    /* update the state */
                    OPAL_RELEASE_THREAD(&lock, &cond, &processing);
                    orte_errmgr.update_state(job, ORTE_JOB_STATE_UNDEF,
                                             &name, state, pid, exit_code);
                    OPAL_ACQUIRE_THREAD(&lock, &cond, &processing);
                }
                count = 1;
            }
            if (ORTE_ERR_UNPACK_READ_PAST_END_OF_BUFFER != OPAL_SOS_GET_ERROR_CODE(rc)) {
                ORTE_ERROR_LOG(rc);
            } else {
                rc = ORTE_SUCCESS;
            }
            jdata->num_daemons_reported++;
            if (orte_report_launch_progress && running) {
                if (0 == jdata->num_daemons_reported % 100 || jdata->num_daemons_reported == orte_process_info.num_procs) {
                    opal_output(orte_clean_output, "Reported: %d (out of %d) daemons - %d (out of %d) procs",
                                (int)jdata->num_daemons_reported, (int)orte_process_info.num_procs,
                                (int)jdata->num_launched, (int)jdata->num_procs);
                }
            }
            break;
                
        case ORTE_PLM_INIT_ROUTES_CMD:
            count=1;
            if (ORTE_SUCCESS != (rc = opal_dss.unpack(msgpkt->buffer, &job, &count, ORTE_JOBID))) {
                ORTE_ERROR_LOG(rc);
                goto CLEANUP;
            }
            name.jobid = job;
            count=1;
            while (ORTE_SUCCESS == opal_dss.unpack(msgpkt->buffer, &vpid, &count, ORTE_VPID)) {
                if (ORTE_VPID_INVALID == vpid) {
                    break;
                }
                name.vpid = vpid;
                /* update the errmgr state */
                orte_errmgr.update_state(job, ORTE_JOB_STATE_REGISTERED,
                                         &name, ORTE_PROC_STATE_REGISTERED,
                                         0, ORTE_ERROR_DEFAULT_EXIT_CODE);
                count=1;
            }
            /* pass the remainder of the buffer to the active module's
             * init_routes API
             */
            if (ORTE_SUCCESS != (rc = orte_routed.init_routes(job, msgpkt->buffer))) {
                ORTE_ERROR_LOG(rc);
            }
            break;

        default:
            ORTE_ERROR_LOG(ORTE_ERR_VALUE_OUT_OF_BOUNDS);
            rc = ORTE_ERR_VALUE_OUT_OF_BOUNDS;
            break;
        }
        
    CLEANUP:
        /* release the message */
        OBJ_RELEASE(msgpkt);
        OBJ_DESTRUCT(&answer);
        if (ORTE_SUCCESS != rc) {
            goto DEPART;
        }
    }
        
 DEPART:
    /* release the thread */
    OPAL_RELEASE_THREAD(&lock, &cond, &processing);
    
    /* see if an error occurred - if so, wakeup the HNP so we can exit */
    if (ORTE_PROC_IS_HNP && ORTE_SUCCESS != rc) {
        orte_jobs_complete();
    }

}

/*
 * handle message from proxies
 * NOTE: The incoming buffer "buffer" is OBJ_RELEASED by the calling program.
 * DO NOT RELEASE THIS BUFFER IN THIS CODE
 */

void orte_plm_base_recv(int status, orte_process_name_t* sender,
                        opal_buffer_t* buffer, orte_rml_tag_t tag,
                        void* cbdata)
{
    int rc;
    
    OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                         "%s plm:base:receive got message from %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(sender)));

    /* don't process this right away - we need to get out of the recv before
     * we process the message as it may ask us to do something that involves
     * more messaging! Instead, setup an event so that the message gets processed
     * as soon as we leave the recv.
     *
     * The macro makes a copy of the buffer, which we release above - the incoming
     * buffer, however, is NOT released here, although its payload IS transferred
     * to the message buffer for later processing
     */
    ORTE_PROCESS_MESSAGE(&recvs, &lock, processing, ready_fd[1], true, sender, &buffer);
    
    /* reissue the recv */
    if (ORTE_SUCCESS != (rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD,
                                                      ORTE_RML_TAG_PLM,
                                                      ORTE_RML_NON_PERSISTENT,
                                                      orte_plm_base_recv,
                                                      NULL))) {
        ORTE_ERROR_LOG(rc);
    }
    
    return;
}

/* where HNP messages come */
void orte_plm_base_receive_process_msg(int fd, short event, void *data)
{
    orte_message_event_t *mev = (orte_message_event_t*)data;

    ORTE_PROCESS_MESSAGE(&recvs, &lock, processing, ready_fd[1], false, &mev->sender, &mev->buffer);
    OBJ_RELEASE(mev);
}
