/*
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
 * Copyright (c) 2007      Sun Microsystems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"
#include "orte/types.h"

#include <string.h>
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif  /* HAVE_SYS_TIME_H */
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif
#include <fcntl.h>

#include "opal/threads/condition.h"
#include "opal/util/bit_ops.h"
#include "opal/class/opal_hash_table.h"
#include "opal/dss/dss.h"
#include "opal/runtime/opal.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/ess/ess.h"
#include "orte/mca/odls/odls_types.h"
#include "orte/mca/rml/rml.h"
#include "orte/util/name_fns.h"
#include "orte/util/show_help.h"
#include "orte/util/proc_info.h"
#include "orte/orted/orted.h"
#include "orte/runtime/orte_wait.h"
#include "orte/runtime/orte_globals.h"

#include "orte/mca/grpcomm/base/base.h"
#include "grpcomm_basic.h"


/* Static API's */
static int init(void);
static void finalize(void);
static int xcast(orte_jobid_t job,
                 opal_buffer_t *buffer,
                 orte_rml_tag_t tag);
static int allgather(opal_buffer_t *sbuf, opal_buffer_t *rbuf);
static int barrier(void);
static int modex(opal_list_t *procs);
static int set_proc_attr(const char *attr_name, const void *data, size_t size);

/* Module def */
orte_grpcomm_base_module_t orte_grpcomm_basic_module = {
    init,
    finalize,
    xcast,
    allgather,
    orte_grpcomm_base_allgather_list,
    barrier,
    set_proc_attr,
    orte_grpcomm_base_get_proc_attr,
    modex,
    orte_grpcomm_base_purge_proc_attrs
};


static bool profile;

/**
 * Initialize the module
 */
static int init(void)
{
    int rc;
    int value;
    
    if (ORTE_SUCCESS != (rc = orte_grpcomm_base_modex_init())) {
        ORTE_ERROR_LOG(rc);
    }
    
    /* if we are profiling and I am the HNP, then start the
     * profiling receive
     */
    mca_base_param_reg_int_name("orte", "grpcomm_recv_on",
                                "Whether to turn on grpcomm recv",
                                false, false, (int)false, &value);
    profile = OPAL_INT_TO_BOOL(value);
    
    if (profile && orte_process_info.hnp) {
        if (ORTE_SUCCESS != (rc = orte_grpcomm_base_comm_start())) {
            ORTE_ERROR_LOG(rc);
        }        
    }
    
    return rc;
}

/**
 * Finalize the module
 */
static void finalize(void)
{
    orte_grpcomm_base_modex_finalize();
    
    /* if we are profiling and I am the HNP, then stop the
     * profiling receive
     */
    if (profile && orte_process_info.hnp) {
        orte_grpcomm_base_comm_stop();
    }
}

/**
 *  A "broadcast-like" function to a job's processes.
 *  @param  jobid   The job whose processes are to receive the message
 *  @param  buffer  The data to broadcast
 */

static int xcast(orte_jobid_t job,
                 opal_buffer_t *buffer,
                 orte_rml_tag_t tag)
{
    int rc = ORTE_SUCCESS;
    opal_buffer_t buf;
    orte_daemon_cmd_flag_t command;
    
    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_output,
                         "%s grpcomm:xcast sent to job %s tag %ld",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_JOBID_PRINT(job), (long)tag));
    
    /* if there is no message to send, then just return ok */
    if (NULL == buffer) {
        return ORTE_SUCCESS;
    }
    
    /* setup a buffer to handle the xcast command */
    OBJ_CONSTRUCT(&buf, opal_buffer_t);
    /* all we need to do is send this to the HNP - the relay logic
     * will ensure everyone else gets it! So tell the HNP to
     * process and relay it. The HNP will use the routed.get_routing_tree
     * to find out who it should relay the message to.
     */
    command = ORTE_DAEMON_PROCESS_AND_RELAY_CMD;
    if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &command, 1, ORTE_DAEMON_CMD))) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
    }
    /* pack the target jobid and tag for use in relay */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &job, 1, ORTE_JOBID))) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
    }
    if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &tag, 1, ORTE_RML_TAG))) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
    }
    
    /* if this isn't intended for the daemon command tag, then we better
     * tell the daemon to deliver it to the procs, and what job is supposed
     * to get it - this occurs when a caller just wants to send something
     * to all the procs in a job. In that use-case, the caller doesn't know
     * anything about inserting daemon commands or what routing algo might
     * be used, so we have to help them out a little. Functions that are
     * sending commands to the daemons themselves are smart enough to know
     * what they need to do.
     */
    if (ORTE_RML_TAG_DAEMON != tag) {
        command = ORTE_DAEMON_MESSAGE_LOCAL_PROCS;
        if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &command, 1, ORTE_DAEMON_CMD))) {
            ORTE_ERROR_LOG(rc);
            goto CLEANUP;
        }
        if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &job, 1, ORTE_JOBID))) {
            ORTE_ERROR_LOG(rc);
            goto CLEANUP;
        }
        if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &tag, 1, ORTE_RML_TAG))) {
            ORTE_ERROR_LOG(rc);
            goto CLEANUP;
        }
    }
    
    /* copy the payload into the new buffer - this is non-destructive, so our
     * caller is still responsible for releasing any memory in the buffer they
     * gave to us
     */
    if (ORTE_SUCCESS != (rc = opal_dss.copy_payload(&buf, buffer))) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
    }
    
    /* if I am the HNP, just set things up so the cmd processor gets called.
     * We don't want to message ourselves as this can create circular logic
     * in the RML. Instead, this macro will set a zero-time event which will
     * cause the buffer to be processed by the cmd processor - probably will
     * fire right away, but that's okay
     * The macro makes a copy of the buffer, so it's okay to release it here
     */
    if (orte_process_info.hnp) {
        ORTE_MESSAGE_EVENT(ORTE_PROC_MY_NAME, &buf, ORTE_RML_TAG_DAEMON, orte_daemon_cmd_processor);
    } else {
        /* otherwise, send it to the HNP for relay */
        if (0 > (rc = orte_rml.send_buffer(ORTE_PROC_MY_HNP, &buf, ORTE_RML_TAG_DAEMON, 0))) {
            ORTE_ERROR_LOG(rc);
            goto CLEANUP;
        }
        rc = ORTE_SUCCESS;
    }
    
CLEANUP:
    OBJ_DESTRUCT(&buf);
    return rc;
}


static bool barrier_recvd;
static bool barrier_timer;

static void barrier_recv(int status, orte_process_name_t* sender,
                         opal_buffer_t *buffer,
                         orte_rml_tag_t tag, void *cbdata)
{
    /* flag as recvd */
    barrier_recvd = true;
}

static void barrier_timer_recv(int status, orte_process_name_t* sender,
                               opal_buffer_t *buffer,
                               orte_rml_tag_t tag, void *cbdata)
{
    barrier_timer = true;
}

static int barrier(void)
{
    opal_buffer_t buf;
    orte_daemon_cmd_flag_t command=ORTE_DAEMON_COLL_CMD;
    orte_grpcomm_coll_t coll_type=ORTE_GRPCOMM_BARRIER;
    int rc;
    struct timeval ompistart, ompistop;
    
    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_output,
                         "%s grpcomm:basic entering barrier",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    if (orte_timing && ORTE_PROC_MY_NAME->vpid == 0) {
        gettimeofday(&ompistart, NULL);
    }
    
    /* everyone sends barrier to local daemon */
    OBJ_CONSTRUCT(&buf, opal_buffer_t);
    /* tell the daemon to collect the data */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &command, 1, ORTE_DAEMON_CMD))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&buf);
        return rc;
    }
    /* tell the daemon we are doing a barrier */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &coll_type, 1, ORTE_GRPCOMM_COLL_T))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&buf);
        return rc;
    }    
    /* send to local daemon */
    if (0 > (rc = orte_rml.send_buffer(ORTE_PROC_MY_DAEMON, &buf, ORTE_RML_TAG_DAEMON, 0))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&buf);
        return rc;
    }
    OBJ_DESTRUCT(&buf);
    
    OPAL_OUTPUT_VERBOSE((2, orte_grpcomm_base_output,
                         "%s grpcomm:basic barrier sent",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    /* now receive the release. Be sure to do this in
     * a manner that allows us to return without being in a recv!
     */
    barrier_recvd = false;
    rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, ORTE_RML_TAG_BARRIER,
                                 ORTE_RML_NON_PERSISTENT, barrier_recv, NULL);
    if (rc != ORTE_SUCCESS) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    ORTE_PROGRESSED_WAIT(barrier_recvd, 0, 1);
    
    OPAL_OUTPUT_VERBOSE((2, orte_grpcomm_base_output,
                         "%s grpcomm:basic received barrier release",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    if (orte_timing) {
        if (ORTE_PROC_MY_NAME->vpid == 0) {
            /* setup a receive to hear when the rank=N proc has received the data
             * release - in most xcast schemes, this will always be the final recvr
             */
            barrier_timer = false;
            orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, ORTE_RML_TAG_COLLECTIVE_TIMER,
                                    ORTE_RML_NON_PERSISTENT, barrier_timer_recv, NULL);
            if (rc != ORTE_SUCCESS) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
            ORTE_PROGRESSED_WAIT(barrier_timer, 0, 1);
            gettimeofday(&ompistop, NULL);
            opal_output(0, "%s time to complete barrier %ld usec",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        (long int)((ompistop.tv_sec - ompistart.tv_sec)*1000000 +
                                   (ompistop.tv_usec - ompistart.tv_usec)));
        } else if (ORTE_PROC_MY_NAME->vpid == orte_process_info.num_procs-1) {
            /* if we are rank=N, send a message back to indicate
             * the xcast completed for timing purposes
             */
            orte_process_name_t name;
            
            name.jobid = ORTE_PROC_MY_NAME->jobid;
            name.vpid = 0;
            OBJ_CONSTRUCT(&buf, opal_buffer_t);
            if (0 > (rc = orte_rml.send_buffer(&name,&buf,ORTE_RML_TAG_COLLECTIVE_TIMER,0))) {
                ORTE_ERROR_LOG(rc);
                OBJ_DESTRUCT(&buf);
                return rc;
            }
            rc = ORTE_SUCCESS;
            OBJ_DESTRUCT(&buf);
        }
    }
    
    return ORTE_SUCCESS;
}

static opal_buffer_t *allgather_buf;
static orte_std_cntr_t allgather_complete;

static void allgather_recv(int status, orte_process_name_t* sender,
                            opal_buffer_t *buffer,
                            orte_rml_tag_t tag, void *cbdata)
{
    int rc;
    
    /* xfer the data */
    if (ORTE_SUCCESS != (rc = opal_dss.copy_payload(allgather_buf, buffer))) {
        ORTE_ERROR_LOG(rc);
    }
    allgather_complete = true;
}

static int allgather(opal_buffer_t *sbuf, opal_buffer_t *rbuf)
{
    int rc;
    orte_daemon_cmd_flag_t command=ORTE_DAEMON_COLL_CMD;
    struct timeval ompistart, ompistop;
    opal_buffer_t coll;
    orte_grpcomm_coll_t coll_type=ORTE_GRPCOMM_ALLGATHER;
    
    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_output,
                         "%s grpcomm:basic entering allgather",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    if (orte_timing && ORTE_PROC_MY_NAME->vpid == 0) {
        gettimeofday(&ompistart, NULL);
    }
    
    /* everyone sends data to their local daemon */
    OBJ_CONSTRUCT(&coll, opal_buffer_t);
    /* tell the daemon to collect the data */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(&coll, &command, 1, ORTE_DAEMON_CMD))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&coll);
        return rc;
    }
    /* tell the daemon we are doing an allgather */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(&coll, &coll_type, 1, ORTE_GRPCOMM_COLL_T))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&coll);
        return rc;
    }    
    /* add our data to it */
    if (ORTE_SUCCESS != (rc = opal_dss.copy_payload(&coll, sbuf))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&coll);
        return rc;
    }
    /* send to local daemon */
    if (0 > (rc = orte_rml.send_buffer(ORTE_PROC_MY_DAEMON, &coll, ORTE_RML_TAG_DAEMON, 0))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&coll);
        return rc;
    }
    OBJ_DESTRUCT(&coll);
    
    OPAL_OUTPUT_VERBOSE((2, orte_grpcomm_base_output,
                         "%s grpcomm:basic allgather buffer sent",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    /* setup the buffer that will recv the results */
    allgather_buf = OBJ_NEW(opal_buffer_t);
    
    /* now receive the final result. Be sure to do this in
     * a manner that allows us to return without being in a recv!
     */
    allgather_complete = false;
    rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, ORTE_RML_TAG_ALLGATHER,
                                 ORTE_RML_NON_PERSISTENT, allgather_recv, NULL);
    if (rc != ORTE_SUCCESS) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    ORTE_PROGRESSED_WAIT(allgather_complete, 0, 1);
    
    /* copy payload to the caller's buffer */
    if (ORTE_SUCCESS != (rc = opal_dss.copy_payload(rbuf, allgather_buf))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(allgather_buf);
        return rc;
    }
    OBJ_RELEASE(allgather_buf);
    
    OPAL_OUTPUT_VERBOSE((2, orte_grpcomm_base_output,
                         "%s allgather buffer received",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    if (orte_timing) {
        if (ORTE_PROC_MY_NAME->vpid == 0) {
            /* setup a receive to hear when the rank=N proc has received the data
             * release - in most xcast schemes, this will always be the final recvr
             */
            barrier_timer = false;
            rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, ORTE_RML_TAG_COLLECTIVE_TIMER,
                                    ORTE_RML_NON_PERSISTENT, barrier_timer_recv, NULL);
            if (ORTE_SUCCESS != rc) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
            ORTE_PROGRESSED_WAIT(barrier_timer, 0, 1);
            gettimeofday(&ompistop, NULL);
            opal_output(0, "%s allgather: time to complete %ld usec",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        (long int)((ompistop.tv_sec - ompistart.tv_sec)*1000000 +
                                   (ompistop.tv_usec - ompistart.tv_usec)));
        } else if (ORTE_PROC_MY_NAME->vpid == orte_process_info.num_procs-1) {
            /* if we are rank=N, send a message back to indicate
             * the xcast completed for timing purposes
             */
            orte_process_name_t name;
            opal_buffer_t buf;
            
            name.jobid = ORTE_PROC_MY_NAME->jobid;
            name.vpid = 0;
            OBJ_CONSTRUCT(&buf, opal_buffer_t);
            if (0 > (rc = orte_rml.send_buffer(&name,&buf,ORTE_RML_TAG_COLLECTIVE_TIMER,0))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
            rc = ORTE_SUCCESS;
            OBJ_DESTRUCT(&buf);
        }
    }
    
    
    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_output,
                         "%s grpcomm:basic allgather completed",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    return ORTE_SUCCESS;
}

/***   MODEX SECTION ***/
static int modex(opal_list_t *procs)
{
    opal_buffer_t buf, rbuf;
    orte_std_cntr_t i, num_procs;
    orte_std_cntr_t cnt;
    orte_process_name_t proc_name;
    int rc=ORTE_SUCCESS;
    int32_t arch;
    bool modex_reqd = false;
    
    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_output,
                         "%s grpcomm:basic: modex entered",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    /* if we were given a list of procs to modex with, then this is happening
     * as part of a connect/accept operation. In this case, we -must- do the
     * modex for two reasons:
     *
     * (a) the modex could involve procs from different mpiruns. In this case,
     *     there is no way for the two sets of procs to know which node the
     *     other procs are on, so we cannot use the profile_file to determine
     *     their contact info
     *
     * (b) in a comm_spawn, the parent job does not have a pidmap for the
     *     child job. Thus, it cannot know where the child procs are located,
     *     and cannot use the profile_file to determine their contact info
     */
    if (NULL != procs || NULL == opal_profile_file || opal_profile) {
        modex_reqd = true;
    } else if (OMPI_ENABLE_HETEROGENEOUS_SUPPORT) {
        /* decide if we need to add the architecture to the modex. Check
         * first to see if hetero is enabled - if not, then we clearly
         * don't need to exchange arch's as they are all identical
         */
        /* Case 1: If different apps in this job were built differently - e.g., some
         * are built 32-bit while others are built 64-bit - then we need to modex
         * regardless of any other consideration. The user is reqd to tell us via a
         * cmd line option if this situation exists, which will result in an mca param
         * being set for us, so all we need to do is check for the global boolean
         * that corresponds to that param
         *
         * Case 2: the nodes are hetero, but the app binaries were built
         * the same - i.e., either they are both 32-bit, or they are both 64-bit, but
         * no mixing of the two. In this case, we include the info in the modex
         */
        if (orte_hetero_apps || !orte_homogeneous_nodes) {
            OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_output,
                                 "%s grpcomm:basic: modex is required",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
            
            modex_reqd = true;
        }
    }
    
    if (modex_reqd) {
        /* setup the buffer that will actually be sent */
        OBJ_CONSTRUCT(&buf, opal_buffer_t);
        OBJ_CONSTRUCT(&rbuf, opal_buffer_t);
        
        /* put our process name in the buffer so it can be unpacked later */
        if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, ORTE_PROC_MY_NAME, 1, ORTE_NAME))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
        
        if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &orte_process_info.arch, 1, OPAL_UINT32))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }        
        
        /* pack the entries we have received */
        if (ORTE_SUCCESS != (rc = orte_grpcomm_base_pack_modex_entries(&buf, &modex_reqd))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
        
        OPAL_OUTPUT_VERBOSE((2, orte_grpcomm_base_output,
                             "%s grpcomm:basic:modex: executing allgather",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        
        /* exchange the buffer with the list of peers (if provided) or all my peers */
        if (NULL == procs) {
            if (ORTE_SUCCESS != (rc = orte_grpcomm.allgather(&buf, &rbuf))) {
                ORTE_ERROR_LOG(rc);
                goto cleanup;
            }
        } else {
            if (ORTE_SUCCESS != (rc = orte_grpcomm.allgather_list(procs, &buf, &rbuf))) {
                ORTE_ERROR_LOG(rc);
                goto cleanup;
            }
        }
        
        OPAL_OUTPUT_VERBOSE((2, orte_grpcomm_base_output,
                             "%s grpcomm:basic:modex: processing modex info",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        
        /* process the results */
        /* extract the number of procs that put data in the buffer */
        cnt=1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(&rbuf, &num_procs, &cnt, ORTE_STD_CNTR))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
        
        OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base_output,
                             "%s grpcomm:basic:modex: received %ld data bytes from %ld procs",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             (long)(rbuf.pack_ptr - rbuf.unpack_ptr), (long)num_procs));
        
        /* if the buffer doesn't have any more data, ignore it */
        if (0 >= (rbuf.pack_ptr - rbuf.unpack_ptr)) {
            goto cleanup;
        }
        
        /* otherwise, process it */
        for (i=0; i < num_procs; i++) {
            /* unpack the process name */
            cnt=1;
            if (ORTE_SUCCESS != (rc = opal_dss.unpack(&rbuf, &proc_name, &cnt, ORTE_NAME))) {
                ORTE_ERROR_LOG(rc);
                goto cleanup;
            }
            
            /* unpack its architecture */
            cnt=1;
            if (ORTE_SUCCESS != (rc = opal_dss.unpack(&rbuf, &arch, &cnt, OPAL_UINT32))) {
                ORTE_ERROR_LOG(rc);
                goto cleanup;
            }
            
            /* update the arch in the ESS
             * RHC: DO NOT UPDATE ARCH IF THE PROC IS NOT IN OUR JOB. THIS IS A TEMPORARY
             * FIX TO COMPENSATE FOR A PROBLEM IN THE CONNECT/ACCEPT CODE WHERE WE EXCHANGE
             * INFO INCLUDING THE ARCH, BUT THEN DO A MODEX THAT ALSO INCLUDES THE ARCH. WE
             * CANNOT UPDATE THE ARCH FOR JOBS OUTSIDE OUR OWN AS THE ESS HAS NO INFO ON
             * THOSE PROCS/NODES - AND DOESN'T NEED IT AS THE MPI LAYER HAS ALREADY SET
             * ITSELF UP AND DOES NOT NEED ESS SUPPORT FOR PROCS IN THE OTHER JOB
             *
             * EVENTUALLY, WE WILL SUPPORT THE ESS HAVING INFO ON OTHER JOBS FOR
             * FAULT TOLERANCE PURPOSES - BUT NOT RIGHT NOW
             */
            if (proc_name.jobid == ORTE_PROC_MY_NAME->jobid) {
                if (ORTE_SUCCESS != (rc = orte_ess.update_arch(&proc_name, arch))) {
                    ORTE_ERROR_LOG(rc);
                    goto cleanup;
                }
            }
            
            OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base_output,
                                 "%s grpcomm:basic:modex: adding modex entry for proc %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(&proc_name)));

            /* update the modex database */
            if (ORTE_SUCCESS != (rc = orte_grpcomm_base_update_modex_entries(&proc_name, &rbuf))) {
                ORTE_ERROR_LOG(rc);
                goto cleanup;
            }
        }
    cleanup:
        OBJ_DESTRUCT(&buf);
        OBJ_DESTRUCT(&rbuf);
    }
    
    
    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_output,
                         "%s grpcomm:basic: modex completed",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    return rc;
}

/* the HNP will -never- execute the following as it is NOT an MPI process */
static int set_proc_attr(const char *attr_name, const void *data, size_t size)
{
    struct stat buf;
    int rc;
    int fd;
    int32_t num_bytes;
    char *nodename, *attr, *prochost;
    char modex_data[8192];
    orte_process_name_t name;
    orte_vpid_t i;
    
    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_output,
                         "%s grpcomm:basic:set_proc_attr for attribute %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), attr_name));

    /* if we are doing a profile, pack this up and send it to the HNP */
    if (opal_profile) {
        opal_buffer_t buffer;
        int32_t isize;
        
        OBJ_CONSTRUCT(&buffer, opal_buffer_t);
        if (ORTE_SUCCESS != (rc = opal_dss.pack(&buffer, &orte_process_info.nodename, 1, OPAL_STRING))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
        if (ORTE_SUCCESS != (rc = opal_dss.pack(&buffer, &attr_name, 1, OPAL_STRING))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
        isize = size;
        if (ORTE_SUCCESS != (rc = opal_dss.pack(&buffer, &isize, 1, OPAL_INT32))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
        if (ORTE_SUCCESS != (rc = opal_dss.pack(&buffer, data, isize, OPAL_BYTE))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
        orte_rml.send_buffer(ORTE_PROC_MY_HNP, &buffer, ORTE_RML_TAG_GRPCOMM_PROFILE, 0);
    cleanup:
        OBJ_DESTRUCT(&buffer);
        /* let it fall through so that the job doesn't hang! */
        return orte_grpcomm_base_set_proc_attr(attr_name, data, size);
    }
    
    /* we always have to set our own attributes in case they are needed for
     * a connect/accept at some later time
     */
    rc = orte_grpcomm_base_set_proc_attr(attr_name, data, size);
    
    /* if we are not doing a profile, then see if the profile file was
     * provided. if not, then we are done
     */
    if (NULL == opal_profile_file) {
        return rc;
    }
    
    /* if the file was provided, then we need to check the file to see if
     * info for this particular attribute is available there. But first,
     * the file must be available
     */
    if (0 != stat(opal_profile_file, &buf)) {
        orte_show_help("help-grpcomm-basic.txt", "grpcomm-basic:file-not-found", true, opal_profile_file);
        return ORTE_ERR_NOT_FOUND;
    }
    
    fd = open(opal_profile_file, O_RDONLY);
    if (fd < 0) {
        orte_show_help("help-grpcomm-basic.txt", "grpcomm-basic:file-cant-open", true, opal_profile_file);
        return ORTE_ERR_NOT_FOUND;
    }
    
    OPAL_OUTPUT_VERBOSE((10, orte_grpcomm_base_output,
                         "%s grpcomm:basic:set_proc_attr reading %s file for attr %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),  opal_profile_file, attr_name));
    
    /* loop through file until end */
    while (0 < read(fd, &num_bytes, sizeof(num_bytes))) {
        OPAL_OUTPUT_VERBOSE((20, orte_grpcomm_base_output,
                             "%s grpcomm:basic:set_proc_attr read %d string length",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), num_bytes));
        /* this is the number of bytes in the nodename */
        memset(modex_data, 0, sizeof(modex_data));
        if (0 > read(fd, modex_data, num_bytes)) {
            opal_output(0, "%s: orte:grpcomm:basic: node name not found", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
            close(fd);
            return ORTE_ERR_NOT_FOUND;
        }
        /* this is the nodename - save it */
        nodename = strdup(modex_data);
        OPAL_OUTPUT_VERBOSE((20, orte_grpcomm_base_output,
                             "%s grpcomm:basic:set_proc_attr got nodename %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), nodename));
        /* get the number of bytes in the attribute name */
        if (0 > read(fd, &num_bytes, sizeof(num_bytes))) {
            opal_output(0, "%s: orte:grpcomm:basic: attribute name size not found", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
            close(fd);
            return ORTE_ERR_NOT_FOUND;
        }
        /* get the attribute name */
        memset(modex_data, 0, sizeof(modex_data));
        if (0 > read(fd, modex_data, num_bytes)) {
            opal_output(0, "%s: orte:grpcomm:basic: attribute name not found", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
            close(fd);
            free(nodename);
            return ORTE_ERR_NOT_FOUND;
        }
        /* save it */
        attr = strdup(modex_data);
        OPAL_OUTPUT_VERBOSE((20, orte_grpcomm_base_output,
                             "%s grpcomm:basic:set_proc_attr got attribute %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), attr));
        /* read the number of bytes in the blob */
        if (0 > read(fd, &num_bytes, sizeof(num_bytes))) {
            opal_output(0, "%s: orte:grpcomm:basic: data size not found", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
            close(fd);
            free(nodename);
            free(attr);
            return ORTE_ERR_NOT_FOUND;
        }
        /* read the bytes so we position ourselves */
        if (0 > read(fd, modex_data, num_bytes)) {
            opal_output(0, "%s: orte:grpcomm:basic: data not found", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
            close(fd);
            free(nodename);
            free(attr);
            return ORTE_ERR_NOT_FOUND;
        }            
        /* is this from the calling component? */
        if (0 == strcmp(attr, attr_name)) {
            /* lookup all procs on the given node */
            name.jobid = ORTE_PROC_MY_NAME->jobid;
            for (i=0; i < orte_process_info.num_procs; i++) {
                name.vpid = i;
                /* if this is me, just skip it - I loaded my info above */
                if (ORTE_PROC_MY_NAME->vpid == name.vpid) {
                    continue;
                }
                prochost = orte_ess.proc_get_hostname(&name);
                if (NULL == prochost) {
                    /* report error - unknown host */
                    opal_output(0, "%s: orte:grpcomm:basic: host for proc %s not found",
                                ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), ORTE_NAME_PRINT(&name));
                    close(fd);
                    free(nodename);
                    free(attr);
                    return ORTE_ERR_NOT_FOUND;
                }
                OPAL_OUTPUT_VERBOSE((20, orte_grpcomm_base_output,
                                     "%s grpcomm:basic:set_proc_attr checking node %s against %s",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), nodename, prochost));
                if (0 == strncmp(nodename, prochost, strlen(prochost))) {
                    /* on this host - load the data into the modex db */
                    if (ORTE_SUCCESS != (rc = orte_grpcomm_base_load_modex_data(&name, (char*)attr_name, modex_data, num_bytes))) {
                        ORTE_ERROR_LOG(rc);
                        return rc;
                    }
                    
                }
            }
        }
        free(nodename);
        free(attr);
    }
    return ORTE_SUCCESS;
}
