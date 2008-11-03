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

#include "opal/threads/condition.h"
#include "opal/util/bit_ops.h"
#include "opal/class/opal_hash_table.h"
#include "opal/dss/dss.h"


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

/* Module def */
orte_grpcomm_base_module_t orte_grpcomm_basic_module = {
    init,
    finalize,
    xcast,
    allgather,
    orte_grpcomm_base_allgather_list,
    barrier,
    orte_grpcomm_base_set_proc_attr,
    orte_grpcomm_base_get_proc_attr,
    modex,
    orte_grpcomm_base_purge_proc_attrs
};


/**
 * Initialize the module
 */
static int init(void)
{
    int rc;
    
    if (ORTE_SUCCESS != (rc = orte_grpcomm_base_modex_init())) {
        ORTE_ERROR_LOG(rc);
    }
    return rc;
}

/**
 * Finalize the module
 */
static void finalize(void)
{
    orte_grpcomm_base_modex_finalize();
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
    int rc;
    int32_t arch;
    bool modex_reqd = false;
    
    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_output,
                         "%s grpcomm:basic: modex entered",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    /* setup the buffer that will actually be sent */
    OBJ_CONSTRUCT(&buf, opal_buffer_t);
    OBJ_CONSTRUCT(&rbuf, opal_buffer_t);
    
    /* put our process name in the buffer so it can be unpacked later */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, ORTE_PROC_MY_NAME, 1, ORTE_NAME))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    
    /* decide if we need to add the architecture to the modex. Check
     * first to see if hetero is enabled - if not, then we clearly
     * don't need to exchange arch's as they are all identical
     */
    if (OMPI_ENABLE_HETEROGENEOUS_SUPPORT) {
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
        if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &orte_process_info.arch, 1, OPAL_UINT32))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }        
    }

    /* pack the entries we have received */
    if (ORTE_SUCCESS != (rc = orte_grpcomm_base_pack_modex_entries(&buf, &modex_reqd))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    
    if (modex_reqd) {
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
            
            if (OMPI_ENABLE_HETEROGENEOUS_SUPPORT) {
                /* are the nodes hetero? */
                if (orte_homogeneous_nodes) {
                    goto unpack_entries;
                }
                /* unpack its architecture */
                cnt=1;
                if (ORTE_SUCCESS != (rc = opal_dss.unpack(&rbuf, &arch, &cnt, OPAL_UINT32))) {
                    ORTE_ERROR_LOG(rc);
                    goto cleanup;
                }
                /* update the arch in the ESS */
                if (ORTE_SUCCESS != (rc = orte_ess.update_arch(&proc_name, arch))) {
                    ORTE_ERROR_LOG(rc);
                    goto cleanup;
                }
            }
            
        unpack_entries:
            /* update the modex database */
            if (ORTE_SUCCESS != (rc = orte_grpcomm_base_update_modex_entries(&proc_name, &rbuf))) {
                ORTE_ERROR_LOG(rc);
                goto cleanup;
            }
        }
    }
    
cleanup:
    OBJ_DESTRUCT(&buf);
    OBJ_DESTRUCT(&rbuf);
    
    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_output,
                         "%s grpcomm:basic: modex completed",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    return rc;
}

