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
#include "orte/util/nidmap.h"
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
static int get_proc_attr(const orte_process_name_t proc,
                         const char * attribute_name, void **val, 
                         size_t *size);

/* Module def */
orte_grpcomm_base_module_t orte_grpcomm_basic_module = {
    init,
    finalize,
    xcast,
    allgather,
    orte_grpcomm_base_allgather_list,
    barrier,
    set_proc_attr,
    get_proc_attr,
    modex,
    orte_grpcomm_base_purge_proc_attrs
};

static bool recv_on;
static opal_buffer_t *profile_buf=NULL;

/**
 * Initialize the module
 */
static int init(void)
{
    int rc;
    int value;
    
    mca_base_param_reg_int_name("orte", "grpcomm_recv_on",
                                "Turn on grpcomm recv for profile purposes",
                                true, false,
                                (int) false, &value);
    recv_on = OPAL_INT_TO_BOOL(value);

    if (ORTE_SUCCESS != (rc = orte_grpcomm_base_modex_init())) {
        ORTE_ERROR_LOG(rc);
    }
    
    if (opal_profile && orte_process_info.mpi_proc) {
        /* if I am an MPI application proc, then create a buffer
         * to pack all my attributes in */
        profile_buf = OBJ_NEW(opal_buffer_t);
        /* seed it with the node name */
        if (ORTE_SUCCESS != (rc = opal_dss.pack(profile_buf, &orte_process_info.nodename, 1, OPAL_STRING))) {
            ORTE_ERROR_LOG(rc);
        }
    }
    
    if (orte_process_info.hnp && recv_on) {
        /* if we are profiling and I am the HNP, then start the
         * profiling receive
         */
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
    opal_byte_object_t bo, *boptr;
    opal_buffer_t profile;
    
    orte_grpcomm_base_modex_finalize();
    
    if (opal_profile && orte_process_info.mpi_proc) {
        /* if I am an MPI proc, send my buffer to the collector */
        boptr = &bo;
        opal_dss.unload(profile_buf, (void**)&boptr->bytes, &boptr->size);
        OBJ_RELEASE(profile_buf);
        /* store it as a single object */
        OBJ_CONSTRUCT(&profile, opal_buffer_t);
        opal_dss.pack(&profile, &boptr, 1, OPAL_BYTE_OBJECT);
        /* send the buffer */
        orte_rml.send_buffer(ORTE_PROC_MY_HNP, &profile, ORTE_RML_TAG_GRPCOMM_PROFILE, 0);
        /* done with buffer */
        OBJ_DESTRUCT(&profile);
    }
    
    if (orte_process_info.hnp && recv_on) {
        /* if we are profiling and I am the HNP, then stop the
         * profiling receive
         */
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

static void barrier_recv(int status, orte_process_name_t* sender,
                         opal_buffer_t *buffer,
                         orte_rml_tag_t tag, void *cbdata)
{
    /* flag as recvd */
    barrier_recvd = true;
}

static int barrier(void)
{
    opal_buffer_t buf;
    orte_daemon_cmd_flag_t command=ORTE_DAEMON_COLL_CMD;
    orte_grpcomm_coll_t coll_type=ORTE_GRPCOMM_BARRIER;
    int rc;
    
    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_output,
                         "%s grpcomm:basic entering barrier",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
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
    opal_buffer_t coll;
    orte_grpcomm_coll_t coll_type=ORTE_GRPCOMM_ALLGATHER;
    
    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_output,
                         "%s grpcomm:basic entering allgather",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
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
    
    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_output,
                         "%s grpcomm:basic allgather completed",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    return ORTE_SUCCESS;
}

/***   MODEX SECTION ***/
static int do_modex(opal_list_t *procs)
{
    opal_buffer_t buf, rbuf;
    orte_std_cntr_t i, num_procs;
    orte_std_cntr_t cnt, j, num_recvd_entries;
    orte_process_name_t proc_name;
    int rc=ORTE_SUCCESS;
    int32_t arch;
    bool modex_reqd;
    orte_nid_t *nid;
    
    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_output,
                         "%s grpcomm:basic:modex: performing modex",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
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
        
        /* unpack the number of entries for this proc */
        cnt=1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(&rbuf, &num_recvd_entries, &cnt, ORTE_STD_CNTR))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
        
        OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base_output,
                             "%s grpcomm:basic:modex adding %d entries for proc %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), num_recvd_entries,
                             ORTE_NAME_PRINT(&proc_name)));
        
        /* find this proc's node in the nidmap */
        if (NULL == (nid = orte_util_lookup_nid(&proc_name))) {
            /* proc wasn't found - return error */
            OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base_output,
                                 "%s grpcomm:basic:modex no nidmap entry for proc %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(&proc_name)));
            ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
            rc = ORTE_ERR_NOT_FOUND;
            goto cleanup;
        }
        
        /*
         * Extract the attribute names and values
         */
        for (j = 0; j < num_recvd_entries; j++) {
            size_t num_bytes;
            orte_attr_t *attr;
            
            attr = OBJ_NEW(orte_attr_t);
            cnt = 1;
            if (ORTE_SUCCESS != (rc = opal_dss.unpack(&rbuf, &(attr->name), &cnt, OPAL_STRING))) {
                ORTE_ERROR_LOG(rc);
                goto cleanup;
            }
            
            cnt = 1;
            if (ORTE_SUCCESS != (rc = opal_dss.unpack(&rbuf, &num_bytes, &cnt, OPAL_SIZE))) {
                ORTE_ERROR_LOG(rc);
                goto cleanup;
            }
            attr->size = num_bytes;
            
            if (num_bytes != 0) {
                if (NULL == (attr->bytes = malloc(num_bytes))) {
                    ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                    rc = ORTE_ERR_OUT_OF_RESOURCE;
                    goto cleanup;
                }
                cnt = (orte_std_cntr_t) num_bytes;
                if (ORTE_SUCCESS != (rc = opal_dss.unpack(&rbuf, attr->bytes, &cnt, OPAL_BYTE))) {
                    ORTE_ERROR_LOG(rc);
                    goto cleanup;
                }
            }
            
            /* add this to the node's attribute list */
            opal_list_append(&nid->attrs, &attr->super);
        }
    }
    
cleanup:
    OBJ_DESTRUCT(&buf);
    OBJ_DESTRUCT(&rbuf);
    return rc;
}

static int modex(opal_list_t *procs)
{
    int rc=ORTE_SUCCESS;
    int fd;
    opal_byte_object_t bo, *boptr;
    int32_t i, n;
    char *nodename, *attr;
    orte_nid_t **nd, *ndptr;
    orte_attr_t *attrdata;
    opal_buffer_t bobuf;

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
     *
     * We also do the modex if we are doing an opal_profile so that the
     * HNP can collect our modex info.
     */
    if (NULL != procs || opal_profile) {
        if (ORTE_SUCCESS != (rc = do_modex(procs))) {
            ORTE_ERROR_LOG(rc);
        }
        return rc;
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
            
            if (ORTE_SUCCESS != (rc = do_modex(procs))) {
                ORTE_ERROR_LOG(rc);
            }
            return rc;
        }
    }
    
    /* no modex is required - see if the data was included in the launch message */
    if (orte_send_profile) {
        /* the info was provided in the nidmap - there is nothing more we have to do */
        OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_output,
                             "%s grpcomm:basic:modex using nidmap",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        return ORTE_SUCCESS;
    }
    
    /* see if a profile file was given to us */
    if (NULL == opal_profile_file) {
        /* if we don't have any other way to do this, then let's default to doing the
         * modex so we at least can function, even if it isn't as fast as we might like
         */
        OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_output,
                             "%s grpcomm:basic: modex is required",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        if (ORTE_SUCCESS != (rc = do_modex(procs))) {
            ORTE_ERROR_LOG(rc);
        }
        return rc;
    }
    
    fd = open(opal_profile_file, O_RDONLY);
    if (fd < 0) {
        orte_show_help("help-orte-runtime.txt", "grpcomm-basic:file-cant-open", true, opal_profile_file);
        return ORTE_ERR_NOT_FOUND;
    }
    
    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_output,
                         "%s grpcomm:basic:modex reading %s file",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),  opal_profile_file));

    /* loop through file until end */
    boptr = &bo;
    nd = (orte_nid_t**)orte_nidmap.addr;
    while (0 < read(fd, &bo.size, sizeof(bo.size))) {
        /* this is the number of bytes in the byte object */
        bo.bytes = malloc(bo.size);
        if (0 > read(fd, bo.bytes, bo.size)) {
            orte_show_help("help-orte-runtime.txt", "orte_nidmap:unable-read-file", true, opal_profile_file);
            close(fd);
            return ORTE_ERR_FILE_READ_FAILURE;
        }
        /* load the byte object into a buffer for unpacking */
        OBJ_CONSTRUCT(&bobuf, opal_buffer_t);
        opal_dss.load(&bobuf, boptr->bytes, boptr->size);
        /* unpack the nodename */
        n = 1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(&bobuf, &nodename, &n, OPAL_STRING))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        /* find this node in nidmap */
        for (i=0, ndptr=NULL; i < orte_nidmap.size && NULL != nd[i]; i++) {
            /* since we may not have kept fqdn hostnames, we can only check
             * for equality to the length of the name in the nid
             */
            if (0 == strncmp(nd[i]->name, nodename, strlen(nd[i]->name))) {
                ndptr = nd[i];
                break;
            }
        }
        free(nodename);  /* done with this */
        if (NULL == ndptr) {
            /* didn't find it! */
            ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
            return ORTE_ERR_NOT_FOUND;
        }
        
        /* loop through the rest of the object to unpack the attr's themselves */
        n = 1;
        while (ORTE_SUCCESS == opal_dss.unpack(&bobuf, &attr, &n, OPAL_STRING)) {
            attrdata = OBJ_NEW(orte_attr_t);
            attrdata->name = strdup(attr);
            /* read the number of bytes in the blob */
            n = 1;
            if (ORTE_SUCCESS != (rc = opal_dss.unpack(&bobuf, &attrdata->size, &n, OPAL_INT32))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
            /* unpack the bytes */
            attrdata->bytes = malloc(attrdata->size);
            if (ORTE_SUCCESS != (rc = opal_dss.unpack(&bobuf, attrdata->bytes, &attrdata->size, OPAL_BYTE))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
            /* add to our list for this node */
            opal_list_append(&ndptr->attrs, &attrdata->super);
        }
        OBJ_DESTRUCT(&bobuf);
    }

    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_output,
                         "%s grpcomm:basic: modex completed",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    return rc;
}

/* the HNP will -never- execute the following as it is NOT an MPI process */
static int set_proc_attr(const char *attr_name, const void *data, size_t size)
{
    int rc;
    
    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_output,
                         "%s grpcomm:basic:set_proc_attr for attribute %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), attr_name));

    /* if we are doing a profile, pack this up */
    if (opal_profile) {
        int32_t isize;
        
        if (ORTE_SUCCESS != (rc = opal_dss.pack(profile_buf, &attr_name, 1, OPAL_STRING))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
        isize = size;
        if (ORTE_SUCCESS != (rc = opal_dss.pack(profile_buf, &isize, 1, OPAL_INT32))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
        if (ORTE_SUCCESS != (rc = opal_dss.pack(profile_buf, data, isize, OPAL_BYTE))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
        /* let it fall through so that the job doesn't hang! */
        return orte_grpcomm_base_set_proc_attr(attr_name, data, size);
    }
    
    /* we always have to set our own attributes in case they are needed for
     * a connect/accept at some later time
     */
cleanup:
    return orte_grpcomm_base_set_proc_attr(attr_name, data, size);
}

static int get_proc_attr(const orte_process_name_t proc,
                         const char * attribute_name, void **val, 
                         size_t *size)
{
    orte_nid_t *nid;
    opal_list_item_t *item;
    orte_attr_t *attr;
    
    /* find this proc's node in the nidmap */
    if (NULL == (nid = orte_util_lookup_nid((orte_process_name_t*)&proc))) {
        /* proc wasn't found - return error */
        OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base_output,
                             "%s grpcomm:basic:get_proc_attr: no modex entry for proc %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(&proc)));
        return ORTE_ERR_NOT_FOUND;
        
    }
    
    /* look for this attribute */
    for (item = opal_list_get_first(&nid->attrs);
         item != opal_list_get_end(&nid->attrs);
         item = opal_list_get_next(item)) {
        attr = (orte_attr_t*)item;
        if (0 == strcmp(attr->name, attribute_name)) {
            /* copy the data to the caller */
            void *copy = malloc(attr->size);
            
            if (copy == NULL) {
                return ORTE_ERR_OUT_OF_RESOURCE;
            }
            memcpy(copy, attr->bytes, attr->size);
            *val = copy;
            *size = attr->size;
            OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base_output,
                                 "%s grpcomm:basic:get_proc_attr: found %d bytes for attr %s on proc %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), (int)attr->size,
                                 attribute_name, ORTE_NAME_PRINT(&proc)));
            return ORTE_SUCCESS;
        }
    }
    
    /* get here if attribute isn't found */
    OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base_output,
                         "%s grpcomm:basic:get_proc_attr: no attr avail or zero byte size for proc %s attribute %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(&proc), attribute_name));
    *val = NULL;
    *size = 0;
    
    return ORTE_SUCCESS;
}
