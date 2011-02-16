/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2007 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2008 Los Alamos National Security, LLC. 
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#include "orte_config.h"
#include "orte/types.h"


#include "orte_config.h"
#include "orte/constants.h"
#include "orte/types.h"

#include <string.h>
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif  /* HAVE_SYS_TIME_H */

#include "opal/util/output.h"
#include "opal/dss/dss.h"
#include "opal/mca/event/event.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/ess/ess.h"
#include "orte/mca/odls/base/base.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/rml/rml_types.h"
#include "orte/mca/routed/routed.h"
#include "orte/runtime/orte_globals.h"
#include "orte/util/name_fns.h"
#include "orte/orted/orted.h"
#include "orte/runtime/orte_wait.h"

#include "orte/mca/grpcomm/base/base.h"

/***************  TUNED COLLECTIVES FOR GRPCOMM MODULES  **************/

/****    AVAILABLE ALGORITHMS    ****/
static int twoproc(opal_buffer_t *sendbuf, opal_buffer_t *recvbuf, int32_t num_entries,
                   orte_jobid_t jobid, orte_vpid_t *vpids);
static int bruck(opal_buffer_t *sendbuf, opal_buffer_t *recvbuf, int32_t num_entries,
                 orte_jobid_t jobid, orte_vpid_t np, orte_vpid_t *vpids);
static int recursivedoubling(opal_buffer_t *sendbuf, opal_buffer_t *recvbuf, int32_t num_entries,
                             orte_jobid_t jobid, orte_vpid_t np, orte_vpid_t *vpids);

/****    LOCAL VARIABLES USED IN COLLECTIVES    ****/
static int num_recvd;
static opal_buffer_t bucket;

/* Receive and process collective messages */
static void process_coll_msg(int fd, short event, void *data)
{
    orte_message_event_t *mev = (orte_message_event_t*)data;

    /* transfer the data to the collecting bucket */
    opal_dss.copy_payload(&bucket, mev->buffer);
    
    /* cleanup */
    OBJ_RELEASE(mev);
    
    /* increment the number recvd */
    num_recvd++;
}

void orte_grpcomm_base_coll_recv(int status, orte_process_name_t* sender,
                                 opal_buffer_t* buffer, orte_rml_tag_t tag,
                                 void* cbdata)
{
    OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base.output,
                         "%s grpcomm:coll:receive got message from %s",
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
    ORTE_MESSAGE_EVENT(sender, buffer, tag, process_coll_msg);
    
    return;
}

/*
 * Switchyard for selecting the collective algorithm to use
 */
int orte_grpcomm_base_allgather(opal_buffer_t *sendbuf, opal_buffer_t *recvbuf, int32_t num_entries,
                                orte_jobid_t jobid, orte_vpid_t np, orte_vpid_t *vpids)
{
    bool has_one;
    orte_vpid_t n;
    
    OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base.output,
                         "%s grpcomm:coll:allgather called with %d entries np %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         num_entries, (int)np));
    
    /* if we only have one proc participating, just copy the data across and return */
    if (1 == np) {
        opal_dss.pack(recvbuf, &num_entries, 1, OPAL_INT32);
        return opal_dss.copy_payload(recvbuf, sendbuf);
    }
    
    if (2 == np) {
        /* only two procs in collective */
        return twoproc(sendbuf, recvbuf, num_entries, jobid, vpids);
    }
    
    /* if we have power of 2 participants, use recursive doubling - otherwise,
     * use bruck algorithm
     */
    has_one = false;
    n = np;
    for ( ; n > 0; n >>= 1) {
        if (n & 0x1) {
            if (has_one) {
                return bruck(sendbuf, recvbuf, num_entries, jobid, np, vpids);
            }
            has_one = true;
        }
    }
    
    /* must be power of two! */
    return recursivedoubling(sendbuf, recvbuf, num_entries, jobid, np, vpids);
}


/*
 * The Two-Proc Algorithm
 *
 * One sends to zero, zero waits to recv from one
 * Zero adds its data to message, sends result back to one
 */
static int twoproc(opal_buffer_t *sendbuf, opal_buffer_t *recvbuf, int32_t num_entries,
                   orte_jobid_t jobid, orte_vpid_t *vpids)
{
    orte_process_name_t peer;
    int32_t num_remote, cnt;
    int rc;
    opal_buffer_t buf;
    
    peer.jobid = jobid;
    
    OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base.output,
                         "%s grpcomm:coll:two-proc algo employed",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    if (vpids[0] == ORTE_PROC_MY_NAME->vpid) {
        /* I send first */
        peer.vpid = vpids[1];
        /* setup a temp buffer so I can inform the other side as to the
         * number of entries in my buffer
         */
        OBJ_CONSTRUCT(&buf, opal_buffer_t);
        opal_dss.pack(&buf, &num_entries, 1, OPAL_INT32);
        opal_dss.copy_payload(&buf, sendbuf);
        OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base.output,
                             "%s grpcomm:coll:two-proc sending to %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(&peer)));
        
        if (0 > (rc = orte_rml.send_buffer(&peer, &buf, ORTE_RML_TAG_DAEMON_COLLECTIVE, 0))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        OBJ_DESTRUCT(&buf);
        
        /* wait for reply */
        num_recvd = 0;
        OBJ_CONSTRUCT(&bucket, opal_buffer_t);
        if (ORTE_SUCCESS != (rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD,
                                                          ORTE_RML_TAG_DAEMON_COLLECTIVE,
                                                          ORTE_RML_NON_PERSISTENT,
                                                          orte_grpcomm_base_coll_recv,
                                                          NULL))) {
            ORTE_ERROR_LOG(rc);
        }
        
        ORTE_PROGRESSED_WAIT(false, num_recvd, 1);
        OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base.output,
                             "%s grpcomm:coll:two-proc got my return message",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        
    } else {
        /* if I am not the start, then I recv first */
        num_recvd = 0;
        OBJ_CONSTRUCT(&bucket, opal_buffer_t);
        if (ORTE_SUCCESS != (rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD,
                                                          ORTE_RML_TAG_DAEMON_COLLECTIVE,
                                                          ORTE_RML_NON_PERSISTENT,
                                                          orte_grpcomm_base_coll_recv,
                                                          NULL))) {
            ORTE_ERROR_LOG(rc);
        }
        
        ORTE_PROGRESSED_WAIT(false, num_recvd, 1);
        OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base.output,
                             "%s grpcomm:coll:two-proc got my starting message",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        
        /* send my data back */
        OBJ_CONSTRUCT(&buf, opal_buffer_t);
        opal_dss.pack(&buf, &num_entries, 1, OPAL_INT32);
        opal_dss.copy_payload(&buf, sendbuf);
        peer.vpid = vpids[0];
        OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base.output,
                             "%s grpcomm:coll:two-proc sending to %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(&peer)));
        if (0 > (rc = orte_rml.send_buffer(&peer, &buf, ORTE_RML_TAG_DAEMON_COLLECTIVE, 0))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        OBJ_DESTRUCT(&buf);
    }
    
    /* extract the number of entries in the remote buffer */
    cnt = 1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(&bucket, &num_remote, &cnt, OPAL_INT32))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* output of a collective begins with the total number of entries */
    num_remote += num_entries;
    if (ORTE_SUCCESS != (rc = opal_dss.pack(recvbuf, &num_remote, 1, OPAL_INT32))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* xfer my data */
    opal_dss.copy_payload(recvbuf, sendbuf);
    /* xfer the recvd data */
    opal_dss.copy_payload(recvbuf, &bucket);
    
    /* cleanup */
    OBJ_DESTRUCT(&bucket);
    
    return ORTE_SUCCESS;
}


/* For a complete description of this algorithm, please look at
 * ompi/mca/coll/tuned/coll_tuned_allgather.c
 */
static int bruck(opal_buffer_t *sendbuf, opal_buffer_t *recvbuf, int32_t num_entries,
                 orte_jobid_t jobid, orte_vpid_t np, orte_vpid_t *vpids)
{
    orte_vpid_t rank, distance, nv;
    orte_process_name_t peer;
    int32_t num_remote, total_entries, cnt;
    opal_buffer_t collection, buf;
    int rc;
    
    OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base.output,
                         "%s grpcomm:coll:bruck algo employed",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    /* initialize */
    total_entries = num_entries;
    
    /* start by seeding the collection with our own data */
    OBJ_CONSTRUCT(&collection, opal_buffer_t);
    opal_dss.copy_payload(&collection, sendbuf);

    /* collective is constrained to take place within the specified jobid */
    peer.jobid = jobid;
    
    /* Communication step:
     At every step i, rank r:
     - doubles the distance
     - sends message containing all data collected so far to rank r - distance
     - receives message containing all data collected so far from rank (r + distance)
     */
    /* find my position in the group of participants. This
     * value is the "rank" we will use in the algo
     */
    rank = ORTE_VPID_INVALID;
    for (nv=0; nv < np; nv++) {
        if (vpids[nv] == ORTE_PROC_MY_NAME->vpid) {
            rank = nv;
            break;
        }
    }

    /* check for bozo case */
    if (ORTE_VPID_INVALID == rank) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return ORTE_ERR_NOT_FOUND;
    }
    
    for (distance = 1; distance < np; distance <<= 1) {

        /* first send my current contents */
        nv = (rank - distance + np) % np;
        peer.vpid = vpids[nv];
        OBJ_CONSTRUCT(&buf, opal_buffer_t);
        opal_dss.pack(&buf, &total_entries, 1, OPAL_INT32);
        opal_dss.copy_payload(&buf, &collection);
        OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base.output,
                             "%s grpcomm:coll:bruck sending to %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(&peer)));
        if (0 > (rc = orte_rml.send_buffer(&peer, &buf, ORTE_RML_TAG_DAEMON_COLLECTIVE, 0))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        OBJ_DESTRUCT(&buf);
        
        /* now setup to recv from my other partner */
        num_recvd = 0;
        nv = (rank + distance) % np;
        peer.vpid = vpids[nv];
        OBJ_CONSTRUCT(&bucket, opal_buffer_t);
        if (ORTE_SUCCESS != (rc = orte_rml.recv_buffer_nb(&peer,
                                                          ORTE_RML_TAG_DAEMON_COLLECTIVE,
                                                          ORTE_RML_NON_PERSISTENT,
                                                          orte_grpcomm_base_coll_recv,
                                                          NULL))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        /* and wait for it to get here */
        ORTE_PROGRESSED_WAIT(false, num_recvd, 1);
        
        /* extract the number of entries in the remote buffer */
        cnt = 1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(&bucket, &num_remote, &cnt, OPAL_INT32))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* add it to our running total */
        total_entries += num_remote;
        
        /* transfer the data to our collection */
        opal_dss.copy_payload(&collection, &bucket);
        
        /* cleanup */
        OBJ_DESTRUCT(&bucket);
    }
    
    /* output of a collective begins with the total number of entries */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(recvbuf, &total_entries, 1, OPAL_INT32))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* transfer the collected data */
    opal_dss.copy_payload(recvbuf, &collection);
    
    /* cleanup */
    OBJ_DESTRUCT(&collection);
    
    return ORTE_SUCCESS;
}

/* For a complete description of this algorithm, please look at
 * ompi/mca/coll/tuned/coll_tuned_allgather.c
 */
static int recursivedoubling(opal_buffer_t *sendbuf, opal_buffer_t *recvbuf, int32_t num_entries,
                             orte_jobid_t jobid, orte_vpid_t np, orte_vpid_t *vpids)
{
    orte_vpid_t rank, distance, nv;
    int32_t num_remote, total_entries, cnt;
    opal_buffer_t collection, buf;
    orte_process_name_t peer;
    int rc;
    
    OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base.output,
                         "%s grpcomm:coll:recdub algo employed",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    /* initialize */
    total_entries = num_entries;
    
    /* start by seeding the collection with our own data */
    OBJ_CONSTRUCT(&collection, opal_buffer_t);
    opal_dss.copy_payload(&collection, sendbuf);
    
    /* collective is constrained to take place within the specified jobid */
    peer.jobid = jobid;
    
    /* Communication step:
     At every step i, rank r:
     - exchanges message containing all data collected so far with rank peer = (r ^ 2^i).
     */
    /* find my position in the group of participants. This
     * value is the "rank" we will use in the algo
     */
    rank = ORTE_VPID_INVALID;
    for (nv=0; nv < np; nv++) {
        if (vpids[nv] == ORTE_PROC_MY_NAME->vpid) {
            rank = nv;
            break;
        }
    }
    
    /* check for bozo case */
    if (ORTE_VPID_INVALID == rank) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return ORTE_ERR_NOT_FOUND;
    }
    
    for (distance = 0x1; distance < np; distance<<=1) {
        
        /* first send my current contents */
        nv = rank ^ distance;
        peer.vpid = vpids[nv];
        OBJ_CONSTRUCT(&buf, opal_buffer_t);
        opal_dss.pack(&buf, &total_entries, 1, OPAL_INT32);
        opal_dss.copy_payload(&buf, &collection);
        OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base.output,
                             "%s grpcomm:coll:recdub sending to %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(&peer)));
        if (0 > (rc = orte_rml.send_buffer(&peer, &buf, ORTE_RML_TAG_DAEMON_COLLECTIVE, 0))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        OBJ_DESTRUCT(&buf);
        
        /* now setup to recv from my other partner */
        num_recvd = 0;
        OBJ_CONSTRUCT(&bucket, opal_buffer_t);
        if (ORTE_SUCCESS != (rc = orte_rml.recv_buffer_nb(&peer,
                                                          ORTE_RML_TAG_DAEMON_COLLECTIVE,
                                                          ORTE_RML_NON_PERSISTENT,
                                                          orte_grpcomm_base_coll_recv,
                                                          NULL))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        /* and wait for it to get here */
        ORTE_PROGRESSED_WAIT(false, num_recvd, 1);
        
        /* extract the number of entries in the remote buffer */
        cnt = 1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(&bucket, &num_remote, &cnt, OPAL_INT32))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* add it to our running total */
        total_entries += num_remote;
        
        /* transfer the data to our collection */
        opal_dss.copy_payload(&collection, &bucket);
        
        /* cleanup */
        OBJ_DESTRUCT(&bucket);
    }
    
    /* output of a collective begins with the total number of entries */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(recvbuf, &total_entries, 1, OPAL_INT32))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* transfer the collected data */
    opal_dss.copy_payload(recvbuf, &collection);
    
    /* cleanup */
    OBJ_DESTRUCT(&collection);
    
    return ORTE_SUCCESS;
}

/****    DAEMON COLLECTIVE SUPPORT    ****/

static void reset_child_participation(orte_jobid_t job)
{
    opal_list_item_t *item;
    orte_odls_child_t *child;
    
    for (item = opal_list_get_first(&orte_local_children);
         item != opal_list_get_end(&orte_local_children);
         item = opal_list_get_next(item)) {
        child = (orte_odls_child_t*)item;
        
        /* is this child part of the specified job? */
        if (child->name->jobid == job) {
            /* clear flag */
            child->coll_recvd = false;
        }
    }    
}

static bool all_children_participated(orte_jobid_t job)
{
    opal_list_item_t *item;
    orte_odls_child_t *child;
    
    for (item = opal_list_get_first(&orte_local_children);
         item != opal_list_get_end(&orte_local_children);
         item = opal_list_get_next(item)) {
        child = (orte_odls_child_t*)item;
        
        /* is this child part of the specified job? */
        if (child->name->jobid == job && !child->coll_recvd) {
            /* if this child has *not* participated yet, return false */
            return false;
        }
    }
    
    /* if we get here, then everyone in the job has participated */
    return true;
    
}

void orte_grpcomm_base_daemon_collective(orte_process_name_t *sender,
                                         opal_buffer_t *data)
{
    orte_jobid_t jobid;
    orte_odls_job_t *jobdat;
    orte_routed_tree_t *child;
    orte_std_cntr_t n;
    opal_list_t daemon_tree;
    opal_list_item_t *item, *next;
    int32_t num_contributors;
    opal_buffer_t buf;
    orte_process_name_t my_parent, proc;
    orte_vpid_t daemonvpid;
    int rc;
    int32_t numc;
    orte_rml_tag_t rmltag;
    
    OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base.output,
                         "%s grpcomm:base:daemon_coll: daemon collective called",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    /* unpack the jobid using this collective */
    n = 1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(data, &jobid, &n, ORTE_JOBID))) {
        ORTE_ERROR_LOG(rc);
        return;
    }
    
    /* lookup the job record for it */
    jobdat = NULL;
    for (item = opal_list_get_first(&orte_local_jobdata);
         item != opal_list_get_end(&orte_local_jobdata);
         item = opal_list_get_next(item)) {
        jobdat = (orte_odls_job_t*)item;
        
        /* is this the specified job? */
        if (jobdat->jobid == jobid) {
            break;
        }
    }
    if (NULL == jobdat) {
        /* race condition - someone sent us a collective before we could
         * parse the add_local_procs cmd. Just add the jobdat object
         * and continue
         */
        jobdat = OBJ_NEW(orte_odls_job_t);
        jobdat->jobid = jobid;
        opal_list_append(&orte_local_jobdata, &jobdat->super);
    }
    
    /* it may be possible to get here prior to having actually finished processing our
     * local launch msg due to the race condition between different nodes and when
     * they start their individual procs. Hence, we have to first ensure that we
     * -have- finished processing the launch msg, or else we won't know whether
     * or not to wait before sending this on
     */
    OPAL_THREAD_LOCK(&jobdat->lock);
    while (!jobdat->launch_msg_processed) {
        opal_condition_wait(&jobdat->cond, &jobdat->lock);
    }
    OPAL_THREAD_UNLOCK(&jobdat->lock);
    
    /* unpack the tag for this collective */
    n = 1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(data, &rmltag, &n, ORTE_RML_TAG))) {
        ORTE_ERROR_LOG(rc);
        return;
    }
    
    /* unpack the number of contributors in this data bucket */
    n = 1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(data, &num_contributors, &n, OPAL_INT32))) {
        ORTE_ERROR_LOG(rc);
        return;
    }
    jobdat->num_contributors += num_contributors;
    
    /* xfer the data */
    opal_dss.copy_payload(&jobdat->collection_bucket, data);
    
    /* count the number of participants collected */
    jobdat->num_collected++;
    
    /* if we haven't already done so, figure out how many participants we
     * should be expecting
     */
    if (jobdat->num_participating < 0) {
        if (0 < jobdat->num_local_procs) {
            /* we have children, so account for our own participation */
            jobdat->num_participating = 1;
        } else {
            jobdat->num_participating = 0;
        }
        /* now see if anyone else will be sending us something */
        OBJ_CONSTRUCT(&daemon_tree, opal_list_t);
        orte_routed.get_routing_tree(&daemon_tree);
        /* unfortunately, there is no simple way to determine which of our "child"
         * daemons in the routing tree will be sending us something. All we can do
         * is brute force a search, though we attempt to keep it as short as possible
         */
        proc.jobid = jobid;
        proc.vpid = 0;
        while (proc.vpid < jobdat->num_procs && 0 < opal_list_get_size(&daemon_tree)) {
            /* get the daemon that hosts this proc */
            daemonvpid = orte_ess.proc_get_daemon(&proc);
            /* is this daemon one of our children, or at least its contribution
             * will pass through one of our children
             */
            item = opal_list_get_first(&daemon_tree);
            while (item != opal_list_get_end(&daemon_tree)) {
                next = opal_list_get_next(item);
                child = (orte_routed_tree_t*)item;
                if (child->vpid == daemonvpid || opal_bitmap_is_set_bit(&child->relatives, daemonvpid)) {
                    /* it does - add to num_participating */
                    jobdat->num_participating++;
                    /* remove this from the list so we don't double count it */
                    opal_list_remove_item(&daemon_tree, item);
                    /* done with search */
                    break;
                }
                item = next;
            }
            proc.vpid++;
        }
    }
    
    OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base.output,
                         "%s grpcomm:base:daemon_coll: daemon collective for job %s from %s type %ld"
                         " num_collected %d num_participating %d num_contributors %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), ORTE_JOBID_PRINT(jobid),
                         ORTE_NAME_PRINT(sender),
                         (long)jobdat->collective_type, jobdat->num_collected,
                         jobdat->num_participating, jobdat->num_contributors));
    
    if (jobdat->num_collected == jobdat->num_participating) {
        /* if I am the HNP, go process the results */
        if (ORTE_PROC_IS_HNP) {
            goto hnp_process;
        }
        
        /* if I am not the HNP, send to my parent */
        OBJ_CONSTRUCT(&buf, opal_buffer_t);
        /* pack the jobid */
        if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &jobid, 1, ORTE_JOBID))) {
            ORTE_ERROR_LOG(rc);
            return;
        }
        /* pack the target tag */
        if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &rmltag, 1, ORTE_RML_TAG))) {
            ORTE_ERROR_LOG(rc);
            return;
        }
        /* pack the number of contributors */
        if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &jobdat->num_contributors, 1, OPAL_INT32))) {
            ORTE_ERROR_LOG(rc);
            return;
        }
        /* xfer the payload*/
        opal_dss.copy_payload(&buf, &jobdat->collection_bucket);
        /* reset everything for next collective */
        jobdat->num_contributors = 0;
        jobdat->num_collected = 0;
        OBJ_DESTRUCT(&jobdat->collection_bucket);
        OBJ_CONSTRUCT(&jobdat->collection_bucket, opal_buffer_t);
        /* send it */
        my_parent.jobid = ORTE_PROC_MY_NAME->jobid;
        my_parent.vpid = orte_routed.get_routing_tree(NULL);
        OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base.output,
                             "%s grpcomm:base:daemon_coll: daemon collective not the HNP - sending to parent %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(&my_parent)));
        if (0 > (rc = orte_rml.send_buffer(&my_parent, &buf, ORTE_RML_TAG_DAEMON_COLLECTIVE, 0))) {
            ORTE_ERROR_LOG(rc);
            return;
        }
        OBJ_DESTRUCT(&buf);
    }
    return;
    
hnp_process:
    OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base.output,
                         "%s grpcomm:base:daemon_coll: daemon collective HNP - xcasting to job %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_JOBID_PRINT(jobid)));
    /* setup a buffer to send the results back to the job members */
    OBJ_CONSTRUCT(&buf, opal_buffer_t);
    
    /* add any collected data */
    numc = jobdat->num_contributors;
    if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &numc, 1, OPAL_INT32))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    if (ORTE_SUCCESS != (rc = opal_dss.copy_payload(&buf, &jobdat->collection_bucket))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    /* reset everything for next collective */
    jobdat->num_contributors = 0;
    jobdat->num_collected = 0;
    OBJ_DESTRUCT(&jobdat->collection_bucket);
    OBJ_CONSTRUCT(&jobdat->collection_bucket, opal_buffer_t);
    /* send the buffer */
    if (ORTE_SUCCESS != (rc = orte_grpcomm.xcast(jobid, &buf, rmltag))) {
        ORTE_ERROR_LOG(rc);
    }
    
cleanup:
    OBJ_DESTRUCT(&buf);
    
    return;    
}

static void process_msg(int fd, short event, void *data)
{
    orte_message_event_t *mev = (orte_message_event_t*)data;
    orte_process_name_t *proc;
    opal_buffer_t *buf, relay;
    int32_t rc, n;
    opal_list_item_t *item;
    orte_odls_child_t *child;
    bool found = false;
    orte_odls_job_t *jobdat;
    orte_rml_tag_t rmltag;
    
    proc = &mev->sender;
    buf = mev->buffer;
    
    /* is the sender a local proc, or a daemon relaying the collective? */
    if (ORTE_PROC_MY_NAME->jobid == proc->jobid) {
        /* this is a relay - call that code */
        orte_grpcomm_base.daemon_coll(proc, buf);
        goto CLEANUP;
    }
    
    for (item = opal_list_get_first(&orte_local_children);
         item != opal_list_get_end(&orte_local_children);
         item = opal_list_get_next(item)) {
        child = (orte_odls_child_t*)item;
        
        /* find this child */
        if (OPAL_EQUAL == opal_dss.compare(proc, child->name, ORTE_NAME)) {
            
            OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base.output,
                                 "%s grpcomm:base:daemon_coll: collecting data from child %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(child->name)));
            
            found = true;
            break;
        }
    }
    
    /* if it wasn't found on the list, then we need to add it - must have
     * come from a singleton
     */
    if (!found) {
        child = OBJ_NEW(orte_odls_child_t);
        if (ORTE_SUCCESS != (rc = opal_dss.copy((void**)&child->name, proc, ORTE_NAME))) {
            ORTE_ERROR_LOG(rc);
            return;
        }
        opal_list_append(&orte_local_children, &child->super);
        /* we don't know any other info about the child, so just indicate it's
         * alive
         */
        child->alive = true;
        /* setup a jobdat for it */
        orte_odls_base_setup_singleton_jobdat(proc->jobid);
    }
    
    /* this was one of our local procs - find the jobdat for this job */
    jobdat = NULL;
    for (item = opal_list_get_first(&orte_local_jobdata);
         item != opal_list_get_end(&orte_local_jobdata);
         item = opal_list_get_next(item)) {
        jobdat = (orte_odls_job_t*)item;
        
        /* is this the specified job? */
        if (jobdat->jobid == proc->jobid) {
            break;
        }
    }
    if (NULL == jobdat) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        rc = ORTE_ERR_NOT_FOUND;
        goto CLEANUP;
    }
    
    /* unpack the target tag */
    n = 1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(buf, &rmltag, &n, ORTE_RML_TAG))) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
    }
    
    /* collect the provided data */
    opal_dss.copy_payload(&jobdat->local_collection, buf);
    
    /* flag this proc as having participated */
    child->coll_recvd = true;
    
    /* now check to see if all local procs in this job have participated */
    if (all_children_participated(proc->jobid)) {
        
        OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base.output,
                             "%s grpcomm:base:daemon_coll: executing collective",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        
        /* prep a buffer to pass it all along */
        OBJ_CONSTRUCT(&relay, opal_buffer_t);
        /* pack the jobid */
        if (ORTE_SUCCESS != (rc = opal_dss.pack(&relay, &proc->jobid, 1, ORTE_JOBID))) {
            ORTE_ERROR_LOG(rc);
            return;
        }
        /* pack the target tag */
        if (ORTE_SUCCESS != (rc = opal_dss.pack(&relay, &rmltag, 1, ORTE_RML_TAG))) {
            ORTE_ERROR_LOG(rc);
            return;
        }
        /* pack the number of contributors */
        if (ORTE_SUCCESS != (rc = opal_dss.pack(&relay, &jobdat->num_local_procs, 1, OPAL_INT32))) {
            ORTE_ERROR_LOG(rc);
            return;
        }
        /* xfer the payload*/
        opal_dss.copy_payload(&relay, &jobdat->local_collection);
        /* refresh the collection bucket for reuse */
        OBJ_DESTRUCT(&jobdat->local_collection);
        OBJ_CONSTRUCT(&jobdat->local_collection, opal_buffer_t);
        reset_child_participation(proc->jobid);
        /* pass this to the daemon collective operation */
        orte_grpcomm_base.daemon_coll(ORTE_PROC_MY_NAME, &relay);
        /* done with the relay */
        OBJ_DESTRUCT(&relay);
        
        OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base.output,
                             "%s grpcomm:base:daemon_coll: collective completed",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    }
    
CLEANUP:
    /* release the message */
    OBJ_RELEASE(mev);
}

void orte_grpcomm_base_daemon_coll_recv(int status, orte_process_name_t* sender,
                                        opal_buffer_t* buffer, orte_rml_tag_t tag,
                                        void* cbdata)
{
    int rc;
    
    OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base.output,
                         "%s grpcomm:base:daemon_coll:receive got message from %s",
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
    ORTE_MESSAGE_EVENT(sender, buffer, tag, process_msg);
    
    /* reissue the recv */
    if (ORTE_SUCCESS != (rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD,
                                                      ORTE_RML_TAG_DAEMON_COLLECTIVE,
                                                      ORTE_RML_NON_PERSISTENT,
                                                      orte_grpcomm_base_daemon_coll_recv,
                                                      cbdata))) {
        ORTE_ERROR_LOG(rc);
    }
    return;
}
