/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2009 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2008-2010 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2010-2011 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "orte/mca/rml/rml.h"
#include "orte/util/name_fns.h"
#include "orte/util/show_help.h"
#include "orte/runtime/orte_globals.h"
#include "orte/mca/errmgr/errmgr.h"

#include "ompi/constants.h"
#include "ompi/mca/dpm/dpm.h"
#include "ompi/mca/common/sm/common_sm_rml.h"

OBJ_CLASS_INSTANCE(
    mca_common_sm_rml_pending_rml_msg_types_t,
    opal_object_t,
    NULL,
    NULL
);

/* ////////////////////////////////////////////////////////////////////////// */
/**
 * this routine assumes that sorted_procs is in the following state:
 *     o all the local procs at the beginning.
 *     o sorted_procs[0] is the lowest named process.
 */
int
mca_common_sm_rml_info_bcast(opal_shmem_ds_t *ds_buf,
                             ompi_proc_t **procs,
                             size_t num_procs,
                             int tag,
                             bool bcast_root,
                             char *msg_id_str,
                             opal_list_t *pending_rml_msgs)
{
    int rc = OMPI_SUCCESS;
    struct iovec iov[MCA_COMMON_SM_RML_MSG_LEN];
    int iovrc;
    size_t p;
    char msg_id_str_to_tx[OPAL_PATH_MAX];

    strncpy(msg_id_str_to_tx, msg_id_str, sizeof(msg_id_str_to_tx) - 1);

    /* let the first item be the queueing id name */
    iov[0].iov_base = (ompi_iov_base_ptr_t)msg_id_str_to_tx;
    iov[0].iov_len = sizeof(msg_id_str_to_tx);
    iov[1].iov_base = (ompi_iov_base_ptr_t)ds_buf;
    iov[1].iov_len = sizeof(opal_shmem_ds_t);

    /* figure out if i am the root proc in the group.
     * if i am, bcast the message the rest of the local procs.
     */
    if (bcast_root) {
        opal_progress_event_users_increment();
        /* first num_procs items should be local procs */
        for (p = 1; p < num_procs; ++p) {
            iovrc = orte_rml.send(&(procs[p]->proc_name), iov,
                                  MCA_COMMON_SM_RML_MSG_LEN, tag, 0);
            if ((ssize_t)(iov[0].iov_len + iov[1].iov_len) > iovrc) {
                ORTE_ERROR_LOG(OMPI_ERR_COMM_FAILURE);
                opal_progress_event_users_decrement();
                rc = OMPI_ERROR;
                goto out;
            }
        }
        opal_progress_event_users_decrement();
    }
    else { /* i am NOT the root ("lowest") proc */
        opal_list_item_t *item;
        mca_common_sm_rml_pending_rml_msg_types_t *rml_msg;
        /* because a component query can be performed simultaneously in multiple
         * threads, the RML messages may arrive in any order.  so first check to
         * see if we previously received a message for me.
         */
        for (item = opal_list_get_first(pending_rml_msgs);
             opal_list_get_end(pending_rml_msgs) != item;
             item = opal_list_get_next(item)) {
            rml_msg = (mca_common_sm_rml_pending_rml_msg_types_t *)item;
            /* was the message for me? */
            if (0 == strcmp(rml_msg->msg_id_str, msg_id_str)) {
                opal_list_remove_item(pending_rml_msgs, item);
                /*                 from ==============> to */
                opal_shmem_ds_copy(&rml_msg->shmem_ds, ds_buf);
                OBJ_RELEASE(item);
                break;
            }
        }
        /* if we didn't find a message already waiting, block on receiving from
         * the RML.
         */
        if (opal_list_get_end(pending_rml_msgs) == item) {
            do {
                /* bump up the libevent polling frequency while we're in this
                 * RML recv, just to ensure we're checking libevent frequently.
                 */
                opal_progress_event_users_increment();
                iovrc = orte_rml.recv(&(procs[0]->proc_name), iov,
                                      MCA_COMMON_SM_RML_MSG_LEN, tag, 0);
                opal_progress_event_users_decrement();
                if (iovrc < 0) {
                    ORTE_ERROR_LOG(OMPI_ERR_RECV_LESS_THAN_POSTED);
                    rc = OMPI_ERROR;
                    goto out;
                }
                /* was the message for me?  if so, we're done */
                if (0 == strcmp(msg_id_str_to_tx, msg_id_str)) {
                    break;
                }
                /* if not, put it on the pending list and try again */
                if (NULL == (rml_msg =
                            OBJ_NEW(mca_common_sm_rml_pending_rml_msg_types_t)))
                {
                    ORTE_ERROR_LOG(OMPI_ERR_OUT_OF_RESOURCE);
                    rc = OMPI_ERROR;
                    goto out;
                }
                /* not for me, so place on list */
                /*                 from ========> to */
                opal_shmem_ds_copy(ds_buf, &rml_msg->shmem_ds);
                memcpy(rml_msg->msg_id_str, msg_id_str_to_tx, OPAL_PATH_MAX);
                opal_list_append(pending_rml_msgs, &(rml_msg->super));
            } while(1);
        }
    }

out:
    return rc;
}

