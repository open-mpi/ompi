/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2009 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2008-2010 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2010-2013 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2014      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "opal/types.h"
#include "opal/dss/dss.h"
#include "opal/util/show_help.h"

#include "ompi/constants.h"
#include "ompi/mca/rte/rte.h"
#include "ompi/mca/common/sm/common_sm_rml.h"

/* only for debug purposes only */
#include <assert.h>
#ifdef HAVE_STRING_H
#include <string.h>
#endif

typedef struct {
    opal_buffer_t buf;
    bool active;
    int status;
} sm_return_t;

static void sml_recv(int status,
                     ompi_process_name_t* peer,
                     opal_buffer_t* buffer,
                     ompi_rml_tag_t tag,
                     void* cbdata)
{
    sm_return_t *smr = (sm_return_t*)cbdata;

    opal_dss.copy_payload(&smr->buf, buffer);
    smr->active = false;
    smr->status = status;
}

/* ////////////////////////////////////////////////////////////////////////// */
/**
 * this routine assumes that sorted_procs is in the following state:
 *     o all the local procs at the beginning.
 *     o procs[0] is the lowest named process.
 */
int
mca_common_sm_rml_info_bcast(opal_shmem_ds_t *out_ds_buf,
                             ompi_proc_t **procs,
                             size_t num_local_procs,
                             int tag,
                             bool proc0,
                             char *msg_id_str)
{
    int rc = OMPI_SUCCESS;
    char *msg_id_str_to_tx = NULL;
    sm_return_t smr;

    OBJ_CONSTRUCT(&smr.buf, opal_buffer_t);

    /* figure out if i am the root proc in the group.  if i am, bcast the
     * message the rest of the local procs. */
    if (proc0) {
        opal_buffer_t *buffer = NULL;
        size_t p;
        if (NULL == (buffer = OBJ_NEW(opal_buffer_t))) {
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
        /* pack the data that we are going to send. first the queueing id, then
         * the shmem_ds buf. note that msg_id_str is used only for verifying
         * "expected" common sm usage.  see "RML Messaging and Our Assumptions"
         * note in common_sm.c for more details. */
        rc = opal_dss.pack(buffer, &msg_id_str, 1, OPAL_STRING);
        if (OPAL_SUCCESS != rc) {
            OMPI_ERROR_LOG(rc);
            rc = OMPI_ERR_PACK_FAILURE;
            OBJ_RELEASE(buffer);
            goto out;
        }
        rc = opal_dss.pack(buffer, out_ds_buf,
                           (int32_t)sizeof(opal_shmem_ds_t),
                           OPAL_BYTE);
        if (OPAL_SUCCESS != rc) {
            OMPI_ERROR_LOG(rc);
            rc = OMPI_ERR_PACK_FAILURE;
            OBJ_RELEASE(buffer);
            goto out;
        }
        /* first num_local_procs items should be local procs */
        for (p = 1; p < num_local_procs; ++p) {
            OBJ_RETAIN(buffer);
            rc = ompi_rte_send_buffer_nb(&(procs[p]->proc_name), buffer, tag,
                                         ompi_rte_send_cbfunc, NULL);
            if (0 > rc) {
                OBJ_RELEASE(buffer);  /* remove the ref from 4 lines above */
                OBJ_RELEASE(buffer);  /* and get rid of our own reference (OBJ_NEW) */
                OMPI_ERROR_LOG(rc);
                rc = OMPI_ERROR;
                goto out;
            }
        }
        OBJ_RELEASE(buffer);
    }
    /* i am NOT the root proc */
    else {
        int32_t num_vals;
        smr.active = true;
        smr.status = OMPI_ERROR;
        ompi_rte_recv_buffer_nb(&(procs[0]->proc_name),tag,
                                OMPI_RML_NON_PERSISTENT,
                                sml_recv, &smr);
        while (smr.active) {
            opal_progress();
        }

        if (OMPI_SUCCESS != smr.status) {
            OMPI_ERROR_LOG(smr.status);
            rc = smr.status;
            goto out;
        }
        /* unpack the buffer */
        num_vals = 1;
        rc = opal_dss.unpack(&smr.buf, &msg_id_str_to_tx, &num_vals,
                             OPAL_STRING);
        if (0 > rc) {
            OMPI_ERROR_LOG(rc);
            rc = OMPI_ERROR;
            goto out;
        }
        num_vals = (int32_t)sizeof(opal_shmem_ds_t);
        rc = opal_dss.unpack(&smr.buf, out_ds_buf, &num_vals, OPAL_BYTE);
        if (0 > rc) {
            OMPI_ERROR_LOG(rc);
            rc = OMPI_ERROR;
            goto out;
        }
        /* the message better be for me.  if not, freak out because this
         * probably means that common sm is being used in a new way that lies
         * outside of our current scope of assumptions. see "RML Messaging and
         * Our Assumptions" note in common_sm.c */
        if (0 != strcmp(msg_id_str_to_tx, msg_id_str)) {
            opal_show_help("help-mpi-common-sm.txt", "unexpected message id",
                           true, ompi_process_info.nodename,
                           msg_id_str, msg_id_str_to_tx);
            rc = OMPI_ERROR;
            goto out;
        }
    }

out:
    if (NULL != msg_id_str_to_tx) {
        free(msg_id_str_to_tx);
        msg_id_str_to_tx = NULL;
    }
    OBJ_DESTRUCT(&smr.buf);
    return rc;
}
