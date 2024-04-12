/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2012-2013 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2013-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2020      Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "prte_config.h"
#include "types.h"

#include "src/pmix/pmix-internal.h"
#include "src/util/name_fns.h"
#include "src/util/pmix_output.h"
#include "src/util/pmix_name_fns.h"

#include "src/mca/errmgr/errmgr.h"
#include "src/mca/oob/base/base.h"
#include "src/runtime/prte_globals.h"
#include "src/threads/pmix_threads.h"

#include "src/rml/rml.h"

int prte_rml_send_buffer_nb(pmix_rank_t rank,
                            pmix_data_buffer_t *buffer,
                            prte_rml_tag_t tag)
{
    prte_rml_recv_t *rcv;
    prte_rml_send_t *snd;

    PMIX_OUTPUT_VERBOSE((1, prte_rml_base.rml_output,
         "%s rml_send_buffer to peer %s at tag %d",
         PRTE_NAME_PRINT(PRTE_PROC_MY_NAME),
         PMIX_RANK_PRINT(rank), tag));

    if (PRTE_RML_TAG_INVALID == tag) {
        /* cannot send to an invalid tag */
        PRTE_ERROR_LOG(PRTE_ERR_BAD_PARAM);
        return PRTE_ERR_BAD_PARAM;
    }
    if (PMIX_RANK_INVALID == rank) {
        /* cannot send to an invalid peer */
        PRTE_ERROR_LOG(PRTE_ERR_BAD_PARAM);
        return PRTE_ERR_BAD_PARAM;
    }

    /* if this is a message to myself, then just post the message
     * for receipt - no need to dive into the oob
     */
    if (PRTE_PROC_MY_NAME->rank == rank) { /* local delivery */
        PMIX_OUTPUT_VERBOSE((1, prte_rml_base.rml_output,
                             "%s rml_send_buffer_to_self at tag %d",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), tag));
        /* copy the message for the recv */
        rcv = PMIX_NEW(prte_rml_recv_t);
        PMIX_LOAD_PROCID(&rcv->sender, PRTE_PROC_MY_NAME->nspace, rank);
        rcv->tag = tag;
        rcv->dbuf = buffer;
        /* post the message for receipt - since the send callback was posted
         * first and has the same priority, it will execute first
         */
        PRTE_RML_ACTIVATE_MESSAGE(rcv);
        return PRTE_SUCCESS;
    }

    snd = PMIX_NEW(prte_rml_send_t);
    PMIX_LOAD_PROCID(&snd->dst, PRTE_PROC_MY_NAME->nspace, rank);
    snd->origin = *PRTE_PROC_MY_NAME;
    snd->tag = tag;
    snd->dbuf = buffer;

    /* activate the OOB send state */
    PRTE_OOB_SEND(snd);

    return PRTE_SUCCESS;
}
