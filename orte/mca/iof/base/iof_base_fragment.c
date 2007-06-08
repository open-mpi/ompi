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
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "orte_config.h"
#include <stdlib.h>
#include <string.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <errno.h>
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_FCNTL_H
#include <sys/fcntl.h>
#endif
#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif
#include "opal/util/output.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/rml/rml_types.h"
#include "orte/mca/iof/base/base.h"
#include "orte/mca/iof/base/iof_base_endpoint.h"
#include "orte/mca/iof/base/iof_base_fragment.h"


/**
 *
 */

static void orte_iof_base_frag_construct(orte_iof_base_frag_t* frag)
{
    OMPI_DEBUG_ZERO(*frag);
    frag->frag_owner = NULL;
    frag->frag_len = 0;
    frag->frag_iov[0].iov_base = (IOVBASE_TYPE*)&frag->frag_hdr;
    frag->frag_iov[0].iov_len = sizeof(frag->frag_hdr);
    frag->frag_iov[1].iov_base = (IOVBASE_TYPE*)frag->frag_data;
    frag->frag_iov[1].iov_len = sizeof(frag->frag_data);
}


static void orte_iof_base_frag_destruct(orte_iof_base_frag_t* frag)
{
    frag->frag_iov[0].iov_base = (IOVBASE_TYPE*)&frag->frag_hdr;
    frag->frag_iov[0].iov_len = sizeof(frag->frag_hdr);
    frag->frag_iov[1].iov_base = (IOVBASE_TYPE*)frag->frag_data;
    frag->frag_iov[1].iov_len = sizeof(frag->frag_data);
}

OBJ_CLASS_INSTANCE(
    orte_iof_base_frag_t,
    opal_free_list_item_t,
    orte_iof_base_frag_construct,
    orte_iof_base_frag_destruct);

/*
 *
 */

static void orte_iof_base_frag_send_cb(
    int status,
    orte_process_name_t* peer,
    struct iovec* msg,
    int count,
    orte_rml_tag_t tag,
    void* cbdata)
{
    orte_iof_base_frag_t* frag = (orte_iof_base_frag_t*)cbdata;
    ORTE_IOF_BASE_FRAG_RETURN(frag);
}

/*
 *
 */

int _orte_iof_base_frag_ack(orte_iof_base_frag_t* frag, bool do_close, 
                            const char* file, int line)
{
    int rc = ORTE_SUCCESS;
  
    if(frag->frag_hdr.hdr_msg.msg_len > 0) {
        frag->frag_hdr.hdr_common.hdr_type = 
            do_close ? ORTE_IOF_BASE_HDR_CLOSE : ORTE_IOF_BASE_HDR_ACK;
        ORTE_IOF_BASE_HDR_MSG_HTON(frag->frag_hdr.hdr_msg);

        /* start non-blocking OOB call to forward header */
        rc = orte_rml.send_nb(
            &frag->frag_src,
            frag->frag_iov,
            1,
            ORTE_RML_TAG_IOF_SVC,
            0,
            orte_iof_base_frag_send_cb,
            frag);
        if(rc != ORTE_SUCCESS) {
            opal_output(0, "orte_iof_base_frag_ack: orte_oob_send failed with status=%d\n", rc);
        }
    }
    return rc;
}

