#include "ompi_config.h"
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
#include "util/output.h"
#include "mca/oob/base/base.h"
#include "mca/iof/base/base.h"
#include "mca/iof/base/iof_base_endpoint.h"
#include "mca/iof/base/iof_base_fragment.h"


/**
 *
 */

static void mca_iof_base_frag_construct(mca_iof_base_frag_t* frag)
{
    frag->frag_owner = NULL;
    frag->frag_len = 0;
    frag->frag_iov[0].iov_base = &frag->frag_hdr;
    frag->frag_iov[0].iov_len = sizeof(frag->frag_hdr);
    frag->frag_iov[1].iov_base = frag->frag_data;
    frag->frag_iov[1].iov_len = sizeof(frag->frag_data);
}


static void mca_iof_base_frag_destruct(mca_iof_base_frag_t* frag)
{
    frag->frag_iov[0].iov_base = &frag->frag_hdr;
    frag->frag_iov[0].iov_len = sizeof(frag->frag_hdr);
    frag->frag_iov[1].iov_base = frag->frag_data;
    frag->frag_iov[1].iov_len = sizeof(frag->frag_data);
}

OBJ_CLASS_INSTANCE(
    mca_iof_base_frag_t,
    ompi_list_item_t,
    mca_iof_base_frag_construct,
    mca_iof_base_frag_destruct);

/*
 *
 */

static void mca_iof_base_frag_send_cb(
    int status,
    ompi_process_name_t* peer,
    struct iovec* msg,
    int count,
    int tag,
    void* cbdata)
{
    mca_iof_base_frag_t* frag = (mca_iof_base_frag_t*)cbdata;
    MCA_IOF_BASE_FRAG_RETURN(frag);
}

/*
 *
 */

int mca_iof_base_frag_ack(mca_iof_base_frag_t* frag)
{
    int rc;
    frag->frag_hdr.hdr_common.hdr_type = MCA_IOF_BASE_HDR_ACK;
    MCA_IOF_BASE_HDR_MSG_HTON(frag->frag_hdr.hdr_msg);

    /* start non-blocking OOB call to forward header */
    rc = mca_oob_send_nb(
        &frag->frag_src,
        frag->frag_iov,
        1,
        MCA_OOB_TAG_IOF_SVC,
        0,
        mca_iof_base_frag_send_cb,
        frag);
    if(rc != OMPI_SUCCESS) {
        ompi_output(0, "mca_iof_base_frag_ack: mca_oob_send failed with status=%d\n", rc);
    }
    return rc;
}

