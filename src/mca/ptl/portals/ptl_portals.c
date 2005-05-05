/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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

#include "ompi_config.h"
#include "portals_config.h"

#include <sys/types.h>
#include <unistd.h>
#include <stdio.h>
#include <errno.h>

#include "include/constants.h"
#include "util/output.h"
#include "mca/pml/pml.h"
#include "mca/pml/base/pml_base_sendreq.h"

#include "ptl_portals.h"
#include "ptl_portals_compat.h"
#include "ptl_portals_sendfrag.h"

mca_ptl_portals_module_t mca_ptl_portals_module = {
    {
        &mca_ptl_portals_component.super,
        0,   /* max size of request cache */
        sizeof(mca_ptl_portals_send_frag_t),   /* byes required by ptl for a request */
        0,   /* max size of first frag */
        0,   /* min size of frag */
        0,   /* max size of frag */
        0,   /* exclusivity */
        0,   /* latency */
        0,   /* bandwidth */
        MCA_PTL_PUT,   /* ptl flags */

        mca_ptl_portals_add_procs,
        mca_ptl_portals_del_procs,
        mca_ptl_portals_finalize,
        mca_ptl_portals_send,
        mca_ptl_portals_send,
        NULL,
        mca_ptl_portals_matched,
        mca_ptl_portals_request_init,
        mca_ptl_portals_request_fini,

        NULL,
        NULL,
        NULL,

        NULL,  /* PTL stack */
        NULL   /* PML use */
    },
};



int
mca_ptl_portals_add_procs(struct mca_ptl_base_module_t* ptl,
                          size_t nprocs, struct ompi_proc_t **procs,
                          struct mca_ptl_base_peer_t** peers,
                          ompi_bitmap_t* reachable)
{
    int ret;
    struct ompi_proc_t *local_proc = ompi_proc_local();
    struct ompi_proc_t *curr_proc;
    ptl_process_id_t *portals_procs;
    size_t i;
    unsigned long distance;
    struct mca_ptl_portals_module_t *myptl = (struct mca_ptl_portals_module_t*) ptl;

    /* make sure our environment is fully initialized.  At end of this
       call, we have a working network handle on our module and
       portals_procs will have the portals process identifier for each
       proc (ordered, in theory) */
    ret = mca_ptl_portals_add_procs_compat((struct mca_ptl_portals_module_t*) ptl,
                                           nprocs, procs, &portals_procs);
    if (OMPI_SUCCESS != ret) return ret;

    /* loop through all procs, setting our reachable flag */
    for (i= 0; i < nprocs ; ++i) {
        curr_proc = procs[i];
        /* BWB - do we want to send to self?  No for now */
        if (curr_proc == local_proc) continue;

        /* make sure we can reach the process */
        ret = PtlNIDist(myptl->ni_handle,
                        portals_procs[i],
                        &distance);
        if (ret != PTL_OK) {
            ompi_output_verbose(100, mca_ptl_portals_component.portals_output,
                                "Could not find distance to process %d", i);
            continue;
        }

        /* set the peer as a pointer to the address */
        peers[i] = (struct mca_ptl_base_peer_t*) &(portals_procs[i]);

        /* and here we can reach */
        ompi_bitmap_set_bit(reachable, i);
    }

    return OMPI_SUCCESS;
}


int
mca_ptl_portals_del_procs(struct mca_ptl_base_module_t *ptl,
			  size_t nprocs,
			  struct ompi_proc_t **procs,
			  struct mca_ptl_base_peer_t **peers)
{
    /* yeah, I have no idea what to do here */

    return OMPI_SUCCESS;
}



int
mca_ptl_portals_module_enable(struct mca_ptl_portals_module_t *ptl,
                              int enable)
{
    int i, ret;

    if (enable == 0) {
        /* disable the unexpected receive queue */
        /* BWB - not really sure how - would have to track a lot more data... */
    } else {
        /* only do all the hard stuff if we haven't created the queue */
        if (ptl->frag_queues_created) return OMPI_SUCCESS;

        /* create an event queue, then the match entries for the match
           entries */
        ret = PtlEQAlloc(ptl->ni_handle,
                         ptl->first_frag_queue_size,
                         PTL_EQ_HANDLER_NONE,
                         &(ptl->frag_receive_eq_handle));
        if (ret != PTL_OK) {
            ompi_output(mca_ptl_portals_component.portals_output,
                        "Failed to allocate event queue: %d", ret);
            return OMPI_ERROR;
        }
        ompi_output_verbose(100, mca_ptl_portals_component.portals_output,
                            "allocated event queue: %d",
                            ptl->frag_receive_eq_handle);

        for (i = 0 ; i < ptl->first_frag_num_entries ; ++i) {
            ret = ptl_portals_new_frag_entry(ptl);
            if (OMPI_SUCCESS != ret) return ret;
            ptl->frag_queues_created = true;
        }
    }

    return OMPI_SUCCESS;
}


int
ptl_portals_new_frag_entry(struct mca_ptl_portals_module_t *ptl)
{
    ptl_handle_me_t me_handle;
    ptl_handle_md_t md_handle;
    ptl_md_t md;
    void *mem;
    int ret;
    ptl_process_id_t proc = { PTL_NID_ANY, PTL_PID_ANY };

    /* create match entry */
    ret = PtlMEAttach(ptl->ni_handle,
                      PTL_PORTALS_FRAG_TABLE_ID,
                      proc,
                      0, /* match bits */
                      0, /* ignore bits */
                      PTL_UNLINK,
                      PTL_INS_AFTER,
                      &me_handle);
    if (PTL_OK != ret) return OMPI_ERROR;

    /* and some memory */
    mem = malloc(ptl->first_frag_entry_size);
    if (NULL == mem) {
        PtlMEUnlink(me_handle);
        return OMPI_ERR_TEMP_OUT_OF_RESOURCE;
    }

    /* and the memory descriptor */
    md.start = mem;
    md.length = ptl->first_frag_entry_size;
    md.threshold = PTL_MD_THRESH_INF;
    md.max_size = md.length - ptl->super.ptl_first_frag_size;
    md.options = PTL_MD_OP_PUT | PTL_MD_MAX_SIZE;
    md.user_ptr = NULL;
    md.eq_handle = ptl->frag_receive_eq_handle;

    ret = PtlMDAttach(me_handle,
                      md,
                      PTL_UNLINK,
                      &md_handle);
    if (PTL_OK != ret) {
        PtlMEUnlink(me_handle);
        return OMPI_ERROR;
    }

    ompi_output_verbose(50, mca_ptl_portals_component.portals_output,
                        "new receive buffer posted");

    return OMPI_SUCCESS;
}


int
mca_ptl_portals_send(struct mca_ptl_base_module_t *ptl_base,
		     struct mca_ptl_base_peer_t *ptl_peer,
		     struct mca_pml_base_send_request_t *sendreq,
		     size_t offset, size_t size, int flags)
{
    mca_ptl_portals_module_t* ptl = (mca_ptl_portals_module_t*) ptl_base;
    ptl_process_id_t *peer_id = (ptl_process_id_t*) ptl_peer;
    mca_ptl_portals_send_frag_t* sendfrag;
    mca_ptl_base_header_t* hdr;
    int ret;
    ptl_md_t md;

    ompi_output_verbose(100, mca_ptl_portals_component.portals_output,
                        "mca_ptl_portals_send to %lu, %lu",
                        peer_id->nid, peer_id->pid);

    if (sendreq->req_cached) {
        sendfrag = (mca_ptl_portals_send_frag_t*)(sendreq+1);
    } else {
        ompi_output(mca_ptl_portals_component.portals_output,
                    "request not cached - not implemented.");
        return OMPI_ERROR;
    }

    /* initialize convertor */
    if (size > 0) {
        ompi_output(mca_ptl_portals_component.portals_output,
                    "request size > 0, not implemented");
        return OMPI_ERROR;
    } else {
        sendfrag->frag_send.frag_base.frag_addr = NULL;
        sendfrag->frag_send.frag_base.frag_size = 0;
    }

    /* setup message header */
    hdr = &sendfrag->frag_send.frag_base.frag_header;
    if(offset == 0) {
        hdr->hdr_common.hdr_flags = flags;
        hdr->hdr_match.hdr_contextid = sendreq->req_base.req_comm->c_contextid;
        hdr->hdr_match.hdr_src = sendreq->req_base.req_comm->c_my_rank;
        hdr->hdr_match.hdr_dst = sendreq->req_base.req_peer;
        hdr->hdr_match.hdr_tag = sendreq->req_base.req_tag;
        hdr->hdr_match.hdr_msg_length = sendreq->req_bytes_packed;
        hdr->hdr_match.hdr_msg_seq = sendreq->req_base.req_sequence;
    } else {
        ompi_output(mca_ptl_portals_component.portals_output,
                    "offset > 0, not implemented");
        return OMPI_ERROR;
    }

    /* fragment state */
#if 0
    sendfrag->frag_send.frag_base.frag_owner = &ptl_peer->peer_ptl->super;
#endif
    sendfrag->frag_send.frag_request = sendreq;
#if 0
    sendfrag->frag_send.frag_base.frag_peer = ptl_peer;
#endif


    /* must update the offset after actual fragment size is determined 
     * before attempting to send the fragment
     */
    mca_pml_base_send_request_offset(sendreq,
        sendfrag->frag_send.frag_base.frag_size);
#if 0
    md.start = mem;
    md.length = ptl->first_frag_entry_size;
    md.threshold = PTL_MD_THRESH_INF;
    md.max_size = md.length - ptl->super.ptl_first_frag_size;
    md.options = PTL_MD_OP_PUT | PTL_MD_MAX_SIZE;
    md.user_ptr = NULL;
    md.eq_handle = ptl->frag_receive_eq_handle;
#endif

    return OMPI_ERROR;
}


int
mca_ptl_portals_finalize(struct mca_ptl_base_module_t *ptl_base)
{
    struct mca_ptl_portals_module_t *ptl =
        (struct mca_ptl_portals_module_t *) ptl_base;
    int ret;

    ret = PtlNIFini(ptl->ni_handle);
    if (PTL_OK != ret) {
        ompi_output_verbose(50, mca_ptl_portals_component.portals_output,
                            "PtlNIFini returned %d\n", ret);
        return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
}
