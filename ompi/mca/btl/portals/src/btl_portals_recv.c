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

#include "include/constants.h"

#include "btl_portals.h"
#include "btl_portals_recv.h"
#include "btl_portals_frag.h"


OBJ_CLASS_INSTANCE(mca_btl_portals_recv_chunk_t,
                   opal_list_item_t,
                   NULL, NULL);

int
mca_btl_portals_recv_enable(mca_btl_portals_module_t *module)
{
    ptl_md_t md;
    ptl_handle_md_t md_h;
    ptl_process_id_t any_proc = {PTL_NID_ANY, PTL_PID_ANY};
    int ret;
    int i;

    /* create the reject entry */
    md.start = NULL;
    md.length = 0;
    md.threshold = PTL_MD_THRESH_INF;
    md.max_size = 0;
    md.options = PTL_MD_TRUNCATE;
    md.user_ptr = NULL;
    md.eq_handle = PTL_EQ_NONE;

    ret = PtlMEAttach(module->portals_ni_h,
                      BTL_PORTALS_SEND_TABLE_ID,
                      any_proc,
                      0, /* match */
                      0, /* ignore */
                      PTL_RETAIN,
                      PTL_INS_AFTER,
                      &(module->portals_recv_reject_me_h));
    if (PTL_OK != ret) {
        opal_output(mca_btl_portals_component.portals_output,
                    "Error creating recv reject ME: %d", ret);
        return OMPI_ERROR;
    }

    ret = PtlMDAttach(module->portals_recv_reject_me_h,
                      md,
                      PTL_RETAIN,
                      &md_h);
    if (PTL_OK != ret) {
        opal_output(mca_btl_portals_component.portals_output,
                    "Error attaching recv reject MD: %d", ret);
        mca_btl_portals_recv_disable(module);
        return OMPI_ERROR;
    }

    /* create the recv chunks */
    for (i = 0 ; i < module->portals_recv_mds_num ; ++i) {
        mca_btl_portals_recv_chunk_t *chunk = 
            mca_btl_portals_recv_chunk_init(module);
        if (NULL == chunk) {
            mca_btl_portals_recv_disable(module);
            return OMPI_ERROR;
        }
        opal_list_append(&(module->portals_recv_chunks),
                         (opal_list_item_t*) chunk);
        mca_btl_portals_activate_chunk(chunk);
    }

    return OMPI_SUCCESS;
}


int
mca_btl_portals_recv_disable(mca_btl_portals_module_t *module)
{
    opal_list_item_t *item;

    if (opal_list_get_size(&module->portals_recv_chunks) > 0) {
        while (NULL != 
               (item = opal_list_remove_first(&module->portals_recv_chunks))) {
            mca_btl_portals_recv_chunk_t *chunk = 
                (mca_btl_portals_recv_chunk_t*) item;
            mca_btl_portals_recv_chunk_free(chunk);
        }
    }

    if (PTL_INVALID_HANDLE != module->portals_recv_reject_me_h) {
        /* destroy the reject entry */
        PtlMEUnlink(module->portals_recv_reject_me_h);
        module->portals_recv_reject_me_h = PTL_INVALID_HANDLE;
    }

    return OMPI_SUCCESS;
}


mca_btl_portals_recv_chunk_t* 
mca_btl_portals_recv_chunk_init(mca_btl_portals_module_t *module)
{
    mca_btl_portals_recv_chunk_t *chunk;

    chunk = OBJ_NEW(mca_btl_portals_recv_chunk_t);
    chunk->btl = module;
    chunk->length = module->portals_recv_mds_size;
    chunk->start = malloc(chunk->length);
    if (chunk->start == NULL) return NULL;

    chunk->me_h = PTL_INVALID_HANDLE;
    chunk->md_h = PTL_INVALID_HANDLE;

    chunk->full = false;
    chunk->pending = 0;

    return chunk;
}


int
mca_btl_portals_recv_chunk_free(mca_btl_portals_recv_chunk_t *chunk)
{
    /* need to clear out the md */
    while (chunk->pending != 0) {
        mca_btl_portals_component_progress();
    }

    if (PTL_INVALID_HANDLE != chunk->md_h) {
        PtlMDUnlink(chunk->md_h);
        chunk->md_h = PTL_INVALID_HANDLE;
    }

    if (NULL != chunk->start) {
        free(chunk->start);
        chunk->start = NULL;
    }
    chunk->length = 0;
    chunk->full = false;

    return OMPI_SUCCESS;
}


int
mca_btl_portals_process_recv(mca_btl_portals_module_t *module, 
                             ptl_event_t *ev)
{
    mca_btl_portals_frag_t *frag = NULL;
    mca_btl_portals_recv_chunk_t *chunk = ev->md.user_ptr;
    mca_btl_base_tag_t tag = (mca_btl_base_tag_t) ev->hdr_data;

    int ret;

    switch (ev->type) {
    case PTL_EVENT_PUT_START:
        opal_output_verbose(90, mca_btl_portals_component.portals_output,
                            "recv: PTL_EVENT_PUT_START for tag %d, link %d",
                            tag, (int) ev->link);

        if (ev->ni_fail_type != PTL_NI_OK) {
            opal_output(mca_btl_portals_component.portals_output,
                        "Failure to start event\n");

            OPAL_THREAD_ADD32(&(chunk->pending), 1);
        }
        break;
    case PTL_EVENT_PUT_END:
        opal_output_verbose(90, mca_btl_portals_component.portals_output,
                            "recv: PTL_EVENT_PUT_END for tag %d, link %d",
                            tag, (int) ev->link);

        if (ev->ni_fail_type != PTL_NI_OK) {
            opal_output(mca_btl_portals_component.portals_output,
                        "Failure to start event\n");
            return OMPI_ERROR;
        } 

        /* ok, we've got data */
        opal_output_verbose(95, mca_btl_portals_component.portals_output,
                            "received data for tag %d\n", tag);

        /* it's a user, so we have to manually setup the segment */
        MCA_BTL_PORTALS_FRAG_ALLOC_USER(module, frag, ret);
        frag->type = MCA_BTL_PORTALS_FRAG_RECV;
        frag->size = ev->mlength;
        frag->base.des_dst = &frag->segment;
        frag->base.des_dst_cnt = 1;
        frag->base.des_src = NULL;
        frag->base.des_src_cnt = 0;

        frag->segment.seg_addr.pval = (((char*) ev->md.start) + ev->offset);
        frag->segment.seg_len = frag->size;
        frag->segment.seg_key.key64 = 0;

        frag->u.recv_frag.chunk = chunk;

        if (ev->md.length - (ev->offset + ev->mlength) < ev->md.max_size) {
            /* the chunk is full.  It's deactivated, automagically but we
               can't start it up again until everyone is done with it.
               The actual reactivation and all that will happen after the
               free completes the last operation... */
            chunk->full = true;
            opal_atomic_mb(); 
        }

        module->portals_reg[tag].cbfunc(&module->super,
                                        tag,
                                        &frag->base,
                                        module->portals_reg[tag].cbdata);
        break;
    default:
        break;
    }

    return OMPI_SUCCESS;
}


