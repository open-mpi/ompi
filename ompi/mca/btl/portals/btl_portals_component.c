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
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include <sys/types.h>
#include <unistd.h>
#include <limits.h>

#include "ompi/constants.h"

#include "opal/util/output.h"
#include "opal/threads/threads.h"
#include "opal/mca/base/mca_base_param.h"
#include "ompi/mca/common/portals/common_portals.h"

#include "btl_portals.h"
#include "btl_portals_frag.h"
#include "btl_portals_send.h"
#include "btl_portals_recv.h"


mca_btl_portals_component_t mca_btl_portals_component = {
    {
      /* First, the mca_base_module_t struct containing meta
         information about the module itself */
      {
        /* Indicate that we are a pml v1.0.0 module (which also
           implies a specific MCA version) */

        MCA_BTL_BASE_VERSION_1_0_1,

        "portals", /* MCA module name */
        OMPI_MAJOR_VERSION,  /* MCA module major version */
        OMPI_MINOR_VERSION,  /* MCA module minor version */
        OMPI_RELEASE_VERSION,  /* MCA module release version */
        mca_btl_portals_component_open,  /* module open */
        mca_btl_portals_component_close  /* module close */
      },
      
      /* Next the MCA v1.0.0 module meta data */
      
      {
        /* Whether the module is checkpointable or not */
        
        false
      },
      
      mca_btl_portals_component_init,  
      mca_btl_portals_component_progress,
    }
};


static opal_output_stream_t portals_output_stream;

int
mca_btl_portals_component_open(void)
{
    int i;
    int dummy;

    ompi_common_portals_register_mca();

    /*
     * get configured state for component 
     */

    /* start up debugging output */
    OBJ_CONSTRUCT(&portals_output_stream, opal_output_stream_t);
    portals_output_stream.lds_is_debugging = true;
    portals_output_stream.lds_want_stdout = true;
    portals_output_stream.lds_file_suffix = "btl-portals";
    mca_base_param_reg_int(&mca_btl_portals_component.super.btl_version,
                           "debug_level",
                           "Debugging verbosity (0 - 100)",
                           false,
                           false,
                           0, 
                           &(portals_output_stream.lds_verbose_level));
    asprintf(&(portals_output_stream.lds_prefix),
             "btl: portals (%s): ", ompi_common_portals_nodeid());
    mca_btl_portals_component.portals_output = 
        opal_output_open(&portals_output_stream);

    mca_base_param_reg_int(&mca_btl_portals_component.super.btl_version,
                           "free_list_init_num",
                           "Initial number of elements to initialize in free lists",
                           false,
                           false,
                           16,
                           &(mca_btl_portals_component.portals_free_list_init_num));
    mca_base_param_reg_int(&mca_btl_portals_component.super.btl_version,
                           "free_list_max_num",
                           "Max number of elements to initialize in free lists",
                           false,
                           false,
                           1024,
                           &(mca_btl_portals_component.portals_free_list_max_num));
    mca_base_param_reg_int(&mca_btl_portals_component.super.btl_version,
                           "free_list_inc_num",
                           "Increment count for free lists",
                           false,
                           false,
                           16,
                           &(mca_btl_portals_component.portals_free_list_inc_num));
    mca_base_param_reg_int(&mca_btl_portals_component.super.btl_version,
                           "eager_frag_limit",
                           "Maximum number of pre-pinned eager fragments",
                           false,
                           false,
                           32,
                           &(mca_btl_portals_component.portals_free_list_eager_max_num));

    /* 
     * fill default module state 
     */
    mca_base_param_reg_int(&mca_btl_portals_component.super.btl_version,
                           "eager_limit",
                           "Maximum size for eager frag",
                           false,
                           false,
                           32 * 1024,
                           &dummy);
    mca_btl_portals_module.super.btl_eager_limit = dummy;

    mca_base_param_reg_int(&mca_btl_portals_component.super.btl_version,
                           "min_send_size",
                           "Minimum size for a send frag",
                           false,
                           false,
                           32 * 1024,
                           &dummy);
    mca_btl_portals_module.super.btl_min_send_size = dummy;
    mca_base_param_reg_int(&mca_btl_portals_component.super.btl_version,
                           "max_send_size",
                           "Maximum size for a send frag",
                           false,
                           false,
                           64 * 1024,
                           &dummy);
    mca_btl_portals_module.super.btl_max_send_size = dummy;
    mca_base_param_reg_int(&mca_btl_portals_component.super.btl_version,
                           "min_rdma_size",
                           "Minimum size for a rdma frag",
                           false,
                           false,
                           64 * 1024,
                           &dummy);
    mca_btl_portals_module.super.btl_min_rdma_size = dummy;
    mca_base_param_reg_int(&mca_btl_portals_component.super.btl_version,
                           "max_rdma_size",
                           "Maximum size for a rdma frag",
                           false,
                           false,
                           INT_MAX,
                           &dummy);
    mca_btl_portals_module.super.btl_max_rdma_size = dummy;

    mca_base_param_reg_int(&mca_btl_portals_component.super.btl_version,
                           "exclusivity",
                           "Exclusivity level for selection process",
                           false,
                           false,
                           60,
                           &dummy);
    mca_btl_portals_module.super.btl_exclusivity = dummy;
    mca_base_param_reg_int(&mca_btl_portals_component.super.btl_version,
                           "latency",
                           "Latency level for short message scheduling",
                           false,
                           false,
                           0,
                           &dummy);
    mca_btl_portals_module.super.btl_latency = dummy;
    mca_base_param_reg_int(&mca_btl_portals_component.super.btl_version,
                           "bandwidth",
                           "Bandwidth level for frag scheduling",
                           false,
                           false,
                           1000,
                           &dummy);
    mca_btl_portals_module.super.btl_bandwidth = dummy;

    /* send in place actually increases our latency because we have to
       hold on to the buffer until we're done with it, rather than
       copy and send.  So don't use it for now. */
    mca_btl_portals_module.super.btl_flags = MCA_BTL_FLAGS_RDMA;

    mca_btl_portals_module.portals_num_procs = 0;
    bzero(&(mca_btl_portals_module.portals_reg),
          sizeof(mca_btl_portals_module.portals_reg));

    for (i = 0 ; i < OMPI_BTL_PORTALS_EQ_SIZE ; ++i) {
        mca_btl_portals_module.portals_eq_sizes[i] = 0;
        mca_btl_portals_module.portals_eq_handles[i] = PTL_EQ_NONE;
    }
    /* eq handles will be created when the module is instantiated.
       Set sizes here */
    mca_base_param_reg_int(&mca_btl_portals_component.super.btl_version,
                           "eq_recv_size",
                           "Size of the receive event queue",
                           false,
                           false,
                           16 * 1024,
                           &(mca_btl_portals_module.portals_eq_sizes[OMPI_BTL_PORTALS_EQ_RECV]));

    mca_base_param_reg_int(&mca_btl_portals_component.super.btl_version,
                           "max_pending_ops",
                           "Maximum number of pending send/rdma frags",
                           false,
                           false,
                           8 * 1024,
                           &(mca_btl_portals_module.portals_max_outstanding_ops));
    /* ops_pending * 2 for end, ack */
    mca_btl_portals_module.portals_eq_sizes[OMPI_BTL_PORTALS_EQ_SEND] = 
        mca_btl_portals_module.portals_max_outstanding_ops * 2;

    mca_btl_portals_module.portals_recv_reject_me_h = PTL_INVALID_HANDLE;

    mca_base_param_reg_int(&mca_btl_portals_component.super.btl_version,
                           "recv_md_num",
                           "Number of send frag receive descriptors",
                           false,
                           false,
                           3,
                           &(mca_btl_portals_module.portals_recv_mds_num));
    mca_base_param_reg_int(&mca_btl_portals_component.super.btl_version,
                           "recv_md_size",
                           "Size of send frag receive descriptors",
                           false,
                           false,
                           10 * 1024 * 1024,
                           &(mca_btl_portals_module.portals_recv_mds_size));

    mca_btl_portals_module.portals_ni_h = PTL_INVALID_HANDLE;
    mca_btl_portals_module.portals_sr_dropped = 0;
    mca_btl_portals_module.portals_outstanding_ops = 0;
    mca_btl_portals_module.portals_rdma_key = 1;

    return OMPI_SUCCESS;
}


int
mca_btl_portals_component_close(void)
{
    /* release resources */
    if (NULL != portals_output_stream.lds_prefix) {
        free(portals_output_stream.lds_prefix);
    }

    /* close debugging stream */
    opal_output_close(mca_btl_portals_component.portals_output);
    mca_btl_portals_component.portals_output = -1;

    return OMPI_SUCCESS;
}


mca_btl_base_module_t**
mca_btl_portals_component_init(int *num_btls, 
                               bool enable_progress_threads,
                               bool enable_mpi_threads)
{
    mca_btl_base_module_t ** btls = malloc(sizeof(mca_btl_base_module_t*));
    btls[0] = (mca_btl_base_module_t*) &mca_btl_portals_module;

    if (enable_progress_threads || enable_mpi_threads) {
        opal_output_verbose(20, mca_btl_portals_component.portals_output,
                            "disabled because threads enabled");
        return NULL;
    }

    /* initialize portals btl.  note that this is in the compat code because
       it's fairly non-portable between implementations */
    if (OMPI_SUCCESS != ompi_common_portals_initialize()) {
        opal_output_verbose(20, mca_btl_portals_component.portals_output,
                            "disabled because compatibility init failed");
        return NULL;
    }

    OBJ_CONSTRUCT(&(mca_btl_portals_module.portals_frag_eager), ompi_free_list_t);
    OBJ_CONSTRUCT(&(mca_btl_portals_module.portals_frag_max), ompi_free_list_t);
    OBJ_CONSTRUCT(&(mca_btl_portals_module.portals_frag_user), ompi_free_list_t);

    /* eager frags */
    ompi_free_list_init(&(mca_btl_portals_module.portals_frag_eager),
                        sizeof(mca_btl_portals_frag_eager_t) + 
                        mca_btl_portals_module.super.btl_eager_limit,
                        OBJ_CLASS(mca_btl_portals_frag_eager_t),
                        mca_btl_portals_component.portals_free_list_init_num,
                        mca_btl_portals_component.portals_free_list_eager_max_num,
                        mca_btl_portals_component.portals_free_list_inc_num,
                        NULL);

    /* send frags */
    ompi_free_list_init(&(mca_btl_portals_module.portals_frag_max),
                        sizeof(mca_btl_portals_frag_max_t) + 
                        mca_btl_portals_module.super.btl_max_send_size,
                        OBJ_CLASS(mca_btl_portals_frag_max_t),
                        mca_btl_portals_component.portals_free_list_init_num,
                        mca_btl_portals_component.portals_free_list_max_num,
                        mca_btl_portals_component.portals_free_list_inc_num,
                        NULL);

    /* user frags */
    ompi_free_list_init(&(mca_btl_portals_module.portals_frag_user),
                        sizeof(mca_btl_portals_frag_user_t),
                        OBJ_CLASS(mca_btl_portals_frag_user_t),
                        mca_btl_portals_component.portals_free_list_init_num,
                        mca_btl_portals_component.portals_free_list_max_num,
                        mca_btl_portals_component.portals_free_list_inc_num,
                        NULL);

    /* recv frags */
    OBJ_CONSTRUCT(&(mca_btl_portals_module.portals_recv_frag),
                  mca_btl_portals_frag_recv_t);

    /* receive block list */
    OBJ_CONSTRUCT(&(mca_btl_portals_module.portals_recv_blocks), opal_list_t);

    /* list for send requests that have to be delayed */
    OBJ_CONSTRUCT(&(mca_btl_portals_module.portals_queued_sends),
                  opal_list_t);

    *num_btls = 1;

    opal_output_verbose(20, mca_btl_portals_component.portals_output,
                        "initialized Portals module");

    return btls;
}


int
mca_btl_portals_component_progress(void)
{
    int num_progressed = 0;
    int ret, which;
    static ptl_event_t ev;
    mca_btl_portals_frag_t *frag = NULL;
    mca_btl_portals_recv_block_t *block = NULL;
    mca_btl_base_tag_t tag;

    if (0 == mca_btl_portals_module.portals_num_procs) {
        return 0;
    }

    while (true) {
        ret = PtlEQPoll(mca_btl_portals_module.portals_eq_handles,
                        OMPI_BTL_PORTALS_EQ_SIZE,
                        0,       /* timeout */
                        &ev,     /* event structure to update */
                        &which); /* which queue the event came from - we don't care */
        switch (ret) {
        case PTL_OK:
            frag = ev.md.user_ptr;
            num_progressed++;

            switch (ev.type) {
            case PTL_EVENT_GET_START:
                /* generated on source (target) when a get from memory starts */
                OPAL_OUTPUT_VERBOSE((900, mca_btl_portals_component.portals_output,
                                     "PTL_EVENT_GET_START for 0x%x, %d",
                                     frag, (int) ev.hdr_data));

                break;

            case PTL_EVENT_GET_END:
                /* generated on source (target) when a get from memory ends */
                OPAL_OUTPUT_VERBOSE((900, mca_btl_portals_component.portals_output,
                                     "PTL_EVENT_GET_END for 0x%x, %d",
                                     frag, (int) ev.hdr_data));

                break;

            case PTL_EVENT_PUT_START:
                /* generated on destination (target) when a put into memory starts */
                OPAL_OUTPUT_VERBOSE((900, mca_btl_portals_component.portals_output,
                                     "PTL_EVENT_PUT_START for 0x%x, %d",
                                     frag, (int) ev.hdr_data));

#if OMPI_ENABLE_DEBUG
                if (ev.ni_fail_type != PTL_NI_OK) {
                    opal_output(mca_btl_portals_component.portals_output,
                                "Failure to start event\n");
                    return OMPI_ERROR;
                }
#endif
                /* if it's a pending unexpected receive, do book keeping. */
                if (ev.hdr_data < MCA_BTL_TAG_MAX) {
                    block = ev.md.user_ptr;
                    OPAL_THREAD_ADD32(&(block->pending), 1);
                }

                break;

            case PTL_EVENT_PUT_END: 
                /* generated on destination (target) when a put into memory ends */
                OPAL_OUTPUT_VERBOSE((900, mca_btl_portals_component.portals_output,
                                     "PTL_EVENT_PUT_END for 0x%x, %d",
                                     frag, (int) ev.hdr_data));

#if OMPI_ENABLE_DEBUG
                if (ev.ni_fail_type != PTL_NI_OK) {
                    opal_output(mca_btl_portals_component.portals_output,
                                "Failure to end event\n");
                    mca_btl_portals_return_block_part(&mca_btl_portals_module,
                                                      block);
                    return OMPI_ERROR;
                }
#endif
                /* if it's an unexpected receive, do book keeping and send to PML */
                if (ev.hdr_data < MCA_BTL_TAG_MAX) {
                    block = ev.md.user_ptr;
                    tag = ev.hdr_data;

                    /* if we ever make this thread hot, need to do
                       something with the receive fragments */
                    frag = &mca_btl_portals_module.portals_recv_frag;
                    frag->segments[0].seg_addr.pval = (((char*) ev.md.start) + ev.offset);
                    frag->segments[0].seg_len = ev.mlength;

                    OPAL_OUTPUT_VERBOSE((90, mca_btl_portals_component.portals_output,
                                         "received send fragment %x (thresh: %d, length %d)", 
                                         frag, ev.md.threshold, (int) ev.mlength));

                    if (ev.md.length - (ev.offset + ev.mlength) < ev.md.max_size ||
                        ev.md.threshold == 1) {
                        /* the block is full.  It's deactivated automagically, but we
                           can't start it up again until everyone is done with it.
                           The actual reactivation and all that will happen after the
                           free completes the last operation... */
                        OPAL_OUTPUT_VERBOSE((90, mca_btl_portals_component.portals_output,
                                             "marking block 0x%x as full", block->start));
                        block->full = true;
                    }

                    assert(NULL != mca_btl_portals_module.portals_reg[tag].cbfunc);

                    mca_btl_portals_module.portals_reg[tag].cbfunc(
                                             &mca_btl_portals_module.super,
                                             tag,
                                             &frag->base,
                                             mca_btl_portals_module.portals_reg[tag].cbdata);
                    mca_btl_portals_return_block_part(&mca_btl_portals_module, block);
                }
                break;

            case PTL_EVENT_REPLY_START:
                /* generated on destination (origin) when a get starts
                   returning data */

                OPAL_OUTPUT_VERBOSE((900, mca_btl_portals_component.portals_output,
                                     "PTL_EVENT_REPLY_START for 0x%x, %d",
                                     frag, (int) ev.hdr_data));

                break;

            case PTL_EVENT_REPLY_END:
                /* generated on destination (origin) when a get is
                   done returning data */

                OPAL_OUTPUT_VERBOSE((90, mca_btl_portals_component.portals_output,
                                     "PTL_EVENT_REPLY_END for 0x%x", frag));

                /* let the PML know we're done */
                frag->base.des_cbfunc(&mca_btl_portals_module.super,
                                      frag->endpoint,
                                      &frag->base,
                                      OMPI_SUCCESS);

                break;

            case PTL_EVENT_SEND_START:
                /* generated on source (origin) when put starts sending */

#if OMPI_ENABLE_DEBUG
                OPAL_OUTPUT_VERBOSE((900, mca_btl_portals_component.portals_output,
                                     "PTL_EVENT_SEND_START for 0x%x, %d",
                                     frag, (int) ev.hdr_data));

                if (ev.ni_fail_type != PTL_NI_OK) {
                    opal_output(mca_btl_portals_component.portals_output,
                                "Failure to start send event\n");
                    frag->base.des_cbfunc(&mca_btl_portals_module.super,
                                          frag->endpoint,
                                          &frag->base,
                                          OMPI_ERROR);
                }
#endif
                break;

            case PTL_EVENT_SEND_END:
                /* generated on source (origin) when put stops sending */
#if OMPI_ENABLE_DEBUG
                OPAL_OUTPUT_VERBOSE((90, mca_btl_portals_component.portals_output,
                                     "PTL_EVENT_SEND_END for 0x%x, %d",
                                     frag, (int) ev.hdr_data));

                if (ev.ni_fail_type != PTL_NI_OK) {
                    opal_output(mca_btl_portals_component.portals_output,
                                "Failure to end send event\n");
                    frag->base.des_cbfunc(&mca_btl_portals_module.super,
                                          frag->endpoint,
                                          &frag->base,
                                          OMPI_ERROR);
                }
#endif
                break;

            case PTL_EVENT_ACK:
                /* ack that a put as completed on other side */

                /* ACK for either send or RDMA put.  Either way, we
                   just call the callback function on goodness.
                   Requeue the put on badness */

                OPAL_OUTPUT_VERBOSE((90, mca_btl_portals_component.portals_output,
                                     "PTL_EVENT_ACK for 0x%x", frag));

#if OMPI_ENABLE_DEBUG
                if (ev.ni_fail_type != PTL_NI_OK) {
                    opal_output(mca_btl_portals_component.portals_output,
                                "Failure to ack event\n");
                    frag->base.des_cbfunc(&mca_btl_portals_module.super,
                                          frag->endpoint,
                                          &frag->base,
                                          OMPI_ERROR);
                } else
#endif

                if (0 == ev.mlength) {
                    /* other side received message but truncated to 0.
                       This should only happen for unexpected
                       messages, and only when the other side has no
                       buffer space available for receiving */
                    opal_output_verbose(50,
                                        mca_btl_portals_component.portals_output,
                                        "message was dropped.  Trying again");
                    
                    mca_btl_portals_send(&mca_btl_portals_module.super,
                                         frag->endpoint,
                                         &frag->base,
                                         frag->hdr.tag);
                } else {
                    /* other side received the message.  should have
                       received entire thing */
                    /* let the PML know we're done */
                    frag->base.des_cbfunc(&mca_btl_portals_module.super,
                                          frag->endpoint,
                                          &frag->base,
                                          OMPI_SUCCESS);
                }

                opal_output_verbose(50, mca_btl_portals_component.portals_output, "fuck");

                if (0 != frag->size) {
                    OPAL_THREAD_ADD32(&mca_btl_portals_module.portals_outstanding_ops,
                                      -1);
                    MCA_BTL_PORTALS_PROGRESS_QUEUED_SENDS();
                }

                break;

            default:
                break;
            }
            break;

        case PTL_EQ_EMPTY:
            /* there's nothing in the queue.  This is actually the
               common case, so the easiest way to make the compiler
               emit something that doesn't completely blow here is to
               just to go back to a good old goto */
            goto done;
            break;

        case PTL_EQ_DROPPED:
            /* not sure how we could deal with this more gracefully */
            opal_output(mca_btl_portals_component.portals_output,
                        "WARNING: EQ events dropped.  Too many messages pending.");
            opal_output(mca_btl_portals_component.portals_output,
                        "WARNING: Giving up in dispair");
            abort();
            break;
        
        default:
            opal_output(mca_btl_portals_component.portals_output,
                        "WARNING: Error in PtlEQPoll (%d).  This shouldn't happen",
                        ret);
            abort();
            break;
        }
    }

 done:
    return num_progressed;
}

