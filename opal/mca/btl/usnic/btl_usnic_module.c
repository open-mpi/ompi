/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2008 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006      Sandia National Laboratories. All rights
 *                         reserved.
 * Copyright (c) 2009-2015 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2014      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2014      Intel, Inc. All rights reserved
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"

#include <errno.h>
#include <string.h>
#include <unistd.h>
#include <stdlib.h>
#include <time.h>

#include "opal_stdint.h"
#include "opal/class/opal_bitmap.h"
#include "opal/prefetch.h"
#include "opal/util/output.h"
#include "opal/datatype/opal_convertor.h"
#include "opal/util/show_help.h"
#include "opal/mca/memchecker/base/base.h"

#if BTL_IN_OPAL
#include "opal/mca/btl/btl.h"
#include "opal/mca/btl/base/btl_base_error.h"
#include "opal/mca/mpool/base/base.h"
#include "opal/mca/mpool/mpool.h"
#else
#include "ompi/mca/btl/btl.h"
#include "ompi/mca/btl/base/btl_base_error.h"
#include "ompi/mca/mpool/base/base.h"
#include "ompi/mca/mpool/mpool.h"
#endif

#include "btl_usnic_compat.h"
#include "btl_usnic.h"
#include "btl_usnic_connectivity.h"
#include "btl_usnic_frag.h"
#include "btl_usnic_proc.h"
#include "btl_usnic_endpoint.h"
#include "btl_usnic_module.h"
#include "btl_usnic_util.h"
#include "btl_usnic_send.h"
#include "btl_usnic_ack.h"
#include "btl_usnic_hwloc.h"
#include "btl_usnic_stats.h"

static void finalize_one_channel(opal_btl_usnic_module_t *module,
                                 struct opal_btl_usnic_channel_t *channel);


/*
 * Loop over all procs sent to us in add_procs and see if we want to
 * add a proc/endpoint for them.
 */
static int add_procs_create_endpoints(opal_btl_usnic_module_t *module,
                                      size_t nprocs,
                                      opal_proc_t **procs,
                                      mca_btl_base_endpoint_t **endpoints)
{
    int rc;
    opal_proc_t* my_proc;
    size_t num_created = 0;

    /* get pointer to my proc structure */
    my_proc = opal_proc_local_get();
    if (NULL == my_proc) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    /* Loop over the procs we were given */
    for (size_t i = 0; i < nprocs; i++) {
        struct opal_proc_t* opal_proc = procs[i];
        opal_btl_usnic_proc_t* usnic_proc;
        mca_btl_base_endpoint_t* usnic_endpoint;

        endpoints[i] = NULL;

        /* Do not create loopback usnic connections */
        if (opal_proc == my_proc) {
            continue;
        }

        /* usNIC does not support loopback to the same machine */
        if (OPAL_PROC_ON_LOCAL_NODE(opal_proc->proc_flags)) {
            continue;
        }

        /* Find (or create if it doesn't exist) this peer's proc.
           This will receive the modex info for that proc.  Note that
           the proc is shared by all usnic modules that are trying
           to reach this destination. */
        usnic_proc = NULL;
        rc = opal_btl_usnic_proc_match(opal_proc, module, &usnic_proc);
        if (OPAL_ERR_UNREACH == rc) {
            /* If the peer doesn't have usnic modex info, then we just
               skip it */
            continue;
        } else if (OPAL_SUCCESS != rc) {
            return OPAL_ERR_OUT_OF_RESOURCE;
        }

        /* Create the endpoint for this proc/module combination.  If we cannot
         * reach this proc via this module, move on to the next proc. */
        usnic_endpoint = NULL;
        rc = opal_btl_usnic_create_endpoint(module, usnic_proc,
                                            &usnic_endpoint);
        if (OPAL_SUCCESS != rc) {
            opal_output_verbose(5, USNIC_OUT,
                                "btl:usnic:%s: unable to create endpoint for module=%p proc=%p\n",
                                __func__, (void *)module, (void *)usnic_proc);
            OBJ_RELEASE(usnic_proc);
            continue;
        }

        /* We like this new endpoint; save it */
        opal_pointer_array_add(&module->all_procs, usnic_proc);

        char str[IPV4STRADDRLEN];
        struct opal_btl_usnic_modex_t *modex =
            &usnic_endpoint->endpoint_remote_modex;
        opal_btl_usnic_snprintf_ipv4_addr(str, sizeof(str),
                                          modex->ipv4_addr,
                                          modex->netmask);

        opal_output_verbose(5, USNIC_OUT,
                            "btl:usnic: new usnic peer endpoint: %s, proirity port %d, data port %d",
                            str,
                            modex->ports[USNIC_PRIORITY_CHANNEL],
                            modex->ports[USNIC_DATA_CHANNEL]);

        endpoints[i] = usnic_endpoint;
        ++num_created;
    }

    opal_output_verbose(5, USNIC_OUT,
                        "btl:usnic: made %" PRIsize_t " endpoints",
                        num_created);
    return OPAL_SUCCESS;
}

/*
 * Print a warning about how the remote peer was unreachable.
 *
 * This is a separate helper function simply because it's somewhat
 * bulky to put inline.
 */
static void add_procs_warn_unreachable(opal_btl_usnic_module_t *module,
                                       opal_btl_usnic_endpoint_t *endpoint)
{
    /* Only show the warning if it is enabled */
    if (!mca_btl_usnic_component.show_route_failures) {
        return;
    }

    char remote[IPV4STRADDRLEN];
    opal_btl_usnic_snprintf_ipv4_addr(remote, sizeof(remote),
                                      endpoint->endpoint_remote_modex.ipv4_addr,
                                      endpoint->endpoint_remote_modex.netmask);

    opal_output_verbose(15, USNIC_OUT,
                        "btl:usnic: %s (which is %s) couldn't reach peer %s",
                        module->fabric_info->fabric_attr->name,
                        module->if_ipv4_addr_str,
                        remote);
    opal_show_help("help-mpi-btl-usnic.txt", "unreachable peer IP",
                   true,
                   opal_process_info.nodename,
                   module->if_ipv4_addr_str,
                   module->fabric_info->fabric_attr->name,
                   opal_get_proc_hostname(endpoint->endpoint_proc->proc_opal),
                   remote);
}

/* A bunch of calls to fi_av_insert() were previously
 * invoked.  Go reap them all.
 */
static int
add_procs_reap_fi_av_inserts(opal_btl_usnic_module_t *module,
                             size_t array_len,
                             struct mca_btl_base_endpoint_t **endpoints)
{
    int ret = OPAL_SUCCESS;
    int num_left;
    size_t i, channel;
    uint32_t event;
    struct fi_eq_entry entry;
    struct fi_eq_err_entry err_entry;

    bool error_occurred = false;

    /* compute num fi_av_insert completions we are waiting for */
    num_left = 0;
    for (i = 0; i < array_len; ++i) {
        if (NULL != endpoints[i]) {
            num_left += USNIC_NUM_CHANNELS;
        }
    }

    /* Loop polling for USD destination creation completion (they were
       individually started in btl_usnic_proc.c)

       In the loop below, the num_left value is decremented by a value
       we get back from the event queue.  There are error cases where
       we (theothetically) can get a corrupted event entry back, and
       not know how much to decrement num_left.  Hence, num_left can
       be inaccurate.  In such cases, this is probably indicative of a
       larger error.  Plus, we're not even all the way through module
       init yet, so only sane thing to do is abort. */
    while (num_left > 0) {
        opal_btl_usnic_addr_context_t *context;

        ret = fi_eq_sread(module->av_eq, &event, &entry, sizeof(entry), -1, 0);
        if (sizeof(entry) == ret) {
            context = entry.context;
            /* The usnic provider returns the number of inserts
               completed in entry.data */
            num_left -= entry.data;
            free(context);
            ret = 0;
        }

        else if (-FI_EAVAIL == ret) {
            ret = fi_eq_readerr(module->av_eq, &err_entry, 0);
            if (sizeof(err_entry) == ret) {
                /* An err_entry is returned for each errored
                   insertion */
                --num_left;

                /* Got some kind of address failure.  This usually means
                   that we couldn't find a route to that peer (e.g., the
                   networking is hosed between us).  So just mark that we
                   can't reach this peer, and print a pretty warning. */
                if (EADDRNOTAVAIL == err_entry.err ||
                     EHOSTUNREACH == err_entry.err) {
                    context = err_entry.context;
                    add_procs_warn_unreachable(module, context->endpoint);

                    /* NULL out this endpoint in the array so that the
                       caller knows it's unreachable */
                    /* RFXXX - index in context? */
                    for (i = 0; i < array_len; ++i) {
                        if (endpoints[i] == context->endpoint) {
                            OBJ_RELEASE(context->endpoint);

                            endpoints[i] = NULL;
                            break;
                        }
                    }
                    ret = 0;
                }

                /* Got some other kind of error -- give up on this
                   interface. */
                else {
                    opal_show_help("help-mpi-btl-usnic.txt",
                                   "libfabric API failed",
                                   true,
                                   opal_process_info.nodename,
                                   module->fabric_info->fabric_attr->name,
                                   "async insertion result", __FILE__, __LINE__,
                                   err_entry.err,
                                   "Failed to insert address to AV");
                    ret = OPAL_ERR_OUT_OF_RESOURCE;
                    error_occurred = true;
                    /* we can't break here, need to finish reaping all inserts */
                    continue;
                }
            } else {
                /* If we get here, it means fi_eq_readerr() failed
                   badly, which means something has gone tremendously
                   wrong.  Probably the only safe thing to do here is
                   exit. */
                opal_show_help("help-mpi-btl-usnic.txt",
                               "internal error during init",
                               true,
                               opal_process_info.nodename,
                               module->fabric_info->fabric_attr->name,
                               "fi_eq_readerr()", __FILE__, __LINE__,
                               ret,
                               "Returned != sizeof(err_entry)");
                ret = OPAL_ERR_OUT_OF_RESOURCE;
                error_occurred = true;

                /* Per above, there's really nothing sane left to do
                   but exit */
                opal_btl_usnic_exit(module);
            }
        } else {
            /* If we get here, it means fi_eq_readerr() failed badly,
               which means something has gone tremendously wrong.
               Probably the only safe thing to do here is exit. */
            opal_show_help("help-mpi-btl-usnic.txt",
                           "internal error during init",
                           true,
                           opal_process_info.nodename,
                           module->fabric_info->fabric_attr->name,
                           "fi_eq_sread()", __FILE__, __LINE__,
                           ret,
                           "Returned != (sizeof(entry) or -FI_EAVAIL)");
            ret = OPAL_ERR_OUT_OF_RESOURCE;
            error_occurred = true;

            /* Per above, there's really nothing sane left to do but
               exit */
            opal_btl_usnic_exit(module);
        }
    }

    /* Look through the list:
       - If something went wrong above, free all endpoints.
       - If an otherwise-valid endpoint has no dest, that means we timed
         out trying to resolve it, so just release that endpoint. */
    size_t num_endpoints_created = 0;
    for (i = 0; i < array_len; i++) {
        if (NULL != endpoints[i]) {
            bool happy;

            happy = true;
            if (error_occurred) {
                happy = false;
            } else {
                for (channel = 0; channel < USNIC_NUM_CHANNELS; ++channel) {
                    if (FI_ADDR_NOTAVAIL ==
                            endpoints[i]->endpoint_remote_addrs[channel]) {
                        happy = false;
                        break;
                    }
                }
            }

            if (happy) {
                ++num_endpoints_created;
            } else {
                OBJ_RELEASE(endpoints[i]);
                endpoints[i] = NULL;
            }
        }
    }

    /* All done */
    opal_output_verbose(5, USNIC_OUT,
                        "btl:usnic: created destinations for %" PRIsize_t
                        " endpoints",
                        num_endpoints_created);
    return ret;
}

/*
 * Add procs to this BTL module, receiving endpoint information from
 * the modex.  This is done in 2 phases:
 *
 * 1. Find (or create) the remote proc, and create the associated
 *    endpoint.
 * 2. Resolve the address handles for all remote endpoints.
 *
 * The second part is a separate loop from the first part to allow the
 * address lookups to be done in parallel.  This comes at a cost,
 * however: we may determine during the 2nd part that we should tear
 * down some or all the endpoints that we created in the 1st part.
 * For example, fi_av_insert() may fail in a fatal way (i.e., we
 * should fail the entire add_procs()), or it may fail for one or more
 * peers (i.e., we should just mark those peers as unreachable and not
 * add a proc or endpoint for them).
 */
static int usnic_add_procs(struct mca_btl_base_module_t* base_module,
                             size_t nprocs,
                             struct opal_proc_t **procs,
                             struct mca_btl_base_endpoint_t** endpoints,
                             opal_bitmap_t* reachable)
{
    opal_btl_usnic_module_t* module = (opal_btl_usnic_module_t*) base_module;
    int rc;

    /* First, create endpoints (and procs, if they're not already
       created) for all the usnic-reachable procs we were given. */
    rc = add_procs_create_endpoints(module, nprocs, procs, endpoints);
    if (OPAL_SUCCESS != rc) {
        goto fail;
    }

    /* For each endpoint that was created, we initiated the process to
       create NUM_CHANNELS fi_addrs.  Go finish all of those.  This
       will be the final determination of whether we can use the
       endpoint or not because we'll find out if each endpoint is
       reachable or not. */
    rc = add_procs_reap_fi_av_inserts(module, nprocs, endpoints);
    if (OPAL_SUCCESS != rc) {
        goto fail;
    }

    /* Find all the endpoints with a complete set of USD destinations
       and mark them as reachable */
    for (size_t i = 0; i < nprocs; ++i) {
        if (NULL != endpoints[i]) {
            bool happy = true;
            for (int channel = 0; channel < USNIC_NUM_CHANNELS; ++channel) {
                if (FI_ADDR_NOTAVAIL ==
                        endpoints[i]->endpoint_remote_addrs[channel]) {
                    happy = false;
                    break;
                }
            }

            if (happy) {
                opal_bitmap_set_bit(reachable, i);
            }
        }
    }

    /* This is fairly gross, but we need to output the connectivity
       map after add_procs() has been called on all existing usnic
       modules.  The only way I can think to do that is to count each
       time add_procs() is called, and when we're at a multiple of
       component.num_modules (i.e., add_procs() has been called on
       each module -- both during MPI_INIT and dynamic process cases),
       call the function to output the map. */
    static int num_times_add_procs_called = 0;
    ++num_times_add_procs_called;
    if (0 == (num_times_add_procs_called %
              mca_btl_usnic_component.num_modules)) {
        opal_btl_usnic_connectivity_map();
    }

    return OPAL_SUCCESS;

 fail:
    /* If we get here, it means something went terribly wrong.  Scorch
       the earth: destroy all endpoints and say that nothing was
       reachable. */
    for (size_t i = 0; i < nprocs; ++i) {
        if (NULL != endpoints[i]) {
            OBJ_RELEASE(endpoints[i]);
            endpoints[i] = NULL;
        }
    }

    return rc;
}

/*
 * Delete the proc as reachable from this module.  If there are
 * multiple usnic modules in a process, we'll come through here
 * multiple times to remove each proc.  The OBJ reference counts
 * will make all the details work out.
 */
static int usnic_del_procs(struct mca_btl_base_module_t *base_module,
                             size_t nprocs,
                             struct opal_proc_t **procs,
                             struct mca_btl_base_endpoint_t **peers)
{
    size_t i, j;
    opal_btl_usnic_module_t *module;
    opal_btl_usnic_endpoint_t *endpoint;
    int index;

    module = (struct opal_btl_usnic_module_t *)base_module;

    for (i = 0; i < nprocs; i++) {
        opal_btl_usnic_proc_t* proc =
            opal_btl_usnic_proc_lookup_ompi(procs[i]);
        if (NULL != proc) {

            /* find endpoint for this module */
            for (j = 0; j < proc->proc_endpoint_count; ++j) {
                endpoint = proc->proc_endpoints[j];
                if (NULL != endpoint && endpoint->endpoint_module == module) {

                    /* This call to usnic_del_procs is actually an
                     * implicit ACK of every packet we have ever sent
                     * ***because it is only ever invoked after an
                     * OOB/grpcomm barrier (in MPI_COMM_DISCONNECT and
                     * MPI_FINALIZE)***, so call handle_ack (via
                     * flush_endpoint) to do all the ACK processing
                     * and release all the data that needs
                     * releasing. */
                    if (!ENDPOINT_DRAINED(endpoint)) {
                        opal_btl_usnic_flush_endpoint(endpoint);
                    }

                    /* We're all done with this endpoint */
                    OBJ_RELEASE(endpoint);

                    break;  /* done once we found match */
                }
            }

            /* remove proc from this module, and decrement its refcount */
            for (index = 0; index < module->all_procs.size; ++index) {
                if (opal_pointer_array_get_item(&module->all_procs, index) ==
                        proc) {
                    OBJ_RELEASE(proc);
                    opal_pointer_array_set_item(&module->all_procs, index,
                            NULL);
                    break;
                }
            }
        }
    }

    return OPAL_SUCCESS;
}


/*
 * Let the PML register a callback function with me
 */
static int usnic_register_pml_err_cb(struct mca_btl_base_module_t* btl,
                                     mca_btl_base_module_error_cb_fn_t cbfunc)
{
    opal_btl_usnic_module_t *module = (opal_btl_usnic_module_t*) btl;

    module->pml_error_callback = cbfunc;

    return OPAL_SUCCESS;
}

/**
 * Allocate control messages or eager frags if BTL does not have
 * INPLACE flag.  To be clear: max it will ever alloc is eager_limit.
 * THEREFORE: eager_limit is the max that ALLOC must always be able to
 * alloc.
 *  --> Contraction in the btl.h documentation.
 */
static mca_btl_base_descriptor_t*
usnic_alloc(struct mca_btl_base_module_t* btl,
              struct mca_btl_base_endpoint_t* endpoint,
              uint8_t order,
              size_t size,
              uint32_t flags)
{
    opal_btl_usnic_send_frag_t *frag;
    opal_btl_usnic_module_t *module = (opal_btl_usnic_module_t*) btl;
    mca_btl_base_descriptor_t *desc;


    /* small is easy, just allocate a small segment */
    if (OPAL_LIKELY(size <= module->max_frag_payload)) {
        opal_btl_usnic_small_send_frag_t *sfrag;

        sfrag = opal_btl_usnic_small_send_frag_alloc(module);
        if (NULL == sfrag) {
            return NULL;
        }
        frag = &sfrag->ssf_base;

    /* between MTU and eager limit, we need to allocate a buffer
     * which can hold the data.  We will allocate a
     * large fragment, and attach the buffer to it.
     */
    } else {
        opal_btl_usnic_large_send_frag_t *lfrag;

        /* truncate to eager_limit */
        if (OPAL_UNLIKELY(size > module->super.btl_eager_limit)) {
            size = module->super.btl_eager_limit;
        }

        lfrag = opal_btl_usnic_large_send_frag_alloc(module);
        if (OPAL_UNLIKELY(NULL == lfrag)) {
            return NULL;
        }
        frag = &lfrag->lsf_base;

        assert(size > 0);
        lfrag->lsf_buffer = malloc(size);
        if (OPAL_UNLIKELY(NULL == lfrag->lsf_buffer)) {
            opal_btl_usnic_frag_return(module, &lfrag->lsf_base.sf_base);
            return NULL;
        }

        /* pointer to buffer for caller */
        frag->sf_base.uf_base.USNIC_SEND_LOCAL[0].seg_addr.pval =
            lfrag->lsf_buffer;

        MSGDEBUG1_OUT("usnic_alloc: packing frag %p on the fly", (void *)frag);
        lfrag->lsf_pack_on_the_fly = true;
    }

#if MSGDEBUG2
        opal_output(0, "usnic_alloc: %s frag=%p, size=%d, flags=0x%x\n",
                (size <= module->max_frag_payload)?"small":"large",
                (void *)frag, (int)size, flags);
#endif

    /* set endpoint */
    frag->sf_endpoint = endpoint;

    /* set up descriptor */
    desc = &frag->sf_base.uf_base;
    desc->des_flags = flags;
    desc->USNIC_SEND_LOCAL[0].seg_len = size;
    desc->USNIC_SEND_LOCAL_COUNT = 1;

    return desc;
}


/**
 * Return an allocated fragment
 *
 * Return the send fragment to the appropriate list
 */
static int usnic_free(struct mca_btl_base_module_t* btl,
                        mca_btl_base_descriptor_t* des)
{
    opal_btl_usnic_frag_t* frag = (opal_btl_usnic_frag_t*)des;

#if MSGDEBUG2
    opal_output(0, "usnic_free: %p (%s)\n", (void*)frag,
            usnic_frag_type(frag->uf_type));
#endif
    /* calling free routine gives us ownership - we need to make sure
     * the flag is set for lower layers.
     */
    frag->uf_base.des_flags |= MCA_BTL_DES_FLAGS_BTL_OWNERSHIP;

    opal_btl_usnic_frag_return_cond((struct opal_btl_usnic_module_t *)btl,
            frag);

    return OPAL_SUCCESS;
}

/* Packs data from the given large send frag into single new segment and
 * returns a pointer to it.  The packed data comes first from SG[0] (PML
 * header) and then second from either SG[1] (if seg_addr is non-NULL) or from
 * the convertor contained in the frag.
 *
 * The frag's bookkeeping data will be updated appropriately. */
static
opal_btl_usnic_chunk_segment_t *
pack_chunk_seg_from_frag(
    struct opal_btl_usnic_module_t* module,
    opal_btl_usnic_large_send_frag_t *lfrag)
{
    opal_btl_usnic_chunk_segment_t *seg;
    uint8_t *copyptr;
    size_t copylen;
    size_t seg_space;
    size_t max_data;

    assert(NULL != lfrag);
    /* never should be attempting to pack if we've already packed everything */
    assert(lfrag->lsf_pack_bytes_left > 0);

    seg = opal_btl_usnic_chunk_segment_alloc(module);
    if (OPAL_UNLIKELY(NULL == seg)) {
        /* TODO look at ways to deal with this case more gracefully, possibly as
         * part of capping the overall BTL memory consumption.  Watch out for
         * possible MPI-layer deadlock. */
        BTL_ERROR(("chunk segment allocation error"));
        abort(); /* XXX */
    }

    seg_space = module->max_chunk_payload;
    copyptr = seg->ss_base.us_payload.raw;

    /* Keep copying in as long as we have space, there is data to be copied, and
     * we aren't using a convertor (SG[1] will be NULL if we have a convertor).
     */
    while (seg_space > 0 &&
           lfrag->lsf_pack_bytes_left > 0 &&
           NULL != lfrag->lsf_cur_ptr) {
        if (seg_space > lfrag->lsf_bytes_left_in_sge) {
            copylen = lfrag->lsf_bytes_left_in_sge;
        } else {
            copylen = seg_space;
        }

        memcpy(copyptr, lfrag->lsf_cur_ptr, copylen);
        seg_space -= copylen;
        copyptr += copylen;
        lfrag->lsf_bytes_left_in_sge -= copylen;
        lfrag->lsf_pack_bytes_left -= copylen;
        if (lfrag->lsf_bytes_left_in_sge > 0) {
            lfrag->lsf_cur_ptr += copylen;
        } else {
            ++lfrag->lsf_cur_sge;
            lfrag->lsf_cur_ptr =
                lfrag->lsf_des_src[lfrag->lsf_cur_sge].seg_addr.pval;
            lfrag->lsf_bytes_left_in_sge =
                lfrag->lsf_des_src[lfrag->lsf_cur_sge].seg_len;
        }
    }

    if (seg_space > 0 && lfrag->lsf_pack_bytes_left > 0) {
        /* the remaining bytes come from a convertor; pack using it */
        assert(NULL == lfrag->lsf_cur_ptr);
        assert(1 == lfrag->lsf_cur_sge);

        copylen = lfrag->lsf_pack_bytes_left;
        if (copylen > seg_space) {
            copylen = seg_space;
        }
        usnic_convertor_pack_simple(&lfrag->lsf_base.sf_convertor, copyptr,
                                    copylen, &max_data);
        seg_space -= max_data;
        lfrag->lsf_bytes_left_in_sge -= max_data;
        lfrag->lsf_pack_bytes_left -= max_data;
    }

    MSGDEBUG1_OUT("%s: packed seg=%p, frag=%p, payload=%zd\n",
                  __func__, (void *)seg, (void *)lfrag,
                  (module->max_chunk_payload - seg_space));

    assert(lfrag->lsf_cur_sge <= 2);
    assert(seg_space < module->max_chunk_payload); /* must make progress */

    seg->ss_parent_frag = &lfrag->lsf_base;
    seg->ss_len = module->max_chunk_payload - seg_space;

    return seg;
}

static int usnic_finalize(struct mca_btl_base_module_t* btl)
{
    opal_btl_usnic_module_t* module = (opal_btl_usnic_module_t*)btl;

    if (module->device_async_event_active) {
        opal_event_del(&(module->device_async_event));
        module->device_async_event_active = false;
    }

    opal_btl_usnic_connectivity_unlisten(module);

    finalize_one_channel(module,
                         &module->mod_channels[USNIC_DATA_CHANNEL]);
    finalize_one_channel(module,
                         &module->mod_channels[USNIC_PRIORITY_CHANNEL]);

    /* Shutdown the stats on this module */
    opal_btl_usnic_stats_finalize(module);

    /* Note that usnic_del_procs will have been called for *all* procs
       by this point, so the module->all_endpoints list will be empty.
       Destruct it. */
    opal_mutex_lock(&module->all_endpoints_lock);
    OBJ_DESTRUCT(&(module->all_endpoints));
    module->all_endpoints_constructed = false;
    opal_mutex_unlock(&module->all_endpoints_lock);

    /* _flush_endpoint should have emptied this list */
    assert(opal_list_is_empty(&(module->pending_resend_segs)));
    OBJ_DESTRUCT(&module->pending_resend_segs);

    /* Similarly, empty the endpoints_that_need_acks list so that
       endpoints don't still have an endpoint_ack_li item still in
       use */
    while (!opal_list_is_empty(&(module->endpoints_that_need_acks))) {
        (void) opal_list_remove_first(&(module->endpoints_that_need_acks));
    }
    OBJ_DESTRUCT(&module->endpoints_that_need_acks);

    /* Note that usnic_del_procs will have been called for *all* procs
       by this point, so the module->all_procs list will be empty.
       Destruct it. */
    OBJ_DESTRUCT(&module->all_procs);

    for (int i = module->first_pool; i <= module->last_pool; ++i) {
        OBJ_DESTRUCT(&module->module_recv_buffers[i]);
    }
    free(module->module_recv_buffers);

    OBJ_DESTRUCT(&module->ack_segs);
    OBJ_DESTRUCT(&module->endpoints_with_sends);
    OBJ_DESTRUCT(&module->small_send_frags);
    OBJ_DESTRUCT(&module->large_send_frags);
    OBJ_DESTRUCT(&module->put_dest_frags);
    OBJ_DESTRUCT(&module->chunk_segs);
    OBJ_DESTRUCT(&module->senders);

    mca_mpool_base_module_destroy(module->super.btl_mpool);

    if (NULL != module->av) {
        fi_close(&module->av->fid);
    }
    if (NULL != module->av_eq) {
        fi_close(&module->av_eq->fid);
    }
    if (NULL != module->dom_eq) {
        fi_close(&module->dom_eq->fid);
    }
    fi_close(&module->domain->fid);
    fi_close(&module->fabric->fid);

    return OPAL_SUCCESS;
}

static inline unsigned
get_send_credits(struct opal_btl_usnic_channel_t *chan)
{
    return chan->credits;
}

static void
usnic_do_resends(
    opal_btl_usnic_module_t *module)
{
    opal_btl_usnic_send_segment_t *sseg;
    opal_btl_usnic_endpoint_t *endpoint;
    struct opal_btl_usnic_channel_t *data_channel;
    int ret;

    data_channel = &module->mod_channels[USNIC_DATA_CHANNEL];

    while ((get_send_credits(data_channel) > 1) &&
           !opal_list_is_empty(&module->pending_resend_segs)) {

        /*
         * If a segment is on the re-send list, it will not
         * be in the retransmit hotel.  Post the segment, then check it in.
         */
        sseg = (opal_btl_usnic_send_segment_t *)
            opal_list_remove_first(&module->pending_resend_segs);
        endpoint = sseg->ss_parent_frag->sf_endpoint;

        /* clobber any stale piggy-backed ACK */
        sseg->ss_base.us_btl_header->ack_present = 0;

        /* Only post this segment if not already posted */
        if (sseg->ss_send_posted == 0) {

            /* resends are always standard segments */
            sseg->ss_channel = USNIC_DATA_CHANNEL;

            /* re-send the segment */
            opal_btl_usnic_post_segment(module, endpoint, sseg);

            /* consume a send credit for this endpoint.  May send us
             * negative, oh well...  This is because the completion routine
             * always increments send credits, and we must balance.
             * Alternative is to mark this as a retrans segment and check in
             * completion, but this ugly way avoids extra checks in the
             * critical path.  And, really, respects the concept of send
             * credits more.
             */
            --endpoint->endpoint_send_credits;
            ++module->stats.num_resends;
        }

        /* restart the retrans timer */
        ret = opal_hotel_checkin(&endpoint->endpoint_hotel,
                sseg, &sseg->ss_hotel_room);
        if (OPAL_UNLIKELY(OPAL_SUCCESS != ret)) {
            BTL_ERROR(("hotel checkin failed\n"));
            abort();    /* should not be possible */
        }
    }
}

/* Given a large send frag (which is at the head of the given endpoint's send
 * queue), generate a new segment, fill it with data, and
 * endpoint_send_segment() it.  Takes care of subsequent frag
 * cleanup/bookkeeping (dequeue, descriptor callback, etc.) if this frag was
 * completed by this segment.
 */
static void
usnic_handle_large_send(
    opal_btl_usnic_module_t *module,
    opal_btl_usnic_endpoint_t *endpoint,
    opal_btl_usnic_send_frag_t *frag)
{
    opal_btl_usnic_large_send_frag_t *lfrag;
    opal_btl_usnic_btl_chunk_header_t *chp;
    opal_btl_usnic_send_segment_t *sseg;
    size_t payload_len;

    assert(frag->sf_base.uf_type == OPAL_BTL_USNIC_FRAG_LARGE_SEND);
    lfrag = (opal_btl_usnic_large_send_frag_t *)frag;
    if (lfrag->lsf_cur_offset == 0) {
        /* assign a fragment ID */
        do {
            lfrag->lsf_frag_id = endpoint->endpoint_next_frag_id++;
        } while (lfrag->lsf_frag_id == 0);
    }

    if (lfrag->lsf_pack_on_the_fly) {
        assert(opal_list_is_empty(&lfrag->lsf_seg_chain));

        /* just pack a single chunk segment and put it on the list */
        sseg = pack_chunk_seg_from_frag(module, lfrag);
    } else {
        /* data was pre-packed in prepare_src */
        sseg = (opal_btl_usnic_send_segment_t *)
            opal_list_remove_first(&lfrag->lsf_seg_chain);
    }

    assert(NULL != sseg);
    payload_len = sseg->ss_len;

    assert(payload_len > 0); /* must have made progress */
    assert(payload_len <= module->max_chunk_payload);
    assert(lfrag->lsf_bytes_left >= payload_len);

    /* set actual packet length */
    sseg->ss_len = sizeof(opal_btl_usnic_btl_chunk_header_t) + payload_len;
    lfrag->lsf_bytes_left -= payload_len;

    /* fill in the chunk's BTL header with frag info */
    chp = sseg->ss_base.us_btl_chunk_header;
    chp->ch_frag_id = lfrag->lsf_frag_id;
    chp->ch_frag_size = lfrag->lsf_base.sf_size;
    chp->ch_frag_offset = lfrag->lsf_cur_offset;
    chp->ch_hdr.tag = lfrag->lsf_tag;

    /* payload length into the header*/
    sseg->ss_base.us_btl_header->payload_len = payload_len;

    /* do the send */
    opal_btl_usnic_endpoint_send_segment(module, sseg);

    /* do fragment bookkeeping */
    lfrag->lsf_cur_offset += payload_len;

#if MSGDEBUG1
    opal_output(0, "%s: payload_len=%zd, bytes_left=%zd on_the_fly=%s\n",
                __func__, payload_len, lfrag->lsf_bytes_left,
                lfrag->lsf_pack_on_the_fly?"true":"false");
#endif
    /* done with fragment? */
    if (lfrag->lsf_bytes_left == 0) {

        /* remove this frag from sending list now because upper layer may
         * decide to put it on some other list in the callback
         */
        opal_list_remove_item(&endpoint->endpoint_frag_send_queue,
                &frag->sf_base.uf_base.super.super);

        /* only callback now if this was not a PUT and we own the fragment,
         * otherwise we need to wait until last byte is ACKed
         */
        if (frag->sf_base.uf_remote_seg[0].seg_addr.pval == NULL &&
                (frag->sf_base.uf_base.des_flags &
                 MCA_BTL_DES_FLAGS_BTL_OWNERSHIP)) {

            OPAL_BTL_USNIC_DO_SEND_FRAG_CB(module, frag, "large");
        }
    }
}

/*
 * Progress the send engine.
 * Should only ever be called from usnic_component_progress() to
 * avoid re-entrancy issues.
 */
void
opal_btl_usnic_module_progress_sends(
    opal_btl_usnic_module_t *module)
{
    opal_btl_usnic_send_frag_t *frag;
    opal_btl_usnic_send_segment_t *sseg;
    opal_btl_usnic_endpoint_t *endpoint;
    struct opal_btl_usnic_channel_t *data_channel;
    struct opal_btl_usnic_channel_t *prio_channel;

    /*
     * Post all the sends that we can
     * resends 1st priority
     * ACKs 2nd priority
     * new sends 3rd
     */
    data_channel = &module->mod_channels[USNIC_DATA_CHANNEL];
    prio_channel = &module->mod_channels[USNIC_PRIORITY_CHANNEL];

    /*
     * Handle all the retransmits we can
     */
    if (OPAL_UNLIKELY(!opal_list_is_empty(&module->pending_resend_segs))) {
        usnic_do_resends(module);
    }

    /*
     * Keep sending as long as there are WQEs and something to do
     */
    while ((get_send_credits(data_channel) > 1) &&
           !opal_list_is_empty(&module->endpoints_with_sends)) {
        opal_btl_usnic_small_send_frag_t *sfrag;
        size_t payload_len;

        /*
         * Grab the first endpoint with a pending send.  Presence on this
         * list means there is a fragment with data ready to go and
         * the endpoint's send window is open, and the endpoint has send
         * credits.
         */

        endpoint = (opal_btl_usnic_endpoint_t *)
            opal_list_get_first(&module->endpoints_with_sends);
        frag = (opal_btl_usnic_send_frag_t *)
            opal_list_get_first(&endpoint->endpoint_frag_send_queue);

        /*
         * small send?  (fragment fits in one segment)
         * Send ptr and length will be in uf_local_seg[0]
         */
        if (frag->sf_base.uf_type == OPAL_BTL_USNIC_FRAG_SMALL_SEND) {

            /* remove this frag from sending list now because upper layer may
             * decide to put it on some other list in the callback
             */
            opal_list_remove_item(&endpoint->endpoint_frag_send_queue,
                    &frag->sf_base.uf_base.super.super);

            sfrag = (opal_btl_usnic_small_send_frag_t *)frag;
            sseg = &sfrag->ssf_segment;

            /* get payload len from segment */
            payload_len = sfrag->ssf_base.sf_size;
            sseg->ss_base.us_btl_header->payload_len = payload_len;

#if MSGDEBUG1
            opal_output(0, "progress send small, frag=%p, ptr=%p, payload=%zd, len=%"PRIu32", ep=%p, tag=%d\n",
                    (void *)frag,
                    (void *)sseg->ss_ptr, payload_len,
                    sseg->ss_len,
                    (void *)frag->sf_endpoint,
                    sseg->ss_base.us_btl_header->tag);
#endif

            /* post the send */
            opal_btl_usnic_endpoint_send_segment(module, sseg);

            /* don't do callback yet if this is a put */
            if (frag->sf_base.uf_remote_seg[0].seg_addr.pval == NULL) {
                /* we have copied the data, perform a callback if
                 * we own the fragment and callback is requested.
                 * If we don't own the fragment, we cannot callback yet
                 * because we are not done with the segment inside.
                 * (ACK not received yet)
                 */
                if ((frag->sf_base.uf_base.des_flags &
                        (MCA_BTL_DES_SEND_ALWAYS_CALLBACK |
                         MCA_BTL_DES_FLAGS_BTL_OWNERSHIP)) ==
                        (MCA_BTL_DES_SEND_ALWAYS_CALLBACK |
                         MCA_BTL_DES_FLAGS_BTL_OWNERSHIP)) {
                    OPAL_BTL_USNIC_DO_SEND_FRAG_CB(module, frag, "small");
                }
            }

        /* Large sends... */
        } else {
            usnic_handle_large_send(module, endpoint, frag);
        }

        /* If no more sends or endpoint send window is closed,
         * or no more send credits, remove from send list
         */
        if (opal_list_is_empty(&endpoint->endpoint_frag_send_queue) ||
            endpoint->endpoint_send_credits <= 0 ||
            !WINDOW_OPEN(endpoint)) {

            opal_list_remove_item(&module->endpoints_with_sends,
                    &endpoint->super);
            endpoint->endpoint_ready_to_send = false;
        }
    }

    /*
     * Handle any ACKs that need to be sent
     */
    endpoint = opal_btl_usnic_get_first_endpoint_needing_ack(module);
    while (get_send_credits(prio_channel) > 1 && endpoint != NULL) {
        opal_btl_usnic_endpoint_t *next_endpoint;

        /* get next in list */
        next_endpoint = opal_btl_usnic_get_next_endpoint_needing_ack(endpoint);

        /* Is it time to send ACK? */
        if (endpoint->endpoint_acktime == 0 ||
            endpoint->endpoint_acktime <= get_nsec()) {
            opal_btl_usnic_ack_send(module, endpoint);
            opal_btl_usnic_remove_from_endpoints_needing_ack(endpoint);
        }

        endpoint = next_endpoint;
    }
}

/*
 *  Initiate a send.
 *
 *  Send completion callbacks can be done from a few different places.
 *
 *  If this is a send from a fragment we do not own, we always have
 *  to wait for the last ACK of the fragment, because we cannot allow
 *  the fragment to be re-used until we know we have no more retransmits to do.
 *
 *  If this is a send from a fragment we own, and we know we have copied the
 *  data from the user's buffer, we can perform the callback immediately
 *  (or possibly not at all, simply returning "1" to indicate completion).
 *
 *  If this is a send from a fragment we own and we have not yet copied out
 *  all the data (as is the case in a large send) then we defer the callback
 *  until the last of the data has been copied out by routines called
 *  from opal_btl_usnic_progress_sends()
 */
static int
usnic_send(
    struct mca_btl_base_module_t* base_module,
    struct mca_btl_base_endpoint_t* base_endpoint,
    struct mca_btl_base_descriptor_t* descriptor,
    mca_btl_base_tag_t tag)
{
    int rc;
    opal_btl_usnic_send_frag_t *frag;
    opal_btl_usnic_small_send_frag_t *sfrag;
    opal_btl_usnic_endpoint_t *endpoint;
    opal_btl_usnic_module_t *module;
    opal_btl_usnic_send_segment_t *sseg;

    endpoint = (opal_btl_usnic_endpoint_t *)base_endpoint;
    module = (opal_btl_usnic_module_t *)base_module;
    frag = (opal_btl_usnic_send_frag_t*) descriptor;

    assert(frag->sf_endpoint == endpoint);
    frag->sf_base.uf_remote_seg[0].seg_addr.pval = NULL;      /* not a PUT */

    opal_btl_usnic_compute_sf_size(frag);
    frag->sf_ack_bytes_left = frag->sf_size;

#if MSGDEBUG2
    opal_output(0, "usnic_send: frag=%p, endpoint=%p, tag=%d, sf_size=%d\n",
            (void *)frag, (void *)endpoint,
            tag, (int)frag->sf_size);
#if MSGDEBUG1
    { unsigned i;
        opal_output(0, "  descriptor->des_flags=0x%x\n", descriptor->des_flags);
        for (i=0; i<descriptor->USNIC_SEND_LOCAL_COUNT; ++i) {
            opal_output(0, "  %d: ptr:%p len:%d\n", i,
                    descriptor->USNIC_SEND_LOCAL[i].seg_addr.pval,
                    descriptor->USNIC_SEND_LOCAL[i].seg_len);
        }
    }
#endif
#endif

    /*
     * If this fragment is small enough to inline,
     * and we have enough send WQEs,
     * then inline and fastpath it
     */
    if (frag->sf_base.uf_type == OPAL_BTL_USNIC_FRAG_SMALL_SEND &&
            frag->sf_ack_bytes_left < module->max_tiny_payload &&
            WINDOW_OPEN(endpoint) &&
            (get_send_credits(&module->mod_channels[USNIC_PRIORITY_CHANNEL]) >=
             module->mod_channels[USNIC_PRIORITY_CHANNEL].fastsend_wqe_thresh)) {
        size_t payload_len;

        sfrag = (opal_btl_usnic_small_send_frag_t *)frag;
        sseg = &sfrag->ssf_segment;

        payload_len = frag->sf_ack_bytes_left;
        sseg->ss_base.us_btl_header->payload_len = payload_len;


        /* copy the 2nd SGE into the segment */
        if (frag->sf_base.uf_base.USNIC_SEND_LOCAL_COUNT > 1) {
            memcpy(((char *)(intptr_t)frag->sf_base.uf_local_seg[0].seg_addr.lval +
                     frag->sf_base.uf_local_seg[0].seg_len),
                    frag->sf_base.uf_local_seg[1].seg_addr.pval,
                    frag->sf_base.uf_local_seg[1].seg_len);

            /* update 1st segment length */
            frag->sf_base.uf_base.USNIC_SEND_LOCAL_COUNT = 1;
            frag->sf_base.uf_local_seg[0].seg_len +=
                frag->sf_base.uf_local_seg[1].seg_len;
        }

        /* assign length */
        sseg->ss_len = sizeof(opal_btl_usnic_btl_header_t) + frag->sf_size;

        sseg->ss_channel = USNIC_PRIORITY_CHANNEL;
        sseg->ss_base.us_btl_header->tag = tag;
#if MSGDEBUG1
        opal_output(0, "INLINE send, sseg=%p", (void *)sseg);
#endif

        /* post the segment now */
        opal_btl_usnic_endpoint_send_segment(module, sseg);

        /* If we own the frag and callback was requested, callback now,
         * else just return 1 to show completion.
         * If we don't own the frag, need to wait for ACK before
         * performing callback on the frag
         */
        if (descriptor->des_flags & MCA_BTL_DES_FLAGS_BTL_OWNERSHIP) {
            if (descriptor->des_flags & MCA_BTL_DES_SEND_ALWAYS_CALLBACK) {
                OPAL_BTL_USNIC_DO_SEND_FRAG_CB(module, frag, "immediate small");
                rc = 0;
            } else {
#if MSGDEBUG1
                opal_output(0, "skipping callback for frag %p, returning 1\n", (void *)frag);
#endif
                rc = 1;
                ++module->stats.pml_send_callbacks;   /* returning "1" is an implicit CB */
            }
        } else {
#if MSGDEBUG1
            opal_output(0, "don't own descriptor, defer callback for frag %p\n", (void *)frag);
#endif
            descriptor->des_flags |= MCA_BTL_DES_SEND_ALWAYS_CALLBACK;
            rc = 0;
        }
    } else {
        /*
         * We move this off to another function because having it inside
         * this function seems to add a little latency, likely due to inlines
         * making the function too big.  In fact, the routine had to go to
         * another file entirely, else the compiler tried to be helpful
         * and inline all by itself.
         */
        rc = opal_btl_usnic_finish_put_or_send(module, endpoint, frag, tag);
        /* FIXME can we clarify flag set/clear ordering? */
        frag->sf_base.uf_base.des_flags |= MCA_BTL_DES_SEND_ALWAYS_CALLBACK;
    }

    ++module->stats.pml_module_sends;

    return rc;
}

#if 0
/*
 * Initiate an immediate send
 */
static int usnic_sendi(struct mca_btl_base_module_t* btl,
                         struct mca_btl_base_endpoint_t* endpoint,
                         struct opal_convertor_t* convertor,
                         void* header,
                         size_t header_size,
                         size_t payload_size,
                         uint8_t order,
                         uint32_t flags,
                         mca_btl_base_tag_t tag,
                         mca_btl_base_descriptor_t** descriptor)
{
    /* JMS write me */
    return OPAL_ERROR;
}
#endif


/*
 * RDMA Memory Pool (de)register callbacks
 */
static int usnic_reg_mr(void* reg_data, void* base, size_t size,
                        mca_mpool_base_registration_t* reg)
{
    opal_btl_usnic_module_t* mod = (opal_btl_usnic_module_t*)reg_data;
    opal_btl_usnic_reg_t* ur = (opal_btl_usnic_reg_t*)reg;
    int rc;

    rc = fi_mr_reg(mod->domain, base, size, 0, 0, 0, 0, &ur->ur_mr, NULL);
    if (0 != rc) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    return OPAL_SUCCESS;
}

static int usnic_dereg_mr(void* reg_data,
                          mca_mpool_base_registration_t* reg)
{
    opal_btl_usnic_reg_t* ur = (opal_btl_usnic_reg_t*)reg;

    if (ur->ur_mr != NULL) {
        if (0 != fi_close(&ur->ur_mr->fid)) {
            opal_output(0, "%s: error unpinning USD memory mr=%p: %s\n",
                        __func__, (void*) ur->ur_mr, strerror(errno));
            return OPAL_ERROR;
        }
    }

    ur->ur_mr = NULL;
    return OPAL_SUCCESS;
}


/*
 * Called back by libevent if an async event occurs on the device
 */
static void module_async_event_callback(int fd, short flags, void *arg)
{
    char *str = NULL;
    bool fatal = false;
    opal_btl_usnic_module_t *module = (opal_btl_usnic_module_t*) arg;
    uint32_t event;
    struct fi_eq_entry entry;

    /* Get the async event */
    int ret = fi_eq_read(module->dom_eq, &event, &entry, sizeof(entry), 0);
    if (-FI_EAGAIN == ret) {
        /* Nothing to see here... */
        return;
    }

    else if (ret != 0) {
        opal_show_help("help-mpi-btl-usnic.txt", "libfabric API failed",
                       true,
                       opal_process_info.nodename,
                       module->fabric_info->fabric_attr->name,
                       "fi_eq_read()", __FILE__, __LINE__,
                       ret,
                       "Failed to get domain event");
        fatal = true;
    }

    else if (event == 42 /* RFXXX FI_LINKSTATE */) {
        opal_memchecker_base_mem_defined(&event, sizeof(event));
        opal_memchecker_base_mem_defined(&entry, sizeof(entry));
        switch (entry.data) {
            case 0: // USD_EVENT_LINK_UP:
            /* This event should never happen, because OMPI will
               ignore ports that are down, and we should only get this
               event if a port *was* down and is now *up*.  But if we
               ever do get it, it should be a harmless event -- just
               ignore it. */
            opal_output_verbose(10, USNIC_OUT,
                                "btl:usnic: got LINK_UP on %s",
                                module->fabric_info->fabric_attr->name);
            break;

            case 1: // USD_EVENT_LINK_DOWN:
            str = "link down";
            /* Fall through */

        default:
            if (NULL == str) {
                str = "unknown event";
            }

            /* For the moment, these are the only other cases libfabric
               will report to us.  However, they're only listed here
               for completeness.  We currently abort if any async
               event other than LINK_UP occurs. */
            opal_show_help("help-mpi-btl-usnic.txt", "async event",
                           true,
                           opal_process_info.nodename,
                           module->fabric_info->fabric_attr->name,
                           str, entry.data);
            fatal = true;
        }
    }

    /* If this is fatal, invoke the upper layer error handler to abort
       the job */
    if (fatal) {
        opal_btl_usnic_exit(module);
        /* Does not return */
    }
}

/*
 * Create a single libfabric endpoint
 */
static int create_ep(opal_btl_usnic_module_t* module,
                            struct opal_btl_usnic_channel_t *channel)
{
    int rc;
    struct sockaddr_in *sin;
    struct fi_info *hint;

    hint = fi_dupinfo(module->fabric_info);
    if (NULL == hint) {
        opal_show_help("help-mpi-btl-usnic.txt",
                       "internal error during init",
                       true,
                       opal_process_info.nodename,
                       module->fabric_info->fabric_attr->name,
                       "fi_dupinfo() failed", __FILE__, __LINE__,
                       -1, "Unknown");
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    hint->rx_attr->size = channel->chan_rd_num;
    hint->tx_attr->size = channel->chan_sd_num;

    /* specific ports requested? */
    sin = hint->src_addr;
    if (0 == mca_btl_usnic_component.udp_port_base) {
        sin->sin_port = 0;
    } else {
        sin->sin_port = htons(mca_btl_usnic_component.udp_port_base +
            opal_process_info.my_local_rank);
    }

    rc = fi_getinfo(FI_VERSION(1, 0), NULL, 0, 0, hint, &channel->info);
    fi_freeinfo(hint);
    if (0 != rc) {
        opal_show_help("help-mpi-btl-usnic.txt",
                       "internal error during init",
                       true,
                       opal_process_info.nodename,
                       module->fabric_info->fabric_attr->name,
                       "fi_getinfo() failed", __FILE__, __LINE__,
                       rc, fi_strerror(-rc));
        return OPAL_ERR_OUT_OF_RESOURCE;
    }
    if (channel->chan_index != USNIC_PRIORITY_CHANNEL) {
        channel->info->caps &= ~(1ULL << 63);
    }

    rc = fi_endpoint(module->domain, channel->info, &channel->ep, NULL);
    if (0 != rc || NULL == channel->ep) {
        opal_show_help("help-mpi-btl-usnic.txt",
                       "internal error during init",
                       true,
                       opal_process_info.nodename,
                       module->fabric_info->fabric_attr->name,
                       "fi_endpoint() failed", __FILE__, __LINE__,
                       rc, fi_strerror(-rc));
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    /* attach CQ to EP */
    rc = fi_ep_bind(channel->ep, &channel->cq->fid, FI_SEND);
    if (0 != rc) {
        opal_show_help("help-mpi-btl-usnic.txt",
                       "internal error during init",
                       true,
                       opal_process_info.nodename,
                       module->fabric_info->fabric_attr->name,
                       "fi_ep_bind() SCQ to EP failed", __FILE__, __LINE__,
                       rc, fi_strerror(-rc));
        return OPAL_ERR_OUT_OF_RESOURCE;
    }
    rc = fi_ep_bind(channel->ep, &channel->cq->fid, FI_RECV);
    if (0 != rc) {
        opal_show_help("help-mpi-btl-usnic.txt",
                       "internal error during init",
                       true,
                       opal_process_info.nodename,
                       module->fabric_info->fabric_attr->name,
                       "fi_ep_bind() RCQ to EP failed", __FILE__, __LINE__,
                       rc, fi_strerror(-rc));
        return OPAL_ERR_OUT_OF_RESOURCE;
    }
    rc = fi_ep_bind(channel->ep, &module->av->fid, FI_RECV);
    if (0 != rc) {
        opal_show_help("help-mpi-btl-usnic.txt",
                       "internal error during init",
                       true,
                       opal_process_info.nodename,
                       module->fabric_info->fabric_attr->name,
                       "fi_ep_bind() AV to EP failed", __FILE__, __LINE__,
                       rc, fi_strerror(-rc));
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    /* Enable the endpoint */
    rc = fi_enable(channel->ep);
    if (0 != rc) {
        opal_show_help("help-mpi-btl-usnic.txt",
                       "internal error during init",
                       true,
                       opal_process_info.nodename,
                       module->fabric_info->fabric_attr->name,
                       "fi_enable() failed", __FILE__, __LINE__,
                       rc, fi_strerror(-rc));
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    /* actual sizes */
    channel->chan_rd_num = channel->info->rx_attr->size;
    channel->chan_sd_num = channel->info->tx_attr->size;

    return OPAL_SUCCESS;
}


/*
 * finalize channel - release all associated resources
 */
static void finalize_one_channel(opal_btl_usnic_module_t *module,
                                 struct opal_btl_usnic_channel_t *channel)
{
    if (NULL != channel->ep) {
        fi_close(&channel->ep->fid);
        channel->ep = NULL;
    }

    /* destroy CQ if created */
    if (NULL != channel->cq) {
        fi_close(&channel->cq->fid);
        channel->cq = NULL;
    }

    if (NULL != channel->info) {
        fi_freeinfo(channel->info);
        channel->info = NULL;
    }

    /* gets set right after constructor called, lets us know recv_segs
     * have been constructed.  Make sure to wait until queues
     * destroyed to destroy the recv_segs */
    if (channel->recv_segs.ctx == module) {
        assert(NULL == channel->ep && NULL == channel->cq);
        OBJ_DESTRUCT(&channel->recv_segs);
    }
}

/*
 * Initialize a channel
 */
static int init_one_channel(opal_btl_usnic_module_t *module,
                            int index,
                            int max_msg_size,
                            int rd_num,
                            int sd_num)
{
    int i;
    int rc;
    uint32_t segsize;
    opal_btl_usnic_recv_segment_t *rseg;
    opal_free_list_item_t* item;
    struct opal_btl_usnic_channel_t *channel;
    struct fi_cq_attr cq_attr;

    channel = &module->mod_channels[index];
    channel->chan_max_msg_size = max_msg_size;
    channel->chan_rd_num = rd_num;
    channel->chan_sd_num = sd_num;
    channel->chan_index = index;
    channel->chan_deferred_recv = NULL;
    channel->chan_error = false;

    channel->fastsend_wqe_thresh = sd_num - 10;

    channel->credits = sd_num;

    /* We did math up in component_init() to know that there should be
       enough CQs available.  So if create_cq() fails, then either the
       memlock limits are too low, or something other than this MPI
       job is consuming CQs. */
    memset(&cq_attr, 0, sizeof(cq_attr));
    cq_attr.format = FI_CQ_FORMAT_CONTEXT;
    cq_attr.wait_obj = FI_WAIT_NONE;
    cq_attr.size = module->cq_num;
    rc = fi_cq_open(module->domain, &cq_attr, &channel->cq, NULL);
    if (0 != rc) {
        opal_show_help("help-mpi-btl-usnic.txt",
                       "internal error during init",
                       true,
                       opal_process_info.nodename,
                       module->fabric_info->fabric_attr->name,
                       "failed to create CQ", __FILE__, __LINE__);
        goto error;
    }

    /* Set up the endpoint for this channel */
    rc = create_ep(module, channel);
    if (OPAL_SUCCESS != rc) {
        goto error;
    }

    /*
     * Initialize pool of receive segments.  Round MTU up to cache
     * line size so that each segment is guaranteed to start on a
     * cache line boundary.
     */
    segsize = (max_msg_size + channel->info->ep_attr->msg_prefix_size +
            opal_cache_line_size - 1) & ~(opal_cache_line_size - 1);
    OBJ_CONSTRUCT(&channel->recv_segs, opal_free_list_t);
    rc =
        usnic_compat_free_list_init(&channel->recv_segs,
                                    sizeof(opal_btl_usnic_recv_segment_t) /* frag size */,
                                    opal_cache_line_size /* frag alignment */,
                                    OBJ_CLASS(opal_btl_usnic_recv_segment_t),
                                    segsize /* payload buffer size */,
                                    opal_cache_line_size /* payload alignmt */,
                                    rd_num /* num erorments to alloc */,
                                    rd_num /* max elements to alloc */,
                                    rd_num /* num elements per alloc */,
                                    module->super.btl_mpool /* mpool for reg */,
                                    0 /* mpool reg flags */,
                                    NULL /* unused0 */,
                                    NULL /* item_init */,
                                    NULL /* item_init_context */);
    channel->recv_segs.ctx = module; /* must come after
                                        free_list_init,
                                        otherwise ctx gets
                                        clobbered */
    if (OPAL_SUCCESS != rc) {
        goto error;
    }

    /* Post receive descriptors */
    for (i = 0; i < rd_num; i++) {
        USNIC_COMPAT_FREE_LIST_GET(&channel->recv_segs, item);
        assert(NULL != item);
        rseg = (opal_btl_usnic_recv_segment_t*)item;

        if (NULL == rseg) {
            opal_show_help("help-mpi-btl-usnic.txt",
                           "internal error during init",
                           true,
                           opal_process_info.nodename,
                           module->fabric_info->fabric_attr->name,
                           "Failed to get receive buffer from freelist",
                           __FILE__, __LINE__);
            goto error;
        }

        /* cannot find length from constructor, set it now */
        rseg->rs_len = segsize;

        rc = fi_recv(channel->ep, rseg->rs_protocol_header, segsize,
                     NULL, FI_ADDR_UNSPEC, rseg);
        if (0 != rc) {
            opal_show_help("help-mpi-btl-usnic.txt",
                           "internal error during init",
                           true,
                           opal_process_info.nodename,
                           module->fabric_info->fabric_attr->name,
                           "Failed to post receive buffer",
                           __FILE__, __LINE__);
            goto error;
        }
    }

    return OPAL_SUCCESS;

error:
    finalize_one_channel(module, channel);
    return OPAL_ERROR;
}

/*
 * generate initial send sequence number
 */
static opal_btl_usnic_seq_t
get_initial_seq_no(void)
{
    opal_btl_usnic_seq_t isn;

    isn = (opal_btl_usnic_seq_t)opal_rand(&opal_btl_usnic_rand_buff);

    return isn;
}

/*************************************************************************
 The following routines are all short utility / convenience functions
 for module_init().
*************************************************************************/

/*
 * Setup some globals on the module
 */
static void init_module_globals(opal_btl_usnic_module_t *module)
{
    OBJ_CONSTRUCT(&module->all_endpoints_lock, opal_mutex_t);
}


/*
 * Initialize our local modex entry from the device attributes
 */
static void init_local_modex_part1(opal_btl_usnic_module_t *module)
{
    /* Fill (most of) the local address information on the module.  We
       don't have the following yet: qp numbers, header length,
       connectivity checker UDP port. */
    opal_btl_usnic_modex_t *modex = &module->local_modex;
    struct fi_info *info = module->fabric_info;
    struct fi_usnic_info *uip = &module->usnic_info;
    struct sockaddr_in *sin;

    sin = info->src_addr;
    modex->ipv4_addr =       sin->sin_addr.s_addr;
    modex->netmask =         uip->ui.v1.ui_netmask_be;
    modex->max_msg_size =    info->ep_attr->max_msg_size;
    modex->link_speed_mbps = uip->ui.v1.ui_link_speed;

    opal_btl_usnic_snprintf_ipv4_addr(module->if_ipv4_addr_str,
                                      sizeof(module->if_ipv4_addr_str),
                                      modex->ipv4_addr,
                                      modex->netmask);

    opal_output_verbose(5, USNIC_OUT,
                        "btl:usnic: %s IP charactertics: %s, %u Mbps",
                        module->fabric_info->fabric_attr->name,
                        module->if_ipv4_addr_str,
                        modex->link_speed_mbps);
}

/*
 * Find the header length for our transport.
 *
 * Do this super-early in the startup process because we need it to
 * calculate some payload lengths (and indirectly, some queue
 * lengths).
 */
static void init_find_transport_header_len(opal_btl_usnic_module_t *module)
{
    mca_btl_usnic_component.transport_header_len =
        module->fabric_info->ep_attr->msg_prefix_size;
    mca_btl_usnic_component.transport_protocol =
        module->fabric_info->ep_attr->protocol;
}

/*
 * How many xQ entries do we want?
 */
static void init_queue_lengths(opal_btl_usnic_module_t *module)
{
    if (-1 == mca_btl_usnic_component.sd_num) {
        module->sd_num = module->fabric_info->tx_attr->size;
    } else {
        module->sd_num = mca_btl_usnic_component.sd_num;
    }
    if (-1 == mca_btl_usnic_component.rd_num) {
        module->rd_num = module->fabric_info->rx_attr->size;
    } else {
        module->rd_num = mca_btl_usnic_component.rd_num;
    }
    if (-1 == mca_btl_usnic_component.cq_num) {
        module->cq_num = module->rd_num + module->sd_num;
    } else {
        module->cq_num = mca_btl_usnic_component.cq_num;
    }

    /*
     * Queue sizes for priority channel scale with # of endpoint. A
     * little bit of chicken and egg here, we really want procs*ports,
     * but we can't know # of ports until we try to initialize, so
     * 32*num_procs is best guess.  User can always override.
     */

    if (-1 == mca_btl_usnic_component.prio_sd_num) {
        module->prio_sd_num = max(128, 32 * USNIC_MCW_SIZE) - 1;
    } else {
        module->prio_sd_num = mca_btl_usnic_component.prio_sd_num;
    }
    if (module->prio_sd_num > 0 &&
        (unsigned) module->prio_sd_num >
         module->fabric_info->tx_attr->size) {
        module->prio_sd_num = module->fabric_info->tx_attr->size;
    }
    if (-1 == mca_btl_usnic_component.prio_rd_num) {
        module->prio_rd_num =
            max(128, 32 * USNIC_MCW_SIZE) - 1;
    } else {
        module->prio_rd_num = mca_btl_usnic_component.prio_rd_num;
    }
    if (module->prio_rd_num > 0 &&
        (unsigned) module->prio_rd_num >
         module->fabric_info->rx_attr->size) {
        module->prio_rd_num = module->fabric_info->rx_attr->size;
    }
}

static void init_payload_lengths(opal_btl_usnic_module_t *module)
{
    /* Find the max payload this port can handle */
    module->max_frag_payload =
        module->local_modex.max_msg_size - /* start with the MTU */
        sizeof(opal_btl_usnic_btl_header_t); /* subtract size of
                                                the BTL header */

    /* same, but use chunk header */
    module->max_chunk_payload =
        module->local_modex.max_msg_size -
        sizeof(opal_btl_usnic_btl_chunk_header_t);

    /* Priorirty queue MTU and max size */
    if (0 == module->max_tiny_msg_size) {
        module->max_tiny_msg_size = 768;
    }
    module->max_tiny_payload = module->max_tiny_msg_size -
        sizeof(opal_btl_usnic_btl_header_t);
}

static void init_pml_values(opal_btl_usnic_module_t *module)
{
    module->super.btl_bandwidth = module->local_modex.link_speed_mbps;

    /* If the eager rndv limit is 0, initialize it to default */
    if (0 == module->super.btl_rndv_eager_limit) {
        module->super.btl_rndv_eager_limit = USNIC_DFLT_RNDV_EAGER_LIMIT;
    }

    /* If the eager send limit is 0, initialize it to default */
    if (0 == module->super.btl_eager_limit) {
        /* 150k for 1 module, 25k for >1 module */
        if (1 == mca_btl_usnic_component.num_modules) {
            module->super.btl_eager_limit =
                USNIC_DFLT_EAGER_LIMIT_1DEVICE;
        } else {
            module->super.btl_eager_limit =
                USNIC_DFLT_EAGER_LIMIT_NDEVICES;
        }
    }

    /* Since we emulate PUT, max_send_size can be same as
       eager_limit */
    module->super.btl_max_send_size =
        module->super.btl_eager_limit;

#if BTL_VERSION == 30
    module->super.btl_put_limit =
        module->super.btl_eager_limit;
#endif
}

static void init_senders(opal_btl_usnic_module_t *module)
{
    /* Make a hash table of senders */
    OBJ_CONSTRUCT(&module->senders, opal_hash_table_t);
    /* JMS This is a fixed size -- BAD!  But since hash table
       doesn't grow dynamically, I don't know what size to put
       here.  I think the long-term solution is to write a better
       hash table... :-( */
    opal_hash_table_init(&module->senders, 4096);
}

static void init_connectivity_checker(opal_btl_usnic_module_t *module)
{
    /* Setup a connectivity agent listener */
    int rc = opal_btl_usnic_connectivity_listen(module);
    if (OPAL_SUCCESS != rc) {
        OPAL_ERROR_LOG(rc);
        ABORT("Failed to notify connectivity agent to listen");
    }
}

static void init_hwloc(opal_btl_usnic_module_t *module)
{
#if OPAL_HAVE_HWLOC
    /* If this process is bound to a single NUMA locality, calculate
       its NUMA distance from this usNIC device */
    if (mca_btl_usnic_component.want_numa_device_assignment) {
        opal_btl_usnic_hwloc_distance(module);
    } else {
        opal_output_verbose(5, USNIC_OUT,
                            "btl:usnic: not sorting devices by NUMA distance (MCA btl_usnic_want_numa_device_assignment)");
    }
#else
    opal_output_verbose(5, USNIC_OUT,
                        "btl:usnic: not sorting devices by NUMA distance (topology support not included)");
#endif
}

static void init_procs(opal_btl_usnic_module_t *module)
{
    /* Setup the pointer array for the procs that will be used by this
       module */
    OBJ_CONSTRUCT(&module->all_procs, opal_pointer_array_t);
    opal_pointer_array_init(&module->all_procs, USNIC_MCW_SIZE, INT_MAX, 32);
}

/*
 * Setup the mpool
 */
static int init_mpool(opal_btl_usnic_module_t *module)
{
    struct mca_mpool_base_resources_t mpool_resources;

    mpool_resources.reg_data = (void*)module;
    mpool_resources.sizeof_reg = sizeof(opal_btl_usnic_reg_t);
    mpool_resources.register_mem = usnic_reg_mr;
    mpool_resources.deregister_mem = usnic_dereg_mr;
    asprintf(&mpool_resources.pool_name, "%s",
             module->fabric_info->fabric_attr->name);
    module->super.btl_mpool =
        mca_mpool_base_module_create(mca_btl_usnic_component.usnic_mpool_name,
                                     &module->super, &mpool_resources);
    if (NULL == module->super.btl_mpool) {
        opal_show_help("help-mpi-btl-usnic.txt",
                       "internal error during init",
                       true,
                       opal_process_info.nodename,
                       module->fabric_info->fabric_attr->name,
                       "create mpool", __FILE__, __LINE__);
        return OPAL_ERROR;
    }

    return OPAL_SUCCESS;
}

static int init_channels(opal_btl_usnic_module_t *module)
{
    int rc;
    struct fi_av_attr av_attr;
    struct fi_eq_attr eq_attr;

    memset(&module->mod_channels[0], 0,
           sizeof(module->mod_channels[0]));
    memset(&module->mod_channels[1], 0,
           sizeof(module->mod_channels[1]));

    memset(&av_attr, 0, sizeof(av_attr));
    av_attr.type = FI_AV_MAP;
    av_attr.flags = FI_EVENT;
    rc = fi_av_open(module->domain, &av_attr, &module->av, NULL);
    if (rc != OPAL_SUCCESS) {
        goto destroy;
    }

    rc = fi_open_ops(&module->av->fid, FI_USNIC_AV_OPS_1, 0,
            (void **)&module->usnic_av_ops, NULL);
    if (rc != OPAL_SUCCESS) {
        goto destroy;
    }

    memset(&eq_attr, 0, sizeof(eq_attr));
    eq_attr.size = 1024;
    eq_attr.wait_obj = FI_WAIT_UNSPEC;
    rc = fi_eq_open(module->fabric, &eq_attr, &module->av_eq, NULL);
    if (rc != OPAL_SUCCESS) {
        goto destroy;
    }
    eq_attr.wait_obj = FI_WAIT_FD;
    rc = fi_eq_open(module->fabric, &eq_attr, &module->dom_eq, NULL);
    if (rc != OPAL_SUCCESS) {
        goto destroy;
    }

    rc = fi_av_bind(module->av, &module->av_eq->fid, 0);
    if (rc != OPAL_SUCCESS) {
        goto destroy;
    }

    rc = fi_domain_bind(module->domain, &module->dom_eq->fid, 0);
    if (rc != OPAL_SUCCESS) {
        goto destroy;
    }

    /* initialize data and priority channels */
    rc = init_one_channel(module,
            USNIC_PRIORITY_CHANNEL,
            module->max_tiny_msg_size,
            module->prio_rd_num, module->prio_sd_num);
    if (rc != OPAL_SUCCESS) {
        goto destroy;
    }
    rc = init_one_channel(module,
            USNIC_DATA_CHANNEL,
            module->fabric_info->ep_attr->max_msg_size,
            module->rd_num, module->sd_num);
    if (rc != OPAL_SUCCESS) {
        goto destroy;
    }

    return OPAL_SUCCESS;

 destroy:
    finalize_one_channel(module,
                         &module->mod_channels[USNIC_DATA_CHANNEL]);
    finalize_one_channel(module,
                         &module->mod_channels[USNIC_PRIORITY_CHANNEL]);

    return rc;
}

/* Fill in the UDP ports of the channel QPs, and fill in the wire
   header length */
static void init_local_modex_part2(opal_btl_usnic_module_t *module)
{
    module->local_modex.isn = get_initial_seq_no();

    /* Place EP number in our local modex information */
    for (int id = 0; id < USNIC_NUM_CHANNELS; ++id) {
        opal_btl_usnic_channel_t *channel = &module->mod_channels[id];
        struct sockaddr_in *sin;
        sin = channel->info->src_addr;
        module->local_modex.ports[id] = ntohs(sin->sin_port);
        module->local_modex.protocol = channel->info->ep_attr->protocol;
    }
}

static void init_async_event(opal_btl_usnic_module_t *module)
{
    int fd;
    int ret;

    ret = fi_control(&module->dom_eq->fid, FI_GETWAIT, &fd);
    if (ret != 0) {
        opal_show_help("help-mpi-btl-usnic.txt",
                    "libfabric API failed",
                   true,
                   opal_process_info.nodename,
                   module->fabric_info->fabric_attr->name,
                   "fi_control(eq, FI_GETWAIT)", __FILE__, __LINE__,
                   ret,
                   fi_strerror(-ret));
        return;
    }

    /* Get the fd to receive events on this device */
    opal_event_set(opal_event_base, &(module->device_async_event), fd,
                   OPAL_EV_READ | OPAL_EV_PERSIST,
                   module_async_event_callback, module);
    opal_event_add(&(module->device_async_event), NULL);
    module->device_async_event_active = true;
}

static void init_random_objects(opal_btl_usnic_module_t *module)
{
    /* list of all endpoints */
    opal_mutex_lock(&module->all_endpoints_lock);
    OBJ_CONSTRUCT(&(module->all_endpoints), opal_list_t);
    module->all_endpoints_constructed = true;
    opal_mutex_unlock(&module->all_endpoints_lock);

    /* Pending send segs list */
    OBJ_CONSTRUCT(&module->pending_resend_segs, opal_list_t);
    OBJ_CONSTRUCT(&module->endpoints_that_need_acks, opal_list_t);

    /* list of endpoints that are ready to send */
    OBJ_CONSTRUCT(&module->endpoints_with_sends, opal_list_t);
}

static void init_freelists(opal_btl_usnic_module_t *module)
{
    int rc;
    uint32_t segsize;

    segsize = (module->local_modex.max_msg_size +
           module->fabric_info->ep_attr->msg_prefix_size +
           opal_cache_line_size - 1) &
        ~(opal_cache_line_size - 1);

    /* Send frags freelists */
    OBJ_CONSTRUCT(&module->small_send_frags, opal_free_list_t);
    rc = usnic_compat_free_list_init(&module->small_send_frags,
                             sizeof(opal_btl_usnic_small_send_frag_t) +
                             mca_btl_usnic_component.transport_header_len,
                             opal_cache_line_size,
                             OBJ_CLASS(opal_btl_usnic_small_send_frag_t),
                             segsize,
                             opal_cache_line_size,
                             module->sd_num * 4,
                             -1,
                             module->sd_num / 2,
                             module->super.btl_mpool,
                             0 /* mpool reg flags */,
                             NULL /* unused0 */,
                             NULL /* item_init */,
                             NULL /* item_init_context */);
    assert(OPAL_SUCCESS == rc);

    OBJ_CONSTRUCT(&module->large_send_frags, opal_free_list_t);
    rc = usnic_compat_free_list_init(&module->large_send_frags,
                             sizeof(opal_btl_usnic_large_send_frag_t) +
                             mca_btl_usnic_component.transport_header_len,
                             opal_cache_line_size,
                             OBJ_CLASS(opal_btl_usnic_large_send_frag_t),
                             0,  /* payload size */
                             0,  /* payload align */
                             module->sd_num / 8,
                             -1,
                             module->sd_num / 8,
                             NULL,
                             0 /* mpool reg flags */,
                             NULL /* unused0 */,
                             NULL /* item_init */,
                             NULL /* item_init_context */);
    assert(OPAL_SUCCESS == rc);

    OBJ_CONSTRUCT(&module->put_dest_frags, opal_free_list_t);
    rc = usnic_compat_free_list_init(&module->put_dest_frags,
                             sizeof(opal_btl_usnic_put_dest_frag_t) +
                             mca_btl_usnic_component.transport_header_len,
                             opal_cache_line_size,
                             OBJ_CLASS(opal_btl_usnic_put_dest_frag_t),
                             0,  /* payload size */
                             0,  /* payload align */
                             module->sd_num / 8,
                             -1,
                             module->sd_num / 8,
                             NULL,
                             0 /* mpool reg flags */,
                             NULL /* unused0 */,
                             NULL /* item_init */,
                             NULL /* item_init_context */);
    assert(OPAL_SUCCESS == rc);

    /* list of segments to use for sending */
    OBJ_CONSTRUCT(&module->chunk_segs, opal_free_list_t);
    rc = usnic_compat_free_list_init(&module->chunk_segs,
                             sizeof(opal_btl_usnic_chunk_segment_t) +
                             mca_btl_usnic_component.transport_header_len,
                             opal_cache_line_size,
                             OBJ_CLASS(opal_btl_usnic_chunk_segment_t),
                             segsize,
                             opal_cache_line_size,
                             module->sd_num * 4,
                             -1,
                             module->sd_num / 2,
                             module->super.btl_mpool,
                             0 /* mpool reg flags */,
                             NULL /* unused0 */,
                             NULL /* item_init */,
                             NULL /* item_init_context */);
    assert(OPAL_SUCCESS == rc);

    /* ACK segments freelist */
    uint32_t ack_segment_len;
    ack_segment_len = (sizeof(opal_btl_usnic_btl_header_t) +
           module->fabric_info->ep_attr->msg_prefix_size +
            opal_cache_line_size - 1) & ~(opal_cache_line_size - 1);
    OBJ_CONSTRUCT(&module->ack_segs, opal_free_list_t);
    rc = usnic_compat_free_list_init(&module->ack_segs,
                             sizeof(opal_btl_usnic_ack_segment_t) +
                             mca_btl_usnic_component.transport_header_len,
                             opal_cache_line_size,
                             OBJ_CLASS(opal_btl_usnic_ack_segment_t),
                             ack_segment_len,
                             opal_cache_line_size,
                             module->sd_num * 4,
                             -1,
                             module->sd_num / 2,
                             module->super.btl_mpool,
                             0 /* mpool reg flags */,
                             NULL /* unused0 */,
                             NULL /* item_init */,
                             NULL /* item_init_context */);
    assert(OPAL_SUCCESS == rc);

    /*
     * Initialize pools of large recv buffers
     *
     * NOTE: (last_pool < first_pool) is _not_ erroneous; recv buffer
     * pools simply won't be used in that case.
     */
    module->first_pool = 16; /* 64 kiB */
    module->last_pool = usnic_fls(module->super.btl_eager_limit-1);
    module->module_recv_buffers = calloc(module->last_pool+1,
            sizeof(opal_free_list_t));
    assert(module->module_recv_buffers != NULL);
    for (int i = module->first_pool; i <= module->last_pool; ++i) {
        size_t elt_size = sizeof(opal_btl_usnic_rx_buf_t) - 1 + (1 << i);
        OBJ_CONSTRUCT(&module->module_recv_buffers[i], opal_free_list_t);
        rc = usnic_compat_free_list_init(&module->module_recv_buffers[i],
                                 elt_size,
                                 opal_cache_line_size,
                                 OBJ_CLASS(opal_btl_usnic_rx_buf_t),
                                 0,   /* payload size */
                                 0,   /* payload align */
                                 128,   /* init elts to alloc */
                                 128, /* max elts to alloc */
                                 128,   /* num elts per alloc */
                                 NULL /* mpool */,
                                 0 /* mpool reg flags */,
                                 NULL /* unused0 */,
                                 NULL /* item_init */,
                                 NULL /* item_init_context */);
        assert(OPAL_SUCCESS == rc);
    }
}

/*
 * Initialize the btl module by allocating
 *  a memory pool, priority and data channels, and free lists
 */
int opal_btl_usnic_module_init(opal_btl_usnic_module_t *module)
{
    init_module_globals(module);
    init_local_modex_part1(module);
    init_find_transport_header_len(module);
    init_queue_lengths(module);
    init_payload_lengths(module);
    init_pml_values(module);
    init_senders(module);
    init_connectivity_checker(module);
    init_hwloc(module);
    init_procs(module);

    int ret;
    if (OPAL_SUCCESS != (ret = init_mpool(module)) ||
        OPAL_SUCCESS != (ret = init_channels(module))) {
        mca_mpool_base_module_destroy(module->super.btl_mpool);
        return ret;
    }

    init_local_modex_part2(module);
    init_async_event(module);
    init_random_objects(module);
    init_freelists(module);
    opal_btl_usnic_stats_init(module);

    /* Setup a connectivity listener.  This fills in the last part of
       the local modex info (the connectivity listener UDP port) */
    if (mca_btl_usnic_component.connectivity_enabled) {
        int rc = opal_btl_usnic_connectivity_listen(module);
        if (OPAL_SUCCESS != rc) {
            OPAL_ERROR_LOG(rc);
            ABORT("Failed to notify connectivity agent to listen");
        }
    } else {
        /* If we're not doing a connectivity check, just set the port
           to 0 */
        module->local_modex.connectivity_udp_port = 0;
    }

    return OPAL_SUCCESS;
}


static int usnic_ft_event(int state)
{
    return OPAL_SUCCESS;
}


opal_btl_usnic_module_t opal_btl_usnic_module_template = {
    .super = {
        .btl_component = &mca_btl_usnic_component.super,

#if BTL_VERSION == 20
        .btl_prepare_dst = opal_btl_usnic_prepare_dst,
        .btl_seg_size = sizeof(mca_btl_base_segment_t),
#elif BTL_VERSION == 30
        .btl_atomic_flags = 0,
        .btl_registration_handle_size = 0,

        .btl_get_limit = 0,
        .btl_get_alignment = 0,
        .btl_put_limit = 0,
        .btl_put_alignment = 0,

        .btl_atomic_op = NULL,
        .btl_atomic_fop = NULL,
        .btl_atomic_cswap = NULL,
#endif

        .btl_exclusivity = MCA_BTL_EXCLUSIVITY_DEFAULT,
        .btl_flags =
            MCA_BTL_FLAGS_SEND |
            MCA_BTL_FLAGS_PUT |
            MCA_BTL_FLAGS_SEND_INPLACE,

        .btl_add_procs = usnic_add_procs,
        .btl_del_procs = usnic_del_procs,
        .btl_register = NULL,
        .btl_finalize = usnic_finalize,

        .btl_alloc = usnic_alloc,
        .btl_free = usnic_free,
        .btl_prepare_src = opal_btl_usnic_prepare_src,
        .btl_send = usnic_send,
        .btl_sendi = NULL,
        .btl_put = opal_btl_usnic_put,
        .btl_get = NULL,
        .btl_dump = mca_btl_base_dump,

        .btl_mpool = NULL,
        .btl_register_error = usnic_register_pml_err_cb,
        .btl_ft_event = usnic_ft_event
    }
};
