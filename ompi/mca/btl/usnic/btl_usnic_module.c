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
 * Copyright (c) 2009-2014 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include <errno.h>
#include <string.h>
#include <unistd.h>
#include <stdlib.h>
#include <time.h>

#include "opal/class/opal_bitmap.h"
#include "opal/prefetch.h"
#include "opal/util/output.h"
#include "opal/datatype/opal_convertor.h"
#include "opal/include/opal_stdint.h"
#include "opal/util/show_help.h"

#include "ompi/mca/btl/btl.h"
#include "ompi/mca/btl/base/btl_base_error.h"
#include "ompi/mca/mpool/base/base.h"
#include "ompi/mca/mpool/mpool.h"
#include "ompi/memchecker.h"

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

static void
ompi_btl_usnic_channel_finalize(
    ompi_btl_usnic_module_t *module,
    struct ompi_btl_usnic_channel_t *channel);


/* Compute and set the proper value for sfrag->sf_size.  This must not be used
 * during usnic_alloc, since the PML might change the segment size after
 * usnic_alloc returns. */
static inline void compute_sf_size(ompi_btl_usnic_send_frag_t *sfrag)
{
    ompi_btl_usnic_frag_t *frag;

    frag = &sfrag->sf_base;

    assert(frag->uf_base.des_src_cnt > 0);
    assert(frag->uf_base.des_src_cnt <= 2);

    /* belt and suspenders: second len should be zero if only one SGE */
    assert(2 == frag->uf_base.des_src_cnt || 0 == frag->uf_src_seg[1].seg_len);

    sfrag->sf_size = 0;
    sfrag->sf_size += frag->uf_src_seg[0].seg_len;
    sfrag->sf_size += frag->uf_src_seg[1].seg_len;
}

/*
 * Loop over all procs sent to us in add_procs and see if we want to
 * add a proc/endpoint for them.
 */
static int add_procs_create_endpoints(ompi_btl_usnic_module_t *module,
                                      size_t nprocs,
                                      ompi_proc_t **ompi_procs,
                                      mca_btl_base_endpoint_t **endpoints)
{
    int rc;
    ompi_proc_t* my_proc;
    size_t num_created = 0;

    /* get pointer to my proc structure */
    my_proc = ompi_proc_local();
    if (NULL == my_proc) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    /* Loop over the procs we were given */
    for (size_t i = 0; i < nprocs; i++) {
        struct ompi_proc_t* ompi_proc = ompi_procs[i];
        ompi_btl_usnic_proc_t* usnic_proc;
        mca_btl_base_endpoint_t* usnic_endpoint;

        endpoints[i] = NULL;

        /* Do not create loopback usnic connections */
        if (ompi_proc == my_proc) {
            continue;
        }

        /* usNIC does not support loopback to the same machine */
        if (OPAL_PROC_ON_LOCAL_NODE(ompi_proc->proc_flags)) {
            continue;
        }

        /* Find (or create if it doesn't exist) this peer's proc.
           This will receive the modex info for that proc.  Note that
           the proc is shared by all usnic modules that are trying
           to reach this destination. */
        usnic_proc = NULL;
        rc = ompi_btl_usnic_proc_match(ompi_proc, module, &usnic_proc);
        if (OMPI_ERR_UNREACH == rc) {
            /* If the peer doesn't have usnic modex info, then we just
               skip it */
            continue;
        } else if (OMPI_SUCCESS != rc) {
            return OMPI_ERR_OUT_OF_RESOURCE;
        }

        /* Create the endpoint for this proc/module combination.  If we cannot
         * reach this proc via this module, move on to the next proc. */
        usnic_endpoint = NULL;
        rc = ompi_btl_usnic_create_endpoint(module, usnic_proc,
                                            &usnic_endpoint);
        if (OMPI_SUCCESS != rc) {
            opal_output_verbose(5, USNIC_OUT,
                                "btl:usnic:%s: unable to create endpoint for module=%p proc=%p\n",
                                __func__, (void *)module, (void *)usnic_proc);
            OBJ_RELEASE(usnic_proc);
            continue;
        }

        /* We like this new endpoint; save it */
        opal_pointer_array_add(&module->all_procs, usnic_proc);

        union ibv_gid gid = usnic_endpoint->endpoint_remote_addr.gid;
        opal_output_verbose(5, USNIC_OUT,
                            "btl:usnic: new usnic peer endpoint: subnet = 0x%016" PRIx64 ", interface = 0x%016" PRIx64,
                            ntoh64(gid.global.subnet_prefix),
                            ntoh64(gid.global.interface_id));

        endpoints[i] = usnic_endpoint;
        ++num_created;
    }

    opal_output_verbose(5, USNIC_OUT,
                        "btl:usnic: made %" PRIsize_t " endpoints",
                        num_created);
    return OMPI_SUCCESS;
}

/*
 * Print a warning about how the remote peer was unreachable.
 *
 * This is a separate helper function simply because it's somewhat
 * bulky to put inline.
 */
static void add_procs_warn_ah_fail(ompi_btl_usnic_module_t *module,
                                   ompi_btl_usnic_endpoint_t *endpoint)
{
    /* Only show the warning if it is enabled */
    if (!mca_btl_usnic_component.show_route_failures) {
        return;
    }

    char local[IPV4STRADDRLEN], remote[IPV4STRADDRLEN];
    ompi_btl_usnic_snprintf_ipv4_addr(local, sizeof(local),
                                      module->local_addr.ipv4_addr,
                                      module->local_addr.cidrmask);
    ompi_btl_usnic_snprintf_ipv4_addr(remote, sizeof(remote),
                                      endpoint->endpoint_remote_addr.ipv4_addr,
                                      endpoint->endpoint_remote_addr.cidrmask);

    opal_output_verbose(15, USNIC_OUT,
                        "btl:usnic: %s/%s (%s) couldn't reach peer %s",
                        ibv_get_device_name(module->device),
                        module->if_name, local, remote);
    opal_show_help("help-mpi-btl-usnic.txt", "create_ah failed",
                   true,
                   ompi_process_info.nodename,
                   local,
                   module->if_name,
                   ibv_get_device_name(module->device),
                   endpoint->endpoint_proc->proc_ompi->proc_hostname,
                   remote);
}

/* The call to ibv_create_ah() may initiate an ARP resolution, and may
 * therefore take some time to complete.  Hence, it will return 1 of 4
 * things:
 *
 * 1. a valid new ah
 * 2. NULL and errno == EAGAIN (ARP not complete; try again later)
 * 3. NULL and errno == EADDRNOTAVAIL (unable to reach peer)
 * 4. NULL and errno != (EAGAIN or ADDRNOTAVAIL) (fatal error)
 *
 * Since ibv_create_ah() is therefore effectively non-blocking, we
 * gang all the endpoint ah creations here in this loop so that we can
 * get some parallelization of ARP resolution.
 */
static int add_procs_create_ahs(ompi_btl_usnic_module_t *module,
                                size_t array_len,
                                struct mca_btl_base_endpoint_t **endpoints)
{
    int ret = OMPI_SUCCESS;
    size_t i;
    size_t num_ah_left;
    time_t ts_last_created;
    struct ibv_ah_attr ah_attr;

    /* memset the ah_attr to both silence valgrind warnings (since the
       attr struct ends up getting written down an fd to the kernel)
       and actually zero out all the fields that we don't care about
       and want to be logically false. */
    memset(&ah_attr, 0, sizeof(ah_attr));
    ah_attr.is_global = 1;
    ah_attr.port_num = 1;

    /* Mark all endpoints as unreachable (this should already be done,
       but just be defensive) */
    for (num_ah_left = i = 0; i < array_len; i++) {
        if (NULL != endpoints[i]) {
            endpoints[i]->endpoint_remote_ah = NULL;
            ++num_ah_left;
        }
    }

    ts_last_created = time(NULL);
    while (num_ah_left > 0) {
        for (i = 0; i < array_len; i++) {
            if (NULL != endpoints[i] &&
                NULL == endpoints[i]->endpoint_remote_ah) {
                ah_attr.grh.dgid = endpoints[i]->endpoint_remote_addr.gid;
                endpoints[i]->endpoint_remote_ah =
                    ibv_create_ah(module->pd, &ah_attr);

                /* Got a successfully-created AH */
                if (NULL != endpoints[i]->endpoint_remote_ah) {
                    ts_last_created = time(NULL);
                    --num_ah_left;
                }

                /* Got some kind of address failure.  This usually
                   means that we couldn't find a route to that peer
                   (e.g., the networking is hosed between us).  So
                   just mark that we can't reach this peer, and print
                   a pretty warning. */
                else if (EADDRNOTAVAIL == errno ||
                         EHOSTUNREACH == errno) {
                    add_procs_warn_ah_fail(module, endpoints[i]);

                    OBJ_RELEASE(endpoints[i]);
                    endpoints[i] = NULL;
                    --num_ah_left;
                }

                /* Got some other kind of error -- give up on this
                   interface. */
                else if (EAGAIN != errno) {
                    opal_show_help("help-mpi-btl-usnic.txt", "ibv API failed",
                                   true,
                                   ompi_process_info.nodename,
                                   ibv_get_device_name(module->device),
                                   module->if_name,
                                   "ibv_create_ah()", __FILE__, __LINE__,
                                   "Failed to create an address handle");
                    ret = OMPI_ERR_OUT_OF_RESOURCE;
                    break;
                }
            }
        }

        /* Has it been too long since our last AH creation (ARP
           resolution)?  If so, we're probably never going to finish,
           so just mark all remaining endpoints as unreachable and
           bail. */
        if (num_ah_left > 0 &&
            time(NULL) > (ts_last_created +
                          mca_btl_usnic_component.arp_timeout)) {
            opal_show_help("help-mpi-btl-usnic.txt", "ibv_create_ah timeout",
                           true,
                           ompi_process_info.nodename,
                           ibv_get_device_name(module->device),
                           module->if_name,
                           mca_btl_usnic_component.arp_timeout);
            break;
        }

        /* If we still have addresses that aren't resolved yet, sleep
           a little to let kernel threads do some work behind the
           scenes */
        if (num_ah_left > 0) {
            usleep(1);
        }
    }

    /* Look through the list:
       - If something went wrong above, free all endpoints.
       - If an otherwise-valid endpoint has no AH, that means we timed
         out trying to resolve it, so just release that endpoint. */
    size_t num_created = 0;
    for (i = 0; i < array_len; i++) {
        if (NULL != endpoints[i]) {
            if (OMPI_SUCCESS != ret ||
                NULL == endpoints[i]->endpoint_remote_ah) {
                OBJ_RELEASE(endpoints[i]);
                endpoints[i] = NULL;
            } else {
                ++num_created;
            }
        }
    }

    /* All done */
    opal_output_verbose(5, USNIC_OUT,
                        "btl:usnic: made %" PRIsize_t " address handles",
                        num_created);
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
 * For example, ibv_create_ah() may fail in a fatal way (i.e., we
 * should fail the entire add_procs()), or it may fail for one or more
 * peers (i.e., we should just mark those peers as unreachable and not
 * add a proc or endpoint for them).
 */
static int usnic_add_procs(struct mca_btl_base_module_t* base_module,
                             size_t nprocs,
                             struct ompi_proc_t **ompi_procs,
                             struct mca_btl_base_endpoint_t** endpoints,
                             opal_bitmap_t* reachable)
{
    ompi_btl_usnic_module_t* module = (ompi_btl_usnic_module_t*) base_module;
    int rc;

    /* First, create endpoints (and procs, if they're not already
       created) for all the usnic-reachable procs we were given. */
    rc = add_procs_create_endpoints(module, nprocs, ompi_procs, endpoints);
    if (OMPI_SUCCESS != rc) {
        goto fail;
    }

    /* Create address handles for all the newly-created endpoints */
    rc = add_procs_create_ahs(module, nprocs, endpoints);
    if (OMPI_SUCCESS != rc) {
        goto fail;
    }

    /* Find all the endpoints with address handles and mark them as
       reachable */
    for (size_t i = 0; i < nprocs; ++i) {
        if (NULL != endpoints[i] &&
            NULL != endpoints[i]->endpoint_remote_ah) {
            opal_bitmap_set_bit(reachable, i);
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
        ompi_btl_usnic_connectivity_map();
    }

    return OMPI_SUCCESS;

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
                             struct ompi_proc_t **procs,
                             struct mca_btl_base_endpoint_t **peers)
{
    size_t i, j;
    ompi_btl_usnic_module_t *module;
    ompi_btl_usnic_endpoint_t *endpoint;
    int index;

    module = (struct ompi_btl_usnic_module_t *)base_module;

    for (i = 0; i < nprocs; i++) {
        ompi_btl_usnic_proc_t* proc =
            ompi_btl_usnic_proc_lookup_ompi(procs[i]);
        if (NULL != proc) {

            /* find endpoint for this module */
            for (j = 0; j < proc->proc_endpoint_count; ++j) {
                endpoint = proc->proc_endpoints[j];
                if (NULL != endpoint &&
                        endpoint->endpoint_module == module) {

                    /* This call to usnic_del_procs is actually an
                     * implicit ACK of every packet we have ever sent
                     * ***because it is only ever invoked after an
                     * OOB/grpcomm barrier (in MPI_COMM_DISCONNECT and
                     * MPI_FINALIZE)***, so call handle_ack (via
                     * flush_endpoint) to do all the ACK processing
                     * and release all the data that needs
                     * releasing. */
                    if (!ENDPOINT_DRAINED(endpoint)) {
                        ompi_btl_usnic_flush_endpoint(endpoint);
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

    return OMPI_SUCCESS;
}


/*
 * Let the PML register a callback function with me
 */
static int usnic_register_pml_err_cb(struct mca_btl_base_module_t* btl, 
                                     mca_btl_base_module_error_cb_fn_t cbfunc)
{
    ompi_btl_usnic_module_t *module = (ompi_btl_usnic_module_t*) btl;

    module->pml_error_callback = cbfunc;

    return OMPI_SUCCESS;
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
    ompi_btl_usnic_send_frag_t *frag;
    ompi_btl_usnic_module_t *module = (ompi_btl_usnic_module_t*) btl;
    mca_btl_base_descriptor_t *desc;


    /* small is easy, just allocate a small segment */
    if (OPAL_LIKELY(size <= module->max_frag_payload)) {
        ompi_btl_usnic_small_send_frag_t *sfrag;

        sfrag = ompi_btl_usnic_small_send_frag_alloc(module);
        if (NULL == sfrag) {
            return NULL;
        }
        frag = &sfrag->ssf_base;

    /* between MTU and eager limit, we need to allocate a buffer
     * which can hold the data.  We will allocate a
     * large fragment, and attach the buffer to it.
     */
    } else {
        ompi_btl_usnic_large_send_frag_t *lfrag;

        /* truncate to eager_limit */
        if (OPAL_UNLIKELY(size > module->super.btl_eager_limit)) {
            size = module->super.btl_eager_limit;
        }

        lfrag = ompi_btl_usnic_large_send_frag_alloc(module);
        if (OPAL_UNLIKELY(NULL == lfrag)) {
            return NULL;
        }
        frag = &lfrag->lsf_base;

        assert(size > 0);
        lfrag->lsf_buffer = malloc(size);
        if (OPAL_UNLIKELY(NULL == lfrag->lsf_buffer)) {
            ompi_btl_usnic_frag_return(module, &lfrag->lsf_base.sf_base);
            return NULL;
        }

        /* pointer to buffer for caller */
        frag->sf_base.uf_base.des_src[0].seg_addr.pval = lfrag->lsf_buffer;

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
    desc->des_src[0].seg_len = size;
    desc->des_src_cnt = 1;

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
    ompi_btl_usnic_frag_t* frag = (ompi_btl_usnic_frag_t*)des;

#if MSGDEBUG2
    opal_output(0, "usnic_free: %p (%s)\n", (void*)frag,
            usnic_frag_type(frag->uf_type));
#endif
    /* calling free routine gives us ownership - we need to make sure
     * the flag is set for lower layers.
     */
    frag->uf_base.des_flags |= MCA_BTL_DES_FLAGS_BTL_OWNERSHIP;

    ompi_btl_usnic_frag_return_cond((struct ompi_btl_usnic_module_t *)btl,
            frag);

    return OMPI_SUCCESS;
}

/*
 * Notes from george:
 *
 * - BTL ALLOC: allocating control messages or eager frags if BTL
     does not have INPLACE flag.  To be clear: max it will ever alloc
     is eager_limit.  THEREFORE: eager_limit is the max that ALLOC
     must always be able to alloc.
     --> Contraction in the btl.h documentation.
 *
 * - BTL PREPARE SRC: max_send_size frags go through here.  Can return
     a smaller size than was asked for.
 *
 * - BTL PREPARE DEST: not used if you don't have PUT/GET
 *
 * - BTL SEND: will be used after ALLOC / PREPARE
 */

/* Responsible for handling "small" frags (reserve + *size <= max_frag_payload)
 * in the same manner as btl_prepare_src.  Must return a smaller amount than
 * requested if the given convertor cannot process the entire (*size).
 */
static inline
ompi_btl_usnic_send_frag_t *
prepare_src_small(
    struct ompi_btl_usnic_module_t* module,
    struct mca_btl_base_endpoint_t* endpoint,
    struct mca_mpool_base_registration_t* registration,
    struct opal_convertor_t* convertor,
    uint8_t order,
    size_t reserve,
    size_t* size,
    uint32_t flags)
{
    ompi_btl_usnic_send_frag_t *frag;
    ompi_btl_usnic_small_send_frag_t *sfrag;
    size_t payload_len;

    payload_len = *size + reserve;
    assert(payload_len <= module->max_frag_payload); /* precondition */

    sfrag = ompi_btl_usnic_small_send_frag_alloc(module);
    if (OPAL_UNLIKELY(NULL == sfrag)) {
        return NULL;
    }
    frag = &sfrag->ssf_base;

    /* In the case of a convertor, we will copy the data in now, since that is
     * the cheapest way to discover how much we can actually send (since we know
     * we will pack it anyway later).  The alternative is to do all of the
     * following:
     * 1) clone_with_position(convertor) and see where the new position ends up
     *    actually being (see ompi_btl_usnic_convertor_pack_peek).  Otherwise we
     *    aren't fulfilling our contract w.r.t. (*size).
     * 2) Add a bunch of branches checking for different cases, both here and in
     *    progress_sends
     * 3) If we choose to defer the packing, we must clone the convertor because
     *    the PML owns it and might reuse it for another prepare_src call.
     *
     * Two convertor clones is likely to be at least as slow as just copying the
     * data and might consume a similar amount of memory.  Plus we still have to
     * pack it later to send it.
     *
     * The reason we do not copy non-convertor buffer at this point is because
     * we might still use INLINE for the send, and in that case we do not want
     * to copy the data at all.
     */
    if (OPAL_UNLIKELY(opal_convertor_need_buffers(convertor))) {
        /* put user data just after end of 1st seg (upper layer header) */
        assert(payload_len <= module->max_frag_payload);
        usnic_convertor_pack_simple(
                convertor,
                (IOVBASE_TYPE*)(intptr_t)(frag->sf_base.uf_src_seg[0].seg_addr.lval + reserve),
                *size,
                size);
        payload_len = reserve + *size;
        frag->sf_base.uf_base.des_src_cnt = 1;
        /* PML will copy header into beginning of segment */
        frag->sf_base.uf_src_seg[0].seg_len = payload_len;
    } else {
        opal_convertor_get_current_pointer(convertor,
                                           &sfrag->ssf_base.sf_base.uf_src_seg[1].seg_addr.pval);
        frag->sf_base.uf_base.des_src_cnt = 2;
        frag->sf_base.uf_src_seg[0].seg_len = reserve;
        frag->sf_base.uf_src_seg[1].seg_len = *size;
    }

    frag->sf_base.uf_base.des_flags = flags;
    frag->sf_endpoint = endpoint;

    return frag;
}

/* Packs data from the given large send frag into single new segment and
 * returns a pointer to it.  The packed data comes first from SG[0] (PML
 * header) and then second from either SG[1] (if seg_addr is non-NULL) or from
 * the convertor contained in the frag.
 *
 * The frag's bookkeeping data will be updated appropriately. */
static
ompi_btl_usnic_chunk_segment_t *
pack_chunk_seg_from_frag(
    struct ompi_btl_usnic_module_t* module,
    ompi_btl_usnic_large_send_frag_t *lfrag)
{
    ompi_btl_usnic_chunk_segment_t *seg;
    uint8_t *copyptr;
    size_t copylen;
    size_t seg_space;
    size_t max_data;

    assert(NULL != lfrag);
    /* never should be attempting to pack if we've already packed everything */
    assert(lfrag->lsf_pack_bytes_left > 0);

    seg = ompi_btl_usnic_chunk_segment_alloc(module);
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
    seg->ss_base.us_sg_entry[0].length = module->max_chunk_payload - seg_space;

    return seg;
}

static
void *
pack_chunk_seg_chain_with_reserve(
    struct ompi_btl_usnic_module_t* module,
    ompi_btl_usnic_large_send_frag_t *lfrag,
    size_t reserve_len,
    opal_convertor_t *convertor,
    size_t max_convertor_bytes,
    size_t *convertor_bytes_packed)
{
    ompi_btl_usnic_chunk_segment_t *seg;
    void *ret_ptr = NULL;
    int n_segs;
    uint8_t *copyptr;
    size_t copylen;
    size_t seg_space;
    size_t max_data;
    bool first_pass;

    assert(NULL != lfrag);
    assert(NULL != convertor_bytes_packed);

    n_segs = 0;
    *convertor_bytes_packed = 0;

    first_pass = true;
    while (*convertor_bytes_packed < max_convertor_bytes ||
           first_pass) {
        seg = ompi_btl_usnic_chunk_segment_alloc(module);
        if (OPAL_UNLIKELY(NULL == seg)) {
            BTL_ERROR(("chunk segment allocation error"));
            abort(); /* XXX */
        }
        ++n_segs;

        seg_space = module->max_chunk_payload;
        copyptr = seg->ss_base.us_payload.raw;

        if (first_pass && reserve_len > 0) {
            /* logic could accommodate >max, but currently doesn't */
            assert(reserve_len <= module->max_chunk_payload);
            ret_ptr = copyptr;
            seg_space -= reserve_len;
            copyptr += reserve_len;
        }

        /* now pack any convertor data */
        if (*convertor_bytes_packed < max_convertor_bytes && seg_space > 0) {
            copylen = max_convertor_bytes - *convertor_bytes_packed;
            if (copylen > seg_space) {
                copylen = seg_space;
            }
            usnic_convertor_pack_simple(convertor, copyptr, copylen, &max_data);
            seg_space -= max_data;
            *convertor_bytes_packed += max_data;

            /* If unable to pack any of the remaining bytes, release the
            * most recently allocated segment and finish processing.
            */
            if (seg_space == module->max_chunk_payload) {
                assert(max_data == 0); /* only way this can happen */
                ompi_btl_usnic_chunk_segment_return(module, seg);
                break;
            }
        }

        /* bozo checks */
        assert(seg_space >= 0);
        assert(seg_space < module->max_chunk_payload);

        /* append segment of data to chain to send */
        seg->ss_parent_frag = &lfrag->lsf_base;
        seg->ss_base.us_sg_entry[0].length = module->max_chunk_payload - seg_space;
        opal_list_append(&lfrag->lsf_seg_chain, &seg->ss_base.us_list.super);

#if MSGDEBUG1
        opal_output(0, "%s: appending seg=%p, frag=%p, payload=%zd\n",
                    __func__, (void *)seg, (void *)lfrag,
                    (module->max_chunk_payload - seg_space));
#endif

        first_pass = false;
    }

    return ret_ptr;
}

/* Responsible for handling "large" frags (reserve + *size > max_frag_payload)
 * in the same manner as btl_prepare_src.  Must return a smaller amount than
 * requested if the given convertor cannot process the entire (*size).
 */
static
ompi_btl_usnic_send_frag_t *
prepare_src_large(
    struct ompi_btl_usnic_module_t* module,
    struct mca_btl_base_endpoint_t* endpoint,
    struct mca_mpool_base_registration_t* registration,
    struct opal_convertor_t* convertor,
    uint8_t order,
    size_t reserve,
    size_t* size,
    uint32_t flags)
{
    ompi_btl_usnic_send_frag_t *frag;
    ompi_btl_usnic_large_send_frag_t *lfrag;
    int rc;

    /* Get holder for the msg */
    lfrag = ompi_btl_usnic_large_send_frag_alloc(module);
    if (OPAL_UNLIKELY(NULL == lfrag)) {
        return NULL;
    }
    frag = &lfrag->lsf_base;

    /* The header location goes in SG[0], payload in SG[1].  If we are using a
     * convertor then SG[1].seg_len is accurate but seg_addr is NULL. */
    frag->sf_base.uf_base.des_src_cnt = 2;

    /* stash header location, PML will write here */
    frag->sf_base.uf_src_seg[0].seg_addr.pval = &lfrag->lsf_ompi_header;
    frag->sf_base.uf_src_seg[0].seg_len = reserve;
    /* make sure upper header small enough */
    assert(reserve <= sizeof(lfrag->lsf_ompi_header));

    if (OPAL_UNLIKELY(opal_convertor_need_buffers(convertor))) {
        /* threshold == -1 means always pack eagerly */
        if (mca_btl_usnic_component.pack_lazy_threshold >= 0 &&
            *size >= (size_t)mca_btl_usnic_component.pack_lazy_threshold) {
            MSGDEBUG1_OUT("packing frag %p on the fly", (void *)frag);
            lfrag->lsf_pack_on_the_fly = true;

            /* tell the PML we will absorb as much as possible while still
             * respecting indivisible element boundaries in the convertor */
            *size = ompi_btl_usnic_convertor_pack_peek(convertor, *size);

            /* Clone the convertor b/c we (the BTL) don't own it and the PML
             * might mutate it after we return from this function. */
            rc = opal_convertor_clone(convertor, &frag->sf_convertor,
                                      /*copy_stack=*/true);
            if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
                BTL_ERROR(("unexpected convertor clone error"));
                abort(); /* XXX */
            }
        }
        else {
            /* pack everything in the convertor into a chain of segments now,
             * leaving space for the PML header in the first segment */
            lfrag->lsf_base.sf_base.uf_src_seg[0].seg_addr.pval =
                pack_chunk_seg_chain_with_reserve(module, lfrag, reserve,
                                                  convertor, *size, size);
        }

        /* We set SG[1] to {NULL,bytes_packed} so that various calculations
         * by both PML and this BTL will be correct.  For example, the PML adds
         * up the bytes in the descriptor segments to determine if an MPI-level
         * request is complete or not. */
        frag->sf_base.uf_src_seg[1].seg_addr.pval = NULL;
        frag->sf_base.uf_src_seg[1].seg_len = *size;
    } else {
        /* convertor not needed, just save the payload pointer in SG[1] */
        lfrag->lsf_pack_on_the_fly = true;
        opal_convertor_get_current_pointer(convertor,
                                           &frag->sf_base.uf_src_seg[1].seg_addr.pval);
        frag->sf_base.uf_src_seg[1].seg_len = *size;
    }

    frag->sf_base.uf_base.des_flags = flags;
    frag->sf_endpoint = endpoint;

    return frag;
}


/**
 * Note the "user" data the PML wishes to communicate and return a descriptor
 * that can be used for send or put.  We create a frag (which is also a
 * descriptor by virtue of its base class) and populate it with enough
 * source information to complete a future send/put.
 *
 * We will create either a small send frag if < than an MTU, otherwise a large
 * send frag.  The convertor will be saved for deferred packing if the user
 * buffer is noncontiguous.  Otherwise it will be saved in one of the
 * descriptor's SGEs.
 *
 * NOTE that the *only* reason this routine is allowed to return a size smaller
 * than was requested is if the convertor cannot process the entire amount.
 */
static mca_btl_base_descriptor_t*
usnic_prepare_src(
    struct mca_btl_base_module_t* base_module,
    struct mca_btl_base_endpoint_t* endpoint,
    struct mca_mpool_base_registration_t* registration,
    struct opal_convertor_t* convertor,
    uint8_t order,
    size_t reserve,
    size_t* size,
    uint32_t flags)
{
    ompi_btl_usnic_module_t *module = (ompi_btl_usnic_module_t*) base_module;
    ompi_btl_usnic_send_frag_t *frag;
    uint32_t payload_len;
#if MSGDEBUG2
    size_t osize = *size;
#endif

    /* Do we need to check the connectivity?  If enabled, we'll check
       the connectivity at either first send to peer X or first ACK to
       peer X. */
    ompi_btl_usnic_check_connectivity(module, endpoint);

    /*
     * if total payload len fits in one MTU use small send, else large
     */
    payload_len = *size + reserve;
    if (payload_len <= module->max_frag_payload) {
        frag = prepare_src_small(module, endpoint, registration, convertor,
                                 order, reserve, size, flags);
    } else {
        frag = prepare_src_large(module, endpoint, registration, convertor,
                                 order, reserve, size, flags);
    }

#if MSGDEBUG2
    opal_output(0, "prep_src: %s %s frag %p, size=%d+%u (was %u), conv=%p\n",
                module->device->name,
                (reserve + *size) <= module->max_frag_payload?"small":"large",
                (void *)frag, (int)reserve, (unsigned)*size, (unsigned)osize,
                (void *)convertor);
#if MSGDEBUG1
    {
        unsigned i;
        mca_btl_base_descriptor_t *desc = &frag->sf_base.uf_base;
        for (i=0; i<desc->des_src_cnt; ++i) {
            opal_output(0, "  %d: ptr:%p len:%d\n", i,
                        (void *)desc->des_src[i].seg_addr.pval,
                        desc->des_src[i].seg_len);
        }
    }
#endif
#endif

    return &frag->sf_base.uf_base;
}

static mca_btl_base_descriptor_t*
usnic_prepare_dst(
    struct mca_btl_base_module_t* base_module,
    struct mca_btl_base_endpoint_t* endpoint,
    struct mca_mpool_base_registration_t* registration,
    struct opal_convertor_t* convertor,
    uint8_t order,
    size_t reserve,
    size_t* size,
    uint32_t flags)
{
    ompi_btl_usnic_put_dest_frag_t *pfrag;
    ompi_btl_usnic_module_t *module;
    void *data_ptr;

    module = (ompi_btl_usnic_module_t *)base_module;

    /* allocate a fragment for this */
    pfrag = (ompi_btl_usnic_put_dest_frag_t *)
        ompi_btl_usnic_put_dest_frag_alloc(module);
    if (NULL == pfrag) {
        return NULL;
    }

    /* find start of the data */
    opal_convertor_get_current_pointer(convertor, (void **) &data_ptr);

    /* make a seg entry pointing at data_ptr */
    pfrag->uf_dst_seg[0].seg_addr.pval = data_ptr;
    pfrag->uf_dst_seg[0].seg_len = *size;

    pfrag->uf_base.order       = order;
    pfrag->uf_base.des_flags   = flags;

#if MSGDEBUG2
    opal_output(0, "prep_dst size=%d, addr=%p, pfrag=%p\n", (int)*size,
            data_ptr, (void *)pfrag);
#endif

    return &pfrag->uf_base;
}


/*
 * Emulate an RDMA put.  We'll send the remote address
 * across to the other side so it will know where to put the data
 */
static int
usnic_put(
    struct mca_btl_base_module_t *btl,
    struct mca_btl_base_endpoint_t *endpoint,
    struct mca_btl_base_descriptor_t *desc)
{
    int rc;
    ompi_btl_usnic_send_frag_t *frag;

    frag = (ompi_btl_usnic_send_frag_t *)desc;

    compute_sf_size(frag);
    frag->sf_ack_bytes_left = frag->sf_size;

#if MSGDEBUG2
    opal_output(0, "usnic_put, frag=%p, size=%d\n", (void *)frag,
            (int)frag->sf_size);
#if MSGDEBUG1
    { unsigned i;
        for (i=0; i<desc->des_src_cnt; ++i) {
            opal_output(0, "  %d: ptr:%p len:%d%s\n", i,
                    desc->des_src[i].seg_addr.pval,
                    desc->des_src[i].seg_len,
                    (i==0)?" (source)":"");
        }
        for (i=0; i<desc->des_dst_cnt; ++i) {
            opal_output(0, "  %d: ptr:%p len:%d%s\n", i,
                    desc->des_dst[i].seg_addr.pval,
                    desc->des_dst[i].seg_len,
                    (i==0)?" (dest)":"");
        }
    }
#endif
#endif

    /* copy out address - why does he not use our provided holder? */
    frag->sf_base.uf_dst_seg[0].seg_addr.pval = desc->des_dst->seg_addr.pval;

    rc = ompi_btl_usnic_finish_put_or_send((ompi_btl_usnic_module_t *)btl,
                                           (ompi_btl_usnic_endpoint_t *)endpoint,
                                           frag,
                                           /*tag=*/MCA_BTL_NO_ORDER);

    return rc;
}

static int usnic_finalize(struct mca_btl_base_module_t* btl)
{
    int i;
    int rc;
    ompi_btl_usnic_module_t* module = (ompi_btl_usnic_module_t*)btl;

    /**** JMS BEGIN DIFFERENCE ****/
    /* Starting with v1.9, usnic_del_procs() is called automatically
       by the PML.  In the v1.7/v1.8 series, it is not -- so we call
       it here right away in usnic_finalize() to simulate/pretend that
       it has already been called by the PML.  In this way, we can
       make the rest of the usnic module teardown as similar as
       possible between trunk/v1.9 and v1.8. */
    ompi_btl_usnic_proc_t *proc;
    for (i = 0; i < opal_pointer_array_get_size(&module->all_procs); ++i) {
        proc = opal_pointer_array_get_item(&module->all_procs, i);
        if (NULL != proc) {
            usnic_del_procs(&module->super, 1, &proc->proc_ompi, NULL);
        }
    }
    /**** JMS END DIFFERENCE ****/

    if (module->device_async_event_active) {
        opal_event_del(&(module->device_async_event));
        module->device_async_event_active = false;
    }

    ompi_btl_usnic_channel_finalize(module,
            &module->mod_channels[USNIC_DATA_CHANNEL]);
    ompi_btl_usnic_channel_finalize(module,
            &module->mod_channels[USNIC_PRIORITY_CHANNEL]);

    /* Shutdown the stats on this module */
    ompi_btl_usnic_stats_finalize(module);

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

    for (i = module->first_pool; i <= module->last_pool; ++i) {
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

    /* destroy the PD after all the CQs and AHs have been destroyed, otherwise
     * we get a minor leak in libusnic_verbs */
    rc = ibv_dealloc_pd(module->pd);
    if (rc) {
        BTL_ERROR(("failed to ibv_dealloc_pd, err=%d (%s)", rc, strerror(rc)));
    }

    rc = ibv_close_device(module->device_context);
    if (-1 == rc) {
        BTL_ERROR(("failed to ibv_close_device"));
    }

    return OMPI_SUCCESS;
}

static void
usnic_do_resends(
    ompi_btl_usnic_module_t *module)
{
    ompi_btl_usnic_send_segment_t *sseg;
    ompi_btl_usnic_endpoint_t *endpoint;
    struct ompi_btl_usnic_channel_t *data_channel;
    int ret;

    data_channel = &module->mod_channels[USNIC_DATA_CHANNEL];

    while (((size_t)data_channel->sd_wqe > 0) &&
           !opal_list_is_empty(&module->pending_resend_segs)) {

        /*
         * If a segment is on the re-send list, it will not
         * be in the retransmit hotel.  Post the segment, then check it in.
         */
        sseg = (ompi_btl_usnic_send_segment_t *)
            opal_list_remove_first(&module->pending_resend_segs);
        endpoint = sseg->ss_parent_frag->sf_endpoint;

        /* clobber any stale piggy-backed ACK */
        sseg->ss_base.us_btl_header->ack_present = 0;

        /* Only post this segment if not already posted */
        if (sseg->ss_send_posted == 0) {

            /* resends are always standard segments */
            sseg->ss_channel = USNIC_DATA_CHANNEL;

            /* re-send the segment */
            ompi_btl_usnic_post_segment(module, endpoint, sseg);

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
    ompi_btl_usnic_module_t *module,
    ompi_btl_usnic_endpoint_t *endpoint,
    ompi_btl_usnic_send_frag_t *frag)
{
    ompi_btl_usnic_large_send_frag_t *lfrag;
    ompi_btl_usnic_btl_chunk_header_t *chp;
    ompi_btl_usnic_send_segment_t *sseg;
    size_t payload_len;

    assert(frag->sf_base.uf_type == OMPI_BTL_USNIC_FRAG_LARGE_SEND);
    lfrag = (ompi_btl_usnic_large_send_frag_t *)frag;
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
        sseg = (ompi_btl_usnic_send_segment_t *)
            opal_list_remove_first(&lfrag->lsf_seg_chain);
    }

    assert(NULL != sseg);
    payload_len = sseg->ss_base.us_sg_entry[0].length;

    assert(payload_len > 0); /* must have made progress */
    assert(payload_len <= module->max_chunk_payload);
    assert(lfrag->lsf_bytes_left >= payload_len);

    /* set actual packet length for verbs */
    assert(1 == sseg->ss_send_desc.num_sge); /* chunk invariant */
    sseg->ss_base.us_sg_entry[0].length =
        sizeof(ompi_btl_usnic_btl_chunk_header_t) + payload_len;
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
    ompi_btl_usnic_endpoint_send_segment(module, sseg);

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
        if (frag->sf_base.uf_dst_seg[0].seg_addr.pval == NULL &&
                (frag->sf_base.uf_base.des_flags &
                 MCA_BTL_DES_FLAGS_BTL_OWNERSHIP)) {

            OMPI_BTL_USNIC_DO_SEND_FRAG_CB(module, frag, "large");
        }
    }
}

/*
 * Progress the send engine.
 * Should only ever be called from usnic_component_progress() to 
 * avoid re-entrancy issues.
 */
void
ompi_btl_usnic_module_progress_sends(
    ompi_btl_usnic_module_t *module)
{
    ompi_btl_usnic_send_frag_t *frag;
    ompi_btl_usnic_send_segment_t *sseg;
    ompi_btl_usnic_endpoint_t *endpoint;
    struct ompi_btl_usnic_channel_t *data_channel;
    struct ompi_btl_usnic_channel_t *prio_channel;

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
    while (((size_t) data_channel->sd_wqe > 0) &&
           !opal_list_is_empty(&module->endpoints_with_sends)) {
        ompi_btl_usnic_small_send_frag_t *sfrag;
        size_t payload_len;

        /*
         * Grab the first endpoint with a pending send.  Presence on this
         * list means there is a fragment with data ready to go and
         * the endpoint's send window is open, and the endpoint has send
         * credits.
         */

        endpoint = (ompi_btl_usnic_endpoint_t *)
            opal_list_get_first(&module->endpoints_with_sends);
        frag = (ompi_btl_usnic_send_frag_t *)
            opal_list_get_first(&endpoint->endpoint_frag_send_queue);

        /*
         * small send?  (fragment fits in one segment)
         * Send ptr and length will be in uf_src_seg[0]
         */
        if (frag->sf_base.uf_type == OMPI_BTL_USNIC_FRAG_SMALL_SEND) {

            /* remove this frag from sending list now because upper layer may
             * decide to put it on some other list in the callback
             */
            opal_list_remove_item(&endpoint->endpoint_frag_send_queue,
                    &frag->sf_base.uf_base.super.super);

            sfrag = (ompi_btl_usnic_small_send_frag_t *)frag;
            sseg = &sfrag->ssf_segment;

            /* get payload len from segment */
            payload_len = sfrag->ssf_base.sf_size;
            sseg->ss_base.us_btl_header->payload_len = payload_len;

#if MSGDEBUG1
            opal_output(0, "progress send small, frag=%p, ptr=%p, payload=%zd, len=%"PRIu32", ep=%p, tag=%d\n",
                    (void *)frag,
                    (void *)sseg->ss_base.us_sg_entry[0].addr, payload_len,
                    sseg->ss_base.us_sg_entry[0].length,
                    (void *)frag->sf_endpoint,
                    sseg->ss_base.us_btl_header->tag);
#endif

            /* post the send */
            ompi_btl_usnic_endpoint_send_segment(module, sseg);

            /* don't do callback yet if this is a put */
            if (frag->sf_base.uf_dst_seg[0].seg_addr.pval == NULL) {
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
                    OMPI_BTL_USNIC_DO_SEND_FRAG_CB(module, frag, "small");
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
    endpoint = ompi_btl_usnic_get_first_endpoint_needing_ack(module);
    while (((size_t)prio_channel->sd_wqe) > 0 && endpoint != NULL) {
        ompi_btl_usnic_endpoint_t *next_endpoint;

        /* get next in list */
        next_endpoint = ompi_btl_usnic_get_next_endpoint_needing_ack(endpoint);

        /* Is it time to send ACK? */
        if (endpoint->endpoint_acktime == 0 ||
            endpoint->endpoint_acktime <= get_nsec()) {
            ompi_btl_usnic_ack_send(module, endpoint);
            ompi_btl_usnic_remove_from_endpoints_needing_ack(endpoint);
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
 *  from ompi_btl_usnic_progress_sends()
 */
static int
usnic_send(
    struct mca_btl_base_module_t* base_module,
    struct mca_btl_base_endpoint_t* base_endpoint,
    struct mca_btl_base_descriptor_t* descriptor,
    mca_btl_base_tag_t tag)
{
    int rc;
    ompi_btl_usnic_send_frag_t *frag;
    ompi_btl_usnic_small_send_frag_t *sfrag;
    ompi_btl_usnic_endpoint_t *endpoint;
    ompi_btl_usnic_module_t *module;
    ompi_btl_usnic_send_segment_t *sseg;

    endpoint = (ompi_btl_usnic_endpoint_t *)base_endpoint;
    module = (ompi_btl_usnic_module_t *)base_module;
    frag = (ompi_btl_usnic_send_frag_t*) descriptor;

    assert(frag->sf_endpoint == endpoint);
    frag->sf_base.uf_dst_seg[0].seg_addr.pval = NULL;      /* not a PUT */

    compute_sf_size(frag);
    frag->sf_ack_bytes_left = frag->sf_size;

#if MSGDEBUG2
    opal_output(0, "usnic_send: frag=%p, endpoint=%p, tag=%d, sf_size=%d\n",
            (void *)frag, (void *)endpoint,
            tag, (int)frag->sf_size);
#if MSGDEBUG1
    { unsigned i;
        opal_output(0, "  descriptor->des_flags=0x%x\n", descriptor->des_flags);
        for (i=0; i<descriptor->des_src_cnt; ++i) {
            opal_output(0, "  %d: ptr:%p len:%d\n", i,
                    descriptor->des_src[i].seg_addr.pval,
                    descriptor->des_src[i].seg_len);
        }
    }
#endif
#endif

    /*
     * If this fragment is small enough to inline,
     * and we have enough send WQEs,
     * then inline and fastpath it
     */
    if (frag->sf_base.uf_type == OMPI_BTL_USNIC_FRAG_SMALL_SEND &&
            frag->sf_ack_bytes_left < module->max_tiny_payload &&
            WINDOW_OPEN(endpoint) &&
            (module->mod_channels[USNIC_PRIORITY_CHANNEL].sd_wqe >= 
             module->mod_channels[USNIC_PRIORITY_CHANNEL].fastsend_wqe_thresh)) {
        size_t payload_len;

        sfrag = (ompi_btl_usnic_small_send_frag_t *)frag;
        sseg = &sfrag->ssf_segment;

        payload_len = frag->sf_ack_bytes_left;
        sseg->ss_base.us_btl_header->payload_len = payload_len;

        /* fix up verbs SG entries */
        sseg->ss_base.us_sg_entry[0].length =
            sizeof(ompi_btl_usnic_btl_header_t) + 
            frag->sf_base.uf_src_seg[0].seg_len;

        if (frag->sf_base.uf_base.des_src_cnt > 1) {

            /* only briefly, we will set it back to 1 before release */
            sseg->ss_send_desc.num_sge = 2;
            sseg->ss_base.us_sg_entry[1].addr =
                frag->sf_base.uf_src_seg[1].seg_addr.lval;
            sseg->ss_base.us_sg_entry[1].length =
                frag->sf_base.uf_src_seg[1].seg_len;
        } else {
            sseg->ss_send_desc.num_sge = 1;
        }

        sseg->ss_send_desc.send_flags |= IBV_SEND_INLINE;
        sseg->ss_channel = USNIC_PRIORITY_CHANNEL;
        sseg->ss_base.us_btl_header->tag = tag;
#if MSGDEBUG1
        opal_output(0, "INLINE send, sseg=%p", (void *)sseg);
#endif

        /* post the segment now */
        ompi_btl_usnic_endpoint_send_segment(module, sseg);

        /* make a copy of the data for retrans */
        if (frag->sf_base.uf_base.des_src_cnt > 1) {
            memcpy(((char *)(intptr_t)frag->sf_base.uf_src_seg[0].seg_addr.lval +
                     frag->sf_base.uf_src_seg[0].seg_len),
                    frag->sf_base.uf_src_seg[1].seg_addr.pval,
                    frag->sf_base.uf_src_seg[1].seg_len);
            /* update 1st segment length */
            frag->sf_base.uf_base.des_src_cnt = 1;
            frag->sf_base.uf_src_seg[0].seg_len +=
                frag->sf_base.uf_src_seg[1].seg_len;
            /* set up VERBS SG list */
            /* this maintains invariant that num_sge=1 */
            sseg->ss_send_desc.num_sge = 1;
            sseg->ss_base.us_sg_entry[0].length =
                sizeof(ompi_btl_usnic_btl_header_t) + frag->sf_size;
        }

        /* If we own the frag and callback was requested, callback now,
         * else just return 1 to show completion.
         * If we don't own the frag, need to wait for ACK before
         * performing callback on the frag
         */
        if (descriptor->des_flags & MCA_BTL_DES_FLAGS_BTL_OWNERSHIP) {
            if (descriptor->des_flags & MCA_BTL_DES_SEND_ALWAYS_CALLBACK) {
                OMPI_BTL_USNIC_DO_SEND_FRAG_CB(module, frag, "immediate small");
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
        rc = ompi_btl_usnic_finish_put_or_send(module, endpoint, frag, tag);
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
    return OMPI_ERROR;
}
#endif


/*
 * RDMA Memory Pool (de)register callbacks
 */
static int usnic_reg_mr(void* reg_data, void* base, size_t size,
                          mca_mpool_base_registration_t* reg)
{
    ompi_btl_usnic_module_t* mod = (ompi_btl_usnic_module_t*)reg_data;
    ompi_btl_usnic_reg_t* ud_reg = (ompi_btl_usnic_reg_t*)reg;

    ud_reg->mr = ibv_reg_mr(mod->pd, base, size, IBV_ACCESS_LOCAL_WRITE |
                            IBV_ACCESS_REMOTE_WRITE | IBV_ACCESS_REMOTE_READ);

    if (NULL == ud_reg->mr) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    return OMPI_SUCCESS;
}


static int usnic_dereg_mr(void* reg_data,
                            mca_mpool_base_registration_t* reg)
{
    ompi_btl_usnic_reg_t* ud_reg = (ompi_btl_usnic_reg_t*)reg;

    if (ud_reg->mr != NULL) {
        if (ibv_dereg_mr(ud_reg->mr)) {
            opal_output(0, "%s: error unpinning UD memory mr=%p: %s\n",
                        __func__, (void*) ud_reg->mr, strerror(errno));
            return OMPI_ERROR;
        }
    }

    ud_reg->mr = NULL;
    return OMPI_SUCCESS;
}


/*
 * Called back by libevent if an async event occurs on the device
 */
static void module_async_event_callback(int fd, short flags, void *arg)
{
    bool got_event = false;
    bool fatal = false;
    ompi_btl_usnic_module_t *module = (ompi_btl_usnic_module_t*) arg;
    struct ibv_async_event event;

    /* Get the async event */
    if (0 != ibv_get_async_event(module->device_context, &event)) {
        /* This shouldn't happen.  If it does, treat this as a fatal
           error. */
        fatal = true;
    } else {
        got_event = true;
    }

    /* Process the event */
    if (got_event) {
        switch (event.event_type) {
        case IBV_EVENT_PORT_ACTIVE:
            /* This event should never happen, because OMPI will
               ignore ports that are down, and we should only get this
               event if a port *was* down and is now *up*.  But if we
               ever do get it, it should be a harmless event -- just
               ignore it. */
            opal_output_verbose(10, USNIC_OUT,
                                "btl:usnic: got IBV_EVENT_PORT_ACTIVE on %s:%d",
                                ibv_get_device_name(module->device), 
                                module->port_num);
            break;

        case IBV_EVENT_QP_FATAL:
        case IBV_EVENT_PORT_ERR:
#if HAVE_DECL_IBV_EVENT_GID_CHANGE
        case IBV_EVENT_GID_CHANGE:
#endif
        default:
            /* For the moment, these are the only other cases
               usnic_verbs.ko will report to us.  However, they're
               only listed here for completeness.  We currently abort
               if any async event other than PORT_ACTIVE occurs. */

            opal_show_help("help-mpi-btl-usnic.txt", "async event",
                           true, 
                           ompi_process_info.nodename,
                           ibv_get_device_name(module->device), 
                           module->if_name,
                           ibv_event_type_str(event.event_type),
                           event.event_type);
            module->pml_error_callback(&module->super, 
                                       MCA_BTL_ERROR_FLAGS_FATAL,
                                       ompi_proc_local(), "usnic");
            /* The PML error callback will likely not return (i.e., it
               will likely kill the job).  But in case someone
               implements a non-fatal PML error callback someday, do
               reasonable things just in case it does actually
               return. */
            fatal = true;
            break;
        }

        /* Ack the event back to verbs */
        ibv_ack_async_event(&event);
    }

    /* If this is fatal, invoke the upper layer error handler to abort
       the job */
    if (fatal) {
        ompi_btl_usnic_exit();
        /* Does not return */
    }
}

/*
 * Create a single UD queue pair.
 */
static int
init_qp(
    ompi_btl_usnic_module_t* module,
    struct ompi_btl_usnic_channel_t *channel)
{
    struct ibv_qp_attr qp_attr;
    struct ibv_qp_init_attr qp_init_attr;

    /* memset to both silence valgrind warnings (since the attr struct
       ends up getting written down an fd to the kernel) and actually
       zero out all the fields that we don't care about / want to be
       logically false. */
    memset(&qp_init_attr, 0, sizeof(qp_init_attr));

    qp_init_attr.send_cq = channel->cq;
    qp_init_attr.recv_cq = channel->cq;
    qp_init_attr.cap.max_send_wr = channel->chan_sd_num;
    qp_init_attr.cap.max_recv_wr = channel->chan_rd_num;
    qp_init_attr.cap.max_send_sge = 1;
    qp_init_attr.cap.max_recv_sge = 1;
    qp_init_attr.qp_type = IBV_QPT_UD;

    /* We did math up in component_init() to know that there should be
       enough QPs available.  So if create_qp fails, then either the
       memlock limits are too low, or something other than this MPI
       job is consuming QPs. */
    channel->qp = ibv_create_qp(module->pd, &qp_init_attr);
    if (NULL == channel->qp) {
        opal_show_help("help-mpi-btl-usnic.txt", "create ibv resource failed",
                       true, 
                       ompi_process_info.nodename,
                       ibv_get_device_name(module->device),
                       module->if_name,
                       "ibv_create_qp()", __FILE__, __LINE__,
                       "Failed to create a usNIC queue pair");
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    /* memset to both silence valgrind warnings (since the attr struct
       ends up getting written down an fd to the kernel) and actually
       zero out all the fields that we don't care about / want to be
       logically false. */
    memset(&qp_attr, 0, sizeof(qp_attr));
    qp_attr.qp_state = IBV_QPS_INIT;
    qp_attr.port_num = module->port_num;

    if (ibv_modify_qp(channel->qp, &qp_attr,
                      IBV_QP_STATE | IBV_QP_PORT)) {
        opal_show_help("help-mpi-btl-usnic.txt", "ibv API failed",
                       true, 
                       ompi_process_info.nodename,
                       ibv_get_device_name(module->device),
                       module->if_name,
                       "ibv_modify_qp()", __FILE__, __LINE__,
                       "Failed to modify an existing queue pair");
        return OMPI_ERR_TEMP_OUT_OF_RESOURCE;
    }

    /* Find the max inline size */
    memset(&qp_attr, 0, sizeof(qp_attr));
    memset(&qp_init_attr, 0, sizeof(qp_init_attr));
    if (ibv_query_qp(channel->qp, &qp_attr, IBV_QP_CAP,
                     &qp_init_attr) != 0) {
        opal_show_help("help-mpi-btl-usnic.txt", "ibv API failed",
                       true, 
                       ompi_process_info.nodename,
                       ibv_get_device_name(module->device), 
                       module->if_name,
                       "ibv_query_qp()", __FILE__, __LINE__,
                       "Failed to query an existing queue pair");
        return OMPI_ERR_TEMP_OUT_OF_RESOURCE;
    }

    /* only grab this once */
    if (channel == &module->mod_channels[USNIC_DATA_CHANNEL]) {
        module->qp_max_inline = qp_attr.cap.max_inline_data;
    }

    return OMPI_SUCCESS;
}


static int move_qp_to_rtr(ompi_btl_usnic_module_t *module,
                          struct ompi_btl_usnic_channel_t *channel)
{
    struct ibv_qp_attr qp_attr;

    memset(&qp_attr, 0, sizeof(qp_attr));

    qp_attr.qp_state = IBV_QPS_RTR;
    if (ibv_modify_qp(channel->qp, &qp_attr, IBV_QP_STATE)) {
        opal_show_help("help-mpi-btl-usnic.txt", "ibv API failed",
                       true, 
                       ompi_process_info.nodename,
                       ibv_get_device_name(module->device),
                       module->if_name,
                       "ibv_modify_qp", __FILE__, __LINE__,
                       "Failed to move QP to RTR state");
        return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
}


static int move_qp_to_rts(ompi_btl_usnic_module_t *module,
                          struct ompi_btl_usnic_channel_t *channel)
{
    struct ibv_qp_attr qp_attr;

    memset(&qp_attr, 0, sizeof(qp_attr));

    qp_attr.qp_state = IBV_QPS_RTS;
    if (ibv_modify_qp(channel->qp, &qp_attr, IBV_QP_STATE)) {
        opal_show_help("help-mpi-btl-usnic.txt", "ibv API failed",
                       true, 
                       ompi_process_info.nodename,
                       ibv_get_device_name(module->device),
                       module->if_name,
                       "ibv_modify_qp", __FILE__, __LINE__,
                       "Failed to move QP to RTS state");
        return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
}

/*
 * finalize channel - release all associated resources
 */
static void
ompi_btl_usnic_channel_finalize(
    ompi_btl_usnic_module_t *module,
    struct ompi_btl_usnic_channel_t *channel)
{
    if (NULL != channel->qp) {
        ibv_destroy_qp(channel->qp);
        channel->qp = NULL;
    }

    /* destroy CQ if created */
    if (NULL != channel->cq) {
        ibv_destroy_cq(channel->cq);
        channel->cq = NULL;
    }

    /* gets set right after constructor called, lets us know recv_segs
     * have been constructed.  Make sure to wait until queues destroyed to destroy
     * the recv_segs
     */
    if (channel->recv_segs.ctx == module) {
        assert(NULL == channel->qp && NULL == channel->cq);
        OBJ_DESTRUCT(&channel->recv_segs);
    }
}

/*
 * Initialize a channel
 */
static int
ompi_btl_usnic_channel_init(
    ompi_btl_usnic_module_t *module,
    struct ompi_btl_usnic_channel_t *channel,
    int index,
    int mtu,
    int rd_num,
    int sd_num)
{
    struct ibv_context *ctx;
    uint32_t segsize;
    ompi_btl_usnic_recv_segment_t *rseg;
    ompi_free_list_item_t* item;
    struct ibv_recv_wr* bad_wr;
    int i;
    int rc;

    ctx = module->device_context;
    channel->chan_mtu = mtu;
    channel->chan_rd_num = rd_num;
    channel->chan_sd_num = sd_num;
    channel->chan_index = index;
    channel->chan_deferred_recv = NULL;
    channel->chan_error = false;

    channel->sd_wqe = sd_num;
    channel->fastsend_wqe_thresh = sd_num - 10;

    /* We did math up in component_init() to know that there should be
       enough CQs available.  So if create_cq fails, then either the
       memlock limits are too low, or something other than this MPI
       job is consuming CQs. */
    channel->cq = ibv_create_cq(ctx, module->cq_num, NULL, NULL, 0);
    if (NULL == channel->cq) {
        opal_show_help("help-mpi-btl-usnic.txt", "create ibv resource failed",
                       true, 
                       ompi_process_info.nodename,
                       ibv_get_device_name(module->device),
                       module->if_name,
                       "ibv_create_cq()", __FILE__, __LINE__,
                       "Failed to create a usNIC completion queue");
        goto error;
    }

    /* Set up the QP for this channel */
    rc = init_qp(module, channel);
    if (OMPI_SUCCESS != rc) {
        goto error;
    }

    /*
     * Initialize pool of receive segments.  round MTU up to cache line size
     * so that each segment is guaranteed to start on a cache line boundary
     */
    segsize = (mtu + opal_cache_line_size - 1) & ~(opal_cache_line_size - 1);
    OBJ_CONSTRUCT(&channel->recv_segs, ompi_free_list_t);
    rc = ompi_free_list_init_new(&channel->recv_segs,
                                 sizeof(ompi_btl_usnic_recv_segment_t),
                                 opal_cache_line_size,
                                 OBJ_CLASS(ompi_btl_usnic_recv_segment_t),
                                 segsize,
                                 opal_cache_line_size,
                                 rd_num,
                                 rd_num,
                                 rd_num,
                                 module->super.btl_mpool);
    channel->recv_segs.ctx = module; /* must come after ompi_free_list_init_new,
                                        otherwise ctx gets clobbered */
    if (OMPI_SUCCESS != rc) {
        goto error;
    }

    /* Post receive descriptors */
    for (i = 0; i < rd_num; i++) {
        OMPI_FREE_LIST_GET_MT(&channel->recv_segs, item);
        assert(NULL != item);
        rseg = (ompi_btl_usnic_recv_segment_t*)item;

        if (NULL == rseg) {
            opal_show_help("help-mpi-btl-usnic.txt",
                           "internal error during init",
                           true, 
                           ompi_process_info.nodename,
                           ibv_get_device_name(module->device),
                           module->if_name,
                           "get freelist buffer()", __FILE__, __LINE__,
                           "Failed to get receive buffer from freelist");
            abort();    /* this is impossible */
        }

        /* cannot find length from constructor, set it now */
        rseg->rs_base.us_sg_entry[0].length = mtu;
        rseg->rs_recv_desc.next = NULL;

        if (ibv_post_recv(channel->qp, &rseg->rs_recv_desc, &bad_wr)) {
            opal_show_help("help-mpi-btl-usnic.txt", "ibv API failed",
                           true, 
                           ompi_process_info.nodename,
                           ibv_get_device_name(module->device),
                           module->if_name,
                           "ibv_post_recv", __FILE__, __LINE__,
                           "Failed to post receive buffer");
            goto error;
        }
    }

    if (OMPI_SUCCESS != move_qp_to_rtr(module, channel)) {
        goto error;
    }

    if (OMPI_SUCCESS != move_qp_to_rts(module, channel)) {
        goto error;
    }

    return OMPI_SUCCESS;

error:
    ompi_btl_usnic_channel_finalize(module, channel);
    return OMPI_ERROR;
}

/*
 * generate initial send sequence number
 */
static ompi_btl_usnic_seq_t
get_initial_seq_no(void)
{
    ompi_btl_usnic_seq_t isn;
    /* only utilize the bottom 62 bits to avoid hitting seq # overflow */
    isn = (((uint64_t)opal_rand(&ompi_btl_usnic_rand_buff) & ((1LL<<30)-1)) << 32) |
        ((uint64_t)opal_rand(&ompi_btl_usnic_rand_buff) & ((1LL<<32)-1));
    isn += 2;      /* guarantee > 1 */

    return isn;
}

/*
 * Initialize the btl module by allocating a protection domain,
 *  memory pool, priority and data channels, and free lists
 */
int ompi_btl_usnic_module_init(ompi_btl_usnic_module_t *module)
{
    int rc;
    int i;
    struct mca_mpool_base_resources_t mpool_resources;
    struct ibv_context *ctx = module->device_context;
    uint32_t ack_segment_len;
    uint32_t segsize;

#if OPAL_HAVE_HWLOC
    /* If this process is bound to a single NUMA locality, calculate
       its NUMA distance from this usNIC device */
    if (mca_btl_usnic_component.want_numa_device_assignment) {
        ompi_btl_usnic_hwloc_distance(module);
    } else {
        opal_output_verbose(5, USNIC_OUT,
                            "btl:usnic: not sorting devices by NUMA distance (MCA btl_usnic_want_numa_device_assignment)");
    }
#else
    opal_output_verbose(5, USNIC_OUT,
                        "btl:usnic: not sorting devices by NUMA distance (topology support not included)");
#endif

    /* Setup the pointer array for the procs that will be used by this
       module */
    OBJ_CONSTRUCT(&module->all_procs, opal_pointer_array_t);
    opal_pointer_array_init(&module->all_procs, ompi_process_info.num_procs,
                            INT_MAX, 32);

    /* Get a PD */
    module->pd = ibv_alloc_pd(ctx);
    if (NULL == module->pd) {
        opal_show_help("help-mpi-btl-usnic.txt", "ibv API failed",
                       true, 
                       ompi_process_info.nodename,
                       ibv_get_device_name(module->device), 
                       module->if_name,
                       "ibv_alloc_pd()", __FILE__, __LINE__,
                       "Failed to create a PD; is the usnic_verbs Linux kernel module loaded?");
        return OMPI_ERROR;
    }

    /* Setup the mpool */
    mpool_resources.reg_data = (void*)module;
    mpool_resources.sizeof_reg = sizeof(ompi_btl_usnic_reg_t);
    mpool_resources.register_mem = usnic_reg_mr;
    mpool_resources.deregister_mem = usnic_dereg_mr;
    asprintf(&mpool_resources.pool_name, "usnic.%" PRIu64, 
             module->local_addr.gid.global.interface_id);
    module->super.btl_mpool =
        mca_mpool_base_module_create(mca_btl_usnic_component.usnic_mpool_name,
                                     &module->super, &mpool_resources);
    if (NULL == module->super.btl_mpool) {
        opal_show_help("help-mpi-btl-usnic.txt", "ibv API failed",
                       true, 
                       ompi_process_info.nodename,
                       ibv_get_device_name(module->device),
                       module->if_name,
                       "create mpool", __FILE__, __LINE__,
                       "Failed to allocate registered memory; check Linux memlock limits");
        goto dealloc_pd;
    }

    /* initialize data and priority channels */
    rc = ompi_btl_usnic_channel_init(module,
            &module->mod_channels[USNIC_PRIORITY_CHANNEL],
            USNIC_PRIORITY_CHANNEL,
            module->tiny_mtu, module->prio_rd_num, module->prio_sd_num);
    if (rc != OMPI_SUCCESS) {
        goto chan_destroy;
    }
    rc = ompi_btl_usnic_channel_init(module,
            &module->mod_channels[USNIC_DATA_CHANNEL],
            USNIC_DATA_CHANNEL,
            module->if_mtu, module->rd_num, module->sd_num);
    if (rc != OMPI_SUCCESS) {
        goto chan_destroy;
    }

    module->local_addr.isn = get_initial_seq_no();

    /* Place QP number in our local address information */
    module->local_addr.qp_num[USNIC_PRIORITY_CHANNEL] =
        module->mod_channels[USNIC_PRIORITY_CHANNEL].qp->qp_num;
    module->local_addr.qp_num[USNIC_DATA_CHANNEL] =
        module->mod_channels[USNIC_DATA_CHANNEL].qp->qp_num;

    /* Get the fd to receive events on this device */
    opal_event_set(opal_event_base, &(module->device_async_event),
                   module->device_context->async_fd, 
                   OPAL_EV_READ | OPAL_EV_PERSIST, 
                   module_async_event_callback, module);
    opal_event_add(&(module->device_async_event), NULL);
    module->device_async_event_active = true;

    /* No more errors anticipated - initialize everything else */

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
    
    segsize = (module->if_mtu + opal_cache_line_size - 1) &
        ~(opal_cache_line_size - 1);

    /* Send frags freelists */
    OBJ_CONSTRUCT(&module->small_send_frags, ompi_free_list_t);
    rc = ompi_free_list_init_new(&module->small_send_frags,
                                 sizeof(ompi_btl_usnic_small_send_frag_t),
                                 opal_cache_line_size,
                                 OBJ_CLASS(ompi_btl_usnic_small_send_frag_t),
                                 segsize,
                                 opal_cache_line_size,
                                 module->sd_num * 4,
                                 -1,
                                 module->sd_num / 2,
                                 module->super.btl_mpool);
    assert(OMPI_SUCCESS == rc);

    OBJ_CONSTRUCT(&module->large_send_frags, ompi_free_list_t);
    rc = ompi_free_list_init_new(&module->large_send_frags,
                                 sizeof(ompi_btl_usnic_large_send_frag_t),
                                 opal_cache_line_size,
                                 OBJ_CLASS(ompi_btl_usnic_large_send_frag_t),
                                 0,  /* payload size */
                                 0,  /* payload align */
                                 module->sd_num / 8,
                                 -1,
                                 module->sd_num / 8,
                                 NULL);
    assert(OMPI_SUCCESS == rc);

    OBJ_CONSTRUCT(&module->put_dest_frags, ompi_free_list_t);
    rc = ompi_free_list_init_new(&module->put_dest_frags,
                                 sizeof(ompi_btl_usnic_put_dest_frag_t),
                                 opal_cache_line_size,
                                 OBJ_CLASS(ompi_btl_usnic_put_dest_frag_t),
                                 0,  /* payload size */
                                 0,  /* payload align */
                                 module->sd_num / 8,
                                 -1,
                                 module->sd_num / 8,
                                 NULL);
    assert(OMPI_SUCCESS == rc);

    /* list of segments to use for sending */
    OBJ_CONSTRUCT(&module->chunk_segs, ompi_free_list_t);
    rc = ompi_free_list_init_new(&module->chunk_segs,
                                 sizeof(ompi_btl_usnic_chunk_segment_t),
                                 opal_cache_line_size,
                                 OBJ_CLASS(ompi_btl_usnic_chunk_segment_t),
                                 segsize,
                                 opal_cache_line_size,
                                 module->sd_num * 4,
                                 -1,
                                 module->sd_num / 2,
                                 module->super.btl_mpool);
    assert(OMPI_SUCCESS == rc);

    /* ACK segments freelist */
    ack_segment_len = (sizeof(ompi_btl_usnic_btl_header_t) +
            opal_cache_line_size - 1) & ~(opal_cache_line_size - 1);
    OBJ_CONSTRUCT(&module->ack_segs, ompi_free_list_t);
    rc = ompi_free_list_init_new(&module->ack_segs,
                                 sizeof(ompi_btl_usnic_ack_segment_t),
                                 opal_cache_line_size,
                                 OBJ_CLASS(ompi_btl_usnic_ack_segment_t),
                                 ack_segment_len,
                                 opal_cache_line_size,
                                 module->sd_num * 4,
                                 -1,
                                 module->sd_num / 2,
                                 module->super.btl_mpool);
    assert(OMPI_SUCCESS == rc);

    /*
     * Initialize pools of large recv buffers
     *
     * NOTE: (last_pool < first_pool) is _not_ erroneous; recv buffer pools
     * simply won't be used in that case.
     */
    module->first_pool = 16; /* 64 kiB */
    module->last_pool = usnic_fls(module->super.btl_eager_limit-1);
    module->module_recv_buffers = calloc(module->last_pool+1,
            sizeof(ompi_free_list_t));
    assert(module->module_recv_buffers != NULL);
    for (i=module->first_pool; i<=module->last_pool; ++i) {
        size_t elt_size = sizeof(ompi_btl_usnic_rx_buf_t) - 1 + (1 << i);
        OBJ_CONSTRUCT(&module->module_recv_buffers[i], ompi_free_list_t);
        rc = ompi_free_list_init_new(&module->module_recv_buffers[i],
                                     elt_size,
                                     opal_cache_line_size,
                                     OBJ_CLASS(ompi_btl_usnic_rx_buf_t),
                                     0,   /* payload size */
                                     0,   /* payload align */
                                     128,   /* init elts to alloc */
                                     128, /* max elts to alloc */
                                     128,   /* num elts per alloc */
                                     NULL /* mpool */);
        assert(OMPI_SUCCESS == rc);
    }

    /* Initialize stats on this module */
    ompi_btl_usnic_stats_init(module);

    /* Setup a connectivity listener */
    if (mca_btl_usnic_component.connectivity_enabled) {
        rc = ompi_btl_usnic_connectivity_listen(module);
        if (OMPI_SUCCESS != rc) {
            OMPI_ERROR_LOG(rc);
            ABORT("Failed to notify connectivity agent to listen");
        }
    } else {
        /* If we're not doing a connectivity check, just set the port
           to 0 */
        module->local_addr.connectivity_udp_port = 0;
    }

    return OMPI_SUCCESS;

chan_destroy:
    for (i=0; i<USNIC_NUM_CHANNELS; ++i) {
        ompi_btl_usnic_channel_finalize(module, &module->mod_channels[i]);
    }

    mca_mpool_base_module_destroy(module->super.btl_mpool);

dealloc_pd:
    rc = ibv_dealloc_pd(module->pd);
    if (rc) {
        BTL_ERROR(("failed to ibv_dealloc_pd, err=%d (%s)", rc, strerror(rc)));
    }
    return OMPI_ERROR;
}


static int usnic_ft_event(int state) 
{
    return OMPI_SUCCESS;
}


ompi_btl_usnic_module_t ompi_btl_usnic_module_template = {
    {
        &mca_btl_usnic_component.super,
        0, /* eager_limit */
        0, /* min_send_size */
        0, /* max_send_size */
        0, /* rdma_pipeline_send_length */
        0, /* rdma_pipeline_frag_size */
        0, /* min_rdma_pipeline_size */
        MCA_BTL_EXCLUSIVITY_DEFAULT,
        0, /* latency */
        0, /* bandwidth */
        MCA_BTL_FLAGS_SEND | 
            MCA_BTL_FLAGS_PUT |
            MCA_BTL_FLAGS_SEND_INPLACE,
#ifndef OMPI_BTL_USNIC_CISCO_V1_6
        sizeof(mca_btl_base_segment_t), /* seg size */
#endif
        usnic_add_procs,
        usnic_del_procs,
        NULL, /*ompi_btl_usnic_register*/
        usnic_finalize,
        usnic_alloc,
        usnic_free,
        usnic_prepare_src,
        usnic_prepare_dst,
        usnic_send,
        NULL, /* ompi_btl_usnic_sendi */
        usnic_put,
        NULL, /*ompi_btl_usnic_get */
        mca_btl_base_dump,
        NULL, /* mpool -- to be filled in later */
        usnic_register_pml_err_cb,
        usnic_ft_event
    }
};
