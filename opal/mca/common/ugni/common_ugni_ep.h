/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2011-2013 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2011      UT-Battelle, LLC. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#if !defined(MPI_COMMON_UGNI_EP_H)
#define MPI_COMMON_UGNI_EP_H

struct opal_common_ugni_device_t;

struct opal_common_ugni_endpoint_t {
    opal_object_t super;
    uint32_t ep_rem_addr, ep_rem_id;         /**< remote information */
    gni_mem_handle_t ep_rem_irq_memhndl;
    struct opal_common_ugni_device_t *dev;   /**< device this endpoint is using */
};
typedef struct opal_common_ugni_endpoint_t opal_common_ugni_endpoint_t;

OBJ_CLASS_DECLARATION(opal_common_ugni_endpoint_t);

/*
 * Get (and retain) a reference to an endpoint to peer_proc. This endpoint
 * needs to be returned with opal_common_ugni_endpoint_return.
 *
 * @param[IN]  dev         uGNI device this endpoint should be bound to.
 * @param[IN]  peer_proc   remote peer the endpoint will be connected to.
 * @param[OUT] ep          uGNI endpoint for the peer
 */
int opal_common_ugni_endpoint_for_proc (struct opal_common_ugni_device_t *dev, opal_proc_t *peer_proc,
                                        opal_common_ugni_endpoint_t **ep);

/*
 * Allocate and bind a uGNI endpoint handle to the remote peer.
 *
 * @param[IN]  cep                common endpoint
 * @param[IN]  cq                 completion queue
 * @param[OUT] ep_handle          uGNI endpoint handle
 */
int opal_common_ugni_ep_create (opal_common_ugni_endpoint_t *cep, gni_cq_handle_t cq, gni_ep_handle_t *ep_handle);

/*
 * Unbind and free the uGNI endpoint handle.
 *
 * @param[IN]  ep_handle    uGNI endpoint handle to unbind and release
 */
int opal_common_ugni_ep_destroy (gni_ep_handle_t *ep_handle);

/*
 * Return (and possibly free) a common endpoint. The endpoint may not be used
 * once it is returned.
 *
 * @param[IN]  ep          uGNI endpoint to return
 */
void opal_common_ugni_endpoint_return (opal_common_ugni_endpoint_t *ep);

#endif /* MPI_COMMON_UGNI_EP_H */
