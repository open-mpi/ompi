/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2021      Google, LLC. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/**
 * @file
 *
 * Shared Memory Single Copy
 *
 * This framework provides support for copying memory from one process to another on the same host
 * system. The components expose process read, write, and may provide a way to map peer memory into
 * this processes memory space.
 */

#ifndef OPAL_MCA_SMSC_H
#define OPAL_MCA_SMSC_H

#include "opal_config.h"
#include "opal/class/opal_object.h"
#include "opal/util/proc.h"

#define MCA_SMSC_BASE_MAJOR_VERSION 1
#define MCA_SMSC_BASE_MINOR_VERSION 0
#define MCA_SMSC_BASE_PATCH_VERSION 0

struct mca_smsc_module_t;

struct mca_smsc_endpoint_t {
    opal_object_t super;
    /** Opal proc object for this peer. */
    opal_proc_t *proc;
};

typedef struct mca_smsc_endpoint_t mca_smsc_endpoint_t;

OBJ_CLASS_DECLARATION(mca_smsc_endpoint_t);

/**
 * @brief Get an endpoint for a peer proc.
 *
 * @param(in) module    shared-memory single-copy module
 * @param(in) peer_proc proc to get an endpoint for
 */
typedef mca_smsc_endpoint_t *(*mca_smsc_module_get_endpoint_fn_t)(opal_proc_t *peer_proc);

/**
 * @brief Return a shared-memory single-copy endpoint.
 *
 * @param(in) module   shared-memory single-copy module
 * @param(in) endpoint shared-memory single-copy endpoint
 *
 * This method returns an endpoint created by get_endpoint. The endpoint should be considered
 * invalid and may be freed after this call completes.
 */
typedef void (*mca_smsc_module_return_endpoint_fn_t)(mca_smsc_endpoint_t *endpoint);

/**
 * @brief Copy to/from a peer process.
 *
 * @param(in) module         shared-memory single-copy module
 * @param(in) endpoint       shared-memory single-copy endpoint
 * @param(in) local_address  local address to use
 * @param(in) remote_address remote address to use
 * @param(in) size           amount to copy
 * @param(in) reg_data       pointer to memory containing registration data (if required)
 *
 * A module must provide both copy_from and copy_to function.
 */
typedef int (*mca_smsc_module_copy_fn_t)(mca_smsc_endpoint_t *endpoint, void *local_address,
                                         void *remote_address, size_t size, void *reg_data);

/**
 * @brief Map a peer's memory onto local memory.
 *
 * @param(in)  module         shared-memory single-copy module
 * @param(in)  endpoint       shared-memory single-copy endpoint
 * @param(in)  flags          flags for this map operation (set to 0)
 * @param(in)  remote_address pointer valid in peer's address space
 * @param(in)  size           size of region to map
 * @param(out) local_mapping local address for peer region
 *
 * @returns a reference to the mapping
 *
 * This method, if provided, provides support for mapping a local peer's memory into this address
 * space. The caller is responsible for verifying that the address is valid or access to the region
 * may result in an access violation (SEGV). The function returns a reference (if needed) that can
 * be used to clear the mapping. It is the caller's responsibility to unmap the region using the
 * returned context.
 */
typedef void *(*mca_smsc_module_map_peer_region_fn_t)(mca_smsc_endpoint_t *endpoint, uint64_t flags,
                                                      void *remote_address, size_t size,
                                                      void **local_mapping);

/**
 * @brief Clear a memory mapping.
 *
 * @param(in)  module         shared-memory single-copy module
 * @param(in) ctx   memory mapping context
 */
typedef void (*mca_smsc_module_unmap_peer_region_fn_t)(void *ctx);

/**
 * @brief Register a memory region for remote access.
 *
 * @param(in) module          shared-memory single-copy module
 * @param(in) local_address   local address to register (ideally page-aligned)
 * @param(in) size            size of the memory region (ideally page-aligned)
 *
 * @returns a pointer to registration data that can be used for copy by a peer process
 *
 * This method registers a region for access by a local peer. The returned data can be passed to a
 * local peer and used by that peer for either copy_to or copy_from.
 */
typedef void *(*mca_smsc_module_register_region_fn_t)(void *local_address, size_t size);

/**
 * @brief Deregister a registered region.
 *
 * @param(in) module     shared-memory single-copy module
 * @param(in) reg_data   registration data returned by the registration function
 *
 * This function deregisters a region from use by a peers copy_from and copy_to function. Once a
 * region has been deregistered the data is immediately not usable by any local peer.
 */
typedef void (*mca_smsc_module_deregister_region_fn_t)(void *reg_data);

enum {
    /** Module requires the local registration of any region that will be used for single-copy
     * operations. It is theresponsibility of the caller to pass this data with the pointer to the
     * peer. */
    MCA_SMSC_FEATURE_REQUIRE_REGISTRATION = 1,
    /** Module can map peer memory into the local processes' address space. */
    MCA_SMSC_FEATURE_CAN_MAP = 2,
};

struct mca_smsc_module_t {
    /** Module features. */
    uint64_t features;

    /** Ignore if MCA_SMSC_FEATURE_REQUIRES_REGISTRATION is not set. */
    size_t registration_data_size;

    /** Get an endpoint for a peer. This function should always return a newly-allocated endpoint.
     * The base will be responsible for caching that endpoint. */
    mca_smsc_module_get_endpoint_fn_t get_endpoint;
    /** Delete an endpoint and clean up all resources associated with it. */
    mca_smsc_module_return_endpoint_fn_t return_endpoint;

    /* All components must provide an implementation of the copy functions. */
    /** Copy data into a peer's memory space. */
    mca_smsc_module_copy_fn_t copy_to;
    /** Copy data from a peer's memory space. */
    mca_smsc_module_copy_fn_t copy_from;

    /* Defined if MCA_SMSC_FEATURE_CAN_MAP is set. */
    /** Map a peer memory region into this processes address space. The module is allowed to cache
     * the mapping and return it in subsequent calls. */
    mca_smsc_module_map_peer_region_fn_t map_peer_region;
    /** Delete a mapping. This is allowed to leave the mapping in place. */
    mca_smsc_module_unmap_peer_region_fn_t unmap_peer_region;

    /* Defined if MCA_SMSC_FEATURE_REQUIRES_REGISTRATION is set. */
    /** Register a memory region for use with single-copy by a remote peer. The module may cache
     * this registration for future use. */
    mca_smsc_module_register_region_fn_t register_region;
    /** Deregister a memory region for use with single-copy. */
    mca_smsc_module_deregister_region_fn_t deregister_region;
};

typedef struct mca_smsc_module_t mca_smsc_module_t;

/**
 * @brief Query if this component can run.
 *
 * @returns OPAL_SUCCESS if the component can run or an opal error code otherwise
 *
 * This function is responsible for verifying the component can run. It should do the minimum amount
 * of work to run at any time during execution. This includes sending any modex message if needed.
 * It should refrain from allocating resources if possible.
 */
typedef int (*mca_smsc_component_query_fn_t)(void);

/**
 * @brief Enable the use of this component and return a module.
 *
 * @returns A module on success or NULL otherwise.
 *
 * This function should do any remaining work (not already done in query) to prepare the component
 * for use. It should return a fully initialized module.
 */
typedef mca_smsc_module_t *(*mca_smsc_component_enable_fn_t)(void);

struct mca_smsc_component_1_0_0_t {
    mca_base_component_t smsc_version;
    mca_base_component_data_t smsc_data;

    /** Priority of this component. Only the winning component will be used. */
    int priority;

    /** Check if this component can be used. */
    mca_smsc_component_query_fn_t query;
    /** Enable the use of this component. */
    mca_smsc_component_enable_fn_t enable;
};

typedef struct mca_smsc_component_1_0_0_t mca_smsc_component_1_0_0_t;
typedef mca_smsc_component_1_0_0_t mca_smsc_component_t;

OPAL_DECLSPEC extern mca_smsc_module_t *mca_smsc;

#if MCA_opal_smsc_DIRECT_CALL
#    include MCA_opal_smsc_DIRECT_CALL_HEADER

#    define MCA_SMSC_CALL_STAMP(a, b, ...)    mca_smsc_##a##_##b(__VA_ARGS__)
#    define MCA_SMSC_CALL_EXPANDER(a, b, ...) MCA_SMSC_CALL_STAMP(a, b, __VA_ARGS__)
#    define MCA_SMSC_CALL(a, ...) \
        MCA_SMSC_CALL_EXPANDER(MCA_opal_smsc_DIRECT_CALL_COMPONENT, a, __VA_ARGS__)

#else

#    define MCA_SMSC_CALL(a, ...) mca_smsc->a(__VA_ARGS__)

#endif /* MCA_opal_smsc_DIRECT_CALL */

/**
 * @brief Check if the selected component has a feature.
 *
 * @param(in) feature  feature to check for (see smsc.h for list of features)
 */
static inline bool mca_smsc_base_has_feature(uint64_t feature)
{
    return (NULL != mca_smsc) && !!(mca_smsc->features & feature);
}

static inline ssize_t mca_smsc_base_registration_data_size(void)
{
    if (NULL == mca_smsc || !mca_smsc_base_has_feature(MCA_SMSC_FEATURE_REQUIRE_REGISTRATION)) {
        return OPAL_ERR_NOT_AVAILABLE;
    }

    return mca_smsc->registration_data_size;
}

#define MCA_SMSC_BASE_VERSION_1_0_0                                                               \
    OPAL_MCA_BASE_VERSION_2_1_0("smsc", MCA_SMSC_BASE_MAJOR_VERSION, MCA_SMSC_BASE_MINOR_VERSION, \
                                MCA_SMSC_BASE_PATCH_VERSION)

#define MCA_SMSC_DEFAULT_VERSION(name)                                                \
    MCA_SMSC_BASE_VERSION_1_0_0, .mca_component_name = name,                          \
                                 MCA_BASE_MAKE_VERSION(component, OPAL_MAJOR_VERSION, \
                                                       OPAL_MINOR_VERSION, OPAL_RELEASE_VERSION)

#endif /* OPAL_MCA_SMSC_H */
