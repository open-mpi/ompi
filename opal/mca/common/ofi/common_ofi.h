/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2015      Intel, Inc. All rights reserved.
 * Copyright (c) 2017      Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2020-2024 Triad National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2021      Amazon.com, Inc. or its affiliates. All rights
 *                         reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OPAL_MCA_COMMON_OFI_H
#define OPAL_MCA_COMMON_OFI_H

#include "opal/util/proc.h"
#include "opal/memoryhooks/memory.h"

#include <rdma/fabric.h>
#include <rdma/fi_cm.h>

BEGIN_C_DECLS

typedef struct opal_common_ofi_module {
    char **prov_include;
    char **prov_exclude;
    int output;
} opal_common_ofi_module_t;

/**
 * When attempting to execute an OFI operation we need to handle
 * resource overrun cases. When a call to an OFI OP fails with -FI_EAGAIN
 * the OFI mtl/btl will attempt to progress any pending Completion Queue
 * events that may prevent additional operations to be enqueued.
 * If the call to ofi progress is successful, then the function call
 * will be retried.
 */
#define OFI_RETRY_UNTIL_DONE(FUNC, RETURN)             \
    do {                                               \
        do {                                           \
            RETURN = FUNC;                             \
            if (OPAL_LIKELY(0 == RETURN)) {break;}     \
            if (OPAL_LIKELY(RETURN == -FI_EAGAIN)) {   \
                opal_progress();                       \
            }                                          \
        } while (OPAL_LIKELY(-FI_EAGAIN == RETURN));   \
    } while (0);


extern opal_common_ofi_module_t opal_common_ofi;

/**
 * Common MCA registration
 *
 * Common MCA registration handlinge.  After calling this function,
 * \code opal_common_ofi.output will be properly initialized.
 *
 * @param component (IN) OFI component being initialized
 *
 * @returns OPAL_SUCCESS on success, OPAL error code on failure
 */
OPAL_DECLSPEC int opal_common_ofi_mca_register(const mca_base_component_t *component);

/**
 * Initializes common objects for libfabric
 *
 * Initialize common libfabric interface.  This should be called from
 * any other OFI component's component_open() call.
 *
 * @note This function is not thread safe and must be called in a
 * serial portion of the code.
 */
OPAL_DECLSPEC int opal_common_ofi_open(void);

/**
 * Cleans up common objects for libfabric
 *
 * Clean up common libfabric interface.  This should be called from
 * any other OFI component's component_close() call.  Resource cleanup
 * is reference counted, so any successful call to
 * opal_common_ofi_init().
 *
 * @note This function is not thread safe and must be called in a
 * serial portion of the code.
 */
OPAL_DECLSPEC int opal_common_ofi_close(void);

/**
 * Export our memory hooks into Libfabric monitor
 *
 * Use Open MPI's memory hooks to provide monitor notifications to
 * Libfabric via the external mr_cache facility.  This must be called
 * before any domain is initialized (ie, before any Libfabric memory
 * monitor is configured).
 *
 * @returns A libfabric error code is returned on error
 */
OPAL_DECLSPEC int opal_common_ofi_export_memory_monitor(void);

/**
 * Search function for provider names
 *
 * This function will take a provider name string and a list of lower
 * provider name strings as inputs. It will return true if the lower
 * provider in the item string matches a lower provider in the list.
 *
 * @param list (IN)    List of strings corresponding to lower providers.
 * @param item (IN)    Single string corresponding to a provider.
 *
 * @return 0           The lower provider of the item string is not in
 *                     list or an input was NULL
 * @return 1           The lower provider of the item string matches
 *                     a string in the item list.
 *
 */
OPAL_DECLSPEC int opal_common_ofi_is_in_list(char **list, char *item);

/**
 * Get the number of providers whose names are included in a list
 *
 * This function takes a list of providers and a list of name strings
 * as inputs, and return the number of providers whose names are included
 * in the name strings.
 *
 * @param provider_list (IN)    List of providers
 * @param list          (IN)    List of name string
 *
 * @return                      Number of matched providers
 *
 */
OPAL_DECLSPEC int opal_common_ofi_count_providers_in_list(struct fi_info *provider_list,
                                                          char **list);

/**
 * Determine whether all providers are included in a list
 *
 * This function takes a list of providers and a list of name strings
 * as inputs, and return whether all provider names are included in the name strings.
 *
 * @param provider_list (IN)    List of providers
 * @param list          (IN)    List of name string
 *
 * @return  0                   At least one provider's name is not included in the name strings.
 * @return  1                   All provider names are included in the name strings.
 *
 */
OPAL_DECLSPEC int opal_common_ofi_providers_subset_of_list(struct fi_info *provider_list,
                                                           char **list);

/**
 * Selects NIC (provider) based on hardware locality
 *
 * The selection is based on the following priority:
 *
 * Single-NIC:
 * 
 *      If only 1 provider is available, always return that provider.
 * 
 * Multi-NIC:
 * 
 *      1. If the process is NOT bound, pick a NIC using (local rank % number
 *      of providers of the same type). This gives a fair chance to each
 *      qualified NIC and balances overall utilization.
 *
 *      2. If the process is bound, we compare providers in the list that have
 *      the same type as the first provider, and find the provider with the
 *      shortest distance to the current process. 
 * 
 *          i. If the provider has PCI BDF data, we attempt to compute the
 *          distance between the NIC and the current process cpuset. The NIC
 *          with the shortest distance is returned.
 * 
 *              * For equidistant NICs, we select a NIC in round-robin fashion
 *              using the package rank of the current process, i.e. (package
 *              rank % number of providers with the same distance).
 *
 *          ii. If we cannot compute the distance between the NIC and the
 *          current process, e.g. PCI BDF data is not available, a NIC will be
 *          selected in a round-robin fashion using package rank, i.e. (package
 *          rank % number of providers of the same type).
 *
 * @param[in]   provider_list   struct fi_info* An initially selected
 *                              provider NIC. The provider name and
 *                              attributes are used to restrict NIC
 *                              selection. This provider is returned if the
 *                              NIC selection fails.
 * 
 * @param[in]   process_info    opal_process_info_t* The current process info
 *
 * @param[out]  provider        struct fi_info* object with the selected
 *                              provider if the selection succeeds
 *                              if the selection fails, returns the fi_info
 *                              object that was initially provided.
 *
 * All errors should be recoverable and will return the initially provided
 * provider. However, if an error occurs we can no longer guarantee
 * that the provider returned is local to the process or that the processes will
 * balance across available NICs.
 *
 */
OPAL_DECLSPEC struct fi_info *opal_common_ofi_select_provider(struct fi_info *provider_list,
                                                              opal_process_info_t *process_info);

/**
 * Obtain EP endpoint name
 *
 * Obtain the EP endpoint name and length for the supplied endpoint fid.
 *
 * @param fid (IN)     fid of (S)EP endpoint
 * @param addr (OUT)   buffer containing endpoint name 
 * @param addrlen (OUT) length of allocated buffer in bytes
 *
 * @return             OPAL_SUCCESS or OPAL error code
 *
 * The caller is responsible for freeing the buffer allocated to
 * contain the endpoint name.
 *
 */
OPAL_DECLSPEC int opal_common_ofi_fi_getname(fid_t fid, void **addr, size_t *addrlen);

END_C_DECLS

#endif /* OPAL_MCA_COMMON_OFI_H */
