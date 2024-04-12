/*
 * Copyright (c) 2007      Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2017-2019 Intel, Inc.  All rights reserved.
 * Copyright (c) 2020      Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/**
 * @file
 *
 * Interface for manipulating how the RML receives contact information
 *
 * Interface for manipulating how the RML receives contact
 * information.  These functions are generally used during prte_init
 * and prte_finalize.
 */

#include "prte_config.h"
#include "types.h"

#include "src/pmix/pmix-internal.h"

BEGIN_C_DECLS

/**
 * Parse a contact information string
 *
 * Parse a contact infromation string, such as that returned by
 * prte_rml.get_contact_info().  Generally used to extract the peer
 * name from a contact information string.  It can also be used to
 * extract the contact URI strings, although this is slightly less
 * useful as the URIs may be RML componenent specific and not have
 * general meaning.
 *
 * @param[in] contact_info  Contact information string for peer
 * @param[out] peer         Peer name in contact_info
 * @param[out] uris         URI strings for peer.  May be NULL if
 *                          information is not needed
 *
 * @retval PRTE_SUCCESS     Information successfully extraced
 * @retval PRTE_ERR_BAD_PARAM The contact_info was not a valid string
 * @retval PRTE_ERROR       An unspecified error occurred
 */
PRTE_EXPORT int prte_rml_parse_uris(const char *contact_inf, pmix_proc_t *peer, char ***uris);

END_C_DECLS
