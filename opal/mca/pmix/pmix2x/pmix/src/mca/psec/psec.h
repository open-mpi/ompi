/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2007-2008 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2015-2016 Intel, Inc.  All rights reserved.
 *
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/**
 * @file
 *
 * This interface is for psecurity support. PMIx doesn't need much in
 * this regard, but we do need a mechanism for authenticating connections.
 *
 * Only *one* plugin will be active in a client, but multiple plugins may
 * be active in a server. Thus, this is a multi-select framework.
 *
 * Available plugins may be defined at runtime via the typical MCA parameter
 * syntax.
 */

#ifndef PMIX_PSEC_H
#define PMIX_PSEC_H

#include <src/include/pmix_config.h>

#include "src/mca/mca.h"
#include "src/mca/base/pmix_mca_base_var.h"
#include "src/mca/base/pmix_mca_base_framework.h"
#include "src/mca/ptl/ptl_types.h"

BEGIN_C_DECLS

/*** forward declaration ***/
struct pmix_peer_t;

/******    MODULE DEFINITION    ******/

/**
 * Initialize the module. Returns an error if the module cannot
 * run, success if it can and wants to be used.
 */
typedef pmix_status_t (*pmix_psec_base_module_init_fn_t)(void);

/**
 * Finalize the module. Tear down any allocated storage, disconnect
 * from any system support (e.g., LDAP server)
 */
typedef void (*pmix_psec_base_module_fini_fn_t)(void);

/****    CLIENT-SIDE FUNCTIONS    ****/
/**
 * Create and return a credential for this client - this
 * could be a string or a byte array, which is why we must
 * also return the length
 */
typedef pmix_status_t (*pmix_psec_base_module_create_cred_fn_t)(pmix_listener_protocol_t protocol,
                                                                char **cred, size_t *len);

/**
 * Perform the client-side handshake. Note that it is not required
 * (and indeed, would be rare) for a protocol to use both the
 * credential and handshake interfaces. It is acceptable, therefore,
 * for one of them to be NULL */
typedef pmix_status_t (*pmix_psec_base_module_client_hndshk_fn_t)(int sd);


/****    SERVER-SIDE FUNCTIONS    ****/
/**
 * Validate a client's credential - the credential could be a string
 * or an array of bytes, which is why we include the length
 */
typedef pmix_status_t (*pmix_psec_base_module_validate_cred_fn_t)(struct pmix_peer_t *peer,
                                                                  pmix_listener_protocol_t protocol,
                                                                  char *cred, size_t len);

/**
 * Perform the server-side handshake. Note that it is not required
 * (and indeed, would be rare) for a protocol to use both the
 * credential and handshake interfaces. It is acceptable, therefore,
 * for one of them to be NULL */
typedef pmix_status_t (*pmix_psec_base_module_server_hndshk_fn_t)(struct pmix_peer_t *peer);

/**
 * Base structure for a PSEC module
 */
typedef struct {
    char *name;
    /* init/finalize */
    pmix_psec_base_module_init_fn_t           init;
    pmix_psec_base_module_fini_fn_t           finalize;
    /** Client-side */
    pmix_psec_base_module_create_cred_fn_t    create_cred;
    pmix_psec_base_module_client_hndshk_fn_t  client_handshake;
    /** Server-side */
    pmix_psec_base_module_validate_cred_fn_t  validate_cred;
    pmix_psec_base_module_server_hndshk_fn_t  server_handshake;
} pmix_psec_module_t;


/* define an API module */

/* get a list of available options - caller must free results
 * when done */
typedef char* (*pmix_psec_API_get_available_modules_fn_t)(void);

/* Select a psec module for a given peer */
typedef pmix_status_t (*pmix_psec_API_assign_module_fn_t)(struct pmix_peer_t *peer,
                                                          const char *options);

/**
 * Create and return a string representation of a credential for this
 * client
 */
typedef pmix_status_t (*pmix_psec_API_create_cred_fn_t)(struct pmix_peer_t *peer,
                                                        pmix_listener_protocol_t protocol,
                                                        char **cred, size_t *len);

/**
 * Perform the client-side handshake. Note that it is not required
 * (and indeed, would be rare) for a protocol to use both the
 * credential and handshake interfaces. It is acceptable, therefore,
 * for one of them to be NULL */
typedef pmix_status_t (*pmix_psec_API_client_hndshk_fn_t)(struct pmix_peer_t *peer, int sd);


/****    SERVER-SIDE FUNCTIONS    ****/
/**
 * Validate a client's connection request
 */
typedef pmix_status_t (*pmix_psec_API_validate_connection_fn_t)(struct pmix_peer_t *peer,
                                                                pmix_listener_protocol_t protocol,
                                                                char *cred, size_t len);

typedef struct {
    pmix_psec_API_get_available_modules_fn_t    get_available_modules;
    pmix_psec_API_assign_module_fn_t            assign_module;
    pmix_psec_API_create_cred_fn_t              create_cred;
    pmix_psec_API_client_hndshk_fn_t            client_handshake;
    pmix_psec_API_validate_connection_fn_t      validate_connection;
} pmix_psec_API_t;

PMIX_EXPORT extern pmix_psec_API_t pmix_psec;

/****    COMPONENT STRUCTURE DEFINITION    ****/

/* define a component-level API for initializing the component */
typedef pmix_status_t (*pmix_psec_base_component_init_fn_t)(void);

/* define a component-level API for finalizing the component */
typedef void (*pmix_psec_base_component_finalize_fn_t)(void);

/* define a component-level API for getting a module */
typedef pmix_psec_module_t* (*pmix_psec_base_component_assign_module_fn_t)(void);

/*
 * the standard component data structure
 */
struct pmix_psec_base_component_t {
    pmix_mca_base_component_t                        base;
    pmix_mca_base_component_data_t                   data;
    int                                              priority;
    pmix_psec_base_component_init_fn_t               init;
    pmix_psec_base_component_finalize_fn_t           finalize;
    pmix_psec_base_component_assign_module_fn_t      assign_module;
};
typedef struct pmix_psec_base_component_t pmix_psec_base_component_t;

/*
 * Macro for use in components that are of type psec
 */
#define PMIX_PSEC_BASE_VERSION_1_0_0 \
    PMIX_MCA_BASE_VERSION_1_0_0("psec", 1, 0, 0)

END_C_DECLS

#endif
