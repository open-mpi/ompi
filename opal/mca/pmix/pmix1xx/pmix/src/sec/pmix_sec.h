/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2007-2008 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2015      Intel, Inc.  All rights reserved.
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
 * This interface is designed to serve as a low-weight plugin mechansim
 * for security support. PMIx doesn't need much in this regard, but we do
 * need a mechanism for authenticating connections. Dlopen and
 * friends are not used, but all the functionality is accessed through
 * struct's of function pointers, so you can swap between multiple
 * different implementations at run time, just like typical plugins.
 * Hence, these entities are referred to as "Security
 * Pseudo-Components" (SPCs).
 *
 * The SPCs are referenced by their names (e.g., "sasl", "munge").
 *
 * Only *one* SPC will be active in a given system. The SPC to be
 * used, however, may be selected at runtime by setting the "PMIX_SEC_MODE"
 * environmental parameter. This param can consist of either:
 *
 * a single SPC name - if this SPC is not available, an error will
 * be returned
 *
 * a comma-separated list of SPC names - the available SPCs will be
 * queried in the specified order. The first one to return "success"
 * will be selected and used. An error will be returned if none of
 * the available SPCs return success.
 *
 * either of the above, with a '^' as the first character - this
 * indicates that the specified SPC(s) are to be excluded from
 * consideration. The remaining SPCs will be queried until one returns
 * "success" - if none return success, then an error will be returned.
 *
 * Module interface:
 *
 * module_init(): The PMIx client and server init functions
 * call pmix_sec_init(), which will invoke this init function on
 * each SPC to see if it wants to run.  SPCs can gracefully
 * remove themselves from consideration in this process by returning
 * PMIX_ERR_NOT_SUPPORTED.
 *
 * initiate_connection(): Executes the client side of the
 * authentication procedure. Returns 0 if successful, or
 * a non-zero error.
 *
 * accept_connection(): Executes the server side of the
 * authentication procedure. Returns 0 if successful, or
 * a non-zero error.
 *
 * module_finalize(): The PMIx client and server finalize functions
 * call pmix_sec_finalize(), which, in turn, calls the
 * module_finalize() function on all available SPCs.
 */

#ifndef PMIX_SEC_H
#define PMIX_SEC_H

#include <private/autogen/config.h>
#include <pmix/rename.h>

#include "src/usock/usock.h"

BEGIN_C_DECLS

/******    MODULE DEFINITION    ******/

/**
 * Initialize the module. Returns an error if the module cannot
 * run, success if it can and wants to be used.
 */
typedef int (*pmix_sec_base_module_init_fn_t)(void);

/**
 * Finalize the module. Tear down any allocated storage, disconnect
 * from any system support (e.g., LDAP server)
 */
typedef void (*pmix_sec_base_module_fini_fn_t)(void);

/****    CLIENT-SIDE FUNCTIONS    ****/
/**
 * Create and return a string representation of a credential for this
 * client
 */
typedef char* (*pmix_sec_base_module_create_cred_fn_t)(void);

/**
 * Perform the client-side handshake. Note that it is not required
 * (and indeed, would be rare) for a protocol to use both the
 * credential and handshake interfaces. It is acceptable, therefore,
 * for one of them to be NULL */
typedef pmix_status_t (*pmix_sec_base_module_client_hndshk_fn_t)(int sd);


/****    SERVER-SIDE FUNCTIONS    ****/
/**
 * Validate a client's credential
 */
typedef pmix_status_t (*pmix_sec_base_module_validate_cred_fn_t)(pmix_peer_t *peer, char *cred);

/**
 * Perform the server-side handshake. Note that it is not required
 * (and indeed, would be rare) for a protocol to use both the
 * credential and handshake interfaces. It is acceptable, therefore,
 * for one of them to be NULL */
typedef pmix_status_t (*pmix_sec_base_module_server_hndshk_fn_t)(pmix_peer_t *peer);

/**
 * Struct for holding CPC module function pointers
 */
typedef struct {
    char *name;
    /* init/finalize */
    pmix_sec_base_module_init_fn_t           init;
    pmix_sec_base_module_fini_fn_t           finalize;
    /** Client-side */
    pmix_sec_base_module_create_cred_fn_t    create_cred;
    pmix_sec_base_module_client_hndshk_fn_t  client_handshake;
    /** Server-side */
    pmix_sec_base_module_validate_cred_fn_t  validate_cred;
    pmix_sec_base_module_server_hndshk_fn_t  server_handshake;
} pmix_sec_base_module_t;

PMIX_DECLSPEC extern pmix_sec_base_module_t pmix_sec;

/* initialize and finalize the security system */
PMIX_DECLSPEC int pmix_sec_init(void);
PMIX_DECLSPEC void pmix_sec_finalize(void);

END_C_DECLS

#endif
