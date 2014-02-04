/*
 * Copyright (c) 2014      Intel, Inc. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/** @file:
 *
 * The Security Framework
 *
 */

#ifndef OPAL_SEC_H
#define OPAL_SEC_H

#include "opal_config.h"
#include "opal/types.h"

#include "opal/mca/mca.h"
#include "opal/dss/dss_types.h"


/* The security framework is a single-select one - i.e.,
 * only one plugin is active at any time, though multiple
 * plugins may build. When init is called, each plugin that
 * built should check to see if it can connect to its
 * respective server - if it can, then it should return
 * success to indicate it is ready to be used.
 *
 * For scalability, it is important that each process only
 * contact the security server once, and only when requested
 * to do so. Thus, the plugin should not get credentials for
 * the process until the first call to "get_my_credentials",
 * and should then cache the results for future use.
 */

BEGIN_C_DECLS

typedef struct {
    char *credential;
    size_t size;
} opal_sec_cred_t;

/*
 * Initialize the module
 */
typedef int (*opal_sec_base_module_init_fn_t)(void);

/*
 * Finalize the module
 */
typedef void (*opal_sec_base_module_finalize_fn_t)(void);

/*
 * Get a security credential for this process - return pointer to
 * a "credential" that I can use for authenticating myself to another process.
 * The value must be returned in a network-byte-ordered form suitable
 * for sending across the network.
 *
 * It isn't expected that the identifier will be used to obtain a
 * certificate as external security systems will have no idea what
 * it means. However, some modules may use it, and there is no way
 * for the opal layer to know a process identifier without being told,
 * do provide it here
 *
 * Function returns OPAL_SUCCESS if a credential was assigned, or an error
 * code indicating why it failed
 */
typedef int (*opal_sec_base_module_get_my_cred_fn_t)(opal_identifier_t *my_id,
                                                     opal_sec_cred_t **cred);

/*
 * Authenticate a security credential - given a security credential,
 * determine if the credential is valid. The credential is passed in
 * a network-byte-ordered form as it came across the network.
 *
 * Function returns OPAL_SUCCESS if the token is authenticated, or an
 * error code indicating why it failed
 */
typedef int (*opal_sec_base_module_auth_fn_t)(opal_sec_cred_t *cred);

/*
 * the standard module data structure
 */
struct opal_sec_base_module_1_0_0_t {
    opal_sec_base_module_init_fn_t          init;
    opal_sec_base_module_finalize_fn_t      finalize;
    opal_sec_base_module_get_my_cred_fn_t   get_my_credential;
    opal_sec_base_module_auth_fn_t          authenticate;
};
typedef struct opal_sec_base_module_1_0_0_t opal_sec_base_module_1_0_0_t;
typedef struct opal_sec_base_module_1_0_0_t opal_sec_base_module_t;

/*
 * the standard component data structure
 */
struct opal_sec_base_component_1_0_0_t {
    mca_base_component_t base_version;
    mca_base_component_data_t base_data;
};
typedef struct opal_sec_base_component_1_0_0_t opal_sec_base_component_1_0_0_t;
typedef struct opal_sec_base_component_1_0_0_t opal_sec_base_component_t;

/*
 * Macro for use in components that are of type sec
 */
#define OPAL_SEC_BASE_VERSION_1_0_0 \
  MCA_BASE_VERSION_2_0_0, \
  "sec", 1, 0, 0

/* Global structure for accessing SEC functions */
OPAL_DECLSPEC extern opal_sec_base_module_t opal_sec;  /* holds base function pointers */

END_C_DECLS

#endif
