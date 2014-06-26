/*
 * Copyright (c) 2014      Intel, Inc. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef OPAL_PMI_H
#define OPAL_PMI_H

#include "opal_config.h"
#include "opal/types.h"

#include "opal/mca/mca.h"
#include "opal/mca/event/event.h"
#include "opal/dss/dss_types.h"

#include "opal/mca/pmi/pmi_types.h"

BEGIN_C_DECLS

/****    DEFINE THE PUBLIC API'S    ****/
/*
 * PMI_Init
 *
 * NOTE: calls to these APIs must be thread-protected as there
 * is NO internal thread safety.
 */
typedef int (*opal_pmi_base_module_init_fn_t)(void);

/*
 * Close a database handle
 *
 * Close the specified database handle. A -1 handle indicates
 * that ALL open database handles are to be closed.
 */
typedef int (*opal_pmi_base_module_fini_fn_t)(void);

/*
 * Store a data value against the primary key  - overwrites any data
 * of matching key that is already present. The data is copied into the database
 * and therefore does not need to be preserved by the caller.
 */
typedef int (*opal_pmi_base_API_store_fn_t)(int pmihandle,
                                               const opal_identifier_t *id,
                                               opal_value_t *kv);

/*
 * Commit data to the database - action depends on implementation within
 * each active component
 */
typedef void (*opal_pmi_base_API_commit_fn_t)(int pmihandle,
                                                 const opal_identifier_t *id);

/*
 * Retrieve data
 *
 * Retrieve data for the given primary key associated with the specified key. Wildcards
 * are supported here as well. Caller is responsible for releasing the returned list
 * of opal_value_t objects.
 */
typedef int (*opal_pmi_base_API_fetch_fn_t)(int pmihandle,
                                               const opal_identifier_t *id,
                                               const char *key,
                                               opal_list_t *kvs);

/*
 * Delete data
 *
 * Delete the data for the given primary key that is associated with the specified key.
 * If a NULL key is provided, all data for the given primary key will be deleted.
 */
typedef int (*opal_pmi_base_API_remove_fn_t)(int pmihandle,
                                                const opal_identifier_t *id,
                                                const char *key);

/*
 * the standard public API data structure
 */
typedef struct {
    opal_pmi_base_API_open_fn_t           open;
    opal_pmi_base_API_close_fn_t          close;
    opal_pmi_base_API_store_fn_t          store;
    opal_pmi_base_API_commit_fn_t         commit;
    opal_pmi_base_API_fetch_fn_t          fetch;
    opal_pmi_base_API_remove_fn_t         remove;
} opal_pmi_base_API_t;



/****    DEFINE THE MODULE API'S    ****/
/* Note that each datastore handle will be associated with
 * a single active module. Thus, storing and fetching data
 * from that module does not require that we pass in the
 * handle itself.
 *
 * NOTE: the call to actually store/fetch data in a given
 * datastore handle must be protected against threaded operations
 * as there is NO thread protection inside the various modules.
 */
struct opal_pmi_base_module_t;

/*
 * Initialize the module
 */
typedef int (*opal_pmi_base_module_init_fn_t)(struct opal_pmi_base_module_t *mod);

/*
 * Finalize the module
 */
typedef void (*opal_pmi_base_module_finalize_fn_t)(struct opal_pmi_base_module_t *mod);

/* store the data in this module */
typedef int (*opal_pmi_base_module_store_fn_t)(struct opal_pmi_base_module_t *mod,
                                                  const opal_identifier_t *id,
                                                  opal_value_t *kv);

/* commit data */
typedef void (*opal_pmi_base_module_commit_fn_t)(struct opal_pmi_base_module_t *mod,
                                                    const opal_identifier_t *id);

/* fetch data from the module */
typedef int (*opal_pmi_base_module_fetch_fn_t)(struct opal_pmi_base_module_t *mod,
                                                  const opal_identifier_t *id,
                                                  const char *key,
                                                  opal_list_t *kvs);

/* remove data */
typedef int (*opal_pmi_base_module_remove_fn_t)(struct opal_pmi_base_module_t *mod,
                                                   const opal_identifier_t *id,
                                                   const char *key);

/*
 * the standard module data structure
 */
typedef struct {
    opal_pmi_base_module_init_fn_t            init;
    opal_pmi_base_module_finalize_fn_t        finalize;
    opal_pmi_base_module_store_fn_t           store;
    opal_pmi_base_module_commit_fn_t          commit;
    opal_pmi_base_module_fetch_fn_t           fetch;
    opal_pmi_base_module_remove_fn_t          remove;
} opal_pmi_base_module_t;

/*
 * the component data structure
 */
/* function to determine if this component is available for use.
 * Note that we do not use the standard component open
 * function as we do not want/need return of a module.
 */
typedef bool (*mca_pmi_base_component_avail_fn_t)(void);

/* create and return a datastore module */
typedef opal_pmi_base_module_t* (*mca_pmi_base_component_create_hdl_fn_t)(void);

/* provide a chance for the component to finalize */
typedef void (*mca_pmi_base_component_finalize_fn_t)(void);

typedef struct {
    mca_base_component_t                      base_version;
    mca_base_component_data_t                 base_data;
    int                                       priority;
    mca_pmi_base_component_avail_fn_t      available;
    mca_pmi_base_component_create_hdl_fn_t create_handle;
    mca_pmi_base_component_finalize_fn_t   finalize;
} opal_pmi_base_component_t;

/*
 * Macro for use in components that are of type pmi
 */
#define OPAL_PMI_BASE_VERSION_2_0_0 \
  MCA_BASE_VERSION_2_0_0, \
  "pmi", 2, 0, 0

/* Global structure for accessing store functions */
OPAL_DECLSPEC extern opal_pmi_base_module_t opal_pmi;  /* holds base function pointers */

END_C_DECLS

#endif
