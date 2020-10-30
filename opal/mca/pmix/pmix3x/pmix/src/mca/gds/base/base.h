/* -*- C -*-
 *
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2012      Los Alamos National Security, Inc.  All rights reserved.
 * Copyright (c) 2014-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2015-2020 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2018      IBM Corporation.  All rights reserved.
 * Copyright (c) 2019      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */
#ifndef PMIX_GDS_BASE_H_
#define PMIX_GDS_BASE_H_

#include "src/include/pmix_config.h"


#ifdef HAVE_SYS_TIME_H
#include <sys/time.h> /* for struct timeval */
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "src/class/pmix_list.h"
#include "src/mca/mca.h"
#include "src/mca/base/pmix_mca_base_framework.h"

#include "src/mca/gds/gds.h"


BEGIN_C_DECLS

/*
 * MCA Framework
 */
PMIX_EXPORT extern pmix_mca_base_framework_t pmix_gds_base_framework;
/**
 * GDS select function
 *
 * Cycle across available components and construct the list
 * of active modules
 */
PMIX_EXPORT pmix_status_t pmix_gds_base_select(pmix_info_t info[], size_t ninfo);

/**
 * Track an active component / module
 */
struct pmix_gds_base_active_module_t {
    pmix_list_item_t super;
    int pri;
    pmix_gds_base_module_t *module;
    pmix_gds_base_component_t *component;
};
typedef struct pmix_gds_base_active_module_t pmix_gds_base_active_module_t;
PMIX_CLASS_DECLARATION(pmix_gds_base_active_module_t);


/* framework globals */
struct pmix_gds_globals_t {
  pmix_list_t actives;
  bool initialized;
  bool selected;
  char *all_mods;
};

typedef enum {
    PMIX_MODEX_KEY_INVALID = -1,
    PMIX_MODEX_KEY_NATIVE_FMT,
    PMIX_MODEX_KEY_KEYMAP_FMT,
    PMIX_MODEX_KEY_MAX
} pmix_gds_modex_key_fmt_t;

/* define a modex blob info */
typedef uint8_t pmix_gds_modex_blob_info_t;

#define PMIX_GDS_COLLECT_BIT        0x0001
#define PMIX_GDS_KEYMAP_BIT         0x0002

#define PMIX_GDS_KEYMAP_IS_SET(byte)        (PMIX_GDS_KEYMAP_BIT & (byte))
#define PMIX_GDS_COLLECT_IS_SET(byte)       (PMIX_GDS_COLLECT_BIT & (byte))

typedef struct pmix_gds_globals_t pmix_gds_globals_t;

typedef void * pmix_gds_base_ctx_t;
typedef pmix_status_t (*pmix_gds_base_store_modex_cb_fn_t)(pmix_gds_base_ctx_t ctx,
                                                           pmix_proc_t *proc,
                                                           pmix_gds_modex_key_fmt_t key_fmt,
                                                           char **kmap,
                                                           pmix_buffer_t *pbkt);

PMIX_EXPORT extern pmix_gds_globals_t pmix_gds_globals;

/* get a list of available support - caller must free results
 * when done. The list is returned as a comma-delimited string
 * of available components in priority order */
PMIX_EXPORT char* pmix_gds_base_get_available_modules(void);


/* Select a gds module based on the provided directives */
PMIX_EXPORT pmix_gds_base_module_t* pmix_gds_base_assign_module(pmix_info_t *info,
                                                                size_t ninfo);

/**
* Add any envars to a peer's environment that the module needs
* to communicate. The API stub will rotate across all active modules, giving
* each a chance to contribute
*
* @return PMIX_SUCCESS on success.
*/
PMIX_EXPORT pmix_status_t pmix_gds_base_setup_fork(const pmix_proc_t *proc,
                                                   char ***env);

PMIX_EXPORT pmix_status_t pmix_gds_base_store_modex(struct pmix_namespace_t *nspace,
                                                    pmix_buffer_t * buff,
                                                    pmix_gds_base_ctx_t ctx,
                                                    pmix_gds_base_store_modex_cb_fn_t cb_fn,
                                                    void *cbdata);

PMIX_EXPORT
pmix_status_t pmix_gds_base_modex_pack_kval(pmix_gds_modex_key_fmt_t key_fmt,
                                            pmix_buffer_t *buf, char ***kmap,
                                            pmix_kval_t *kv);

PMIX_EXPORT
pmix_status_t pmix_gds_base_modex_unpack_kval(pmix_gds_modex_key_fmt_t key_fmt,
                                              pmix_buffer_t *buf, char **kmap,
                                              pmix_kval_t *kv);
END_C_DECLS

#endif
