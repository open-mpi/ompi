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
 * Copyright (c) 2014-2019 Intel, Inc.  All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */
#ifndef PMIX_PMDL_BASE_H_
#define PMIX_PMDL_BASE_H_

#include <src/include/pmix_config.h>


#ifdef HAVE_SYS_TIME_H
#include <sys/time.h> /* for struct timeval */
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "src/class/pmix_list.h"
#include "src/class/pmix_pointer_array.h"
#include "src/mca/mca.h"
#include "src/mca/base/pmix_mca_base_framework.h"

#include "src/mca/pmdl/pmdl.h"


BEGIN_C_DECLS

/*
 * MCA Framework
 */
PMIX_EXPORT extern pmix_mca_base_framework_t pmix_pmdl_base_framework;
/**
 * PMDL select function
 *
 * Cycle across available components and construct the list
 * of active modules
 */
PMIX_EXPORT pmix_status_t pmix_pmdl_base_select(void);

/**
 * Track an active component / module
 */
struct pmix_pmdl_base_active_module_t {
    pmix_list_item_t super;
    int pri;
    pmix_pmdl_module_t *module;
    pmix_pmdl_base_component_t *component;
};
typedef struct pmix_pmdl_base_active_module_t pmix_pmdl_base_active_module_t;
PMIX_EXPORT PMIX_CLASS_DECLARATION(pmix_pmdl_base_active_module_t);

/* framework globals */
struct pmix_pmdl_globals_t {
    pmix_lock_t lock;
    pmix_list_t actives;
    bool initialized;
};
typedef struct pmix_pmdl_globals_t pmix_pmdl_globals_t;

PMIX_EXPORT extern pmix_pmdl_globals_t pmix_pmdl_globals;

PMIX_EXPORT pmix_status_t pmix_pmdl_base_harvest_envars(char *nspace,
                                                        pmix_info_t info[], size_t ninfo,
                                                        pmix_list_t *ilist);
PMIX_EXPORT pmix_status_t pmix_pmdl_base_setup_nspace(pmix_namespace_t *nptr,
                                                      uint32_t appnum,
                                                      pmix_info_t *info);
PMIX_EXPORT pmix_status_t pmix_pmdl_base_setup_nspace_kv(pmix_namespace_t *nptr,
                                                         uint32_t appnum,
                                                         pmix_kval_t *kv);
PMIX_EXPORT pmix_status_t pmix_pmdl_base_setup_client(pmix_namespace_t *nptr,
                                                      pmix_rank_t rank,
                                                      uint32_t appnum);
PMIX_EXPORT pmix_status_t pmix_pmdl_base_setup_fork(const pmix_proc_t *peer, char ***env);
PMIX_EXPORT void pmix_pmdl_base_deregister_nspace(const char *nptr);

END_C_DECLS

#endif
