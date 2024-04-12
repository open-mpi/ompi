/*
 * Copyright (c) 2004-2008 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2010      Oracle and/or its affiliates.  All rights reserved.
 * Copyright (c) 2014-2016 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2019-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2020      Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/** @file:
 *
 * Populates global structure with system-specific information.
 *
 * Notes: add limits.h, compute size of integer and other types via sizeof(type)*CHAR_BIT
 *
 */

#ifndef _PRTE_NAME_FNS_H_
#define _PRTE_NAME_FNS_H_

#include "prte_config.h"

#ifdef HAVE_STDINT_h
#    include <stdint.h>
#endif

#include "types.h"

#include "src/class/pmix_list.h"
#include "src/pmix/pmix-internal.h"

BEGIN_C_DECLS

typedef uint8_t prte_ns_cmp_bitmask_t; /**< Bit mask for comparing process names */
#define PRTE_NS_CMP_NONE  0x00
#define PRTE_NS_CMP_JOBID 0x02
#define PRTE_NS_CMP_VPID  0x04
#define PRTE_NS_CMP_ALL   0x0f
#define PRTE_NS_CMP_WILD  0x10

/* useful define to print name args in output messages */
PRTE_EXPORT char *prte_util_print_name_args(const pmix_proc_t *name);
#define PRTE_NAME_PRINT(n) prte_util_print_name_args(n)

PRTE_EXPORT char *prte_util_print_jobids(const pmix_nspace_t job);
#define PRTE_JOBID_PRINT(n) prte_util_print_jobids(n)

PRTE_EXPORT char *prte_util_print_vpids(const pmix_rank_t vpid);
#define PRTE_VPID_PRINT(n) prte_util_print_vpids(n)

PRTE_EXPORT char *prte_util_print_job_family(const pmix_nspace_t job);
#define PRTE_JOB_FAMILY_PRINT(n) prte_util_print_job_family(n)

PRTE_EXPORT char *prte_util_print_local_jobid(const pmix_nspace_t job);
#define PRTE_LOCAL_JOBID_PRINT(n) prte_util_print_local_jobid(n)

PRTE_EXPORT char *prte_pretty_print_timing(int64_t secs, int64_t usecs);

#define PRTE_LOCAL_JOBID(n) prte_util_get_local_jobid(n)

__prte_attribute_always_inline__ static inline int prte_util_get_local_jobid(pmix_nspace_t n)
{
    char *ptr;
    int lid;

    ptr = strrchr(n, '@');
    if (NULL == ptr) {
        /* this isn't a PRRTE job */
        return -1;
    }
    ++ptr;
    lid = strtoul(ptr, NULL, 10);
    return lid;
}

/* List of names for general use */
struct prte_namelist_t {
    pmix_list_item_t super; /**< Allows this item to be placed on a list */
    pmix_proc_t name;       /**< Name of a process */
};
typedef struct prte_namelist_t prte_namelist_t;

PRTE_EXPORT PMIX_CLASS_DECLARATION(prte_namelist_t);

PRTE_EXPORT int prte_util_convert_vpid_to_string(char **vpid_string, const pmix_rank_t vpid);
PRTE_EXPORT int prte_util_convert_string_to_process_name(pmix_proc_t *name,
                                                         const char *name_string);
PRTE_EXPORT int prte_util_convert_process_name_to_string(char **name_string,
                                                         const pmix_proc_t *name);

PRTE_EXPORT int prte_util_compare_name_fields(prte_ns_cmp_bitmask_t fields,
                                              const pmix_proc_t *name1, const pmix_proc_t *name2);

PRTE_EXPORT char *prte_util_make_version_string(const char *scope, int major, int minor, int release,
                                                const char *greek, const char *repo);

END_C_DECLS
#endif
