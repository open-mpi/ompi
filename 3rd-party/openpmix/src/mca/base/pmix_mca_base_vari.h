/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
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
 * Copyright (c) 2008      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2012-2013 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2016-2020 Intel, Inc.  All rights reserved.
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
 * This is the private declarations for the MCA variable system.
 * This file is internal to the MCA variable system and should not
 * need to be used by any other elements in PMIX except the
 * special case of the ompi_info command.
 *
 * All the rest of the doxygen documentation in this file is marked as
 * "internal" and won't show up unless you specifically tell doxygen
 * to generate internal documentation (by default, it is skipped).
 */

#ifndef PMIX_MCA_BASE_VAR_INTERNAL_H
#define PMIX_MCA_BASE_VAR_INTERNAL_H

#include "src/include/pmix_config.h"

#include "src/class/pmix_hash_table.h"
#include "src/class/pmix_list.h"
#include "src/class/pmix_object.h"
#include "src/class/pmix_pointer_array.h"
#include "src/class/pmix_value_array.h"
#include "src/mca/base/pmix_mca_base_var.h"

BEGIN_C_DECLS

/* Internal flags start at bit 16 */
#define PMIX_MCA_BASE_VAR_FLAG_EXTERNAL_MASK 0x0000ffff

#define PMIX_VAR_FLAG_ISSET(var, flag) (!!((var).mbp_flags & (flag)))
#define PMIX_VAR_IS_VALID(var)        (!!((var).mbv_flags & PMIX_MCA_BASE_VAR_FLAG_VALID))
#define PMIX_VAR_IS_SYNONYM(var)      (!!((var).mbv_flags & PMIX_MCA_BASE_VAR_FLAG_SYNONYM))
#define PMIX_VAR_IS_DEPRECATED(var)   (!!((var).mbv_flags & PMIX_MCA_BASE_VAR_FLAG_DEPRECATED))

PMIX_EXPORT extern const char *pmix_var_type_names[];
PMIX_EXPORT extern const size_t pmix_var_type_sizes[];
PMIX_EXPORT extern bool pmix_mca_base_var_initialized;
PMIX_EXPORT extern pmix_list_t pmix_mca_base_var_file_values;
PMIX_EXPORT extern pmix_list_t pmix_mca_base_var_override_values;

/**
 * \internal
 *
 * Structure for holding param names and values read in from files.
 */
struct pmix_mca_base_var_file_value_t {
    /** Allow this to be an PMIX OBJ */
    pmix_list_item_t super;

    /** Parameter name */
    char *mbvfv_var;
    /** Parameter value */
    char *mbvfv_value;
    /** File it came from */
    char *mbvfv_file;
    /** Line it came from */
    int mbvfv_lineno;
};

/**
 * \internal
 *
 * Convenience typedef
 */
typedef struct pmix_mca_base_var_file_value_t pmix_mca_base_var_file_value_t;

/**
 * Object declaration for pmix_mca_base_var_file_value_t
 */
PMIX_CLASS_DECLARATION(pmix_mca_base_var_file_value_t);

/**
 * \internal
 *
 * Get a group
 *
 * @param[in]  group_index Group index
 * @param[out] group       Returned group if it exists
 * @param[in]  invalidok   Return group even if it has been deregistered
 */
PMIX_EXPORT int pmix_mca_base_var_group_get_internal(const int group_index,
                                                     pmix_mca_base_var_group_t **group,
                                                     bool invalidok);

/**
 * \internal
 *
 * Parse a parameter file.
 */
PMIX_EXPORT int pmix_mca_base_parse_paramfile(const char *paramfile, pmix_list_t *list);

/**
 * \internal
 *
 * Add a variable to a group
 */
PMIX_EXPORT int pmix_mca_base_var_group_add_var(const int group_index, const int param_index);

/**
 * \internal
 *
 * Generate a full name with _ between all of the non-NULL arguments
 */
PMIX_EXPORT int pmix_mca_base_var_generate_full_name4(const char *project, const char *framework,
                                                      const char *component, const char *variable,
                                                      char **full_name);

/**
 * \internal
 *
 * Call save_value callback for generated internal mca parameter storing env variables
 */
PMIX_EXPORT int pmix_mca_base_internal_env_store(void);

/**
 * \internal
 *
 * Initialize/finalize MCA variable groups
 */
PMIX_EXPORT int pmix_mca_base_var_group_init(void);
PMIX_EXPORT int pmix_mca_base_var_group_finalize(void);

END_C_DECLS

#endif /* PMIX_MCA_BASE_VAR_INTERNAL_H */
