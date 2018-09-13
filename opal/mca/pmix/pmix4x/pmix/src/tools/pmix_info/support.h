/*
 * Copyright (c) 2012-2013 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2014 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2017 IBM Corporation.  All rights reserved.
 * Copyright (c) 2017-2018 Intel, Inc. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/** @file **/

#ifndef PMIX_INFO_REGISTER_H
#define PMIX_INFO_REGISTER_H

#include "pmix_config.h"

#include "src/class/pmix_list.h"
#include "src/class/pmix_pointer_array.h"
#include "src/util/cmd_line.h"
#include "src/mca/base/base.h"

BEGIN_C_DECLS

PMIX_EXPORT extern const char *pmix_info_path_prefix;

PMIX_EXPORT extern const char *pmix_info_type_all;
PMIX_EXPORT extern const char *pmix_info_type_pmix;
PMIX_EXPORT extern const char *pmix_info_component_all;
extern const char *pmix_info_param_all;

PMIX_EXPORT extern const char *pmix_info_ver_full;
extern const char *pmix_info_ver_major;
extern const char *pmix_info_ver_minor;
extern const char *pmix_info_ver_release;
extern const char *pmix_info_ver_greek;
extern const char *pmix_info_ver_repo;

PMIX_EXPORT extern const char *pmix_info_ver_all;
extern const char *pmix_info_ver_mca;
extern const char *pmix_info_ver_type;
extern const char *pmix_info_ver_component;


/*
 * Component-related functions
 */
typedef struct {
    pmix_list_item_t super;
    char *type;
    pmix_list_t *components;
    pmix_list_t *failed_components;
} pmix_info_component_map_t;
PMIX_EXPORT PMIX_CLASS_DECLARATION(pmix_info_component_map_t);


PMIX_EXPORT int pmix_info_init(int argc, char **argv,
                               pmix_cmd_line_t *pmix_info_cmd_line);

PMIX_EXPORT void pmix_info_finalize(void);

PMIX_EXPORT void pmix_info_register_types(pmix_pointer_array_t *mca_types);

PMIX_EXPORT int pmix_info_register_framework_params(pmix_pointer_array_t *component_map);

PMIX_EXPORT void pmix_info_close_components(void);
PMIX_EXPORT void pmix_info_err_params(pmix_pointer_array_t *component_map);

PMIX_EXPORT void pmix_info_do_params(bool want_all_in, bool want_internal,
                                     pmix_pointer_array_t *mca_type,
                                     pmix_pointer_array_t *component_map,
                                     pmix_cmd_line_t *pmix_info_cmd_line);

PMIX_EXPORT void pmix_info_show_path(const char *type, const char *value);

PMIX_EXPORT void pmix_info_do_path(bool want_all, pmix_cmd_line_t *cmd_line);

PMIX_EXPORT void pmix_info_show_mca_params(const char *type,
                                           const char *component,
                                           pmix_mca_base_var_info_lvl_t max_level,
                                           bool want_internal);

PMIX_EXPORT void pmix_info_show_mca_version(const pmix_mca_base_component_t *component,
                                            const char *scope, const char *ver_type);

PMIX_EXPORT void pmix_info_show_component_version(pmix_pointer_array_t *mca_types,
                                                  pmix_pointer_array_t *component_map,
                                                  const char *type_name,
                                                  const char *component_name,
                                                  const char *scope, const char *ver_type);

PMIX_EXPORT char *pmix_info_make_version_str(const char *scope,
                                             int major, int minor, int release,
                                             const char *greek,
                                             const char *repo);

PMIX_EXPORT void pmix_info_show_pmix_version(const char *scope);

PMIX_EXPORT void pmix_info_do_arch(void);

PMIX_EXPORT void pmix_info_do_hostname(void);

PMIX_EXPORT void pmix_info_do_type(pmix_cmd_line_t *pmix_info_cmd_line);

PMIX_EXPORT void pmix_info_out(const char *pretty_message, const char *plain_message, const char *value);

PMIX_EXPORT void pmix_info_out_int(const char *pretty_message,
                                   const char *plain_message,
                                   int value);

PMIX_EXPORT int pmix_info_register_project_frameworks (const char *project_name,
                                                       pmix_mca_base_framework_t **frameworks,
                                                       pmix_pointer_array_t *component_map);

END_C_DECLS

#endif
