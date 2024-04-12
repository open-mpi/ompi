/*
 * Copyright (c) 2004-2009 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2016-2019 Intel, Inc.  All rights reserved.
 * Copyright (c) 2018      Intel, Inc.  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef PRTE_INFO_TOOL_H
#define PRTE_INFO_TOOL_H
#include "prte_config.h"

#include "src/class/pmix_list.h"
#include "src/class/pmix_pointer_array.h"
#include "src/mca/mca.h"
#include "src/util/pmix_cmd_line.h"
#include "src/util/pmix_printf.h"

BEGIN_C_DECLS

/*
 * Globals
 */

extern bool prte_info_pretty;
extern pmix_cli_result_t prte_info_cmd_line;

extern const char *prte_info_type_all;
extern const char *prte_info_type_prte;
extern const char *prte_info_type_base;

extern pmix_pointer_array_t mca_types;

/*
 * Version-related strings and functions
 */

extern const char *prte_info_ver_full;
extern const char *prte_info_ver_major;
extern const char *prte_info_ver_minor;
extern const char *prte_info_ver_release;
extern const char *prte_info_ver_greek;
extern const char *prte_info_ver_svn;

void prte_info_do_version(bool want_all);
void prte_info_show_prte_version(const char *scope);
void prte_info_show_component_version(const char *type_name, const char *component_name,
                                      const char *scope, const char *ver_type);

/*
 * Parameter/configuration-related functions
 */

extern const char *prte_info_component_all;
extern const char *prte_info_param_all;

extern const char *prte_info_path_prefix;
extern const char *prte_info_path_bindir;
extern const char *prte_info_path_libdir;
extern const char *prte_info_path_incdir;
extern const char *prte_info_path_mandir;
extern const char *prte_info_path_pkglibdir;
extern const char *prte_info_path_sysconfdir;
extern const char *prte_info_path_exec_prefix;
extern const char *prte_info_path_sbindir;
extern const char *prte_info_path_libexecdir;
extern const char *prte_info_path_datarootdir;
extern const char *prte_info_path_datadir;
extern const char *prte_info_path_sharedstatedir;
extern const char *prte_info_path_localstatedir;
extern const char *prte_info_path_infodir;
extern const char *prte_info_path_pkgdatadir;
extern const char *prte_info_path_pkgincludedir;

void prte_info_do_params(bool want_all, bool want_internal);
void prte_info_show_mca_params(const char *type, const char *component, bool want_internal);

void prte_info_do_path(bool want_all);
void prte_info_show_path(const char *type, const char *value);

void prte_info_do_arch(void);
void prte_info_do_hostname(void);
void prte_info_do_config(bool want_all);
void prte_info_show_prte_version(const char *scope);

/*
 * Output-related functions
 */
void prte_info_out(const char *pretty_message, const char *plain_message, const char *value);
void prte_info_out_int(const char *pretty_message, const char *plain_message, int value);
/*
 * Component-related functions
 */
typedef struct {
    pmix_list_item_t super;
    char *type;
    pmix_list_t *components;
    pmix_list_t *failed_components;
} prte_info_component_map_t;
PRTE_EXPORT PMIX_CLASS_DECLARATION(prte_info_component_map_t);

extern pmix_pointer_array_t prte_component_map;

void prte_info_components_open(void);
void prte_info_components_close(void);

END_C_DECLS

#endif /* PRTE_INFO_H */
