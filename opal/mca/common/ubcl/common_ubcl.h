/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2024      Bull SAS.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OPAL_MCA_COMMON_UBCL_H
#define OPAL_MCA_COMMON_UBCL_H

#include "opal/mca/mca.h"
#include "opal/class/opal_list.h"
#include "opal/util/show_help.h"
#include <ubcl_api.h>

#define MCA_REGISTER_COMMON_UBCL(name, desc, type, var)                                          \
    mca_base_var_register("ompi", "mpi", "common_ubcl", name, desc, type, NULL, 0,               \
                          MCA_BASE_VAR_FLAG_SETTABLE, OPAL_INFO_LVL_9, MCA_BASE_VAR_SCOPE_LOCAL, \
                          var)

BEGIN_C_DECLS

struct mca_opal_common_ubcl_component_t {
    /* MCA params */
    int output;
    int verbose;
    bool gdb_attach;
    bool ld_library_path_fail_warn;
    bool search_opt_ubcl;
    bool force_ld_lib_dlopen;
    const char** ubcl_search_path;

    /* Miscellaneous */
    int32_t is_init;
    int32_t is_registered;
    int32_t is_dlopen;
};
typedef struct mca_opal_common_ubcl_component_t mca_opal_common_ubcl_component_t;
OPAL_DECLSPEC extern mca_opal_common_ubcl_component_t mca_opal_common_ubcl_component;

struct mca_common_ubcl_endpoint_t {
    uint32_t type;
    int32_t refcount;
    uint64_t rank;
};
typedef struct mca_common_ubcl_endpoint_t mca_common_ubcl_endpoint_t;


void mca_common_ubcl_register_mca(void);
int mca_common_ubcl_init(void);
int mca_common_ubcl_fini(void);
int mca_common_ubcl_is_init(void);

#define common_ubcl_generic(__token, ...) \
    opal_output(mca_opal_common_ubcl_component.output, "[COMMON/UBCL] "__token __VA_ARGS__)

#define common_ubcl_error(...)   common_ubcl_generic("ERROR:   ", __VA_ARGS__)
#define common_ubcl_warning(...) common_ubcl_generic("WARNING: ", __VA_ARGS__)
#define common_ubcl_log(...)     common_ubcl_generic("         ", __VA_ARGS__)

#define common_ubcl_log_verbose(__lvl, ...) \
    opal_output_verbose(__lvl, mca_opal_common_ubcl_component.output, "[COMMON/UBCL] "__VA_ARGS__)

#define mca_common_ubcl_help(name, ...) \
    opal_show_help("help-mpi-common-ubcl.txt", name, true, "[COMMON/UBCL]", ##__VA_ARGS__)

END_C_DECLS

#endif /* OPAL_MCA_COMMON_UBCL_H */
