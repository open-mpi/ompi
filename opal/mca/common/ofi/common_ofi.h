/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2015      Intel, Inc. All rights reserved.
 * Copyright (c) 2017      Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2020      Triad National Security, LLC. All rights
 *                         reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OPAL_MCA_COMMON_OFI_H
#define OPAL_MCA_COMMON_OFI_H

#include "opal_config.h"
#include "opal/mca/base/mca_base_var.h"
#include "opal/mca/base/mca_base_framework.h"
#include "opal/util/proc.h"
#include <rdma/fabric.h>

BEGIN_C_DECLS

typedef struct opal_common_ofi_module {
    char **prov_include;
    char **prov_exclude;
    int verbose;
    int registered;
    int output;
} opal_common_ofi_module_t;

extern opal_common_ofi_module_t opal_common_ofi;

OPAL_DECLSPEC int opal_common_ofi_register_mca_variables(const mca_base_component_t *component);
OPAL_DECLSPEC void opal_common_ofi_mca_register(void);
OPAL_DECLSPEC void opal_common_ofi_mca_deregister(void);

/*
 * @param list (IN)    List of strings corresponding to lower providers.
 * @param item (IN)    Single string corresponding to a provider.
 *
 * @return 0           The lower provider of the item string is not in
 *                     list or an input was NULL
 * @return 1           The lower provider of the item string matches
 *                     a string in the item list.
 *
 * This function will take a provider name string and a list of lower
 * provider name strings as inputs. It will return true if the lower
 * provider in the item string matches a lower provider in the list.
 *
 */
OPAL_DECLSPEC int opal_common_ofi_is_in_list(char **list, char *item);

END_C_DECLS

struct fi_info* opal_mca_common_ofi_select_provider(struct fi_info *provider_list, opal_process_info_t process_info);

#endif /* OPAL_MCA_COMMON_OFI_H */
