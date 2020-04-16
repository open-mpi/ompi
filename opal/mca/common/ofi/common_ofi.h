/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2015      Intel, Inc. All rights reserved.
 * Copyright (c) 2017      Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OPAL_MCA_COMMON_OFI_H
#define OPAL_MCA_COMMON_OFI_H
#include <rdma/fabric.h>

OPAL_DECLSPEC int mca_common_ofi_register_mca_variables(void);

struct fi_info* opal_mca_common_ofi_select_provider(struct fi_info *provider_list, int rank);

#endif /* OPAL_MCA_COMMON_OFI_H */
