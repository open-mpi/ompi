/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2019-2024 Bull SAS.  All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/**
 * @file pml_ubcl_utils.h
 *
 * UBCL PML
 *
 * Contains some usefull fonctions
 *
 */

#ifndef MCA_PML_UBCL_UTILS_H
#define MCA_PML_UBCL_UTILS_H

#include "ompi/mca/common/ubcl/common_ubcl.h"
#include "ompi/mca/pml/ubcl/pml_ubcl.h"
#include "opal/util/output.h"

#define PML_UBCL_COMP_NAME "PML/UBCL"

#define mca_pml_ubcl_log(lvl, ...) \
    opal_output_verbose(lvl, mca_pml_ubcl_component.output, __VA_ARGS__)

#define mca_pml_ubcl_warn(err, format, ...) \
    _mca_common_ubcl_error(__FILE__, __LINE__, err, false, 5, mca_pml_ubcl_component.output, mca_pml_ubcl_component.is_init, mca_pml_ubcl_component.verbose, PML_UBCL_COMP_NAME, format, ##__VA_ARGS__)
#define mca_pml_ubcl_error(err, format, ...) \
    _mca_common_ubcl_error(__FILE__, __LINE__, err, true, 1, mca_pml_ubcl_component.output, mca_pml_ubcl_component.is_init, mca_pml_ubcl_component.verbose, PML_UBCL_COMP_NAME, format, ##__VA_ARGS__)

ubcl_cid_t mca_pml_ubcl_compute_ubcl_cid(int tag, int cid);

#endif /*MCA_PML_UBCL_UTILS_H */
