/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2025 Bull SAS.  All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/**
 * @file
 *
 * Bull eXtreme Interconnect utilities
 *
 * Contains some usefull fonctions
 *
 */

#ifndef MCA_OSC_UBCL_UTILS_H
#define MCA_OSC_UBCL_UTILS_H

#include "ompi/mca/common/ubcl/common_ubcl.h"
#include "opal/util/output.h"

#define OSC_UBCL_COMP_NAME "OSC/UBCL"

#define mca_osc_ubcl_log(lvl, ...) \
    opal_output_verbose(lvl, mca_osc_ubcl_component.output, __VA_ARGS__)

#define mca_osc_ubcl_warn(err, format, ...) \
    _mca_common_ubcl_error(__FILE__, __LINE__, err, false, 5, mca_osc_ubcl_component.output, mca_osc_ubcl_component.is_init, mca_osc_ubcl_component.verbose, OSC_UBCL_COMP_NAME, format, ##__VA_ARGS__)
#define mca_osc_ubcl_error(err, format, ...) \
    _mca_common_ubcl_error(__FILE__, __LINE__, err,  true, 1, mca_osc_ubcl_component.output, mca_osc_ubcl_component.is_init, mca_osc_ubcl_component.verbose, OSC_UBCL_COMP_NAME, format, ##__VA_ARGS__)
#define mca_osc_ubcl_help(...) opal_show_help("help-mpi-osc-ubcl.txt", ##__VA_ARGS__)

#endif /*MCA_OSC_UBCL_UTILS_H */
