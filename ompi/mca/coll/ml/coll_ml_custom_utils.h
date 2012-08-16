/*
 * Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
 * Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/** @file */

#ifndef MCA_COLL_ML_CUSTOM_UTILS_H
#define MCA_COLL_ML_CUSTOM_UTILS_H

#include "ompi_config.h"

#include "coll_ml.h"

/* the function is used to check if the bcol name is used in this ml module */
int mca_coll_ml_check_if_bcol_is_used(const char *bcol_name, const mca_coll_ml_module_t *ml_module,
        int topo_index);

/* The function is used to check if the bcol component was REQUESTED by user */
int mca_coll_ml_check_if_bcol_is_requested(const char *component_name);

END_C_DECLS

#endif /* MCA_COLL_ML_ML_H */
