/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
 * Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_BCOL_BASE_H
#define MCA_BCOL_BASE_H

#include "ompi_config.h"

#include "opal/mca/mca.h"
#include "opal/class/opal_list.h"
#include "ompi/mca/bcol/bcol.h"

/*
 * Global functions for BCOL
 */

BEGIN_C_DECLS

OMPI_DECLSPEC extern opal_list_t mca_bcol_base_components_in_use;
OMPI_DECLSPEC extern char *ompi_bcol_bcols_string;

OMPI_DECLSPEC extern mca_base_framework_t ompi_bcol_base_framework;

OMPI_DECLSPEC int mca_bcol_base_init(bool enable_progress_threads, bool enable_mpi_threads);

struct mca_bcol_base_module_t;
OMPI_DECLSPEC int mca_bcol_base_bcol_fns_table_init(struct mca_bcol_base_module_t *bcol_module);

OMPI_DECLSPEC int mca_bcol_base_fn_table_construct(struct mca_bcol_base_module_t *bcol_module);

OMPI_DECLSPEC int mca_bcol_base_fn_table_destroy(struct mca_bcol_base_module_t *bcol_module);

OMPI_DECLSPEC int mca_bcol_base_set_attributes(struct mca_bcol_base_module_t *bcol_module,
                mca_bcol_base_coll_fn_comm_attributes_t *comm_attribs,
                mca_bcol_base_coll_fn_invoke_attributes_t *inv_attribs,
                mca_bcol_base_module_collective_fn_primitives_t bcol_fn,
                mca_bcol_base_module_collective_fn_primitives_t progress_fn);

END_C_DECLS

#endif /* MCA_BCOL_BASE_H */
