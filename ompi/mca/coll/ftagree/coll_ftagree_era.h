/*
 * Copyright (c) 2015-2019 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#ifndef MCA_COLL_FTAGREE_ERA_EXPORT_H
#define MCA_COLL_FTAGREE_ERA_EXPORT_H

#include "coll_ftagree.h"

BEGIN_C_DECLS

/*
 * Early Returning Specific
 */
int mca_coll_ftagree_era_comm_init(ompi_communicator_t *comm, mca_coll_ftagree_module_t *module);
int mca_coll_ftagree_era_comm_finalize(mca_coll_ftagree_module_t *module);
int mca_coll_ftagree_era_init(void);
int mca_coll_ftagree_era_finalize(void);

END_C_DECLS

#endif /* MCA_COLL_FTAGREE_ERA_EXPORT_H */
