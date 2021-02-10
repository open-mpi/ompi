/*
 * Copyright (c) 2013-2020 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2013-2017 Inria.  All rights reserved.
 * Copyright (c) 2019      Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "pml_monitoring.h"

int mca_pml_monitoring_add_comm(struct ompi_communicator_t* comm)
{
    return pml_selected_module.pml_add_comm(comm);
}

int mca_pml_monitoring_del_comm(struct ompi_communicator_t* comm)
{
    mca_common_monitoring_coll_cache_name(comm);
    return pml_selected_module.pml_del_comm(comm);
}

#if OPAL_ENABLE_FT_MPI
int mca_pml_monitoring_revoke_comm(struct ompi_communicator_t* comm, bool coll_only)
{
    return pml_selected_module.pml_revoke_comm(comm, coll_only);
}
#endif
