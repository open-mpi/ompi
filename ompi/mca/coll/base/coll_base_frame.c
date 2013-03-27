/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */


#include "ompi_config.h"
#include <stdio.h>

#include "ompi/constants.h"
#include "opal/mca/mca.h"
#include "opal/util/output.h"
#include "opal/mca/base/base.h"


#include "ompi/mca/coll/coll.h"
#include "ompi/mca/coll/base/base.h"

/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */
#include "ompi/mca/coll/base/static-components.h"

/*
 * Ensure all function pointers are NULL'ed out to start with
 */
static void coll_base_module_construct(mca_coll_base_module_t *m)
{
    m->coll_module_enable = NULL;

    /* Collective function pointers */
    /* blocking functions */
    m->coll_allgather = NULL;
    m->coll_allgatherv = NULL;
    m->coll_allreduce = NULL;
    m->coll_alltoall = NULL;
    m->coll_alltoallv = NULL;
    m->coll_alltoallw = NULL;
    m->coll_barrier = NULL;
    m->coll_bcast = NULL;
    m->coll_exscan = NULL;
    m->coll_gather = NULL;
    m->coll_gatherv = NULL;
    m->coll_reduce = NULL;
    m->coll_reduce_scatter_block = NULL;
    m->coll_reduce_scatter = NULL;
    m->coll_scan = NULL;
    m->coll_scatter = NULL;
    m->coll_scatterv = NULL;

    /* nonblocking functions */
    m->coll_iallgather = NULL;
    m->coll_iallgatherv = NULL;
    m->coll_iallreduce = NULL;
    m->coll_ialltoall = NULL;
    m->coll_ialltoallv = NULL;
    m->coll_ialltoallw = NULL;
    m->coll_ibarrier = NULL;
    m->coll_ibcast = NULL;
    m->coll_iexscan = NULL;
    m->coll_igather = NULL;
    m->coll_igatherv = NULL;
    m->coll_ireduce = NULL;
    m->coll_ireduce_scatter_block = NULL;
    m->coll_ireduce_scatter = NULL;
    m->coll_iscan = NULL;
    m->coll_iscatter = NULL;
    m->coll_iscatterv = NULL;

    /* FT event */
    m->ft_event = NULL;
}

OBJ_CLASS_INSTANCE(mca_coll_base_module_t, opal_object_t, 
                   coll_base_module_construct, NULL);

MCA_BASE_FRAMEWORK_DECLARE(ompi, coll, "Collectives", NULL, NULL, NULL,
                           mca_coll_base_static_components, 0);
