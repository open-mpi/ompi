/*
 * Copyright (c) 2013      Sandia National Laboratories. All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 */

#include "ompi_config.h"

#include <stdio.h>
#include "opal/mca/common/pmi/common_pmi.h"

#include "opal/threads/tsd.h"
#include "ompi/constants.h"
#include "ompi/mca/rte/rte.h"

#include "rte_pmi.h"


static void
coll_construct(ompi_rte_collective_t *coll)
{
    coll->id = 0;
    coll->active = false;
}


OBJ_CLASS_INSTANCE(ompi_rte_collective_t, opal_object_t, coll_construct, NULL);


int ompi_rte_modex(ompi_rte_collective_t *coll)
{
    int len, ret;
    char *kvs;

    len = mca_common_pmi_kvslen();
    kvs = malloc(len);
    if (NULL == kvs) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    mca_common_pmi_kvsname(kvs, len);
    return mca_common_pmi_commit(kvs);
}


int
ompi_rte_barrier(ompi_rte_collective_t *coll)
{
    int ret;

    ret = mca_common_pmi_barrier();
    if (OPAL_SUCCESS != ret) 
        return OMPI_ERROR;

    coll->active = false;
    return OMPI_SUCCESS;
}
