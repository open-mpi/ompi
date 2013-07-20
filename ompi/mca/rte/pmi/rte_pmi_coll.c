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
#include <pmi.h>
#if WANT_PMI2_SUPPORT
#include <pmi2.h>
#endif

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


int
ompi_rte_modex(ompi_rte_collective_t *coll)
{
    int len, ret;
    char *kvs;

    ret = PMI_KVS_Get_name_length_max(&len);
    if (PMI_SUCCESS != ret) return OMPI_ERROR;

    kvs = malloc(len);
    if (NULL == kvs) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    ret = PMI_KVS_Get_my_name(kvs, len);
    if (PMI_SUCCESS != ret) return OMPI_ERROR;

    ret = PMI_KVS_Commit(kvs);
    if (PMI_SUCCESS != ret) return OMPI_ERROR;

    ret = PMI_Barrier();
    if (PMI_SUCCESS != ret) return OMPI_ERROR;

    return OMPI_SUCCESS;
}


int
ompi_rte_barrier(ompi_rte_collective_t *coll)
{
    int ret;

    ret = PMI_Barrier();
    if (PMI_SUCCESS != ret) return OMPI_ERROR;

    coll->active = false;

    return OMPI_SUCCESS;
}
