/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include <string.h>
#include "pml_ptl_array.h"


static void mca_ptl_array_construct(mca_ptl_array_t* array)
{
    array->ptl_procs = 0;
    array->ptl_size = 0;
    array->ptl_index = 0;
    array->ptl_reserve = 0;
}


static void mca_ptl_array_destruct(mca_ptl_array_t* array)
{
    if(array->ptl_procs != 0)
        free(array->ptl_procs);
}

OBJ_CLASS_INSTANCE(
    mca_pml_teg_ptl_array_t,
    ompi_object_t,
    mca_ptl_array_construct,
    mca_ptl_array_destruct
);

int mca_ptl_array_reserve(mca_ptl_array_t* array, size_t size)
{
    mca_ptl_proc_t *procs;
    if(array->ptl_reserve >= size)
        return OMPI_SUCCESS;
    
    procs = (mca_ptl_proc_t *)realloc(array->ptl_procs, sizeof(mca_ptl_proc_t)*size);
    if(NULL == procs)
        return OMPI_ERR_OUT_OF_RESOURCE;
    array->ptl_procs = procs;
    array->ptl_reserve = size;
    memset(array->ptl_procs+array->ptl_size, 0, (size-array->ptl_size)*sizeof(mca_ptl_proc_t));
    return OMPI_SUCCESS;
}

