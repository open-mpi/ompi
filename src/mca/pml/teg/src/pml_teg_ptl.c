/*
 * $HEADER$
 */

#include "ompi_config.h"

#include "pml_teg_ptl.h"


static void mca_pml_base_ptl_construct(mca_pml_base_ptl_t* ptl)
{
    OBJ_CONSTRUCT(&ptl->ptl_cache, ompi_list_t);
    OBJ_CONSTRUCT(&ptl->ptl_cache_lock, ompi_mutex_t);
    ptl->ptl = NULL;
    ptl->ptl_cache_size = 0;
    ptl->ptl_cache_alloc = 0;
}

static void mca_pml_base_ptl_destruct(mca_pml_base_ptl_t* ptl)
{
    OBJ_DESTRUCT(&ptl->ptl_cache);
    OBJ_DESTRUCT(&ptl->ptl_cache_lock);
}

OBJ_CLASS_INSTANCE(
    mca_pml_base_ptl_t,
    ompi_list_t,
    mca_pml_base_ptl_construct,
    mca_pml_base_ptl_destruct
);

