/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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
#include "portals_config.h"

#include "btl_portals.h"

/* BWB - README - BWB - README - BWB - README - BWB - README - BWB
 *
 * These are stub functions that return error so that the
 * initialization code can be developed and the whole thing will
 * link.  This file will disappear once all functions are
 * implemented.  Do not implement any functions in this file.
 *
 * BWB - README - BWB - README - BWB - README - BWB - README - BWB */


int
mca_btl_portals_register(struct mca_btl_base_module_t* btl,
                         mca_btl_base_tag_t tag,
                         mca_btl_base_module_recv_cb_fn_t cbfunc,
                         void* cbdata)
{
    printf("btl register\n");
    return OMPI_SUCCESS;
}


mca_btl_base_descriptor_t* 
mca_btl_portals_alloc(struct mca_btl_base_module_t* btl, 
                      size_t size)
{
    printf("btl alloc: %d\n", size);
    
    return NULL;
}

int
mca_btl_portals_free(struct mca_btl_base_module_t* btl, 
                     mca_btl_base_descriptor_t* des)
{
    printf("btl free\n");
    return OMPI_ERR_NOT_IMPLEMENTED;
}


mca_btl_base_descriptor_t* 
mca_btl_portals_prepare_src(struct mca_btl_base_module_t* btl,
                            struct mca_btl_base_endpoint_t* peer,
                            mca_mpool_base_registration_t* registration, 
                            struct ompi_convertor_t* convertor,
                            size_t reserve,
                            size_t* size)
{
    printf("btl prepare src\n");
    return NULL;
}


mca_btl_base_descriptor_t* 
mca_btl_portals_prepare_dst(struct mca_btl_base_module_t* btl, 
                            struct mca_btl_base_endpoint_t* peer,
                            mca_mpool_base_registration_t* registration, 
                            struct ompi_convertor_t* convertor,
                            size_t reserve,
                            size_t* size)
{
    printf("btl prepare dst\n");
    return NULL;
}


int
mca_btl_portals_send(struct mca_btl_base_module_t* btl,
                     struct mca_btl_base_endpoint_t* btl_peer,
                     struct mca_btl_base_descriptor_t* descriptor, 
                     mca_btl_base_tag_t tag)
{
    printf("btl send\n");
    return OMPI_ERR_NOT_IMPLEMENTED;
}


int
mca_btl_portals_put(struct mca_btl_base_module_t* btl,
                    struct mca_btl_base_endpoint_t* btl_peer,
                    struct mca_btl_base_descriptor_t* decriptor)
{
    printf("btl put\n");
    return OMPI_ERR_NOT_IMPLEMENTED;
}


int
mca_btl_portals_get(struct mca_btl_base_module_t* btl,
                    struct mca_btl_base_endpoint_t* btl_peer,
                    struct mca_btl_base_descriptor_t* decriptor)
{
    printf("btl get\n");
    return OMPI_ERR_NOT_IMPLEMENTED;
}
