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

#include "bmi_portals.h"

/* BWB - README - BWB - README - BWB - README - BWB - README - BWB
 *
 * These are stub functions that return error so that the
 * initialization code can be developed and the whole thing will
 * link.  This file will disappear once all functions are
 * implemented.  Do not implement any functions in this file.
 *
 * BWB - README - BWB - README - BWB - README - BWB - README - BWB */


int
mca_bmi_portals_register(struct mca_bmi_base_module_t* bmi,
                         mca_bmi_base_tag_t tag,
                         mca_bmi_base_module_recv_cb_fn_t cbfunc,
                         void* cbdata)
{
    printf("bmi register\n");
    return OMPI_SUCCESS;
}


mca_bmi_base_descriptor_t* 
mca_bmi_portals_alloc(struct mca_bmi_base_module_t* bmi, 
                      size_t size)
{
    printf("bmi alloc: %d\n", size);
    
    return NULL;
}

int
mca_bmi_portals_free(struct mca_bmi_base_module_t* bmi, 
                     mca_bmi_base_descriptor_t* des)
{
    printf("bmi free\n");
    return OMPI_ERR_NOT_IMPLEMENTED;
}


mca_bmi_base_descriptor_t* 
mca_bmi_portals_prepare_src(struct mca_bmi_base_module_t* bmi,
                            struct mca_bmi_base_endpoint_t* peer,
                            mca_mpool_base_registration_t* registration, 
                            struct ompi_convertor_t* convertor,
                            size_t reserve,
                            size_t* size)
{
    printf("bmi prepare src\n");
    return NULL;
}


mca_bmi_base_descriptor_t* 
mca_bmi_portals_prepare_dst(struct mca_bmi_base_module_t* bmi, 
                            struct mca_bmi_base_endpoint_t* peer,
                            mca_mpool_base_registration_t* registration, 
                            struct ompi_convertor_t* convertor,
                            size_t reserve,
                            size_t* size)
{
    printf("bmi prepare dst\n");
    return NULL;
}


int
mca_bmi_portals_send(struct mca_bmi_base_module_t* bmi,
                     struct mca_bmi_base_endpoint_t* bmi_peer,
                     struct mca_bmi_base_descriptor_t* descriptor, 
                     mca_bmi_base_tag_t tag)
{
    printf("bmi send\n");
    return OMPI_ERR_NOT_IMPLEMENTED;
}


int
mca_bmi_portals_put(struct mca_bmi_base_module_t* bmi,
                    struct mca_bmi_base_endpoint_t* bmi_peer,
                    struct mca_bmi_base_descriptor_t* decriptor)
{
    printf("bmi put\n");
    return OMPI_ERR_NOT_IMPLEMENTED;
}


int
mca_bmi_portals_get(struct mca_bmi_base_module_t* bmi,
                    struct mca_bmi_base_endpoint_t* bmi_peer,
                    struct mca_bmi_base_descriptor_t* decriptor)
{
    printf("bmi get\n");
    return OMPI_ERR_NOT_IMPLEMENTED;
}
