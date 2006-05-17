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
#ifdef HAVE_STDINT_H
#include <stdint.h>
#endif
#if HAVE_STRING_H
#include <string.h>
#endif  /* HAVE_STRING_H */
#include "ompi/mca/mpool/mpool.h"
#include "ompi/mca/mpool/base/base.h"
#include "opal/threads/mutex.h" 

/**
 * Memory Pool Registration
 */

static void mca_mpool_base_registration_constructor( mca_mpool_base_registration_t * reg )
{
    reg->mpool = NULL;
    reg->base = NULL;
    reg->bound = NULL;
    reg->ref_count = 0;
}

static void mca_mpool_base_registration_destructor( mca_mpool_base_registration_t * reg )
{
    
}

OBJ_CLASS_INSTANCE(
    mca_mpool_base_registration_t,
    opal_list_item_t,
    mca_mpool_base_registration_constructor,
    mca_mpool_base_registration_destructor);

/**
 * Function to allocate special memory according to what the user requests in
 * the info object.
 *
 * If the user passes in a valid info structure then the function will
 * try to allocate the memory and register it with every mpool that there is a
 * key for it in the info struct. If it fails at registering the memory with 
 * one of the requested mpools, an error will be returned. Also, if there is a 
 * key in info that does not match any mpool, an error will be returned.
 *
 * If the info parameter is MPI_INFO_NULL, then this function will try to allocate
 * the memory and register it wih as many mpools as possible. However, 
 * if any of the registratons fail the mpool will simply be ignored.
 *
 * @param size the size of the memory area to allocate
 * @param info an info object which tells us what kind of memory to allocate
 *
 * @retval pointer to the allocated memory
 * @retval NULL on failure
 */
void * mca_mpool_base_alloc(size_t size, ompi_info_t * info)
{
    opal_list_item_t * item;
    int num_modules = opal_list_get_size(&mca_mpool_base_modules);
    int reg_module_num = 0;
    int i, num_keys;
    mca_mpool_base_selected_module_t * current;
    mca_mpool_base_selected_module_t * no_reg_function = NULL;
    mca_mpool_base_selected_module_t ** has_reg_function = NULL;
    mca_mpool_base_registration_t * registration;
    void * mem = NULL;
    char * key;
    bool match_found;

    if (num_modules > 0) {
        has_reg_function = (mca_mpool_base_selected_module_t **)
                           malloc(num_modules * sizeof(mca_mpool_base_module_t *));
    }

    if(&ompi_mpi_info_null == info)
    {
        for(item = opal_list_get_first(&mca_mpool_base_modules);
            item != opal_list_get_end(&mca_mpool_base_modules);
            item = opal_list_get_next(item)) {
            current = ((mca_mpool_base_selected_module_t *) item);
            if(current->mpool_module->flags & MCA_MPOOL_FLAGS_MPI_ALLOC_MEM) {
                if(NULL == current->mpool_module->mpool_register){
                    no_reg_function = current;
                }
                else {
                    has_reg_function[reg_module_num++] = current;
                }
            }
        }
    }
    else
    {
        ompi_info_get_nkeys(info, &num_keys);
        key = malloc(MPI_MAX_INFO_KEY + 1);
        for(i = 0; i < num_keys; i++)
        {
            match_found = false;
            ompi_info_get_nthkey(info, i, key);
            for(item = opal_list_get_first(&mca_mpool_base_modules);
                item != opal_list_get_end(&mca_mpool_base_modules);
                item = opal_list_get_next(item)) 
            {
                current = ((mca_mpool_base_selected_module_t *)item);
                if(0 == strcmp(key, 
                       current->mpool_module->mpool_component->mpool_version.mca_component_name))
                {
                    match_found = true;
                    if(NULL == current->mpool_module->mpool_register)
                    {
                        if(NULL != no_reg_function)
                        {
                           /* there was more than one requested mpool that lacks 
                            * a registration function, so return failure */
                            free(key);
                            if (NULL != has_reg_function) {
                                free(has_reg_function);
                            }
                            return NULL;
                        }
                        no_reg_function = current;
                    }
                    else
                    {
                        has_reg_function[reg_module_num++] = current;
                    }
                }
            }
            if(!match_found)
            {
                /* one of the keys given to us by the user did not match any
                 * mpools, so return an error */
                free(key);
                if (NULL != has_reg_function) {
                    free(has_reg_function);
                }
                return NULL;
            }
        }
        free(key);
    }
    
    if(NULL == no_reg_function && 0 == reg_module_num)
    {
        if (NULL != has_reg_function) {
            free(has_reg_function);
        }
        if(&ompi_mpi_info_null == info)
        {
            /* if the info argument was NULL and there were no useable mpools,
             * just malloc the memory and return it */
            mem = malloc(size);
            if(NULL != mem)
                {
                    return mem;
                }
            
        }
        /* the user passed info but we were not able to use any of the mpools 
         * specified */
        return NULL;
    }
    
    
    i = 0;
    num_modules = 0;
    if(NULL != no_reg_function)
    {
        mca_mpool_base_module_t* mpool = no_reg_function->mpool_module;
        mem = mpool->mpool_alloc(mpool, size, 0, MCA_MPOOL_FLAGS_PERSIST, &registration);
        num_modules++;
    }
    else
    {
        mca_mpool_base_module_t* mpool = has_reg_function[i]->mpool_module;
        mem = mpool->mpool_alloc(mpool, size, 0, MCA_MPOOL_FLAGS_PERSIST, &registration);
        i++;
        num_modules++;
    }
    
    while(i < reg_module_num)
    {
        mca_mpool_base_module_t* mpool = has_reg_function[i]->mpool_module;
        if(OMPI_SUCCESS != mpool->mpool_register(mpool, mem, size, MCA_MPOOL_FLAGS_PERSIST,  &registration))
        {
            if (NULL != has_reg_function) {
                free(has_reg_function);
            }
            return NULL;
        }
        num_modules++;
        i++;
    }

    if (NULL != has_reg_function) {
        free(has_reg_function);
    }
    return mem;
}

/**
 * Function to free memory previously allocated by mca_mpool_base_alloc
 *
 * @param base pointer to the memory to free
 *
 * @retval OMPI_SUCCESS
 * @retval OMPI_ERR_BAD_PARAM if the passed base pointer was invalid
 */
int mca_mpool_base_free(void * base)
{
    int rc = OMPI_SUCCESS;
    opal_list_item_t * item;
    mca_mpool_base_selected_module_t * current = NULL;
    mca_mpool_base_selected_module_t * free_function = NULL;
    uint32_t i, cnt;
    ompi_pointer_array_t regs;
    OBJ_CONSTRUCT(&regs, ompi_pointer_array_t);
                                                                                                       
    for(item = opal_list_get_first(&mca_mpool_base_modules);
        item != opal_list_get_end(&mca_mpool_base_modules);
        item = opal_list_get_next(item)) {
        current = ((mca_mpool_base_selected_module_t *) item);
        /* 
         * Check if a mpool has been used for allocating the memory. This 
         * approach only works for the case that the user specified MPI_INFO_NULL
         * in MPI_Alloc_mem.
         * Maybe all possible mpools should be asked if they can free the 
         * memory until the right returns OK ?
         */
        if(current->mpool_module->flags & MCA_MPOOL_FLAGS_MPI_ALLOC_MEM) {
            if(NULL == current->mpool_module->mpool_register){
                free_function = current;
            } else {
                if ( NULL == free_function ) {
                    free_function = current;
                }
            }
        }
        if(NULL != current->mpool_module->mpool_find)  {
            rc = current->mpool_module->mpool_find(
                current->mpool_module,
                base,
                1,
                &regs,
                &cnt
                );
            if(OMPI_SUCCESS != rc) {
                continue;
            }
            for(i = 0; i < cnt; i++) {
                mca_mpool_base_registration_t* reg = (mca_mpool_base_registration_t*)
                    ompi_pointer_array_get_item(&regs, i);
                                                                                                       
                rc = current->mpool_module->mpool_deregister(current->mpool_module, reg);
                if(OMPI_SUCCESS != rc) {
                    goto cleanup;
                }
            }
            ompi_pointer_array_remove_all(&regs);
        }
    }

    /* free the memory */
    if ( NULL == free_function ) {
        /* If there is no mpool the memory has been allocated with malloc */
        free(base);
    } else {
        /* free the memory with the mpool that was responsible for the allocation.
         * The registration is NULL because the buffer has been unregistered above.
         */
        free_function->mpool_module->mpool_free(free_function->mpool_module, base, NULL);
    }

cleanup:
    OBJ_DESTRUCT(&regs);
    return rc;
}

