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
#ifdef HAVE_STDINT_H
#include <stdint.h>
#endif
#if HAVE_STRING_H
#include <string.h>
#endif  /* HAVE_STRING_H */
#include "mca/mpool/mpool.h"
#include "mca/mpool/base/base.h"
#include "threads/mutex.h" 

ompi_rb_tree_t mca_mpool_base_tree;
ompi_free_list_t mca_mpool_base_mem_list;
ompi_mutex_t mca_mpool_base_tree_lock; 


/**
 * Searches the mpool to see if it has allocated the memory that is passed in.
 * If so it returns an array of mpools the memory is registered with.
 *
 * @param base pointer to the memory to lookup
 *
 * @retval NULL if the memory is not in any mpool
 * @retval pointer to an array of type mca_mpool_base_reg_mpool_t
 */
static inline struct mca_mpool_base_chunk_t * mca_mpool_base_find_nl(void * base)
{
    mca_mpool_base_key_t key;
    key.bottom = base;
    key.top = base;
    return (mca_mpool_base_chunk_t *)ompi_rb_tree_find(&mca_mpool_base_tree, &key);
}

/**
 * Searches the mpool to see if it has allocated the memory that is passed in.
 * If so it returns an array of mpools the memory is registered with.
 *
 * @param base pointer to the memory to lookup
 *
 * @retval NULL if the memory is not in any mpool
 * @retval pointer to an array of type mca_mpool_base_reg_mpool_t
 */
struct mca_mpool_base_chunk_t * mca_mpool_base_find(void * base)
{
    mca_mpool_base_chunk_t* found;
    mca_mpool_base_chunk_t* copy;

    OMPI_THREAD_LOCK(&mca_mpool_base_tree_lock);
    if(NULL != (found = mca_mpool_base_find(base))) {
        mca_mpool_base_reg_mpool_t* reg;
        copy = OBJ_NEW(mca_mpool_base_chunk_t);
        *copy = *found;
        reg = copy->mpools;
        while(NULL != reg->mpool) {
            OBJ_RETAIN(reg->mpool_registration);
            reg++;
        }
    } else {
        copy = NULL;
    }
    OMPI_THREAD_UNLOCK(&mca_mpool_base_tree_lock);
    return copy;
}

/**
 * Memory Pool Registration
 */

static void mca_mpool_base_registration_constructor( mca_mpool_base_registration_t * reg )
{
    reg->mpool = NULL;
    reg->base = NULL;
    reg->bound = NULL;
}

static void mca_mpool_base_registration_destructor( mca_mpool_base_registration_t * reg )
{
    if(NULL != reg->mpool) {
        reg->mpool->mpool_deregister(
            reg->mpool,
            reg->base,
            reg->bound - reg->base + 1,
            reg);
    }
}

OBJ_CLASS_INSTANCE(
    mca_mpool_base_registration_t,
    opal_list_item_t,
    mca_mpool_base_registration_constructor,
    mca_mpool_base_registration_destructor);

/**
 * Function for the red black tree to compare 2 keys
 *
 * @param key1 a pointer to the 1st key
 * @param key2 a pointer to the second key
 *
 * @retval -1 if key1 is below key2
 * @retval 1 if key 1 is above key2
 * @retval 0 if the keys are the same
 */
int mca_mpool_base_tree_node_compare(void * key1, void * key2)
{
    if(((mca_mpool_base_key_t *) key1)->bottom <
       ((mca_mpool_base_key_t *) key2)->bottom)
    {
        return -1;
    }
    else if(((mca_mpool_base_key_t *) key1)->bottom >
            ((mca_mpool_base_key_t *) key2)->top)
    {
        return 1;
    }
    else
    {
        return 0;
    }
}

int mca_mpool_base_insert(void * addr, size_t size, 
                          mca_mpool_base_module_t* mpool, 
                          void* user_data,
                          mca_mpool_base_registration_t* registration)
{
    opal_list_item_t *item; 
    int rc; 
    OMPI_FREE_LIST_GET(&mca_mpool_base_mem_list, item, rc);
    if(rc != OMPI_SUCCESS) 
        return rc; 

    ((mca_mpool_base_chunk_t *) item)->key.bottom = addr;
    ((mca_mpool_base_chunk_t *) item)->key.top = (void *) 
        ((char *) addr + size - 1);
    ((mca_mpool_base_chunk_t *) item)->mpools[0].mpool = mpool; 
    ((mca_mpool_base_chunk_t *) item)->mpools[0].user_data = user_data;
    ((mca_mpool_base_chunk_t *) item)->mpools[0].mpool_registration = registration;

    OMPI_THREAD_LOCK(&mca_mpool_base_tree_lock); 
    rc = ompi_rb_tree_insert(&mca_mpool_base_tree, 
                        &((mca_mpool_base_chunk_t *)item)->key, item);
    OMPI_THREAD_UNLOCK(&mca_mpool_base_tree_lock); 
    if(OMPI_SUCCESS != rc) {
        OMPI_FREE_LIST_RETURN(&mca_mpool_base_mem_list, item);
        return rc; 
    }
    return OMPI_SUCCESS; 
}

/**
 * Function to remove previously memory from the tree without freeing it
 *
 * @param base pointer to the memory to free
 *
 * @retval OMPI_SUCCESS
 * @retval OMPI_ERR_BAD_PARAM if the passed base pointer was invalid
 */
int mca_mpool_base_remove(void * base)
{
    int rc; 
    mca_mpool_base_chunk_t *chunk;

    OMPI_THREAD_LOCK(&mca_mpool_base_tree_lock); 
    if(NULL == (chunk = mca_mpool_base_find_nl(base))) {
        OMPI_THREAD_UNLOCK(&mca_mpool_base_tree_lock); 
        return OMPI_ERR_BAD_PARAM;
    }
    rc =  ompi_rb_tree_delete(&mca_mpool_base_tree, &chunk->key); 
    OMPI_THREAD_UNLOCK(&mca_mpool_base_tree_lock);
    return rc;
}

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
    mca_mpool_base_selected_module_t ** has_reg_function = (mca_mpool_base_selected_module_t **) 
                           malloc(num_modules * sizeof(mca_mpool_base_module_t *));
    mca_mpool_base_registration_t * registration;
    void * mem = NULL;
    char * key;
    bool match_found;

    if(&ompi_mpi_info_null == info)
    {
        for(item = opal_list_get_first(&mca_mpool_base_modules);
            item != opal_list_get_end(&mca_mpool_base_modules);
            item = opal_list_get_next(item)) 
        {
            current = ((mca_mpool_base_selected_module_t *) item);
            if(NULL == current->mpool_module->mpool_register)
            {
                no_reg_function = current;
            }
            else
            {
                has_reg_function[reg_module_num++] = current;
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
                            free(has_reg_function);
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
                free(has_reg_function);
                return NULL;
            }
        }
        free(key);
    }
    
    OMPI_FREE_LIST_GET(&mca_mpool_base_mem_list, item, i);
    if(NULL == no_reg_function && 0 == reg_module_num)
    {
        free(has_reg_function);
        if(&ompi_mpi_info_null == info)
        {
            /* if the info argument was NULL and there were no useable mpools,
             * just malloc the memory and return it */
            ((mca_mpool_base_chunk_t *) item)->mpools[0].mpool = NULL;
            mem = malloc(size);
            if(NULL != mem)
            {
                ((mca_mpool_base_chunk_t *) item)->key.bottom = mem;
                ((mca_mpool_base_chunk_t *) item)->key.top = (void *) 
                                                     ((char *) mem + size - 1);
                OMPI_THREAD_LOCK(&mca_mpool_base_tree_lock); 

                ompi_rb_tree_insert(&mca_mpool_base_tree, 
                                    &((mca_mpool_base_chunk_t *)item)->key, item);
                OMPI_THREAD_UNLOCK(&mca_mpool_base_tree_lock); 
                return mem;
            }
        }
        OMPI_FREE_LIST_RETURN(&mca_mpool_base_mem_list, item);
        /* the user passed info but we were not able to use any of the mpools 
         * specified */
        return NULL;
    }
    
    
    i = 0;
    num_modules = 0;
    if(NULL != no_reg_function)
    {
        mca_mpool_base_module_t* mpool = no_reg_function->mpool_module;
        mem = mpool->mpool_alloc(mpool, size, 0, &registration);
        ((mca_mpool_base_chunk_t *) item)->key.bottom = mem;
        ((mca_mpool_base_chunk_t *) item)->key.top = (void *)((char *) mem + size - 1);
        ((mca_mpool_base_chunk_t *) item)->mpools[num_modules].mpool = mpool;
        ((mca_mpool_base_chunk_t *) item)->mpools[num_modules].user_data = (void*) no_reg_function->user_data;
        ((mca_mpool_base_chunk_t *) item)->mpools[num_modules++].mpool_registration = registration;
        num_modules++;
    }
    else
    {
        mca_mpool_base_module_t* mpool = has_reg_function[i]->mpool_module;
        mem = mpool->mpool_alloc(mpool, size, 0, &registration);
        ((mca_mpool_base_chunk_t *) item)->key.bottom = mem;
        ((mca_mpool_base_chunk_t *) item)->key.top = (void *) ((char *) mem + size - 1);
        ((mca_mpool_base_chunk_t *) item)->mpools[num_modules].mpool = mpool;
        ((mca_mpool_base_chunk_t *) item)->mpools[num_modules].user_data = has_reg_function[i]->user_data;
        ((mca_mpool_base_chunk_t *) item)->mpools[num_modules++].mpool_registration = registration;
        i++;
        num_modules++;
    }
    
    while(i < reg_module_num && MCA_MPOOL_BASE_MAX_REG > num_modules)
    {
        mca_mpool_base_module_t* mpool = has_reg_function[i]->mpool_module;
        if(OMPI_SUCCESS != mpool->mpool_register(mpool, mem, size, &registration))
        {
            if(info == &ompi_mpi_info_null)
            {
                continue;
            }
            ((mca_mpool_base_chunk_t *) item)->mpools[i].mpool = NULL;
            mca_mpool_base_free(mem);
            OMPI_FREE_LIST_RETURN(&mca_mpool_base_mem_list, item);
            free(has_reg_function);
            return NULL;
        }
        ((mca_mpool_base_chunk_t *) item)->mpools[num_modules].mpool = mpool;
        ((mca_mpool_base_chunk_t *) item)->mpools[num_modules].user_data = has_reg_function[i]->user_data;
        ((mca_mpool_base_chunk_t *) item)->mpools[num_modules].mpool_registration = registration;
        num_modules++;
        i++;
    }

    if(MCA_MPOOL_BASE_MAX_REG > num_modules)
    {
        ((mca_mpool_base_chunk_t *) item)->mpools[num_modules].mpool = NULL;
    }
    OMPI_THREAD_LOCK(&mca_mpool_base_tree_lock); 
    ompi_rb_tree_insert(&mca_mpool_base_tree, 
                        &((mca_mpool_base_chunk_t *)item)->key, item);
    OMPI_THREAD_UNLOCK(&mca_mpool_base_tree_lock); 
    free(has_reg_function);
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
    mca_mpool_base_chunk_t * chunk;
    int i = 0;
    int rc; 

    OMPI_THREAD_LOCK(&mca_mpool_base_tree_lock); 
    if(NULL == (chunk = mca_mpool_base_find_nl(base)))
    {
        OMPI_THREAD_UNLOCK(&mca_mpool_base_tree_lock); 
        return OMPI_ERR_BAD_PARAM;
    }

    /* if no special mpool was used to allocate the memory, call free */
    if(chunk->mpools[0].mpool == NULL)
    {
        free(chunk->key.bottom);
        OMPI_FREE_LIST_RETURN(&mca_mpool_base_mem_list, (opal_list_item_t*) chunk);
        rc = ompi_rb_tree_delete(&mca_mpool_base_tree, &chunk->key); 
        OMPI_THREAD_UNLOCK(&mca_mpool_base_tree_lock); 
        return rc;
    }

    while(MCA_MPOOL_BASE_MAX_REG > i && NULL != chunk->mpools[i].mpool) { i++; };

    i -= 1;
    for( ; i > 0; i--)
    {
        chunk->mpools[i].mpool->mpool_deregister(chunk->mpools[i].mpool, 
                                                 chunk->key.bottom, 
                                                 ((char *) chunk->key.top - (char *) chunk->key.bottom + 1), 
                                                 chunk->mpools[i].mpool_registration
                                                 );
    }
    chunk->mpools[i].mpool->mpool_free(chunk->mpools[i].mpool, chunk->key.bottom, chunk->mpools[i].mpool_registration);
    OMPI_FREE_LIST_RETURN(&mca_mpool_base_mem_list, (opal_list_item_t *) chunk);

    rc = ompi_rb_tree_delete(&mca_mpool_base_tree, &chunk->key); 
    OMPI_THREAD_UNLOCK(&mca_mpool_base_tree_lock); 
    return rc;
}

