/* Standard system includes */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Open MPI includes */

/* Other IB PTL includes */
#include "ptl_ib.h"
#include "ptl_ib_memory.h"
#include "ptl_ib_priv.h"

static void mca_ptl_ib_mem_registry_construct(ompi_object_t *object);
static void mca_ptl_ib_mem_registry_destruct(ompi_object_t *object);
static void mca_ptl_ib_mem_registry_info_construct(ompi_object_t *object);
static void mca_ptl_ib_mem_registry_info_destruct(ompi_object_t *object);

static int mca_ptl_ib_mem_registry_info_compare(void *key1, void *key2);

static int mca_ptl_ib_mem_registry_real_deregister(
    mca_ptl_ib_mem_registry_t *registry,
    mca_ptl_ib_mem_registry_info_t *info);

OBJ_CLASS_INSTANCE(mca_ptl_ib_mem_registry_info_t, ompi_list_item_t, 
    mca_ptl_ib_mem_registry_info_construct, mca_ptl_ib_mem_registry_info_destruct);
    
OBJ_CLASS_INSTANCE(mca_ptl_ib_mem_registry_t, ompi_rb_tree_t, mca_ptl_ib_mem_registry_construct,
    mca_ptl_ib_mem_registry_destruct);
 
static void mca_ptl_ib_mem_registry_construct(ompi_object_t *object)
{
    mca_ptl_ib_mem_registry_t *registry = (mca_ptl_ib_mem_registry_t *)object;
    int i;

    ompi_rb_tree_init(&(registry->rb_tree), mca_ptl_ib_mem_registry_info_compare);

    OBJ_CONSTRUCT(&(registry->info_free_list), ompi_free_list_t);
    ompi_free_list_init(&registry->info_free_list, sizeof(mca_ptl_ib_mem_registry_info_t),
        OBJ_CLASS(mca_ptl_ib_mem_registry_info_t), 32, -1, 32, NULL);

    registry->hints_log_size = mca_ptl_ib_component.ib_mem_registry_hints_log_size;
    /* sanity check -- enforce lower bound for hash calculation */
    if (registry->hints_log_size < 1) {
        registry->hints_log_size = 1;
    }

    registry->hints = (ompi_ptr_t *)malloc((1 << registry->hints_log_size) *
        sizeof(ompi_ptr_t));
    
    registry->hints_log_size = mca_ptl_ib_component.ib_mem_registry_hints_log_size;
    registry->hints_size = (registry->hints) ? (1 << registry->hints_log_size) : 0;
    for (i = 0; i < registry->hints_size; i++) {
        registry->hints[i].pval = (void *)NULL;
    }

    registry->ib_state = NULL;
    registry->evictable = NULL;

    return;
}

static void mca_ptl_ib_mem_registry_destruct(ompi_object_t *object)
{
    /* memory regions that are being tracked are not deregistered here */
    mca_ptl_ib_mem_registry_t *registry = (mca_ptl_ib_mem_registry_t *)object;
    OBJ_DESTRUCT(&(registry->info_free_list));
    if (registry->hints_size != 0) {
        free(registry->hints);
        registry->hints = (ompi_ptr_t *)NULL;
        registry->hints_size = 0;
    }
    return;
}

static void mca_ptl_ib_mem_registry_info_construct(ompi_object_t *object)
{
    mca_ptl_ib_mem_registry_info_t *info = (mca_ptl_ib_mem_registry_info_t *)object;
    info->next = NULL;
    info->ref_cnt = 0;
    info->hndl = VAPI_INVAL_HNDL;
    memset(&(info->request), 0, sizeof(VAPI_mr_t));
    memset(&(info->reply), 0, sizeof(VAPI_mr_t));
    return;
}

static void mca_ptl_ib_mem_registry_info_destruct(ompi_object_t *object)
{
    return;
}

static int mca_ptl_ib_mem_registry_info_compare(void *request, void *treenode)
{
    int result;
    VAPI_mr_t *mr1 = (VAPI_mr_t *)request;
    VAPI_mr_t *mr2 = (VAPI_mr_t *)treenode;
    uint64_t start1 = mr1->start;
    uint64_t start2 = mr2->start;
    uint64_t end1 = start1 + mr1->size;
    uint64_t end2 = start2 + mr2->size;

    if (end1 < start2) {
        /* non-overlapping mr1 < mr2 */
        result = -1;
    }
    else if (start1 > end2) {
        /* non-overlapping mr1 > mr2 */
        result = 1;
    }
    else if ((end1 <= end2) && (start1 >= start2)) {
        /* completely overlapping mr1 and mr2 (mr2 may be bigger) */
        if ((mr1->acl & mr2->acl) == mr1->acl) {
            /* minimum access permissions met */
            result = 0;
        }
        else {
            /* oops -- access permissions not good enough */
            result = 1;
        }
    }
    else if (start1 < start2) {
        /* partially overlapping mr1 < mr2 */
        result =  -1;
    }
    else {
        /* partially overlapping mr1 > mr2 */
        result = 1;
    }
    
    return result;
}

mca_ptl_ib_mem_registry_info_t *mca_ptl_ib_mem_registry_register(
    mca_ptl_ib_mem_registry_t *registry, VAPI_mr_t *mr)
{
    mca_ptl_ib_mem_registry_info_t *info = mca_ptl_ib_mem_registry_find(registry, mr);
    mca_ptl_ib_mem_registry_info_t *next_to_evict;
    ompi_list_item_t *item;
    VAPI_ret_t vapi_result;
    int rc;
    
    if (info == (mca_ptl_ib_mem_registry_info_t *)NULL) {
        /* create new entry and register memory region */
        item = (ompi_list_item_t *)info;
        OMPI_FREE_LIST_GET(&(registry->info_free_list), item, rc);
        info = (mca_ptl_ib_mem_registry_info_t *)item;
        if (OMPI_SUCCESS != rc) {
            /* error - return null pointer */
            return info;
        }
        memcpy(&(info->request),mr,sizeof(VAPI_mr_t));
        info->ref_cnt = 1;
        do {
            vapi_result = VAPI_register_mr(registry->ib_state->nic, mr, 
                &(info->hndl), &(info->reply));
            if (VAPI_OK != vapi_result) {
                if (VAPI_EAGAIN == vapi_result) {
                    /* evict an unused memory region, if at all possible */
                    if (NULL != registry->evictable) {
                        next_to_evict = registry->evictable->next;
                        mca_ptl_ib_mem_registry_real_deregister(registry, registry->evictable);
                        registry->evictable = next_to_evict;
                    }
                }
                else {
                    /* fatal error */
                    item = (ompi_list_item_t *)info;
                    OMPI_FREE_LIST_RETURN(&(registry->info_free_list), item);
                    info = NULL;
                    return info;
                }
            }
        } while ((VAPI_OK != vapi_result) && (NULL != info));
        /* insert a reference to this information into the red/black tree */
        rc = ompi_rb_tree_insert(&(registry->rb_tree), &(info->reply), info);
        /* aargh! what do we do if the tree insert fails... */
        mca_ptl_ib_mem_registry_insert_hint(registry, &(info->reply), info);
    }
    else {
        (info->ref_cnt)++;
    }
    
    return info;
}

mca_ptl_ib_mem_registry_info_t *mca_ptl_ib_register_mem_with_registry(
    mca_ptl_ib_state_t *ib_state,
    void *addr, size_t len)
{
    mca_ptl_ib_mem_registry_info_t *info;
    VAPI_mr_t mr;

    mr.acl = VAPI_EN_LOCAL_WRITE | VAPI_EN_REMOTE_WRITE;
    mr.l_key = 0;
    mr.r_key = 0;
    mr.pd_hndl = ib_state->ptag;
    mr.size = len;
    mr.start = (VAPI_virt_addr_t) (MT_virt_addr_t) addr;
    mr.type = VAPI_MR;

    info = mca_ptl_ib_mem_registry_register(&(ib_state->mem_registry),&mr);
    return info;
}

int mca_ptl_ib_deregister_mem_with_registry(
    mca_ptl_ib_state_t *ib_state,
    void *addr, size_t len)
{
    VAPI_mr_t mr;
    int rc;

    mr.acl = VAPI_EN_LOCAL_WRITE | VAPI_EN_REMOTE_WRITE;
    mr.l_key = 0;
    mr.r_key = 0;
    mr.pd_hndl = ib_state->ptag;
    mr.size = len;
    mr.start = (VAPI_virt_addr_t) (MT_virt_addr_t) addr;
    mr.type = VAPI_MR;

    rc =  mca_ptl_ib_mem_registry_deregister(&(ib_state->mem_registry),&mr);
    return rc;
}

static int mca_ptl_ib_mem_registry_real_deregister(
    mca_ptl_ib_mem_registry_t *registry,
    mca_ptl_ib_mem_registry_info_t *info)
{
    ompi_list_item_t *item;
    VAPI_ret_t vapi_result;
    int i;

    /* clear hints array of references to this info object */
    for (i = 0; i < registry->hints_size; i++) {
        if (registry->hints[i].pval == info) {
            registry->hints[i].pval = (void *)NULL;
        }
    }
    /* delete the info object from the red/black tree */
    ompi_rb_tree_delete(&(registry->rb_tree), &(info->reply));
    /* do the real deregistration */
    vapi_result = VAPI_deregister_mr(registry->ib_state->nic, info->hndl); 
    /* return the info object to the free list */
    item = (ompi_list_item_t *)info;
    OMPI_FREE_LIST_RETURN(&(registry->info_free_list), item);
    /* return an error if we could not successfully deregister memory region */
    if (VAPI_OK != vapi_result) {
        return OMPI_ERROR;
    }
    return OMPI_SUCCESS;
}

int mca_ptl_ib_mem_registry_deregister(
    mca_ptl_ib_mem_registry_t *registry, VAPI_mr_t *mr)
{
    mca_ptl_ib_mem_registry_info_t *info = mca_ptl_ib_mem_registry_find(registry, mr);

    if (info != NULL) {
        if (info->ref_cnt > 0) {
            (info->ref_cnt)--;
            if (0 == info->ref_cnt) {
                info->next = registry->evictable;
                registry->evictable = info;
            }
        }
    }
    else {
        return OMPI_ERR_NOT_FOUND;
    }

    return OMPI_SUCCESS;
}

void mca_ptl_ib_mem_registry_init(
    mca_ptl_ib_mem_registry_t *registry,
    mca_ptl_ib_state_t *ib_state) 
{
    registry->ib_state = ib_state;
    return;
}
