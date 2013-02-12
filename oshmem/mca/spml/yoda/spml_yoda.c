/*
 * Copyright (c) 2012      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "oshmem_config.h"

#include "orte/include/orte/types.h"
#include "orte/runtime/orte_globals.h"

#include "opal/datatype/opal_convertor.h"

#include "ompi/datatype/ompi_datatype.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/btl/btl.h"
#include "ompi/mca/btl/base/base.h"
#include "ompi/mca/btl/openib/btl_openib_endpoint.h"
#include "ompi/mca/btl/sm/btl_sm_frag.h"

#include "oshmem/proc/proc.h"
#include "oshmem/mca/memheap/memheap.h"
#include "oshmem/mca/memheap/base/base.h"
#include "oshmem/mca/spml/spml.h"

#include "spml_yoda.h"
#include "spml_yoda_putreq.h"
#include "spml_yoda_getreq.h"
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#define ILLEGAL_ORDER -1
#include "oshmem/runtime/runtime.h"
mca_spml_yoda_module_t mca_spml_yoda = {
    {
        /* Init mca_spml_base_module_t */
        mca_spml_yoda_add_procs,
        mca_spml_yoda_del_procs,
        mca_spml_yoda_enable,
        mca_spml_yoda_register,
        mca_spml_yoda_deregister,
        mca_spml_base_oob_get_mkeys,
        mca_spml_yoda_put,
        mca_spml_yoda_put_nb,
        mca_spml_yoda_get,
        mca_spml_yoda_recv,
        mca_spml_yoda_send,
        mca_spml_base_wait,
        mca_spml_base_wait_nb,
        mca_spml_yoda_fence
    }
};

#ifdef OSHMEM_WAIT_COMPLETION_DEBUG
char **op_type_dbg;//[OSHMEM_WAIT_COMPLETION_DEBUG][20];
char **btl_name_dbg;//[OSHMEM_WAIT_COMPLETION_DEBUG][20];
int pe_dst_dbg[OSHMEM_WAIT_COMPLETION_DEBUG];
int msg_length_dbg[OSHMEM_WAIT_COMPLETION_DEBUG];
uint64_t src_dbg[OSHMEM_WAIT_COMPLETION_DEBUG];
uint64_t dst_dbg[OSHMEM_WAIT_COMPLETION_DEBUG];
static void save_dbg_history(char op_type[], char btl_name[], int pe_dst, int msg_length, void *src, void *dst)
{
    int i;
    for (i=0; i<OSHMEM_WAIT_COMPLETION_DEBUG-1; i++)
    {
        pe_dst_dbg[i]=pe_dst_dbg[i+1];
        msg_length_dbg[i]=msg_length_dbg[i+1];
        src_dbg[i]=src_dbg[i+1];
        dst_dbg[i] = dst_dbg[i+1];
        strcpy(op_type_dbg[i],op_type_dbg[i+1]);
        strcpy(btl_name_dbg[i],btl_name_dbg[i+1]);
    }
    pe_dst_dbg[OSHMEM_WAIT_COMPLETION_DEBUG-1] = pe_dst;
    msg_length_dbg[OSHMEM_WAIT_COMPLETION_DEBUG-1] = msg_length;
    src_dbg[OSHMEM_WAIT_COMPLETION_DEBUG-1] = (uint64_t)src;
    dst_dbg[OSHMEM_WAIT_COMPLETION_DEBUG-1] = (uint64_t)dst;
    strcpy(op_type_dbg[OSHMEM_WAIT_COMPLETION_DEBUG-1],op_type);
    strcpy(btl_name_dbg[OSHMEM_WAIT_COMPLETION_DEBUG-1],btl_name);
}
void condition_dbg_init(void)
{
    int i;
    op_type_dbg = (char **)malloc(sizeof(char*)*OSHMEM_WAIT_COMPLETION_DEBUG);
    btl_name_dbg = (char **)malloc(sizeof(char*)*OSHMEM_WAIT_COMPLETION_DEBUG);
    for (i=0; i<OSHMEM_WAIT_COMPLETION_DEBUG; i++){
        op_type_dbg[i] = (char *)malloc(sizeof(char)*20);
        btl_name_dbg[i] = (char *)malloc(sizeof(char)*20);
    }
    oshmem_request_cond.my_pe = oshmem_my_proc_id();
    oshmem_request_cond.pe_dest = pe_dst_dbg;
    oshmem_request_cond.msg_length = msg_length_dbg;
    oshmem_request_cond.btl_name = btl_name_dbg;
    oshmem_request_cond.op_name = op_type_dbg;
    oshmem_request_cond.src = (uint64_t *)src_dbg;
    oshmem_request_cond.dst = (uint64_t *)dst_dbg;
}

void condition_dbg_finalize(void)
{
    int i;
    for (i=0; i<OSHMEM_WAIT_COMPLETION_DEBUG; i++){
        if (NULL != op_type_dbg[i])
            free(op_type_dbg[i]);
        if (NULL != btl_name_dbg[i])
            free(btl_name_dbg[i]);
    }
    if (NULL != op_type_dbg)
        free(op_type_dbg);
    if (NULL != btl_name_dbg)
        free(btl_name_dbg);
}
#endif

static int btl_name_to_id(char *btl_name)
{
    if (0 == strcmp(btl_name, "sm")) {
        return YODA_BTL_SM;
    }
    else if (0 == strcmp(btl_name, "openib")) {
        return YODA_BTL_OPENIB;
    }
    else if (0 == strcmp(btl_name, "self")) {
        return YODA_BTL_SELF;
    }
    return YODA_BTL_UNKNOWN;
}

static char *btl_type2str(int btl_type)
{
    switch(btl_type) {
        case YODA_BTL_UNKNOWN:
            return "unknown btl";
        case YODA_BTL_SELF:
            return "self";
        case YODA_BTL_OPENIB:
            return "openib";
        case YODA_BTL_SM:
            return "sm";
    }
    return "bad_btl_type";
}

/**
 * note: we have to reg memory directly with btl because no proc will have a full btl list in proc_bml
 */
int mca_spml_yoda_deregister(mca_spml_mkey_t *mkeys)
{
    int i;
    struct yoda_btl *ybtl;

    if (!mkeys)
        return OSHMEM_SUCCESS;

    for(i = 0; i < mca_spml_yoda.n_btls; i++) { 
        ybtl = &mca_spml_yoda.btl_type_map[i];
        if (mkeys[i].spml_context) {
            ybtl->btl->btl_free(ybtl->btl, mkeys[i].spml_context);
        }
    }
    free(mkeys);

    return OSHMEM_SUCCESS;
}

mca_spml_mkey_t *mca_spml_yoda_register(
        void* addr,
        size_t size,
        uint64_t shmid,
        int *count
        )
{
    int i;
    mca_btl_base_descriptor_t* des = NULL;
    const opal_datatype_t *datatype = &opal_datatype_wchar;
    opal_convertor_t convertor;
    mca_spml_mkey_t *mkeys;
    struct yoda_btl *ybtl;
    oshmem_proc_t *proc_self;

    SPML_VERBOSE(10, "address %p len %llu", addr, (unsigned long long)size);
    *count = 0;
    /* make sure everything is initialized to 0 */
    mkeys = (mca_spml_mkey_t *)calloc(1, mca_spml_yoda.n_btls * sizeof(*mkeys));
    if(!mkeys){
        return NULL;
    }

    proc_self = oshmem_proc_group_find(oshmem_group_all, oshmem_my_proc_id());
    /* create convertor */
    OBJ_CONSTRUCT(&convertor, opal_convertor_t);

    /* Register proc memory in every rdma BTL. */
    for(i = 0; i < mca_spml_yoda.n_btls; i++) { 

        ybtl = &mca_spml_yoda.btl_type_map[i];
        if (!ybtl->use_cnt) {
            SPML_VERBOSE(10, "%s: present but not in use. SKIP registration", btl_type2str(ybtl->btl_type));
            continue;
        }

        /* initialize convertor */
        opal_convertor_copy_and_prepare_for_recv(proc_self->proc_convertor,
                datatype,
                size,
                addr,
                0,
                &convertor);

        switch (ybtl->btl_type) {
            case YODA_BTL_SM:
                /* shadow sm btl */
                if ((int)MEMHEAP_SHM_GET_ID(shmid) != MEMHEAP_SHM_INVALID) {
                    mkeys[i].key = shmid;
                    mkeys[i].spml_context = 0;
                    mkeys[i].va_base = 0; /* memory must be shmat'ed localy upon reception of rkey */ 
                }
                else {
                    des = ybtl->btl->btl_prepare_src(ybtl->btl,
                            0, 
                            NULL, &convertor, MCA_BTL_NO_ORDER,
                            0, &size, 0);
                    if (NULL == des) {
                        SPML_ERROR("%s: failed to register memory. ", btl_type2str(ybtl->btl_type));
                        goto err;
                    }
                    
                    mkeys[i].key = ((mca_btl_sm_segment_t *)des->des_src)->key;
                    mkeys[i].spml_context = des;
                    mkeys[i].va_base = (unsigned long)addr;
                }
                break;

            case YODA_BTL_OPENIB:
                des = ybtl->btl->btl_prepare_dst(ybtl->btl,
                        0,
                        NULL, &convertor, MCA_BTL_NO_ORDER,
                        0, &size, 0);
                if (NULL == des) {
                    SPML_ERROR("%s: failed to register memory. ", btl_type2str(ybtl->btl_type));
                    goto err;
                }

                mkeys[i].ib.rkey = ((mca_btl_openib_segment_t *)des->des_dst)->key;
                mkeys[i].ib.lkey = ((mca_btl_openib_segment_t *)des->des_dst)->lkey;
                mkeys[i].spml_context = des;
                mkeys[i].va_base = (unsigned long)addr;
                break;

            case YODA_BTL_SELF:
                SPML_VERBOSE(10, "self btl - doing nothing");
                mkeys[i].key = 0;
                mkeys[i].spml_context = 0;
                mkeys[i].va_base = (unsigned long)addr;  
                break;

            default:
                SPML_ERROR("unsupported btl: %d\n", ybtl->btl_type);
                goto err;
        }

        SPML_VERBOSE(5,"rank %d btl %s rkey %x lkey %x key %llx address 0x%llX len %llu shmid 0x%X|0x%X", 
                oshmem_proc_local_proc->proc_name.vpid, 
                btl_type2str(ybtl->btl_type), 
                mkeys[i].ib.rkey,
                mkeys[i].ib.lkey,
                (unsigned long long)mkeys[i].key,
                (unsigned long long)mkeys[i].va_base, 
                (unsigned long long)size,
				MEMHEAP_SHM_GET_TYPE(shmid), MEMHEAP_SHM_GET_ID(shmid)
                );
    }
    OBJ_DESTRUCT(&convertor);
    *count = mca_spml_yoda.n_btls; 
    return mkeys;

err:
    mca_spml_yoda_deregister(mkeys);
    return NULL;
}



/*
 * For each proc setup a datastructure that indicates the BTLs
 * that can be used to reach the destination.
 */
static void mca_spml_yoda_error_handler(
        struct mca_btl_base_module_t* btl, int32_t flags,
        ompi_proc_t* errproc, char* btlinfo ) { 
    oshmem_shmem_abort(-1);
}

/*  make global btl list&map */
static int create_btl_list(void)
{
    int btl_id;
    char *btl_name;
    int size;
    opal_list_item_t *item;
    mca_btl_base_selected_module_t *btl_sm;
    int i;

    size = opal_list_get_size(&mca_btl_base_modules_initialized);
    if (0 >= size) {
        SPML_ERROR("no btl(s) available");
        return OSHMEM_ERROR;
    }
    SPML_VERBOSE(50, "found %d capable btls", size);

    mca_spml_yoda.btl_type_map = (struct yoda_btl *)calloc(size, sizeof(struct yoda_btl));
    if (! mca_spml_yoda.btl_type_map) 
        return OSHMEM_ERROR;

    mca_spml_yoda.n_btls = 0;
    for (i = 0, item = opal_list_get_first(&mca_btl_base_modules_initialized) ;
            item != opal_list_get_end(&mca_btl_base_modules_initialized) ;
            item = opal_list_get_next(item), i++) {

        btl_sm = (mca_btl_base_selected_module_t *)item;
        btl_name = btl_sm->btl_component->btl_version.mca_component_name;
        btl_id = btl_name_to_id(btl_name);

        SPML_VERBOSE(50, "found btl (%s) btl_id=%d", btl_name, btl_id);
        if (YODA_BTL_UNKNOWN == btl_id) {
            SPML_VERBOSE(5, "unknown btl: %s btl_id=%d", btl_name, btl_id);
            continue;
        }
        mca_spml_yoda.btl_type_map[mca_spml_yoda.n_btls].btl = btl_sm->btl_module;
        mca_spml_yoda.btl_type_map[mca_spml_yoda.n_btls].btl_type = btl_id;
        mca_spml_yoda.n_btls++;
    }

    if (0 == mca_spml_yoda.n_btls) {
        SPML_ERROR("can not find any suitable btl");
        return OSHMEM_ERROR;
    }

    return OSHMEM_SUCCESS;
}

static int _find_btl_id(mca_bml_base_btl_t *bml_btl)
{
    int i;

    for (i = 0; i < mca_spml_yoda.n_btls; i++) {
        if (mca_spml_yoda.btl_type_map[i].btl == bml_btl->btl)
            return i;
    }
    return -1;
}

/* for each proc create transport ids which are indexes into global
 * btl list&map 
 */
static int create_btl_idx(int dst_pe)
{
    oshmem_proc_t *proc;
    int btl_id;
    mca_bml_base_endpoint_t* endpoint;
    mca_bml_base_btl_t* bml_btl = 0;
    int i, size;
    mca_bml_base_btl_array_t *btl_array;
    int sm_index = -1;

    proc = oshmem_proc_group_find(oshmem_group_all, dst_pe);
    endpoint = (mca_bml_base_endpoint_t*)proc->proc_bml;
    assert(endpoint);
    size = mca_bml_base_btl_array_get_size(btl_array = &endpoint->btl_rdma);

    if (0 >= size) {
        int is_sm_btl = 0;
        //Possibly this is SM BTL with KNEM disabled? Then we should use send based get/put
        /* 
            This hack is necessary for the case when KNEM is not available.
            In this case we still want to use send/recv of SM BTL for put and get
            but SM BTL is not in the rdma list anymore
        */
        size = mca_bml_base_btl_array_get_size(btl_array = &endpoint->btl_eager);
        if (size > 0) {
            int btl_id = -1;
            for (sm_index = 0; sm_index < size; sm_index++) {
                bml_btl = mca_bml_base_btl_array_get_index(btl_array, sm_index);
                btl_id = _find_btl_id(bml_btl);
                is_sm_btl = (btl_id != -1) && ((mca_spml_yoda.btl_type_map[btl_id].btl_type == YODA_BTL_SM) || (btl_id == YODA_BTL_OPENIB));
                if (is_sm_btl) {
                    size = 1;
                    break;
                }
            }
        }
        if (!is_sm_btl) {
            SPML_ERROR("no RDMA capable transport for dest pe=%d", dst_pe);
            return OSHMEM_ERROR;
        }
    }

    proc->transport_ids = (char *)malloc(size * sizeof(char));
    if (!proc->transport_ids) 
        return OSHMEM_ERROR;

    proc->num_transports = size;

    for (i = 0; i < size; i++) {
        bml_btl = mca_bml_base_btl_array_get_index(btl_array, (sm_index>=0)?(sm_index):(i));
        btl_id = _find_btl_id(bml_btl);
        SPML_VERBOSE(50, "dst_pe(%d) use btl (%s) btl_id=%d", 
                dst_pe, 
                bml_btl->btl->btl_component->btl_version.mca_component_name, btl_id);
        if (btl_id < 0) {
            SPML_ERROR("unknown btl: dst_pe(%d) use btl (%s) btl_id=%d",
                    dst_pe,
                    bml_btl->btl->btl_component->btl_version.mca_component_name, btl_id);
            return OSHMEM_ERROR;
        }
        proc->transport_ids[i] = btl_id;
        mca_spml_yoda.btl_type_map[btl_id].use_cnt++;
    }
    return OSHMEM_SUCCESS;
}

static int destroy_btl_list(void)
{
    if (mca_spml_yoda.btl_type_map)
        free(mca_spml_yoda.btl_type_map);

    return OSHMEM_SUCCESS;
}


static int destroy_btl_idx(int dst_pe)
{
    oshmem_proc_t *proc;

    proc = oshmem_proc_group_find(oshmem_group_all, dst_pe);
    if (proc->transport_ids)
        free(proc->transport_ids);

    return OSHMEM_SUCCESS;
}

int mca_spml_yoda_add_procs(oshmem_proc_t** procs, size_t nprocs)
{
    opal_bitmap_t reachable;
    int rc;
    size_t i;

    if(nprocs == 0)
        return OSHMEM_SUCCESS;

    /* we don't have any endpoint data we need to cache on the
     * oshmem_proc_t, so set proc_spml to NULL */
    for (i = 0 ; i < nprocs ; ++i) {
        procs[i]->proc_pml = NULL; 
    }

    OBJ_CONSTRUCT(&reachable, opal_bitmap_t);
    rc = opal_bitmap_init(&reachable, (int)nprocs);
    if(OSHMEM_SUCCESS != rc)
        return rc;

    rc = mca_bml.bml_add_procs( nprocs, 
            (ompi_proc_t**)procs, 
            &reachable );

    if(OSHMEM_SUCCESS != rc){
        SPML_ERROR("SPML YODA: shmem error\n");
        goto cleanup_and_return;
    }

    rc = mca_bml.bml_register_error(mca_spml_yoda_error_handler);
    if(OMPI_SUCCESS != rc)
        goto cleanup_and_return;

    /* create btl index and map */
    rc = create_btl_list();
    if(OSHMEM_SUCCESS != rc)
        goto cleanup_and_return;

    for (i = 0; i < nprocs; i++) {
        rc = create_btl_idx(i);
        if (OSHMEM_SUCCESS != rc)
            goto cleanup_and_return;
    }

cleanup_and_return:
    OBJ_DESTRUCT(&reachable);

    return rc;
}


int mca_spml_yoda_del_procs(oshmem_proc_t** procs, size_t nprocs)
{
    size_t i;

    mca_bml.bml_del_procs(nprocs, (ompi_proc_t**)procs); 
    for (i = 0; i < nprocs; i++) {
        destroy_btl_idx(i);
    }
    destroy_btl_list();

    return OSHMEM_SUCCESS;
}

static inline mca_bml_base_btl_t *get_next_btl(int dst, int *btl_id)
{
    mca_bml_base_endpoint_t* endpoint;
    mca_bml_base_btl_t* bml_btl;
    oshmem_proc_t *proc;
    mca_bml_base_btl_array_t *btl_array = 0;
    int size = 0;
    int sm_index = 0;

    /* get endpoint and btl */
    proc = oshmem_proc_group_all(dst);
    if (!proc) {
        SPML_ERROR("Can not find destination proc for pe=%d", dst);
        return NULL;
    }

    endpoint = (mca_bml_base_endpoint_t*)proc->proc_bml;
    if (!endpoint) {
        SPML_ERROR("pe=%d proc has no endpoint", dst);
        return NULL;
    }
        
    /* At the moment always return first transport */
    size = mca_bml_base_btl_array_get_size(btl_array = &endpoint->btl_rdma);

    if (0 >= size) {
        int is_sm_btl = 0;
        //Possibly this is SM BTL with KNEM disabled? Then we should use send based get/put
        /* 
            This hack is necessary for the case when KNEM is not available.
            In this case we still want to use send/recv of SM BTL for put and get
            but SM BTL is not in the rdma list anymore
        */
        size = mca_bml_base_btl_array_get_size(btl_array = &endpoint->btl_eager);
        if (size > 0) {
            int btl_id = -1;
            for (sm_index = 0; sm_index < size; sm_index++) { 
                bml_btl = mca_bml_base_btl_array_get_index(btl_array, sm_index);
                btl_id = _find_btl_id(bml_btl);
                is_sm_btl = (btl_id != -1) && ((mca_spml_yoda.btl_type_map[btl_id].btl_type == YODA_BTL_SM) || (btl_id == YODA_BTL_OPENIB));
                if (is_sm_btl) {
                    size = 1;
                    break;
                }
            }
        }
        if (!is_sm_btl) {
            SPML_ERROR("no RDMA capable transport for dest pe=%d", dst);
            oshmem_shmem_abort(-1);
        }
    }

    bml_btl = mca_bml_base_btl_array_get_index(btl_array, sm_index);
    *btl_id = proc->transport_ids[0];
#if 0
    assert(*btl_id >= 0 && *btl_id < YODA_BTL_MAX);
    SPML_VERBOSE(100, "pe=%d reachable via btl %s %d", dst, 
            bml_btl->btl->btl_component->btl_version.mca_component_name, *btl_id);
#endif
    return bml_btl;
}

static inline void calc_nfrags(mca_bml_base_btl_t* bml_btl, size_t size, unsigned *frag_size, int *nfrags)
{
    *frag_size = bml_btl->btl->btl_max_send_size;
    *nfrags = 1+(size-1)/(*frag_size);
}



static inline int mca_spml_yoda_put_internal(void *dst_addr, size_t size, void *src_addr, int dst, int is_nb)
{
    int rc = OSHMEM_SUCCESS;
    mca_spml_yoda_put_request_t *putreq = NULL;
    mca_bml_base_btl_t* bml_btl;
    mca_btl_base_descriptor_t* des = NULL;
    mca_btl_base_segment_t* segment;
    mca_spml_yoda_rdma_frag_t* frag; 
    int nfrags, need_copy;
    int i;
    unsigned ncopied = 0;
    unsigned frag_size = 0;
    char *p_src, *p_dst;

    uint64_t rva;
    uint64_t offset = 0;

    mca_spml_mkey_t *r_mkey;
    uint64_t rkey;
    mca_spml_mkey_t *l_mkey;
    uint32_t lkey;
    int btl_id = 0;
    struct yoda_btl *ybtl;


    /* find bml_btl and its global btl_id */
    bml_btl = get_next_btl(dst, &btl_id);
    if (!bml_btl) oshmem_shmem_abort(-1);

    /* Get rkey of remote PE (dst proc) which must be on memheap  */
    r_mkey = mca_memheap.memheap_get_cached_mkey(dst, (unsigned long)dst_addr, btl_id, &rva);
    if(!r_mkey) {
        SPML_ERROR("pe=%d: %p is not address of shared variable", dst, dst_addr);
        oshmem_shmem_abort(-1);
        return OSHMEM_ERROR;
    }

#if 0
    SPML_VERBOSE(100, "put: pe:%d dst=%p <- src: %p sz=%d. dst_rva=%p, dst_rkey=0x%lx", 
            dst, dst_addr, src_addr, (int)size, (void *)rva, r_mkey->key);
#endif


    ybtl = &mca_spml_yoda.btl_type_map[btl_id];

    /* check if source is on memheap. If not: 
       - copy it to temporary space. Temporary buffer will be allocated from BML.
       - check if we need to split source in part and send it with several
       put operations 
     */  


    nfrags = 1;
    need_copy = 0;
    rkey = 0;
    lkey = 0;

    switch(ybtl->btl_type) {
    case YODA_BTL_OPENIB:
        l_mkey = mca_memheap.memheap_get_local_mkey((unsigned long)src_addr, btl_id);
        if ((!is_nb && size > (size_t)mca_btl_openib_rdma_inline_size(bml_btl)) || l_mkey == 0) {
            need_copy = 1;
            calc_nfrags(bml_btl, size, &frag_size, &nfrags);
        }
        else
            lkey = l_mkey->ib.lkey;
        rkey = r_mkey->ib.rkey;
        break;

    case YODA_BTL_SM:
        /* check if we doing put into shm attached segment and if so 
         * just do memcpy
         */
        if (OPAL_LIKELY(mca_memheap.memheap_is_symmetric_addr((unsigned long)dst_addr) && (unsigned long)dst_addr != rva)) {
            // todo: may need to run opal progress from time to time
            memcpy((void *)(unsigned long)rva, src_addr, size);
            return OSHMEM_SUCCESS;
        }

        offset = mca_memheap.memheap_find_offset(dst, btl_id, (unsigned long)dst_addr, rva);
        rkey = r_mkey->key;
        
        /* TODO: 
         * 1. KNEM does not support blocking mode (need_copy = 1) (see OSHMEM_SM_PUT_SYNC_MODE to enable it)
         * 2. SM(w/o KNEM) does not support non-blocking mode (need_copy = 0)
         */
        if ((size < mca_spml_yoda.knem_threshold) || (!mca_spml_yoda.use_knem)) {
            need_copy = 1;
            calc_nfrags(bml_btl, size, &frag_size, &nfrags);
        }
        break;

    case YODA_BTL_SELF:
        /* self btl ignores rkey/lkey */
        break;
    default:
        SPML_ERROR("btl %d bad btl type %d", btl_id, ybtl->btl_type);
        oshmem_shmem_abort(-1);
    }


    p_src = (char*)src_addr;
    p_dst = (char*)(unsigned long)rva; //dst_addr;

    for (i = 0; i < nfrags; i++) {

        /* Allocating send request from free list */
        putreq = mca_spml_yoda_putreq_alloc(dst);
        frag = &putreq->put_frag;

        /* ToDo:
         * - allocate buffer
         * - memcopy data if needed
         * - call to put with SEND descriptor.
         */

        ncopied = i < nfrags - 1 ? frag_size : (char *)src_addr + size - p_src;

        /* allocate buffer */
        mca_bml_base_alloc(bml_btl, &des,
                MCA_BTL_NO_ORDER,
                need_copy ? ncopied : 0, /* hack: allocate dummy segment if we are sending from symmetric heap */
                MCA_BTL_DES_SEND_ALWAYS_CALLBACK);

        if (OPAL_UNLIKELY(!des)) {
            SPML_ERROR("shmem OOM error need %d bytes", ncopied);
            SPML_ERROR("src=%p nfrags = %d need_copy = %d frag_size=%d", src_addr, nfrags, need_copy, frag_size);
            oshmem_shmem_abort(-1);
        }

        assert(NULL != des);
        assert(NULL != des->des_src);

        if (need_copy) {
            segment = des->des_src;
            memcpy((IOVBASE_TYPE*)((unsigned char*)segment->seg_addr.pval), p_src, ncopied);
            p_src += ncopied;
            frag->btl_seg = 0;
        }
        else {
            frag->btl_seg = des->des_src;
            frag->rdma_segs[1].base_seg.seg_addr.pval = (void*)p_src;
            frag->rdma_segs[1].base_seg.seg_len       = ncopied;

            /* Get lkey value of the symmetric heap */
            if (ybtl->btl_type == YODA_BTL_OPENIB)
            {
                frag->rdma_segs[1].openib_seg.lkey = lkey;
            }
            des->des_src = &frag->rdma_segs[1].base_seg;
        }

        frag->rdma_segs[0].base_seg.seg_addr.lval = (uintptr_t)p_dst;
        frag->rdma_segs[0].base_seg.seg_len       = ncopied;
        if (ybtl->btl_type == YODA_BTL_SM)
        {
            frag->rdma_segs[0].sm_seg.key = rkey;
        }
        else if (ybtl->btl_type == YODA_BTL_OPENIB)
        {
            frag->rdma_segs[0].openib_seg.key = (uint32_t)rkey;
        }
        des->des_dst = &frag->rdma_segs[0].base_seg;
        frag->rdma_req = putreq;

        p_dst += ncopied;

        des->des_cbdata = frag;
        des->des_cbfunc = mca_spml_yoda_put_completion;

        OPAL_THREAD_ADD32(&mca_spml_yoda.n_active_puts, 1);
        /* put the data to remote side */
        if (OPAL_UNLIKELY(YODA_BTL_SM == ybtl->btl_type)) {
            /* in the case of SM BTL we may use both: PUT via KNEM and SEND via shared memory fifos.
             * Choosing depending on the threshold */  
            if ((size < mca_spml_yoda.knem_threshold) || (!mca_spml_yoda.use_knem)) {
                /*
                 * This ugly hack is done to support following configuration as:
                 * OSHMEM + SM => put/get for small messages using send()
                 */
                rc = mca_bml_base_send(bml_btl, des, BTL_SM_HDR_TYPE_PUT_AS_SEND);
                if (1 == rc) rc = OSHMEM_SUCCESS;
            }
            else {
                ((mca_btl_sm_segment_t *) des->des_src)->key = offset ; //des_src is not used in SM KNEM put, so we pass offset through key64 field
                rc = mca_bml_base_put(bml_btl, des);
            }
        }
        else {
            rc = mca_bml_base_put(bml_btl, des);
        }

#ifdef OSHMEM_WAIT_COMPLETION_DEBUG
        if (YODA_BTL_SM == ybtl->btl_type) {
            oshmem_request_cond.puts_counter_sm++;
        }
        else if (YODA_BTL_OPENIB == ybtl->btl_type) {
            oshmem_request_cond.puts_counter_openib++;
        }
        save_dbg_history("PUT", btl_type2str(ybtl->btl_type), dst, size, src_addr, dst_addr);
        //condition_dbg_init();
#endif
        if( OPAL_UNLIKELY(OSHMEM_SUCCESS != rc) ) {
            if(OSHMEM_ERR_OUT_OF_RESOURCE == rc) {
                /* No free resources, Block on completion here */
                SPML_ERROR("shmem error: OSHMEM_ERR_OUT_OF_RESOURCE");
                oshmem_request_wait_completion(&putreq->req_put.req_base.req_oshmem);
            } else {
                SPML_ERROR("shmem error");
            }
            /* exit with errro */
            SPML_ERROR("shmem error: ret = %i, send_pe = %i, dest_pe = %i",rc, oshmem_my_proc_id(),dst);
            oshmem_shmem_abort(-1);
            rc = OSHMEM_ERROR;
        }
    }

    return rc;
}

int mca_spml_yoda_put(void *dst_addr, size_t size, void *src_addr, int dst)
{

    return mca_spml_yoda_put_internal(dst_addr, size, src_addr, dst, 0);
}

int mca_spml_yoda_put_nb(void* dst_addr, size_t size, void* src_addr, int dst, void **handle)
{
    UNREFERENCED_PARAMETER(handle);

    return mca_spml_yoda_put_internal(dst_addr, size, src_addr, dst, 1);
}


int mca_spml_yoda_fence(void)
{

    while (0 < mca_spml_yoda.n_active_puts) { 
        oshmem_request_wait_any_completion();
    }
    return OSHMEM_SUCCESS;
}




int mca_spml_yoda_enable(bool enable)
{
    if( false == enable ) {
        return OSHMEM_SUCCESS;
    }

    OBJ_CONSTRUCT(&mca_spml_yoda.lock, opal_mutex_t);

    /**
     *If we get here this is the SPML who get selected for the run. We
     * should get ownership for the put and get requests list, and
     * initialize them with the size of our own requests.
     */

    ompi_free_list_init_new( &mca_spml_base_put_requests,
            sizeof(mca_spml_yoda_put_request_t),
            opal_cache_line_size,
            OBJ_CLASS(mca_spml_yoda_put_request_t),
            0,opal_cache_line_size,
            mca_spml_yoda.free_list_num,
            mca_spml_yoda.free_list_max,
            mca_spml_yoda.free_list_inc,
            NULL );

    ompi_free_list_init_new( &mca_spml_base_get_requests,
            sizeof(mca_spml_yoda_get_request_t),
            opal_cache_line_size,
            OBJ_CLASS(mca_spml_yoda_get_request_t),
            0,opal_cache_line_size,
            mca_spml_yoda.free_list_num,
            mca_spml_yoda.free_list_max,
            mca_spml_yoda.free_list_inc,
            NULL );

    mca_spml_yoda.enabled = true;

#ifdef OSHMEM_WAIT_COMPLETION_DEBUG
    condition_dbg_init();
#endif

return OSHMEM_SUCCESS;
}



/**
 * shmem_get reads data from a remote address
 * in the symmetric heap via RDMA READ.
 * Get operation:
 * 1. Get the rkey to the remote address. 
 * 2. Allocate a get request.
 * 3. Allocated a temporary pre-registered buffer
 *    to copy the data to.
 * 4. Init the request descriptor with remote side
 *    data and local side data.
 * 5. Read the remote buffer to a pre-registered
 *    buffer on the local PE using RDMA READ.
 * 6. Copy the received data to dst_addr if an
 *    intermediate pre-register buffer was used.
 * 7. Clear the request and return.
 *
 * src_addr - address on remote pe.
 * size - the amount on bytes to be read.
 * dst_addr - address on the local pe. 
 * src - the pe of remote process.
 */
int mca_spml_yoda_get(
        void* src_addr,
        size_t size,
        void* dst_addr,
        int src)
{
    int rc = OSHMEM_SUCCESS;
    uint64_t rkey;
    uint32_t lkey;
    mca_spml_mkey_t *r_mkey, *l_mkey;
    uint64_t rva;
    uint64_t offset = 0;
    unsigned ncopied = 0;
    unsigned frag_size = 0;
    char *p_src, *p_dst;
    int i;
    int nfrags, need_copy;
    mca_spml_yoda_get_request_t* getreq = NULL;
    mca_bml_base_btl_t* bml_btl = NULL;                 
    mca_btl_base_descriptor_t* des = NULL;
    mca_spml_yoda_rdma_frag_t* frag = NULL;
    struct mca_spml_yoda_getreq_parent get_holder;
    struct yoda_btl *ybtl;
    int btl_id = 0;

    /* find bml_btl and its global btl_id */
    bml_btl = get_next_btl(src, &btl_id);
    if (!bml_btl) oshmem_shmem_abort(-1);

    /**
     * Get the address to the remote rkey. 
     */ 
    r_mkey = mca_memheap.memheap_get_cached_mkey(src, (unsigned long)src_addr, btl_id, &rva);
    if (!r_mkey) {
        SPML_ERROR("pe=%d: %p is not address of shared variable", src, src_addr);
        oshmem_shmem_abort(-1);
        return rc;
    }
#if 0
    SPML_VERBOSE(100, "get: pe:%d src=%p -> dst: %p sz=%d. src_rva=%p, src_rkey=0x%lx", 
            src, src_addr, dst_addr, (int)size, (void *)rva, r_mkey->key);
#endif

    /* check if dest is on memheap. If not:
       - copy it to temporary space. Temporary buffer will be allocated from BML.
       - check if we need to split source in part and send it with several
       put operations
     */

    /* TODO: unify with the same code in put */
    ybtl = &mca_spml_yoda.btl_type_map[btl_id];

    nfrags = 1;
    need_copy = 0;
    rkey = 0;
    lkey = 0;

    switch (ybtl->btl_type) {
        case YODA_BTL_OPENIB:
            l_mkey = mca_memheap.memheap_get_local_mkey((unsigned long)dst_addr, btl_id);
            if (!l_mkey) {
                need_copy = 1;
                calc_nfrags(bml_btl, size, &frag_size, &nfrags);
            }
            else
                lkey = l_mkey->ib.lkey;
            rkey = r_mkey->ib.rkey;
            break;

        case YODA_BTL_SM:
            /* check if we doing put into shm attached segment and if so 
             * just do memcpy.
             */
            if (OPAL_LIKELY(mca_memheap.memheap_is_symmetric_addr((unsigned long)src_addr) && (unsigned long)src_addr != rva)) {
                memcpy(dst_addr, (void *)(unsigned long)rva, size);
                /* must call progress here to avoid deadlock. Scenarion:
                 * pe1 pols pe2 via shm get. pe2 tries to get static variable from node one, which goes to sm btl
                 * In this case pe2 is stuck forever because pe1 never calls opal_progress.
                 * May be we do not need to call progress on every get() here but rather once in a while.
                 */
                opal_progress();
                return OSHMEM_SUCCESS;
            }
            offset = mca_memheap.memheap_find_offset(src, btl_id, (unsigned long)src_addr,rva);
            rkey = r_mkey->key;
            if ((size < mca_spml_yoda.knem_threshold) || (!mca_spml_yoda.use_knem)) {
                 need_copy = 1;
                 calc_nfrags(bml_btl, size, &frag_size, &nfrags);
            }
            break;

        case YODA_BTL_SELF:
             /* self btl ignores rkey/lkey */
            break;
        default:
            SPML_ERROR("btl %d bad btl type %d", btl_id, ybtl->btl_type);
            oshmem_shmem_abort(-1);
    }

    p_src = (char*)(unsigned long)rva; //src_addr;
    p_dst = (char*)dst_addr;
    get_holder.active_count = 0;

    for ( i = 0; i < nfrags; i++) {
        /**
         * Allocating a get request from a pre-allocated 
         * and pre-registered free list.
         */      
        getreq = mca_spml_yoda_getreq_alloc(src);
        assert(getreq);
        frag = &getreq->get_frag;

        getreq->parent = &get_holder;

        ncopied = i < nfrags - 1 ? frag_size : (char *)dst_addr + size - p_dst;

        /**
         * Allocate a temporary buffer on the local PE.
         * The local buffer will store the data read 
         * from the remote address.
         */ 
        mca_bml_base_alloc( bml_btl, &des,
                MCA_BTL_NO_ORDER,
                need_copy ? ncopied : 0,    /* hack */
                MCA_BTL_DES_SEND_ALWAYS_CALLBACK);

        assert(NULL != des);
        assert(NULL != des->des_src);

        /**
         * A Hack: swap as btl allocates only des_src and not des_dst
         * TODO: undo hack in completion callback
         * Undo Hack in handle_wc under openin/btl_openib_component.c.
         */ 
        des->des_dst = des->des_src;
        des->des_src = NULL;

        if (!need_copy) { 
            frag->btl_seg = des->des_dst;
            frag->rdma_segs[1].base_seg.seg_addr.pval = (void*)p_dst;
            frag->rdma_segs[1].base_seg.seg_len       = ncopied;

            if (ybtl->btl_type == YODA_BTL_OPENIB)
            {
                frag->rdma_segs[1].openib_seg.lkey = lkey;
            }
            des->des_dst = &frag->rdma_segs[1].base_seg;

            getreq->p_dst = 0;
        }
        else {
            frag->btl_seg = 0;
            /*
             * Init the lkey value
             * Get the lkey of the intermediate buffer pre-allocated on the
             * source decriptor (pointed by destination descriptor).
             */
            ((mca_btl_openib_segment_t *)des->des_dst)->lkey = ((mca_btl_openib_segment_t *)des->des_dst)->key;
            getreq->p_dst = (uint64_t*)p_dst;
        }

        /**
         * Initialize the remote data fragment 
         * with remote address data required for
         * executing RDMA READ from a remote buffer.
         */ 
        frag->rdma_segs[0].base_seg.seg_addr.lval = (uintptr_t)p_src;
        frag->rdma_segs[0].base_seg.seg_len       = ncopied;

        if (ybtl->btl_type == YODA_BTL_SM)
        {
            frag->rdma_segs[0].sm_seg.key = rkey;
        }
        else if (ybtl->btl_type == YODA_BTL_OPENIB)
        {
            frag->rdma_segs[0].openib_seg.key = (uint32_t)rkey;
        }
        des->des_src = &frag->rdma_segs[0].base_seg;

        frag->rdma_req = getreq;

        /**
         * Init remote side descriptor.
         */ 
        des->des_src_cnt = 1;
        des->des_cbdata = frag;
        des->des_cbfunc = mca_spml_yoda_get_completion;


        /**
         *  Get the data from remote side
         *  using RDMA READ.
         */
        if (YODA_BTL_SM == ybtl->btl_type) {
            ((mca_btl_sm_segment_t *)des->des_dst)->key = offset ; //des_dst is not used in SM KNEM get, so we pass offset through key64 field
            if ((size < mca_spml_yoda.knem_threshold) || (!mca_spml_yoda.use_knem)) {
               /*
                * This ugly hack is done to support following configuration as:
                * OSHMEM + SM => put/get for small messages using send()
                */
                rc = mca_bml_base_send(bml_btl, des, BTL_SM_HDR_TYPE_GET_AS_SEND);
                if (1 == rc) rc = OSHMEM_SUCCESS;
            }
            else {
                des->des_flags |= MCA_BTL_DES_FLAGS_SHMEM_REQUEST;
                rc = mca_bml_base_get(bml_btl, des);
            }
        }
        else {
            des->des_flags |= MCA_BTL_DES_FLAGS_SHMEM_REQUEST;
            rc = mca_bml_base_get(bml_btl, des);
        }

#ifdef OSHMEM_WAIT_COMPLETION_DEBUG
        save_dbg_history("GET", btl_type2str(ybtl->btl_type), src, size, dst_addr, src_addr);
#endif

        if( OPAL_UNLIKELY(OSHMEM_SUCCESS != rc) ) {
            if(OSHMEM_ERR_OUT_OF_RESOURCE == rc) {
                /* No free resources, Block on completion here */
                oshmem_request_wait_completion(&getreq->req_get.req_base.req_oshmem);
                return OSHMEM_SUCCESS;
            } else {
                SPML_ERROR("oshmem_get: error %d", rc);
                oshmem_shmem_abort(-1);
                /*
                   ORTE_ERROR_LOG(rc);
                   orte_errmgr.abort(-1, NULL);
                 */

                return rc;
            }
        }
        p_dst += ncopied;
        p_src += ncopied;
        OPAL_THREAD_ADD32(&get_holder.active_count, 1);
    }

    /* revisit if we really need this for self and sm */
    if (YODA_BTL_SM == ybtl->btl_type || YODA_BTL_SELF == ybtl->btl_type)
        opal_progress();

    /* Wait for completion on request */
    while (get_holder.active_count > 0)
        oshmem_request_wait_completion(&getreq->req_get.req_base.req_oshmem);

    return rc;
}


int mca_spml_yoda_send(void* buf, size_t size, int dst, mca_spml_base_put_mode_t sendmode)
{
    int rc = OSHMEM_SUCCESS;

    rc = MCA_PML_CALL(send(buf, 
                           size, 
                           &(ompi_mpi_unsigned_char.dt), 
                           dst, 
                           0, 
                           (mca_pml_base_send_mode_t)sendmode, 
                           &(ompi_mpi_comm_world.comm)));

    return rc;
}


int mca_spml_yoda_recv(void* buf, size_t size, int src)
{
    int rc = OSHMEM_SUCCESS;

    rc = MCA_PML_CALL(recv(buf, 
                           size, 
                           &(ompi_mpi_unsigned_char.dt), 
                           src, 
                           0, 
                           &(ompi_mpi_comm_world.comm), 
                           NULL));

    return rc;
}

