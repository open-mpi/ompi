#include "ompi_config.h"

#include "class/ompi_hash_table.h"
#include "mca/base/mca_base_module_exchange.h"

#include "ptl_ib.h"
#include "ptl_ib_vapi.h"
#include "ptl_ib_proc.h"

static void mca_ptl_ib_proc_construct(mca_ptl_ib_proc_t* proc);
static void mca_ptl_ib_proc_destruct(mca_ptl_ib_proc_t* proc);

OBJ_CLASS_INSTANCE(mca_ptl_ib_proc_t, 
        ompi_list_item_t, mca_ptl_ib_proc_construct, 
        mca_ptl_ib_proc_destruct);

void mca_ptl_ib_proc_construct(mca_ptl_ib_proc_t* proc)
{
    proc->proc_ompi = 0;
    proc->proc_addrs = 0;
    proc->proc_addr_count = 0;
    proc->proc_peers = 0;
    proc->proc_peer_count = 0;
    OBJ_CONSTRUCT(&proc->proc_lock, ompi_mutex_t);
    /* add to list of all proc instance */
    OMPI_THREAD_LOCK(&mca_ptl_ib_component.ib_lock);
    ompi_list_append(&mca_ptl_ib_component.ib_procs, &proc->super);
    OMPI_THREAD_UNLOCK(&mca_ptl_ib_component.ib_lock);
}

/*
 * Cleanup ib proc instance
 */

void mca_ptl_ib_proc_destruct(mca_ptl_ib_proc_t* proc)
{
    /* remove from list of all proc instances */
    OMPI_THREAD_LOCK(&mca_ptl_ib_component.ib_lock);
    ompi_list_remove_item(&mca_ptl_ib_component.ib_procs, &proc->super);
    OMPI_THREAD_UNLOCK(&mca_ptl_ib_component.ib_lock);

    /* release resources */
    if(NULL != proc->proc_peers) {
        free(proc->proc_peers);
    }
}


/*
 * Look for an existing IB process instances based on the associated
 * ompi_proc_t instance.
 */
static mca_ptl_ib_proc_t* mca_ptl_ib_proc_lookup_ompi(ompi_proc_t* ompi_proc)
{
    mca_ptl_ib_proc_t* ib_proc;

    OMPI_THREAD_LOCK(&mca_ptl_ib_component.ib_lock);

    for(ib_proc = (mca_ptl_ib_proc_t*)
            ompi_list_get_first(&mca_ptl_ib_component.ib_procs);
            ib_proc != (mca_ptl_ib_proc_t*)
            ompi_list_get_end(&mca_ptl_ib_component.ib_procs);
            ib_proc  = (mca_ptl_ib_proc_t*)ompi_list_get_next(ib_proc)) {

        if(ib_proc->proc_ompi == ompi_proc) {
            OMPI_THREAD_UNLOCK(&mca_ptl_ib_component.ib_lock);
            return ib_proc;
        }

    }

    OMPI_THREAD_UNLOCK(&mca_ptl_ib_component.ib_lock);

    return NULL;
}

/*
 * Create a IB process structure. There is a one-to-one correspondence
 * between a ompi_proc_t and a mca_ptl_ib_proc_t instance. We cache
 * additional data (specifically the list of mca_ptl_ib_peer_t instances, 
 * and published addresses) associated w/ a given destination on this
 * datastructure.
 */

mca_ptl_ib_proc_t* mca_ptl_ib_proc_create(ompi_proc_t* ompi_proc)
{
    int rc, my_rank, i;
    size_t size;
    char* str_rank;
    VAPI_ret_t ret;

    mca_ptl_ib_module_t* module = NULL;

    mca_ptl_ib_proc_t* module_proc = NULL;

    module_proc = mca_ptl_ib_proc_lookup_ompi(ompi_proc);

    if(module_proc != NULL) {
        return module_proc;
    }

    module_proc = OBJ_NEW(mca_ptl_ib_proc_t);

    /* Initialize number of peer */
    module_proc->proc_peer_count = 0;

    module_proc->proc_ompi = ompi_proc;

    /* build a unique identifier (of arbitrary
     * size) to represent the proc */
    module_proc->proc_guid = ompi_proc->proc_name;

    D_PRINT("Creating proc for %d\n", ompi_proc->proc_name.vpid);

    /* lookup ib parameters exported by
     * this proc */
    rc = mca_base_modex_recv(
            &mca_ptl_ib_component.super.ptlm_version,
            ompi_proc,
            (void**)&module_proc->proc_addrs,
            &size);

    if(rc != OMPI_SUCCESS) {
        ompi_output(0, "mca_ptl_ib_proc_create: mca_base_modex_recv: "
                "failed with return value=%d", rc);
        OBJ_RELEASE(module_proc);
        return NULL;
    }

    D_PRINT("UD q.p. obtained is: %d, Lid : %d\n",
            module_proc->proc_addrs[0].qp_num,
            module_proc->proc_addrs[0].lid);

    if(0 != (size % sizeof(mca_ptl_ib_ud_addr_t))) {
        ompi_output(0, "mca_ptl_ib_proc_create: mca_base_modex_recv: "
                "invalid size %d\n", size);
        return NULL;
    }

    module_proc->proc_addr_count = size / sizeof(mca_ptl_ib_ud_addr_t);

    /* allocate space for peer array - one for
     * each exported address
     */

    module_proc->proc_peers = (mca_ptl_base_peer_t**)
        malloc(module_proc->proc_addr_count * sizeof(mca_ptl_base_peer_t*));

    if(NULL == module_proc->proc_peers) {
        OBJ_RELEASE(module_proc);
        return NULL;
    }

    /* HACK: Till dyn. connection management comes through,
     * just establish the RC connection here */

    str_rank = getenv("OMPI_MCA_pcm_cofs_procid");
    if(NULL != str_rank) {
        my_rank = atoi(str_rank);
    } else {
        D_PRINT("Rank, what rank?");
    }

    if(my_rank != ompi_proc->proc_name.vpid) {
        D_PRINT("I %d Have to create connection for %d",
                my_rank, ompi_proc->proc_name.vpid);

        module = mca_ptl_ib_component.ib_ptl_modules[0];

        /* Make the RC QP transitions */
        if(mca_ptl_ib_rc_qp_init(module->nic,
                module->my_qp_hndl,
                module_proc->proc_addrs[0].qp_num,
                module_proc->proc_addrs[0].lid)
                != OMPI_SUCCESS) {
            return NULL;
        }

        /* Allocate the send and recv buffers */

        module->send_buf =
            malloc(sizeof(mca_ptl_ib_send_buf_t) * NUM_BUFS);

        if(NULL == module->send_buf) {
            return NULL;
        }
        memset(module->send_buf,
                0, sizeof(mca_ptl_ib_send_buf_t) * NUM_BUFS);

        if(mca_ptl_ib_register_mem(module->nic, 
                    module->ptag,
                    module->send_buf, 
                    sizeof(mca_ptl_ib_send_buf_t) * NUM_BUFS, 
                    &module->send_buf_hndl)
                != OMPI_SUCCESS) {
            return NULL;
        }

        module->recv_buf =
            malloc(sizeof(mca_ptl_ib_recv_buf_t) * NUM_BUFS);

        if(NULL == module->recv_buf) {
            return NULL;
        }

        memset(module->recv_buf,
                0, sizeof(mca_ptl_ib_recv_buf_t) * NUM_BUFS);

        if(mca_ptl_ib_register_mem(module->nic, 
                    module->ptag,
                    module->recv_buf, 
                    sizeof(mca_ptl_ib_recv_buf_t) * NUM_BUFS, 
                    &module->recv_buf_hndl)
                != OMPI_SUCCESS) {
            return NULL;
        }

        /* Prepare the receivs */
        for(i = 0; i < NUM_BUFS; i++) {
            module->recv_buf[i].desc.rr.comp_type = VAPI_SIGNALED;
            module->recv_buf[i].desc.rr.opcode = VAPI_RECEIVE;
            module->recv_buf[i].desc.rr.id = (VAPI_virt_addr_t)
                (MT_virt_addr_t) &module->recv_buf[i];
            module->recv_buf[i].desc.rr.sg_lst_len = 1;
            module->recv_buf[i].desc.rr.sg_lst_p = &(module->recv_buf[i].desc.sg_entry);
            module->recv_buf[i].desc.sg_entry.len = 4096;
            module->recv_buf[i].desc.sg_entry.lkey = module->recv_buf_hndl.lkey;
            module->recv_buf[i].desc.sg_entry.addr = 
                (VAPI_virt_addr_t) (MT_virt_addr_t) (module->recv_buf[i].buf);
        }

        /* Post the receives */
        for(i = 0; i < NUM_BUFS; i++) {
            ret = VAPI_post_rr(module->nic,
                    module->my_qp_hndl,
                    &module->recv_buf[i].desc.rr);
            if(VAPI_OK != ret) {
                MCA_PTL_IB_VAPI_RET(ret, "VAPI_post_rr");
                return NULL;
            }
        }
    }

    if(1 == my_rank) {
        sleep(2);
    }

    return module_proc;
}


/*
 * Note that this routine must be called with the lock on the process
 * already held.  Insert a ptl instance into the proc array and assign 
 * it an address.
 */
int mca_ptl_ib_proc_insert(mca_ptl_ib_proc_t* module_proc, 
        mca_ptl_base_peer_t* module_peer)
{
    /* insert into peer array */
    module_peer->peer_proc = module_proc;
    module_proc->proc_peers[module_proc->proc_peer_count++] = module_peer;

#if 0 /* TODO: don't quite understand what this means for IB ptl.
         will come back to it later */
    /*
     * Look through the proc instance for an address that is on the
     * directly attached network. If we don't find one, pick the first
     * unused address.
     */

    for(i = 0; i < module_proc->proc_addr_count; i++) {

        mca_ptl_ib_ud_addr_t* peer_addr = module_proc->proc_addrs + i;

        module_peer->peer_addr = peer_addr;
    }
#endif

    return OMPI_SUCCESS;
}
