/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2015 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2006-2007 Voltaire All rights reserved.
 * Copyright (c) 2014      Intel, Inc. All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2015      Mellanox Technologies. All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"

#include "opal/util/arch.h"
#include "opal/mca/pmix/pmix.h"

#include "btl_openib.h"
#include "btl_openib_proc.h"
#include "connect/base.h"
#include "connect/connect.h"

static void mca_btl_openib_proc_btl_construct(mca_btl_openib_proc_btlptr_t* elem);
static void mca_btl_openib_proc_btl_destruct(mca_btl_openib_proc_btlptr_t* elem);

OBJ_CLASS_INSTANCE(mca_btl_openib_proc_btlptr_t,
        opal_list_item_t, mca_btl_openib_proc_btl_construct,
        mca_btl_openib_proc_btl_destruct);

static void mca_btl_openib_proc_btl_construct(mca_btl_openib_proc_btlptr_t* elem)
{
    elem->openib_btl = NULL;
}

static void mca_btl_openib_proc_btl_destruct(mca_btl_openib_proc_btlptr_t* elem)
{
    elem->openib_btl = NULL;
}

static void mca_btl_openib_proc_construct(mca_btl_openib_proc_t* proc);
static void mca_btl_openib_proc_destruct(mca_btl_openib_proc_t* proc);

OBJ_CLASS_INSTANCE(mca_btl_openib_proc_t,
        opal_list_item_t, mca_btl_openib_proc_construct,
        mca_btl_openib_proc_destruct);

void mca_btl_openib_proc_construct(mca_btl_openib_proc_t* ib_proc)
{
    ib_proc->proc_opal           = 0;
    ib_proc->proc_ports          = NULL;
    ib_proc->proc_port_count     = 0;
    ib_proc->proc_endpoints      = 0;
    ib_proc->proc_endpoint_count = 0;
    OBJ_CONSTRUCT(&ib_proc->proc_lock, opal_mutex_t);
    OBJ_CONSTRUCT(&ib_proc->openib_btls, opal_list_t);
}

/*
 * Cleanup ib proc instance
 */

void mca_btl_openib_proc_destruct(mca_btl_openib_proc_t* ib_proc)
{
    mca_btl_openib_proc_btlptr_t* elem;

    /* release resources */
    if(NULL != ib_proc->proc_endpoints) {
        free(ib_proc->proc_endpoints);
    }
    if (NULL != ib_proc->proc_ports) {
        int i, j;
        for (i = 0; i < ib_proc->proc_port_count; ++i) {
            for (j = 0; j < ib_proc->proc_ports[i].pm_cpc_data_count; ++j) {
                if (NULL != ib_proc->proc_ports[i].pm_cpc_data[j].cbm_modex_message) {
                    free(ib_proc->proc_ports[i].pm_cpc_data[j].cbm_modex_message);
                }
            }
        }
        free(ib_proc->proc_ports);
    }
    OBJ_DESTRUCT(&ib_proc->proc_lock);

    elem = (mca_btl_openib_proc_btlptr_t*)opal_list_remove_first(&ib_proc->openib_btls);
    while( NULL != elem ){
            OBJ_RELEASE(elem);
            elem = (mca_btl_openib_proc_btlptr_t*)opal_list_remove_first(&ib_proc->openib_btls);
    }
    OBJ_DESTRUCT(&ib_proc->openib_btls);
}


/*
 * Look for an existing IB process instances based on the associated
 * opal_proc_t instance.
 */
static mca_btl_openib_proc_t* ibproc_lookup_no_lock(opal_proc_t* proc)
{
    mca_btl_openib_proc_t* ib_proc;

    for(ib_proc = (mca_btl_openib_proc_t*)
            opal_list_get_first(&mca_btl_openib_component.ib_procs);
            ib_proc != (mca_btl_openib_proc_t*)
            opal_list_get_end(&mca_btl_openib_component.ib_procs);
            ib_proc  = (mca_btl_openib_proc_t*)opal_list_get_next(ib_proc)) {
        if(ib_proc->proc_opal == proc) {
            return ib_proc;
        }
    }
    return NULL;
}

static mca_btl_openib_proc_t* ibproc_lookup_and_lock(opal_proc_t* proc)
{
    mca_btl_openib_proc_t* ib_proc;

    /* get the process from the list */
    opal_mutex_lock(&mca_btl_openib_component.ib_lock);
    ib_proc = ibproc_lookup_no_lock(proc);
    opal_mutex_unlock(&mca_btl_openib_component.ib_lock);
    if( NULL != ib_proc ){
        /* if we were able to find it - lock it.
         * NOTE: we want to lock it outside of list locked region */
        opal_mutex_lock(&ib_proc->proc_lock);
    }
    return ib_proc;
}

static void inline unpack8(char **src, uint8_t *value)
{
    /* Copy one character */
    *value = (uint8_t) **src;
    /* Most the src ahead one */
    ++*src;
}

/*
 * Create a IB process structure. There is a one-to-one correspondence
 * between a opal_proc_t and a mca_btl_openib_proc_t instance. We
 * cache additional data (specifically the list of
 * mca_btl_openib_endpoint_t instances, and published addresses)
 * associated w/ a given destination on this datastructure.
 */

mca_btl_openib_proc_t* mca_btl_openib_proc_get_locked(opal_proc_t* proc)
{
    mca_btl_openib_proc_t *ib_proc = NULL, *ib_proc_ret = NULL;
    size_t msg_size;
    uint32_t size;
    int rc, i, j;
    void *message;
    char *offset;
    int modex_message_size;
    mca_btl_openib_modex_message_t dummy;
    bool is_new = false;

    /* Check if we have already created a IB proc
     * structure for this ompi process */
    ib_proc = ibproc_lookup_and_lock(proc);
    if (NULL != ib_proc) {
        /* Gotcha! */
        return ib_proc;
    }

    /* All initialization has to be an atomic operation. we do the following assumption:
     * - we let all concurent threads to try to do the initialization;
     * - when one has finished it locks ib_lock and checks if corresponding
     *   process is still missing;
     * - if so - new proc is added, otherwise - initialized proc struct is released.
     */

    /* First time, gotta create a new IB proc
     * out of the opal_proc ... */
    ib_proc = OBJ_NEW(mca_btl_openib_proc_t);

    /* Initialize number of peer */
    ib_proc->proc_endpoint_count = 0;
    ib_proc->proc_opal = proc;

    /* query for the peer address info */
    OPAL_MODEX_RECV(rc, &mca_btl_openib_component.super.btl_version,
                    &proc->proc_name, &message, &msg_size);
    if (OPAL_SUCCESS != rc) {
        BTL_VERBOSE(("[%s:%d] opal_modex_recv failed for peer %s",
                   __FILE__, __LINE__,
                   OPAL_NAME_PRINT(proc->proc_name)));
        goto err_exit;
    }
    if (0 == msg_size) {
        goto err_exit;
    }

    /* Message was packed in btl_openib_component.c; the format is
       listed in a comment in that file */
    modex_message_size = ((char *) &(dummy.end)) - ((char*) &dummy);

    /* Unpack the number of modules in the message */
    offset = (char *) message;
    unpack8(&offset, &(ib_proc->proc_port_count));
    BTL_VERBOSE(("unpack: %d btls", ib_proc->proc_port_count));
    if (ib_proc->proc_port_count > 0) {
        ib_proc->proc_ports = (mca_btl_openib_proc_modex_t *)
            malloc(sizeof(mca_btl_openib_proc_modex_t) *
                   ib_proc->proc_port_count);
    } else {
        ib_proc->proc_ports = NULL;
    }

    /* Loop over unpacking all the ports */
    for (i = 0; i < ib_proc->proc_port_count; i++) {

        /* Unpack the modex comment message struct */
        size = modex_message_size;
        memcpy(&(ib_proc->proc_ports[i].pm_port_info), offset, size);
#if !defined(WORDS_BIGENDIAN) && OPAL_ENABLE_HETEROGENEOUS_SUPPORT
        MCA_BTL_OPENIB_MODEX_MSG_NTOH(ib_proc->proc_ports[i].pm_port_info);
#endif
        offset += size;
        BTL_VERBOSE(("unpacked btl %d: modex message, offset now %d",
                     i, (int)(offset-((char*)message))));

        /* Unpack the number of CPCs that follow */
        unpack8(&offset, &(ib_proc->proc_ports[i].pm_cpc_data_count));
        BTL_VERBOSE(("unpacked btl %d: number of cpcs to follow %d (offset now %d)",
                     i, ib_proc->proc_ports[i].pm_cpc_data_count,
                     (int)(offset-((char*)message))));
        ib_proc->proc_ports[i].pm_cpc_data = (opal_btl_openib_connect_base_module_data_t *)
            calloc(ib_proc->proc_ports[i].pm_cpc_data_count,
                   sizeof(opal_btl_openib_connect_base_module_data_t));
        if (NULL == ib_proc->proc_ports[i].pm_cpc_data) {
            goto err_exit;
        }

        /* Unpack the CPCs */
        for (j = 0; j < ib_proc->proc_ports[i].pm_cpc_data_count; ++j) {
            uint8_t u8;
            opal_btl_openib_connect_base_module_data_t *cpcd;
            cpcd = ib_proc->proc_ports[i].pm_cpc_data + j;
            unpack8(&offset, &u8);
            BTL_VERBOSE(("unpacked btl %d: cpc %d: index %d (offset now %d)",
                         i, j, u8, (int)(offset-(char*)message)));
            cpcd->cbm_component =
                opal_btl_openib_connect_base_get_cpc_byindex(u8);
            BTL_VERBOSE(("unpacked btl %d: cpc %d: component %s",
                         i, j, cpcd->cbm_component->cbc_name));

            unpack8(&offset, &cpcd->cbm_priority);
            unpack8(&offset, &cpcd->cbm_modex_message_len);
            BTL_VERBOSE(("unpacked btl %d: cpc %d: priority %d, msg len %d (offset now %d)",
                         i, j, cpcd->cbm_priority,
                         cpcd->cbm_modex_message_len,
                         (int)(offset-(char*)message)));
            if (cpcd->cbm_modex_message_len > 0) {
                cpcd->cbm_modex_message = malloc(cpcd->cbm_modex_message_len);
                if (NULL == cpcd->cbm_modex_message) {
                    BTL_ERROR(("Failed to malloc"));
                    goto err_exit;
                }
                memcpy(cpcd->cbm_modex_message, offset,
                       cpcd->cbm_modex_message_len);
                offset += cpcd->cbm_modex_message_len;
                BTL_VERBOSE(("unpacked btl %d: cpc %d: blob unpacked %d %x (offset now %d)",
                             i, j,
                             ((uint32_t*)cpcd->cbm_modex_message)[0],
                             ((uint32_t*)cpcd->cbm_modex_message)[1],
                             (int)(offset-((char*)message))));
            }
        }
    }

    if (0 == ib_proc->proc_port_count) {
        ib_proc->proc_endpoints = NULL;
    } else {
        ib_proc->proc_endpoints = (volatile mca_btl_base_endpoint_t**)
            malloc(ib_proc->proc_port_count *
                   sizeof(mca_btl_base_endpoint_t*));
    }
    if (NULL == ib_proc->proc_endpoints) {
        goto err_exit;
    }

    BTL_VERBOSE(("unpacking done!"));

    /* Finally add this process to the initialized procs list */
    opal_mutex_lock(&mca_btl_openib_component.ib_lock);

    ib_proc_ret = ibproc_lookup_no_lock(proc);
    if (NULL == ib_proc_ret) {
        /* if process can't be found in this list - insert it locked
         * it is safe to lock ib_proc here because this thread is
         * the only one who knows about it so far */
        opal_mutex_lock(&ib_proc->proc_lock);
        opal_list_append(&mca_btl_openib_component.ib_procs, &ib_proc->super);
        ib_proc_ret = ib_proc;
        is_new = true;
    } else {
        /* otherwise - release module_proc */
        OBJ_RELEASE(ib_proc);
    }
    opal_mutex_unlock(&mca_btl_openib_component.ib_lock);

    /* if we haven't insert the process - lock it here so we
     * won't lock mca_btl_openib_component.ib_lock */
    if( !is_new ){
        opal_mutex_lock(&ib_proc_ret->proc_lock);
    }

    return ib_proc_ret;

err_exit:

    fprintf(stderr,"%d: error exit from mca_btl_openib_proc_create\n", OPAL_PROC_MY_NAME.vpid);
    if( NULL != ib_proc ){
        OBJ_RELEASE(ib_proc);
    }
    return NULL;
}

int mca_btl_openib_proc_remove(opal_proc_t *proc,
                               mca_btl_base_endpoint_t *endpoint)
{
    size_t i;
    mca_btl_openib_proc_t* ib_proc = NULL;

    /* Remove endpoint from the openib BTL version of the proc as
       well */
    ib_proc = ibproc_lookup_and_lock(proc);
    if (NULL != ib_proc) {
        for (i = 0; i < ib_proc->proc_endpoint_count; ++i) {
            if (ib_proc->proc_endpoints[i] == endpoint) {
                ib_proc->proc_endpoints[i] = NULL;
                if (i == ib_proc->proc_endpoint_count - 1) {
                    --ib_proc->proc_endpoint_count;
                }
                opal_mutex_unlock(&ib_proc->proc_lock);
                return OPAL_SUCCESS;
            }
        }
    }

    return OPAL_ERR_NOT_FOUND;
}

/*
 * Note that this routine must be called with the lock on the process
 * already held.  Insert a btl instance into the proc array and assign
 * it an address.
 */
int mca_btl_openib_proc_insert(mca_btl_openib_proc_t* module_proc,
        mca_btl_base_endpoint_t* module_endpoint)
{
    /* insert into endpoint array */


#ifndef WORDS_BIGENDIAN
    /* if we are little endian and our peer is not so lucky, then we
       need to put all information sent to him in big endian (aka
       Network Byte Order) and expect all information received to
       be in NBO.  Since big endian machines always send and receive
       in NBO, we don't care so much about that case. */
    if (module_proc->proc_opal->proc_arch & OPAL_ARCH_ISBIGENDIAN) {
        module_endpoint->nbo = true;
    }
#endif

    /* only allow eager rdma if the peers agree on the size of a long */
    if((module_proc->proc_opal->proc_arch & OPAL_ARCH_LONGISxx) !=
       (opal_proc_local_get()->proc_arch & OPAL_ARCH_LONGISxx)) {
        module_endpoint->use_eager_rdma = false;
    }

    module_endpoint->endpoint_proc = module_proc;
    module_proc->proc_endpoints[module_proc->proc_endpoint_count++] = module_endpoint;
    return OPAL_SUCCESS;
}

int mca_btl_openib_proc_reg_btl(mca_btl_openib_proc_t* ib_proc,
                                mca_btl_openib_module_t* openib_btl)
{
    mca_btl_openib_proc_btlptr_t* elem;


    for(elem = (mca_btl_openib_proc_btlptr_t*)opal_list_get_first(&ib_proc->openib_btls);
            elem != (mca_btl_openib_proc_btlptr_t*)opal_list_get_end(&ib_proc->openib_btls);
            elem  = (mca_btl_openib_proc_btlptr_t*)opal_list_get_next(elem)) {
        if(elem->openib_btl == openib_btl) {
            /* this is normal return meaning that this BTL has already touched this ib_proc */
            return OPAL_ERR_RESOURCE_BUSY;
        }
    }

    elem = OBJ_NEW(mca_btl_openib_proc_btlptr_t);
    if( NULL == elem ){
        return OPAL_ERR_OUT_OF_RESOURCE;
    }
    elem->openib_btl = openib_btl;
    opal_list_append(&ib_proc->openib_btls, &elem->super);
    return OPAL_SUCCESS;
}
