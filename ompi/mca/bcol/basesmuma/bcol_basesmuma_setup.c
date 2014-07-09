/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
 * Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
 * Copyright (c) 2013-2014 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2014 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2014      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/**
 * @file
 *
 */

#include "ompi_config.h"
#include "mpi.h"
#include "ompi/constants.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mca/mpool/base/base.h"
#include "ompi/mca/bcol/bcol.h"
#include "ompi/mca/bcol/base/base.h"
#include "ompi/patterns/comm/coll_ops.h"

#include "opal/class/opal_object.h"
#include "opal/dss/dss.h"

#include "bcol_basesmuma.h"

int base_bcol_basesmuma_setup_ctl_struct(
    mca_bcol_basesmuma_module_t *sm_bcol_module,
    mca_bcol_basesmuma_component_t *cs,
    sm_buffer_mgmt *ctl_mgmt);

/* this is the new one, uses the pml allgather */
int base_bcol_basesmuma_exchange_offsets(
    mca_bcol_basesmuma_module_t *sm_bcol_module,
    void **result_array, uint64_t mem_offset, int loop_limit,
    int leading_dim)
{
    int ret=OMPI_SUCCESS,i;
    int count;
    int index_in_group;
    char *send_buff;
    char *recv_buff;
    uint64_t rem_mem_offset;

    /* malloc some memory */
    count = sizeof(uint64_t) + sizeof(int);
    send_buff = (char *) malloc(count);
    recv_buff = (char *) malloc(count *
                           sm_bcol_module->super.sbgp_partner_module->group_size);
    /*  exchange the base pointer for the controls structures - gather
     *  every one else's infromation.
     */


    /* pack the offset of the allocated region */
    memcpy((void *) send_buff, (void *) &(sm_bcol_module->super.sbgp_partner_module->my_index), sizeof(int));
    memcpy((void *) (send_buff+ sizeof(int)), (void *) &(mem_offset), sizeof(uint64_t));

    /* get the offsets from all procs, so can setup the control data
     * structures.
     */

    ret=comm_allgather_pml((void *) send_buff,(void *) recv_buff,count,
            MPI_BYTE,
            sm_bcol_module->super.sbgp_partner_module->my_index,
            sm_bcol_module->super.sbgp_partner_module->group_size,
            sm_bcol_module->super.sbgp_partner_module->group_list,
            sm_bcol_module->super.sbgp_partner_module->group_comm);
    if( OMPI_SUCCESS != ret ) {
        goto exit_ERROR;
    }

    /* get the control stucture offsets within the shared memory
     *   region and populate the control structures - we do not assume
     *   any symmetry in memory layout of each process
     */

    /* loop over the procs in the group */
    for(i = 0; i < sm_bcol_module->super.sbgp_partner_module->group_size; i++){
        int array_id;
        /* get this peer's index in the group */
        memcpy((void *) &index_in_group, (void *) (recv_buff + i*count) , sizeof(int));

        /* get the offset */
        memcpy((void *) &rem_mem_offset, (void *) (recv_buff + i*count + sizeof(int)), sizeof(uint64_t));

        array_id=SM_ARRAY_INDEX(leading_dim,0,index_in_group);
        result_array[array_id]=(void *)(uintptr_t)rem_mem_offset;

    }

exit_ERROR:
    /* clean up */
    if( NULL != send_buff ) {
        free(send_buff);
        send_buff = NULL;
    }
    if( NULL != recv_buff ) {
        free(recv_buff);
        recv_buff = NULL;
    }

    return ret;


}

#if 0
int base_bcol_basesmuma_exchange_offsets(
    mca_bcol_basesmuma_module_t *sm_bcol_module,
    void **result_array, uint64_t mem_offset, int loop_limit,
    int leading_dim)
{
    int ret=OMPI_SUCCESS,i,dummy;
    int index_in_group, pcnt;
    opal_list_t peers;
    ompi_namelist_t *peer;
    ompi_proc_t *proc_temp, *my_id;
    opal_buffer_t *send_buffer = OBJ_NEW(opal_buffer_t);
    opal_buffer_t *recv_buffer = OBJ_NEW(opal_buffer_t);
    uint64_t rem_mem_offset;

    /*  exchange the base pointer for the controls structures - gather
     *  every one else's infromation.
     */
    /* get list of procs that will participate in the communication */
    OBJ_CONSTRUCT(&peers, opal_list_t);
    for (i = 0; i < sm_bcol_module->super.sbgp_partner_module->group_size; i++) {
        /* get the proc info */
        proc_temp = ompi_comm_peer_lookup(
                sm_bcol_module->super.sbgp_partner_module->group_comm,
                sm_bcol_module->super.sbgp_partner_module->group_list[i]);
        peer = OBJ_NEW(ompi_namelist_t);
        peer->name.jobid = proc_temp->proc_name.jobid;
        peer->name.vpid = proc_temp->proc_name.vpid;
        opal_list_append(&peers,&peer->super); /* this is with the new field called "super" in ompi_namelist_t struct */
    }
    /* pack up the data into the allgather send buffer */
        if (NULL == send_buffer || NULL == recv_buffer) {
            opal_output (ompi_bcol_base_framework.framework_output, "Cannot allocate memory for sbuffer or rbuffer\n");
            ret = OMPI_ERROR;
            goto exit_ERROR;
        }

    /* get my proc information */
    my_id = ompi_proc_local();

    /* pack my information */
    ret = opal_dss.pack(send_buffer,
        &(sm_bcol_module->super.sbgp_partner_module->my_index),1,OPAL_UINT32);

    if (OMPI_SUCCESS != ret) {
        opal_output (ompi_bcol_base_framework.framework_output, "Error packing my_index!!\n");
        goto exit_ERROR;
    }

    /* pack the offset of the allocated region */
    ret = opal_dss.pack(send_buffer,&(mem_offset),1,OPAL_UINT64);
    if (OMPI_SUCCESS != ret) {
        goto exit_ERROR;
    }

    /* get the offsets from all procs, so can setup the control data
     * structures.
     */
    if (OMPI_SUCCESS != (ret = ompi_rte_allgather_list(&peers, send_buffer, recv_buffer))) {
        opal_output (ompi_bcol_base_framework.framework_output, "ompi_rte_allgather_list returned error %d\n", ret);
        goto exit_ERROR;
    }

        /* unpack the dummy */
        pcnt=1;
        ret = opal_dss.unpack(recv_buffer,&dummy, &pcnt, OPAL_INT32);
        if (OMPI_SUCCESS != ret) {
                opal_output (ompi_bcol_base_framework.framework_output, "unpack returned error %d for dummy\n",ret);
                goto exit_ERROR;
        }

    /* get the control stucture offsets within the shared memory
     *   region and populate the control structures - we do not assume
     *   any symmetry in memory layout of each process
     */

    /* loop over the procs in the group */
    for(i = 0; i < sm_bcol_module->super.sbgp_partner_module->group_size; i++){
        int array_id;
        pcnt=1;
        ret = opal_dss.unpack(recv_buffer,&index_in_group, &pcnt, OPAL_UINT32);
        if (OMPI_SUCCESS != ret) {
            opal_output (ompi_bcol_base_framework.framework_output, "unpack returned error %d for remote index_in_group\n",ret);
            goto exit_ERROR;
        }

        /* get the offset */
        pcnt=1;
        ret = opal_dss.unpack(recv_buffer,&rem_mem_offset, &pcnt, OPAL_UINT64);
        if (OMPI_SUCCESS != ret) {
            opal_output (ompi_bcol_base_framework.framework_output, "unpack returned error %d for remote memory offset\n",ret);
            goto exit_ERROR;
        }

        array_id=SM_ARRAY_INDEX(leading_dim,0,index_in_group);
        result_array[array_id]=(void *)rem_mem_offset;

    }

    /* clean up */
    peer=(ompi_namelist_t *)opal_list_remove_first(&peers);
    while( NULL !=peer) {
        OBJ_RELEASE(peer);
        peer=(ompi_namelist_t *)opal_list_remove_first(&peers);
    }
    OBJ_DESTRUCT(&peers);
    if( send_buffer ) {
        OBJ_RELEASE(send_buffer);
    }
    if( recv_buffer ) {
        OBJ_RELEASE(recv_buffer);
    }

    return ret;

exit_ERROR:

    /* free peer list */
    peer=(ompi_namelist_t *)opal_list_remove_first(&peers);
    while( NULL !=peer) {
        OBJ_RELEASE(peer);
        peer=(ompi_namelist_t *)opal_list_remove_first(&peers);
    }
    OBJ_DESTRUCT(&peers);
    if( send_buffer ) {
        OBJ_RELEASE(send_buffer);
    }
    if( recv_buffer ) {
        OBJ_RELEASE(recv_buffer);
    }
    return ret;
}
#endif


static int base_bcol_basesmuma_exchange_ctl_params(
    mca_bcol_basesmuma_module_t *sm_bcol_module,
    mca_bcol_basesmuma_component_t *cs,
    sm_buffer_mgmt *ctl_mgmt, list_data_t *data_blk)
{
    int ret=OMPI_SUCCESS,i,loop_limit;
    int leading_dim, buf_id;
    void *mem_offset;
    unsigned char *base_ptr;
    mca_bcol_basesmuma_ctl_struct_t *ctl_ptr;

    /* data block base offset in the mapped file */
    mem_offset = (void *)((uintptr_t)data_blk->data -
                          (uintptr_t)cs->sm_ctl_structs->data_addr);

    /* number of buffers in data block */
    loop_limit=cs->basesmuma_num_mem_banks+ctl_mgmt->number_of_buffs;
    leading_dim=ctl_mgmt->size_of_group;
    ret=comm_allgather_pml(&mem_offset, ctl_mgmt->ctl_buffs, sizeof(void *),
                           MPI_BYTE, sm_bcol_module->super.sbgp_partner_module->my_index,
                           sm_bcol_module->super.sbgp_partner_module->group_size,
                           sm_bcol_module->super.sbgp_partner_module->group_list,
                           sm_bcol_module->super.sbgp_partner_module->group_comm);
    if( OMPI_SUCCESS != ret ) {
        goto exit_ERROR;
    }

#if 0
    ret=base_bcol_basesmuma_exchange_offsets( sm_bcol_module,
            (void **)ctl_mgmt->ctl_buffs, mem_offset, loop_limit, leading_dim);
    if( OMPI_SUCCESS != ret ) {
        goto exit_ERROR;
    }
#endif

    /* convert memory offset to virtual address in current rank */
    for (i=0;i< sm_bcol_module->super.sbgp_partner_module->group_size;i++) {

        /* get the base pointer */
        int array_id=SM_ARRAY_INDEX(leading_dim,0,i);
        if( i == sm_bcol_module->super.sbgp_partner_module->my_index) {
            /* me */
            base_ptr=cs->sm_ctl_structs->map_addr;
        } else {
            base_ptr=sm_bcol_module->ctl_backing_files_info[i]->sm_mmap->map_addr;
        }
        ctl_mgmt->ctl_buffs[array_id]=(void *)
            (uintptr_t)(((uint64_t)(uintptr_t)ctl_mgmt->ctl_buffs[array_id])+(uint64_t)(uintptr_t)base_ptr);
        for( buf_id = 1 ; buf_id < loop_limit ; buf_id++ ) {
            int array_id_m1=SM_ARRAY_INDEX(leading_dim,(buf_id-1),i);
            array_id=SM_ARRAY_INDEX(leading_dim,buf_id,i);
            ctl_mgmt->ctl_buffs[array_id]=(void *) (uintptr_t)((uint64_t)(uintptr_t)(ctl_mgmt->ctl_buffs[array_id_m1])+
                (uint64_t)(uintptr_t)sizeof(mca_bcol_basesmuma_ctl_struct_t));
        }
    }
    /* initialize my control structues */
    for( buf_id = 0 ; buf_id < loop_limit ; buf_id++ ) {

        int my_idx=sm_bcol_module->super.sbgp_partner_module->my_index;
        int array_id=SM_ARRAY_INDEX(leading_dim,buf_id,my_idx);
        ctl_ptr = (mca_bcol_basesmuma_ctl_struct_t *)
                ctl_mgmt->ctl_buffs[array_id];

        /* initialize the data structures - RLG, this is only one data
         * structure that needs to be initialized, more are missing */
        ctl_ptr->sequence_number=-1;
        ctl_ptr->flag=-1;
        ctl_ptr->index=0;
        ctl_ptr->src_ptr = NULL;
    }

    return ret;

exit_ERROR:

    return ret;
}

static int base_bcol_basesmuma_setup_ctl (mca_bcol_basesmuma_module_t *sm_bcol_module,
                                          mca_bcol_basesmuma_component_t *cs)
{
    const int my_index = sm_bcol_module->super.sbgp_partner_module->my_index;;
    bcol_basesmuma_smcm_file_t input_file;
    int ret;

    /* exchange remote addressing information if it has not already been done  */
    if (NULL == sm_bcol_module->ctl_backing_files_info) {
        input_file.file_name=cs->sm_ctl_structs->map_path;
        input_file.size=cs->sm_ctl_structs->map_size;
        input_file.size_ctl_structure=0;
        input_file.data_seg_alignment=BASESMUMA_CACHE_LINE_SIZE;
        input_file.mpool_size=cs->sm_ctl_structs->map_size;
        ret = bcol_basesmuma_smcm_allgather_connection(sm_bcol_module,
                                                       sm_bcol_module->super.sbgp_partner_module,
                                                       &(cs->sm_connections_list),
                                                       &(sm_bcol_module->ctl_backing_files_info),
                                                       sm_bcol_module->super.sbgp_partner_module->group_comm,
                                                       input_file, cs->clt_base_fname,
                                                       false);
        if (OMPI_SUCCESS != ret) {
            return ret;
        }
    }

    /* fill in the pointer to other ranks scartch shared memory */
    if (NULL == sm_bcol_module->shared_memory_scratch_space) {
        sm_bcol_module->shared_memory_scratch_space =
            calloc (sm_bcol_module->super.sbgp_partner_module->group_size, sizeof (void *));
        if (!sm_bcol_module->shared_memory_scratch_space) {
            opal_output (ompi_bcol_base_framework.framework_output, "Cannot allocate memory for shared_memory_scratch_space.");
            return OMPI_ERR_OUT_OF_RESOURCE;
        }

        for (int i = 0 ; i < sm_bcol_module->super.sbgp_partner_module->group_size ; ++i) {
            if (i == my_index) {
                /* local file data is not cached in this list */
                continue;
            }

            sm_bcol_module->shared_memory_scratch_space[i] =
                (void *)((intptr_t) sm_bcol_module->ctl_backing_files_info[i]->sm_mmap +
                         cs->scratch_offset_from_base_ctl_file);
        }

        sm_bcol_module->shared_memory_scratch_space[my_index] =
            (void *)((intptr_t) cs->sm_ctl_structs->map_addr + cs->scratch_offset_from_base_ctl_file);
    }

    return OMPI_SUCCESS;
}

int base_bcol_basesmuma_setup_ctl_struct(
    mca_bcol_basesmuma_module_t *sm_bcol_module,
    mca_bcol_basesmuma_component_t *cs,
    sm_buffer_mgmt *ctl_mgmt)
{
    int n_ctl, n_levels;
    int n_ctl_structs;
    size_t malloc_size;

    /*
     * set my no user-data conrol structures
     */
    /* number of banks and regions per bank are already a power of 2 */
    n_ctl_structs=cs->basesmuma_num_mem_banks*
        cs->basesmuma_num_regions_per_bank;

    /* initialize the control structure management struct -
     * for collectives without user data
     *---------------------------------------------------------------
     */

    ctl_mgmt->number_of_buffs=n_ctl_structs;
    ctl_mgmt->num_mem_banks=
        cs->basesmuma_num_mem_banks;

    ctl_mgmt->num_buffs_per_mem_bank=
        cs->basesmuma_num_regions_per_bank;
    ctl_mgmt->size_of_group=
        sm_bcol_module->super.sbgp_partner_module->group_size;
    roundup_to_power_radix(2,cs->basesmuma_num_regions_per_bank,&n_levels);
    ctl_mgmt->log2_num_buffs_per_mem_bank=n_levels;

    roundup_to_power_radix(2,n_ctl_structs,&n_levels);
    ctl_mgmt->log2_number_of_buffs=n_levels;
    ctl_mgmt->mask=n_ctl_structs-1;
    sm_bcol_module->super.n_poll_loops=cs->n_poll_loops;

    malloc_size=
        (ctl_mgmt->number_of_buffs +
         ctl_mgmt->num_mem_banks ) *
         ctl_mgmt->size_of_group *
         sizeof(void *);
    ctl_mgmt->ctl_buffs = malloc(malloc_size);
    if (!ctl_mgmt->ctl_buffs) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    /*
     * setup the no-data buffer managment data
     */
    n_ctl = ctl_mgmt->num_mem_banks;
    ctl_mgmt->ctl_buffs_mgmt = (mem_bank_management_t *) calloc (n_ctl, sizeof (mem_bank_management_t));
    if (!ctl_mgmt->ctl_buffs_mgmt) {
        opal_output (ompi_bcol_base_framework.framework_output, "Cannot allocate memory for ctl_buffs_mgmt");
        free (ctl_mgmt->ctl_buffs);
        ctl_mgmt->ctl_buffs = NULL;
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    /* initialize each individual element */
    for (int i = 0 ; i < n_ctl ; ++i) {
        opal_list_item_t *item;
        opal_mutex_t *mutex_ptr;

        ctl_mgmt->ctl_buffs_mgmt[i].available_buffers=
            ctl_mgmt->num_buffs_per_mem_bank;
        ctl_mgmt->ctl_buffs_mgmt[i].number_of_buffers=
            ctl_mgmt->num_buffs_per_mem_bank;
        mutex_ptr = &(ctl_mgmt->ctl_buffs_mgmt[i].mutex);
        OBJ_CONSTRUCT(mutex_ptr, opal_mutex_t);
        ctl_mgmt->ctl_buffs_mgmt[i].index_shared_mem_ctl_structs=i;

        item = (opal_list_item_t *)&(ctl_mgmt->ctl_buffs_mgmt[i].nb_barrier_desc);
        OBJ_CONSTRUCT(item, opal_list_item_t);
        ctl_mgmt->ctl_buffs_mgmt[i].nb_barrier_desc.sm_module =
            sm_bcol_module;
        ctl_mgmt->ctl_buffs_mgmt[i].nb_barrier_desc.pool_index = i;
        /* get the sm_buffer_mgmt pointer for the control structures */
        ctl_mgmt->ctl_buffs_mgmt[i].nb_barrier_desc.coll_buff = ctl_mgmt;
    }

    return OMPI_SUCCESS;
}

/*
 * this function initializes the internal scratch buffers and control
 * structures that will be used by the module. It also intitializes
 * the payload buffer management structures.
 */
int base_bcol_basesmuma_setup_library_buffers(
    mca_bcol_basesmuma_module_t *sm_bcol_module,
    mca_bcol_basesmuma_component_t *cs)
{
    int ret=OMPI_SUCCESS,i;
    int n_ctl_structs;
    size_t ctl_segement_size,total_memory;
    int max_elements;
    unsigned char *data_ptr;

    /* */
    /* setup the control struct memory */
    if(!cs->sm_ctl_structs) {
        ret = mca_bcol_basesmuma_allocate_sm_ctl_memory(cs);
        if(OMPI_SUCCESS != ret) {
            opal_output (ompi_bcol_base_framework.framework_output, "In bcol_comm_query mca_bcol_basesmuma_allocate_sm_ctl_memory failed\n");
            return ret;
        }
        /*
         * put the memory onto the free list - we have worried about
         * alignment in the mpool allocation, and assume that the
         * ctl structures have the approriate size to mantain alignment
         */

        /* figure out segment size */
        n_ctl_structs=cs->basesmuma_num_mem_banks*
            cs->basesmuma_num_regions_per_bank;

        /* add memory for the control structure used for recycling the banks */
        n_ctl_structs+=cs->basesmuma_num_mem_banks;

        ctl_segement_size=n_ctl_structs*
            sizeof(mca_bcol_basesmuma_ctl_struct_t);

        total_memory=cs->sm_ctl_structs->map_size - (
            (char *)(cs->sm_ctl_structs->data_addr)-
            (char *)(cs->sm_ctl_structs->map_addr));
        total_memory-=cs->my_scratch_shared_memory_size;
        max_elements=total_memory/ctl_segement_size;

        /* populate the free list */
        data_ptr=cs->sm_ctl_structs->data_addr;

        for( i=0 ; i < max_elements ; i++ ) {
            list_data_t *item = OBJ_NEW(list_data_t);
            if( !item ) {
                return OMPI_ERR_OUT_OF_RESOURCE;
            }
            item->data=(void *)data_ptr;
            opal_list_append(&(cs->ctl_structures),(opal_list_item_t *)item);
            data_ptr+=ctl_segement_size;
        }
        /* set the scratch memory pointer and offset */
        cs->my_scratch_shared_memory=(char *)data_ptr;
        cs->scratch_offset_from_base_ctl_file=(size_t)
            ((char *)data_ptr-(char *)cs->sm_ctl_structs->map_addr);


        /* At this stage the memory is mapped and ready to use by the local rank.
         * However, the memory of other processes has not yet been mmaped into the
         * memory of this process.
         */
    }

    /* intialize no_userdata_ctl */
    sm_bcol_module->no_userdata_ctl=(list_data_t *)
        opal_list_remove_last(&(cs->ctl_structures));
    if (!sm_bcol_module->no_userdata_ctl) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    /* intialize userdata_ctl */
    sm_bcol_module->userdata_ctl = (list_data_t *)
        opal_list_remove_last(&(cs->ctl_structures));
    if (!sm_bcol_module->userdata_ctl) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    ret = base_bcol_basesmuma_setup_ctl (sm_bcol_module, cs);
    if (OMPI_SUCCESS != ret) {
        return ret;
    }

    ret = base_bcol_basesmuma_setup_ctl_struct (sm_bcol_module, cs, &(sm_bcol_module->colls_no_user_data));
    if( OMPI_SUCCESS != ret ) {
        return ret;
    }

    ret = base_bcol_basesmuma_setup_ctl_struct (sm_bcol_module, cs, &(sm_bcol_module->colls_with_user_data));
    if( OMPI_SUCCESS != ret ) {
        return ret;
    }

    /* used for blocking recursive doubling barrier */
    sm_bcol_module->index_blocking_barrier_memory_bank=0;

    /* gather the offsets of the control structs relative to the base
     *   of the shared memory file, and fill in the table with the
     *   address of all the control structues.
     */
    ret = base_bcol_basesmuma_exchange_ctl_params(sm_bcol_module, cs,
        &(sm_bcol_module->colls_no_user_data),sm_bcol_module->no_userdata_ctl);
    if( OMPI_SUCCESS != ret ) {
        return ret;
    }

    ret = base_bcol_basesmuma_exchange_ctl_params(sm_bcol_module, cs,
        &(sm_bcol_module->colls_with_user_data),sm_bcol_module->userdata_ctl);
    if( OMPI_SUCCESS != ret ) {
        return ret;
    }

    return OMPI_SUCCESS;
}

OBJ_CLASS_INSTANCE(list_data_t,
        opal_list_item_t, NULL, NULL);
