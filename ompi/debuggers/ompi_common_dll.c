/*
 * Copyright (c) 2007-2008 Cisco, Inc.  All rights resereved.
 * Copyright (c) 2004-2007 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

/**********************************************************************
 * Copyright (C) 2000-2004 by Etnus, LLC.
 * Copyright (C) 1999 by Etnus, Inc.
 * Copyright (C) 1997-1998 Dolphin Interconnect Solutions Inc.
 *
 * Permission is hereby granted to use, reproduce, prepare derivative
 * works, and to redistribute to others.
 *
 *				  DISCLAIMER
 *
 * Neither Dolphin Interconnect Solutions, Etnus LLC, nor any of their
 * employees, makes any warranty express or implied, or assumes any
 * legal liability or responsibility for the accuracy, completeness,
 * or usefulness of any information, apparatus, product, or process
 * disclosed, or represents that its use would not infringe privately
 * owned rights.
 *
 * This code was written by
 * James Cownie: Dolphin Interconnect Solutions. <jcownie@dolphinics.com>
 *               Etnus LLC <jcownie@etnus.com>
 **********************************************************************/

#include "ompi_config.h"

#include "ompi_common_dll_defs.h"

/* Basic callbacks into the debugger */
const mqs_basic_callbacks *mqs_basic_entrypoints;

#if defined(WORDS_BIGENDIAN)
static int host_is_big_endian = 1;
#else
static int host_is_big_endian = 0;
#endif

/*
 * Open MPI use a bunch of lists in order to keep track of the
 * internal objects. We have to make sure we're able to find all of
 * them in the image and compute their ofset in order to be able to
 * parse them later.  We need to find the opal_list_item_t, the
 * opal_list_t, the ompi_free_list_item_t, and the ompi_free_list_t.
 *
 * Once we have these offsets, we should make sure that we have access
 * to all requests lists and types. We're looking here only at the
 * basic type for the requests as they hold all the information we
 * need to export to the debugger.
 */
int ompi_fill_in_type_info(mqs_image *image, char **message)
{
    char* missing_in_action;
    mpi_image_info * i_info = (mpi_image_info *)mqs_get_image_info (image);

    {
        mqs_type* qh_type = mqs_find_type( image, "opal_list_item_t", mqs_lang_c );
        if( !qh_type ) {
            missing_in_action = "opal_list_item_t";
            goto type_missing;
        }
        i_info->opal_list_item_t.type = qh_type;
        i_info->opal_list_item_t.size = mqs_sizeof(qh_type);
        i_info->opal_list_item_t.offset.opal_list_next = mqs_field_offset(qh_type, "opal_list_next");
    }
    {
        mqs_type* qh_type = mqs_find_type( image, "opal_list_t", mqs_lang_c );
        if( !qh_type ) {
            missing_in_action = "opal_list_t";
            goto type_missing;
        }
        i_info->opal_list_t.type = qh_type;
        i_info->opal_list_t.size = mqs_sizeof(qh_type);
        i_info->opal_list_t.offset.opal_list_sentinel = mqs_field_offset(qh_type, "opal_list_sentinel");
    }
    {
        mqs_type* qh_type = mqs_find_type( image, "ompi_free_list_item_t", mqs_lang_c );
        if( !qh_type ) {
            missing_in_action = "ompi_free_list_item_t";
            goto type_missing;
        }
        /* This is just an overloaded opal_list_item_t */
        i_info->ompi_free_list_item_t.type = qh_type;
        i_info->ompi_free_list_item_t.size = mqs_sizeof(qh_type);
    }
    {
        mqs_type* qh_type = mqs_find_type( image, "ompi_free_list_t", mqs_lang_c );
        if( !qh_type ) {
            missing_in_action = "ompi_free_list_t";
            goto type_missing;
        }
        i_info->ompi_free_list_t.type = qh_type;
        i_info->ompi_free_list_t.size = mqs_sizeof(qh_type);
        i_info->ompi_free_list_t.offset.fl_frag_class = mqs_field_offset(qh_type, "fl_frag_class");
        i_info->ompi_free_list_t.offset.fl_mpool = mqs_field_offset(qh_type, "fl_mpool");
        i_info->ompi_free_list_t.offset.fl_frag_size = mqs_field_offset(qh_type, "fl_frag_size");
        i_info->ompi_free_list_t.offset.fl_frag_alignment = mqs_field_offset(qh_type, "fl_frag_alignment");
        i_info->ompi_free_list_t.offset.fl_allocations = mqs_field_offset(qh_type, "fl_allocations");
        i_info->ompi_free_list_t.offset.fl_max_to_alloc = mqs_field_offset(qh_type, "fl_max_to_alloc");
        i_info->ompi_free_list_t.offset.fl_num_per_alloc = mqs_field_offset(qh_type, "fl_num_per_alloc");
        i_info->ompi_free_list_t.offset.fl_num_allocated = mqs_field_offset(qh_type, "fl_num_allocated");
    }
    {
        mqs_type* qh_type = mqs_find_type( image, "opal_hash_table_t", mqs_lang_c );
        if( !qh_type ) {
            missing_in_action = "opal_hash_table_t";
            goto type_missing;
        }
        i_info->opal_hash_table_t.type = qh_type;
        i_info->opal_hash_table_t.size = mqs_sizeof(qh_type);
        i_info->opal_hash_table_t.offset.ht_table = mqs_field_offset(qh_type, "ht_table");
        i_info->opal_hash_table_t.offset.ht_table_size = mqs_field_offset(qh_type, "ht_table_size");
        i_info->opal_hash_table_t.offset.ht_size = mqs_field_offset(qh_type, "ht_size");
        i_info->opal_hash_table_t.offset.ht_mask = mqs_field_offset(qh_type, "ht_mask");
    }
    /*
     * Now let's look for all types required for reading the requests.
     */
    {
        mqs_type* qh_type = mqs_find_type( image, "ompi_request_t", mqs_lang_c );
        if( !qh_type ) {
            missing_in_action = "ompi_request_t";
            goto type_missing;
        }
        i_info->ompi_request_t.type = qh_type;
        i_info->ompi_request_t.size = mqs_sizeof(qh_type);
        i_info->ompi_request_t.offset.req_type = mqs_field_offset(qh_type, "req_type");
        i_info->ompi_request_t.offset.req_status = mqs_field_offset(qh_type, "req_status");
        i_info->ompi_request_t.offset.req_complete = mqs_field_offset(qh_type, "req_complete");
        i_info->ompi_request_t.offset.req_state = mqs_field_offset(qh_type, "req_state");
        i_info->ompi_request_t.offset.req_f_to_c_index = mqs_field_offset(qh_type, "req_f_to_c_index");
    }
    {
        mqs_type* qh_type = mqs_find_type( image, "mca_pml_base_request_t", mqs_lang_c );
        if( !qh_type ) {
            missing_in_action = "mca_pml_base_request_t";
            goto type_missing;
        }
        i_info->mca_pml_base_request_t.type = qh_type;
        i_info->mca_pml_base_request_t.size = mqs_sizeof(qh_type);
        i_info->mca_pml_base_request_t.offset.req_addr = mqs_field_offset(qh_type, "req_addr");
        i_info->mca_pml_base_request_t.offset.req_count = mqs_field_offset(qh_type, "req_count");
        i_info->mca_pml_base_request_t.offset.req_peer = mqs_field_offset(qh_type, "req_peer");
        i_info->mca_pml_base_request_t.offset.req_tag = mqs_field_offset(qh_type, "req_tag");
        i_info->mca_pml_base_request_t.offset.req_comm = mqs_field_offset(qh_type, "req_comm");
        i_info->mca_pml_base_request_t.offset.req_datatype = mqs_field_offset(qh_type, "req_datatype");
        i_info->mca_pml_base_request_t.offset.req_proc = mqs_field_offset(qh_type, "req_proc");
        i_info->mca_pml_base_request_t.offset.req_sequence = mqs_field_offset(qh_type, "req_sequence");
        i_info->mca_pml_base_request_t.offset.req_type = mqs_field_offset(qh_type, "req_type");
        i_info->mca_pml_base_request_t.offset.req_pml_complete = mqs_field_offset(qh_type, "req_pml_complete");
    }
    {
        mqs_type* qh_type = mqs_find_type( image, "mca_pml_base_send_request_t", mqs_lang_c );
        if( !qh_type ) {
            missing_in_action = "mca_pml_base_send_request_t";
            goto type_missing;
        }
        i_info->mca_pml_base_send_request_t.type = qh_type;
        i_info->mca_pml_base_send_request_t.size = mqs_sizeof(qh_type);
        i_info->mca_pml_base_send_request_t.offset.req_addr = mqs_field_offset(qh_type, "req_addr");
        i_info->mca_pml_base_send_request_t.offset.req_bytes_packed = mqs_field_offset(qh_type, "req_bytes_packed");
        i_info->mca_pml_base_send_request_t.offset.req_send_mode = mqs_field_offset(qh_type, "req_send_mode");
    }
    {
        mqs_type* qh_type = mqs_find_type( image, "mca_pml_base_recv_request_t", mqs_lang_c );
        if( !qh_type ) {
            missing_in_action = "mca_pml_base_recv_request_t";
            goto type_missing;
        }
        i_info->mca_pml_base_recv_request_t.type = qh_type;
        i_info->mca_pml_base_recv_request_t.size = mqs_sizeof(qh_type);
        i_info->mca_pml_base_recv_request_t.offset.req_bytes_packed = mqs_field_offset(qh_type, "req_bytes_packed");
    }
    /*
     * Gather information about the received fragments and theirs headers.
     */
#if 0  /* Disabled until I find a better way */
    {
        mqs_type* qh_type = mqs_find_type( image, "mca_pml_ob1_common_hdr_t", mqs_lang_c );
        if( !qh_type ) {
            missing_in_action = "mca_pml_ob1_common_hdr_t";
            goto type_missing;
        }
        i_info->mca_pml_ob1_common_hdr_t.type = qh_type;
        i_info->mca_pml_ob1_common_hdr_t.size = mqs_sizeof(qh_type);
        i_info->mca_pml_ob1_common_hdr_t.offset.hdr_type = mqs_field_offset(qh_type, "hdr_type");
        i_info->mca_pml_ob1_common_hdr_t.offset.hdr_flags = mqs_field_offset(qh_type, "hdr_flags");
    }
    {
        mqs_type* qh_type = mqs_find_type( image, "mca_pml_ob1_match_hdr_t", mqs_lang_c );
        if( !qh_type ) {
            missing_in_action = "mca_pml_ob1_match_hdr_t";
            goto type_missing;
        }
        i_info->mca_pml_ob1_match_hdr_t.type = qh_type;
        i_info->mca_pml_ob1_match_hdr_t.size = mqs_sizeof(qh_type);
        i_info->mca_pml_ob1_match_hdr_t.offset.hdr_common = mqs_field_offset(qh_type, "hdr_common");
        i_info->mca_pml_ob1_match_hdr_t.offset.hdr_ctx    = mqs_field_offset(qh_type, "hdr_ctx");
        i_info->mca_pml_ob1_match_hdr_t.offset.hdr_src    = mqs_field_offset(qh_type, "hdr_src");
        i_info->mca_pml_ob1_match_hdr_t.offset.hdr_tag    = mqs_field_offset(qh_type, "hdr_tag");
        i_info->mca_pml_ob1_match_hdr_t.offset.hdr_seq    = mqs_field_offset(qh_type, "hdr_seq");
    }
    {
        mqs_type* qh_type = mqs_find_type( image, "mca_pml_ob1_recv_frag_t", mqs_lang_c );
        if( !qh_type ) {
            missing_in_action = "mca_pml_ob1_recv_frag_t";
            goto type_missing;
        }
        i_info->mca_pml_ob1_recv_frag_t.type = qh_type;
        i_info->mca_pml_ob1_recv_frag_t.size = mqs_sizeof(qh_type);
        i_info->mca_pml_ob1_recv_frag_t.offset.hdr = mqs_field_offset(qh_type, "hdr");
        i_info->mca_pml_ob1_recv_frag_t.offset.request = mqs_field_offset(qh_type, "request");
    }
#endif
    /*
     * And now let's look at the communicator and group structures.
     */
    {
        mqs_type* qh_type = mqs_find_type( image, "opal_pointer_array_t", mqs_lang_c );
        if( !qh_type ) {
            missing_in_action = "opal_pointer_array_t";
            goto type_missing;
        }
        i_info->opal_pointer_array_t.type = qh_type;
        i_info->opal_pointer_array_t.size = mqs_sizeof(qh_type);
        i_info->opal_pointer_array_t.offset.lowest_free = mqs_field_offset(qh_type, "lowest_free");
        i_info->opal_pointer_array_t.offset.number_free = mqs_field_offset(qh_type, "number_free");
        i_info->opal_pointer_array_t.offset.size = mqs_field_offset(qh_type, "size");
        i_info->opal_pointer_array_t.offset.addr = mqs_field_offset(qh_type, "addr");
    }
    {
        mqs_type* qh_type = mqs_find_type( image, "ompi_communicator_t", mqs_lang_c );
        if( !qh_type ) {
            missing_in_action = "ompi_communicator_t";
            goto type_missing;
        }
        i_info->ompi_communicator_t.type = qh_type;
        i_info->ompi_communicator_t.size = mqs_sizeof(qh_type);
        i_info->ompi_communicator_t.offset.c_name = mqs_field_offset(qh_type, "c_name");
        i_info->ompi_communicator_t.offset.c_contextid = mqs_field_offset(qh_type, "c_contextid");
        i_info->ompi_communicator_t.offset.c_my_rank = mqs_field_offset(qh_type, "c_my_rank" );
        i_info->ompi_communicator_t.offset.c_local_group = mqs_field_offset(qh_type, "c_local_group" );
        i_info->ompi_communicator_t.offset.c_remote_group = mqs_field_offset(qh_type, "c_remote_group" );
        i_info->ompi_communicator_t.offset.c_flags = mqs_field_offset(qh_type, "c_flags" );

        i_info->ompi_communicator_t.offset.c_f_to_c_index = mqs_field_offset(qh_type, "c_f_to_c_index" );

        i_info->ompi_communicator_t.offset.c_topo_comm = mqs_field_offset(qh_type, "c_topo_comm" );
        i_info->ompi_communicator_t.offset.c_keyhash = mqs_field_offset(qh_type, "c_keyhash" );
    }
    {
        mqs_type* qh_type = mqs_find_type( image, "mca_topo_base_comm_1_0_0_t", mqs_lang_c );
        if( !qh_type ) {
            missing_in_action = "mca_topo_base_comm_1_0_0_t";
            goto type_missing;
        }
        i_info->ompi_mca_topo_base_comm_1_0_0_t.type = qh_type;
        i_info->ompi_mca_topo_base_comm_1_0_0_t.size = mqs_sizeof(qh_type);
        i_info->ompi_mca_topo_base_comm_1_0_0_t.offset.mtc_ndims_or_nnodes = 
            mqs_field_offset(qh_type, "mtc_ndims_or_nnodes");
        i_info->ompi_mca_topo_base_comm_1_0_0_t.offset.mtc_dims_or_index = 
            mqs_field_offset(qh_type, "mtc_dims_or_index");
        i_info->ompi_mca_topo_base_comm_1_0_0_t.offset.mtc_periods_or_edges = 
            mqs_field_offset(qh_type, "mtc_periods_or_edges" );
        i_info->ompi_mca_topo_base_comm_1_0_0_t.offset.mtc_reorder = 
            mqs_field_offset(qh_type, "mtc_reorder" );
    }
    {
        mqs_type* qh_type = mqs_find_type( image, "ompi_group_t", mqs_lang_c );
        if( !qh_type ) {
            missing_in_action = "ompi_group_t";
            goto type_missing;
        }
        i_info->ompi_group_t.type = qh_type;
        i_info->ompi_group_t.size = mqs_sizeof(qh_type);
        i_info->ompi_group_t.offset.grp_proc_count = mqs_field_offset(qh_type, "grp_proc_count");
        i_info->ompi_group_t.offset.grp_my_rank = mqs_field_offset(qh_type, "grp_my_rank");
        i_info->ompi_group_t.offset.grp_flags = mqs_field_offset(qh_type, "grp_flags" );
    }
    {
        mqs_type* qh_type = mqs_find_type( image, "ompi_status_public_t", mqs_lang_c );
        if( !qh_type ) {
            missing_in_action = "ompi_status_public_t";
            goto type_missing;
        }
        i_info->ompi_status_public_t.type = qh_type;
        i_info->ompi_status_public_t.size = mqs_sizeof(qh_type);
        i_info->ompi_status_public_t.offset.MPI_SOURCE = mqs_field_offset(qh_type, "MPI_SOURCE");
        i_info->ompi_status_public_t.offset.MPI_TAG = mqs_field_offset(qh_type, "MPI_TAG");
        i_info->ompi_status_public_t.offset.MPI_ERROR = mqs_field_offset(qh_type, "MPI_ERROR" );
        i_info->ompi_status_public_t.offset._count = mqs_field_offset(qh_type, "_count" );
        i_info->ompi_status_public_t.offset._cancelled = mqs_field_offset(qh_type, "_cancelled" );
    }
    {
        mqs_type* qh_type = mqs_find_type( image, "ompi_datatype_t", mqs_lang_c );
        if( !qh_type ) {
            missing_in_action = "ompi_datatype_t";
            goto type_missing;
        }
        i_info->ompi_datatype_t.type = qh_type;
        i_info->ompi_datatype_t.size = mqs_sizeof(qh_type);
        i_info->ompi_datatype_t.offset.size = mqs_field_offset(qh_type, "size");
        i_info->ompi_datatype_t.offset.name = mqs_field_offset(qh_type, "name");
    }

    /* All the types are here. Let's succesfully return. */
    return mqs_ok;

 type_missing:
    /*
     * One of the required types is missing in the image. We are
     * unable to extract the information we need from the pointers. We
     * did our best but here we're at our limit. Give up!
     */
    *message = missing_in_action;
    printf( "The following type is missing %s\n", missing_in_action );
    return err_missing_type;
}

/***********************************************************************
 * Functions to access the image memory. They are specialized based    *
 * on the type we want to access and the debugged process architecture *
 ***********************************************************************/
mqs_taddr_t ompi_fetch_pointer (mqs_process *proc, mqs_taddr_t addr, 
                                mpi_process_info *p_info)
{
    int isize = p_info->sizes.pointer_size;
    char buffer[8];                  /* ASSUME the type fits in 8 bytes */
    mqs_taddr_t res = 0;

    if (mqs_ok == mqs_fetch_data (proc, addr, isize, buffer))
        mqs_target_to_host (proc, buffer, 
                            ((char *)&res) + (host_is_big_endian ? sizeof(mqs_taddr_t)-isize : 0), 
                            isize);

    return res;
} /* fetch_pointer */

/***********************************************************************/
mqs_tword_t ompi_fetch_int (mqs_process *proc, mqs_taddr_t addr, 
                            mpi_process_info *p_info)
{
    int isize = p_info->sizes.int_size;
    char buffer[8];                  /* ASSUME the type fits in 8 bytes */
    mqs_tword_t res = 0;

    if (mqs_ok == mqs_fetch_data (proc, addr, isize, buffer)) {
        mqs_target_to_host (proc, buffer, 
                            ((char *)&res) + (host_is_big_endian ? sizeof(mqs_tword_t)-isize : 0), 
                            isize);
    }
    return res;
} /* fetch_int */

/***********************************************************************/
mqs_tword_t ompi_fetch_bool(mqs_process *proc, mqs_taddr_t addr, 
                            mpi_process_info *p_info)
{
    int isize = p_info->sizes.bool_size;
    mqs_tword_t res = 0;

    mqs_fetch_data (proc, addr, isize, &res);
    return (0 == res ? 0 : 1);
} /* fetch_bool */

/***********************************************************************/
mqs_taddr_t ompi_fetch_size_t(mqs_process *proc, mqs_taddr_t addr, 
                              mpi_process_info *p_info)
{
    int isize = p_info->sizes.size_t_size;
    char buffer[8];                  /* ASSUME the type fits in 8 bytes */
    mqs_taddr_t res = 0;

    if (mqs_ok == mqs_fetch_data (proc, addr, isize, buffer))
        mqs_target_to_host (proc, buffer, 
                            ((char *)&res) + (host_is_big_endian ? sizeof(mqs_taddr_t)-isize : 0), 
                            isize);
  
    return res;
} /* fetch_size_t */

/***********************************************************************/

int ompi_fetch_opal_pointer_array_info(mqs_process *proc, mqs_taddr_t addr, 
                                       mpi_process_info *p_info,
                                       int *size, int *lowest_free, 
                                       int *number_free)
{
    mqs_image *image = mqs_get_image(proc);
    mpi_image_info *i_info = (mpi_image_info *) mqs_get_image_info(image);

    *size = ompi_fetch_int(proc, 
                           addr + i_info->opal_pointer_array_t.offset.size,
                           p_info);
    *lowest_free = ompi_fetch_int(proc, 
                                  addr + i_info->opal_pointer_array_t.offset.lowest_free,
                                  p_info);
    *number_free = ompi_fetch_int(proc, 
                                  addr + i_info->opal_pointer_array_t.offset.number_free,
                                  p_info);
    return mqs_ok;
}

/***********************************************************************/

int ompi_fetch_opal_pointer_array_item(mqs_process *proc, mqs_taddr_t addr, 
                                       mpi_process_info *p_info, int index,
                                       mqs_taddr_t *item)
{
    mqs_image *image = mqs_get_image(proc);
    mpi_image_info *i_info = (mpi_image_info *) mqs_get_image_info(image);
    int size, lowest_free, number_free;
    mqs_taddr_t base;

    if (index < 0) {
        return mqs_no_information;
    }

    ompi_fetch_opal_pointer_array_info(proc, addr, p_info, &size, 
                                       &lowest_free, &number_free);
    if (index >= size) {
        return mqs_no_information;
    }

    base = ompi_fetch_pointer(proc, 
                              addr + i_info->opal_pointer_array_t.offset.addr,
                              p_info);
    *item = ompi_fetch_pointer(proc,
                               base + index * p_info->sizes.pointer_size,
                               p_info);

    return mqs_ok;
}
