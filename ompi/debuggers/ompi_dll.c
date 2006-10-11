/* -*- Mode: C; c-basic-offset:4 ; -*- */
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

/* Update log
 *
 * Jul 12 2001 FNW: Add a meaningful ID to the communicator name, and switch
 *                  to using the recv_context as the unique_id field.
 * Mar  6 2001 JHC: Add mqs_get_comm_group to allow a debugger to acquire
 *                  processes less eagerly.
 * Dec 13 2000 JHC: totalview/2514: Modify image_has_queues to return
 *                  a silent FALSE if none of the expected data is
 *                  present. This way you won't get complaints when
 *                  you try this on non MPICH processes.
 * Sep  8 2000 JVD: #include <string.h> to silence Linux Alpha compiler warnings.
 * Mar 21 2000 JHC: Add the new entrypoint mqs_dll_taddr_width
 * Nov 26 1998 JHC: Fix the problem that we weren't handling
 *                  MPIR_Ignore_queues properly.
 * Oct 22 1998 JHC: Fix a zero allocation problem
 * Aug 19 1998 JHC: Fix some problems in our use of target_to_host on
 *                  big endian machines.
 * May 28 1998 JHC: Use the extra information we can return to say
 *                  explicitly that sends are only showing non-blocking ops
 * May 19 1998 JHC: Changed the names of the structs and added casts
 *                  where needed to reflect the change to the way we handle
 *                  type safety across the interface.
 * Oct 27 1997 JHC: Created by exploding db_message_state_mpich.cxx
 */

/*
 * This file is an example of how to use the DLL interface to handle
 * message queue display from a debugger.  It provides all of the
 * functions required to display MPICH message queues.
 * It has been tested with TotalView.
 *
 * James Cownie <jcownie@dolphinics.com>
 */

/**
 * Right now there is no MPI2 support
 */
#define FOR_MPI2  0

/* 
   The following was added by William Gropp to improve the portability 
   to systems with non-ANSI C compilers 
 */

#include "ompi_config.h"

#ifdef HAVE_NO_C_CONST
#define const
#endif
#if defined(HAVE_STRING_H)
#include <string.h>
#endif  /* defined(HAVE_STRING_H) */
#if defined(HAVE_STDLIB_H)
#include <stdlib.h>
#endif  /* defined(HAVE_STDLIB_H) */

#include "ompi/request/request.h"
#include "ompi/mca/pml/base/pml_base_request.h"

/* 
   End of inclusion
 */

#include "mpi_interface.h"
#include "ompi_dll_defs.h"

/* Essential macros for C */
#ifndef NULL
#define NULL ((void *)0)
#endif
#ifndef TRUE
#define TRUE (0==0)
#endif
#ifndef FALSE
#define FALSE (0==1)
#endif

#ifdef OLD_STYLE_CPP_CONCAT
#define concat(a,b) a/**/b
#define stringize(a) "a"
#else
#define concat(a,b) a##b
#define stringize(a) #a
#endif

/**********************************************************************/
/* Set up the basic callbacks into the debugger, also work out 
 * one crucial piece of info about the machine we're running on.
 */
static const mqs_basic_callbacks *mqs_basic_entrypoints;
static int host_is_big_endian;

void mqs_setup_basic_callbacks (const mqs_basic_callbacks * cb)
{
  int t = 1;

  host_is_big_endian    = (*(char *)&t) != 1;
  mqs_basic_entrypoints = cb;
} /* mqs_setup_callbacks */

/**********************************************************************/
/* Macros to make it transparent that we're calling the TV functions
 * through function pointers.
 */
#define mqs_malloc           (mqs_basic_entrypoints->mqs_malloc_fp)
#define mqs_free             (mqs_basic_entrypoints->mqs_free_fp)
#define mqs_prints           (mqs_basic_entrypoints->mqs_dprints_fp)
#define mqs_put_image_info   (mqs_basic_entrypoints->mqs_put_image_info_fp)
#define mqs_get_image_info   (mqs_basic_entrypoints->mqs_get_image_info_fp)
#define mqs_put_process_info (mqs_basic_entrypoints->mqs_put_process_info_fp)
#define mqs_get_process_info (mqs_basic_entrypoints->mqs_get_process_info_fp)

/* These macros *RELY* on the function already having set up the conventional
 * local variables i_info or p_info.
 */
#define mqs_find_type        (i_info->image_callbacks->mqs_find_type_fp)
#define mqs_field_offset     (i_info->image_callbacks->mqs_field_offset_fp)
#define mqs_sizeof           (i_info->image_callbacks->mqs_sizeof_fp)
#define mqs_get_type_sizes   (i_info->image_callbacks->mqs_get_type_sizes_fp)
#define mqs_find_function    (i_info->image_callbacks->mqs_find_function_fp)
#define mqs_find_symbol      (i_info->image_callbacks->mqs_find_symbol_fp)

#define mqs_get_image        (p_info->process_callbacks->mqs_get_image_fp)
#define mqs_get_global_rank  (p_info->process_callbacks->mqs_get_global_rank_fp)
#define mqs_fetch_data       (p_info->process_callbacks->mqs_fetch_data_fp)
#define mqs_target_to_host   (p_info->process_callbacks->mqs_target_to_host_fp)

/**********************************************************************/
/* Version handling functions.
 * This one should never be changed.
 */
int mqs_version_compatibility (void)
{
    return MQS_INTERFACE_COMPATIBILITY;
} /* mqs_version_compatibility */

/* This one can say what you like */
char *mqs_version_string (void)
{
    return "Open MPI message queue support for parallel debuggers compiled on " __DATE__;
} /* mqs_version_string */

/* So the debugger can tell what interface width the library was compiled with */
int mqs_dll_taddr_width (void)
{
    return sizeof (mqs_taddr_t);
} /* mqs_dll_taddr_width */

/**********************************************************************/
/* Additional error codes and error string conversion.
 */
enum {
    err_silent_failure  = mqs_first_user_code,

    err_no_current_communicator,
    err_bad_request,
    err_no_store,

    err_failed_qhdr,
    err_unexpected,
    err_posted,

    err_failed_queue,
    err_first,

    err_context_id,
    err_tag,
    err_tagmask,
    err_lsrc,
    err_srcmask,
    err_next,
    err_ptr,

    err_missing_type,
    err_missing_symbol,

    err_db_shandle,
    err_db_comm,
    err_db_target,
    err_db_tag,
    err_db_data,
    err_db_byte_length,
    err_db_next,

    err_failed_rhandle,
    err_is_complete,
    err_buf,
    err_len,
    err_s,

    err_failed_status,
    err_count,
    err_MPI_SOURCE,
    err_MPI_TAG,

    err_failed_commlist,
    err_sequence_number,
    err_comm_first,

    err_failed_communicator,
    err_np,
    err_lrank_to_grank,
    err_send_context,
    err_recv_context,
    err_comm_next,
    err_comm_name,

    err_all_communicators,
    err_mpid_recvs,
    err_group_corrupt
};

/**********************************************************************/
/* Forward declarations 
 */
static mqs_taddr_t fetch_pointer (mqs_process * proc, mqs_taddr_t addr, mpi_process_info *p_info);
static mqs_tword_t fetch_int (mqs_process * proc, mqs_taddr_t addr, mpi_process_info *p_info);
static mqs_tword_t fetch_bool(mqs_process * proc, mqs_taddr_t addr, mpi_process_info *p_info);

/* Internal structure we hold for each communicator */
typedef struct communicator_t
{
    struct communicator_t * next;
    group_t *               group;		/* Translations */
    int                     recv_context;	/* Unique ID for the communicator */
    mqs_taddr_t             comm_ptr;
    int                     present;
    mqs_communicator        comm_info;		/* Info needed at the higher level */
} communicator_t;

#if defined(CODE_NOT_USED)
/**********************************************************************/
/* Functions to handle translation groups.
 * We have a list of these on the process info, so that we can
 * share the group between multiple communicators.
 */
/**********************************************************************/
/* Translate a process number */
static int translate (group_t *this, int index) 
{ 	
    if (index == MQS_INVALID_PROCESS ||
        ((unsigned int)index) >= ((unsigned int) this->entries))
        return MQS_INVALID_PROCESS;
    else
        return this->local_to_global[index]; 
} /* translate */

/**********************************************************************/
/* Reverse translate a process number i.e. global to local*/
static int reverse_translate (group_t * this, int index) 
{ 	
    int i;
    for ( i = 0; i < this->entries; i++ )
        if( this->local_to_global[i] == index )
            return i;

    return MQS_INVALID_PROCESS;
} /* reverse_translate */
#endif  /* CODE_NOT_USED */

/**********************************************************************/
/* Search the group list for this group, if not found create it.
 */
static group_t * find_or_create_group (mqs_process *proc,
				       mqs_taddr_t table)
{
    mpi_process_info *p_info = (mpi_process_info *)mqs_get_process_info (proc);
    mqs_image * image          = mqs_get_image (proc);
    mpi_image_info *i_info   = (mpi_image_info *)mqs_get_image_info (image);
    int intsize = p_info->sizes.int_size;
    communicator_t *comm  = p_info->communicator_list;
    int *tr;
    char *trbuffer;
    int i;
    group_t *g;
    int np;

    np = fetch_int( proc,
                    table + i_info->ompi_group_t.offset.grp_proc_count,
                    p_info );
    if( np < 0 ) {
        printf( "Get a size for the communicator = %d\n", np );
        return NULL;  /* Makes no sense ! */
    }
    /* Iterate over each communicator seeing if we can find this group */
    for (;comm; comm = comm->next) {
        g = comm->group;
        if (g && g->table_base == table) {
            g->ref_count++;			/* Someone else is interested */
            return g;
        }
    }

    /* Hmm, couldn't find one, so fetch it */	
    g = (group_t *)mqs_malloc (sizeof (group_t));
    tr = (int *)mqs_malloc (np*sizeof(int));
    trbuffer = (char *)mqs_malloc (np*intsize);
    g->local_to_global = tr;

    if (mqs_ok != mqs_fetch_data (proc, table, np*intsize, trbuffer) ) {
        mqs_free (g);
        mqs_free (tr);
        mqs_free (trbuffer);
        return NULL;
    }

    /* This code is assuming that sizeof(int) is the same on target and host...
     * that's a bit flaky, but probably actually OK.
     */
    for( i = 0; i < np; i++ )
        mqs_target_to_host( proc, trbuffer+intsize*i, &tr[i], intsize );

    mqs_free(trbuffer);

    g->entries = np;
    g->ref_count = 1;
    return g;
} /* find_or_create_group */

/***********************************************************************/
static void group_decref (group_t * group)
{
    if (--(group->ref_count) == 0) {
        mqs_free (group->local_to_global);
        mqs_free (group);
    }
} /* group_decref */

/***********************************************************************
 * Perform basic setup for the image, we just allocate and clear
 * our info.
 */
int mqs_setup_image (mqs_image *image, const mqs_image_callbacks *icb)
{
    mpi_image_info *i_info = (mpi_image_info *)mqs_malloc (sizeof (mpi_image_info));

    if (!i_info)
        return err_no_store;

    memset ((void *)i_info, 0, sizeof (mpi_image_info));
    i_info->image_callbacks = icb;		/* Before we do *ANYTHING* */

    mqs_put_image_info (image, (mqs_image_info *)i_info);
  
    return mqs_ok;
} /* mqs_setup_image */


/***********************************************************************
 * Check for all the information we require to access the MPICH message queues.
 * Stash it into our structure on the image if we're succesful.
 */

/* A macro to save a lot of typing. */
#define GETOFFSET(type, field)                                          \
    do {                                                                \
        i_info->concat(field,_offs) = mqs_field_offset(type, stringize(field)); \
        if (i_info->concat(field,_offs) < 0)                            \
            return concat (err_,field);                                 \
    } while (0)

int mqs_image_has_queues (mqs_image *image, char **message)
{
    mpi_image_info * i_info = (mpi_image_info *)mqs_get_image_info (image);
    char* missing_in_action;

    /* Default failure message ! */
    *message = "The symbols and types in the Open MPI library used by TotalView\n"
        "to extract the message queues are not as expected in\n"
        "the image '%s'\n"
        "No message queue display is possible.\n"
        "This is probably an MPICH version or configuration problem.";

    /* Force in the file containing our breakpoint function, to ensure that 
     * types have been read from there before we try to look them up.
     */
    mqs_find_function (image, "MPIR_Breakpoint", mqs_lang_c, NULL);

    /* Are we supposed to ignore this ? (e.g. it's really an HPF runtime using the
     * MPICH process acquisition, but not wanting queue display) 
     */
    if (mqs_find_symbol (image, "MPIR_Ignore_queues", NULL) == mqs_ok) {
        *message = NULL;				/* Fail silently */
        return err_silent_failure;
    }

    /**
     * Open MPI use a bunch of lists in order to kep track of the internal
     * objects. We have to make sure we're able to find all of them in the image
     * and compute their ofset in order to be able to parse them later.
     * We need to find the opal_list_item_t, the opal_list_t, the ompi_free_list_item_t,
     * and the ompi_free_list_t.
     *
     * Once we have these offsets, we should make sure that we have access to all
     * requests lists and types. We're looking here only at the basic type for the
     * requests as they hold all the information we need to export to the debugger.
     */
    {
        mqs_type* qh_type = mqs_find_type( image, "opal_list_item_t", mqs_lang_c );
        if( !qh_type ) {
            missing_in_action = "opal_list_item_t";
            goto type_missing;
        }
        i_info->opal_list_item_t.size = mqs_sizeof(qh_type);
        i_info->opal_list_item_t.offset.opal_list_next = mqs_field_offset(qh_type, "opal_list_next");
    }
    {
        mqs_type* qh_type = mqs_find_type( image, "opal_list_t", mqs_lang_c );
        if( !qh_type ) {
            missing_in_action = "opal_list_t";
            goto type_missing;
        }
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
        i_info->ompi_free_list_item_t.size = mqs_sizeof(qh_type);
    }
    {
        mqs_type* qh_type = mqs_find_type( image, "ompi_free_list_memory_t", mqs_lang_c );
        if( !qh_type ) {
            missing_in_action = "ompi_free_list_memory_t";
            goto type_missing;
        }
        /* This is just an overloaded opal_list_item_t */
        i_info->ompi_free_list_memory_t.size = mqs_sizeof(qh_type);
    }
    {
        mqs_type* qh_type = mqs_find_type( image, "ompi_free_list_t", mqs_lang_c );
        if( !qh_type ) {
            missing_in_action = "ompi_free_list_t";
            goto type_missing;
        }
        i_info->ompi_free_list_t.size = mqs_sizeof(qh_type);
        i_info->ompi_free_list_t.offset.fl_elem_size = mqs_field_offset(qh_type, "fl_elem_size");
        i_info->ompi_free_list_t.offset.fl_header_space = mqs_field_offset(qh_type, "fl_header_space");
        i_info->ompi_free_list_t.offset.fl_alignment = mqs_field_offset(qh_type, "fl_alignment");
        i_info->ompi_free_list_t.offset.fl_allocations = mqs_field_offset(qh_type, "fl_allocations");
        i_info->ompi_free_list_t.offset.fl_max_to_alloc = mqs_field_offset(qh_type, "fl_max_to_alloc");
        i_info->ompi_free_list_t.offset.fl_num_per_alloc = mqs_field_offset(qh_type, "fl_num_per_alloc");
        i_info->ompi_free_list_t.offset.fl_num_allocated = mqs_field_offset(qh_type, "fl_num_allocated");
    }
    /**
     * Now let's look for all types required for reading the requests.
     */
    {
        mqs_type* qh_type = mqs_find_type( image, "ompi_request_t", mqs_lang_c );
        if( !qh_type ) {
            missing_in_action = "ompi_request_t";
            goto type_missing;
        }
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
        i_info->mca_pml_base_request_t.size = mqs_sizeof(qh_type);
        i_info->mca_pml_base_request_t.offset.req_addr = mqs_field_offset(qh_type, "req_addr");
        i_info->mca_pml_base_request_t.offset.req_count = mqs_field_offset(qh_type, "req_count");
        i_info->mca_pml_base_request_t.offset.req_peer = mqs_field_offset(qh_type, "req_peer");
        i_info->mca_pml_base_request_t.offset.req_tag = mqs_field_offset(qh_type, "req_tag");
        i_info->mca_pml_base_request_t.offset.req_comm = mqs_field_offset(qh_type, "req_comm");
        i_info->mca_pml_base_request_t.offset.req_proc = mqs_field_offset(qh_type, "req_proc");
        i_info->mca_pml_base_request_t.offset.req_sequence = mqs_field_offset(qh_type, "req_sequence");
        i_info->mca_pml_base_request_t.offset.req_type = mqs_field_offset(qh_type, "req_type");
    }
    {
        mqs_type* qh_type = mqs_find_type( image, "mca_pml_base_send_request_t", mqs_lang_c );
        if( !qh_type ) {
            missing_in_action = "mca_pml_base_send_request_t";
            goto type_missing;
        }
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
        i_info->mca_pml_base_recv_request_t.size = mqs_sizeof(qh_type);
        i_info->mca_pml_base_recv_request_t.offset.req_bytes_packed = mqs_field_offset(qh_type, "req_bytes_packed");
    }
    /**
     * Gather information about the receive fragments and theirs headers.
     */
    {
        mqs_type* qh_type = mqs_find_type( image, "mca_pml_ob1_common_hdr_t", mqs_lang_c );
        if( !qh_type ) {
            missing_in_action = "mca_pml_ob1_common_hdr_t";
            goto type_missing;
        }
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
        i_info->mca_pml_ob1_recv_frag_t.size = mqs_sizeof(qh_type);
        i_info->mca_pml_ob1_recv_frag_t.offset.hdr = mqs_field_offset(qh_type, "hdr");
        i_info->mca_pml_ob1_recv_frag_t.offset.request = mqs_field_offset(qh_type, "request");
    }

    /**
     * And now let's look at the communicator and group structures.
     */
    {
        mqs_type* qh_type = mqs_find_type( image, "ompi_pointer_array_t", mqs_lang_c );
        if( !qh_type ) {
            missing_in_action = "ompi_pointer_array_t";
            goto type_missing;
        }
        i_info->ompi_pointer_array_t.size = mqs_sizeof(qh_type);
        i_info->ompi_pointer_array_t.offset.lowest_free = mqs_field_offset(qh_type, "lowest_free");
        i_info->ompi_pointer_array_t.offset.size = mqs_field_offset(qh_type, "size");
        i_info->ompi_pointer_array_t.offset.addr = mqs_field_offset(qh_type, "addr");
    }
    {
        mqs_type* qh_type = mqs_find_type( image, "ompi_communicator_t", mqs_lang_c );
        if( !qh_type ) {
            missing_in_action = "ompi_communicator_t";
            goto type_missing;
        }
        i_info->ompi_communicator_t.size = mqs_sizeof(qh_type);
        i_info->ompi_communicator_t.offset.c_name = mqs_field_offset(qh_type, "c_name");
        i_info->ompi_communicator_t.offset.c_contextid = mqs_field_offset(qh_type, "c_contextid");
        i_info->ompi_communicator_t.offset.c_my_rank = mqs_field_offset(qh_type, "c_my_rank" );
        i_info->ompi_communicator_t.offset.c_local_group = mqs_field_offset(qh_type, "c_local_group" );
    }
    {
        mqs_type* qh_type = mqs_find_type( image, "ompi_group_t", mqs_lang_c );
        if( !qh_type ) {
            missing_in_action = "ompi_group_t";
            goto type_missing;
        }
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
        i_info->ompi_status_public_t.size = mqs_sizeof(qh_type);
        i_info->ompi_status_public_t.offset.MPI_SOURCE = mqs_field_offset(qh_type, "MPI_SOURCE");
        i_info->ompi_status_public_t.offset.MPI_TAG = mqs_field_offset(qh_type, "MPI_TAG");
        i_info->ompi_status_public_t.offset.MPI_ERROR = mqs_field_offset(qh_type, "MPI_ERROR" );
        i_info->ompi_status_public_t.offset._count = mqs_field_offset(qh_type, "_count" );
        i_info->ompi_status_public_t.offset._cancelled = mqs_field_offset(qh_type, "_cancelled" );
    }

    /* All the types are here. Let's succesfully return. */
    return mqs_ok;

 type_missing:
    /**
     * One of the required types are missing in the image. We are unable to extract
     * the information we need from the pointers. Give up!
     */
    *message = missing_in_action;
    return err_missing_type;
} /* mqs_image_has_queues */

/***********************************************************************
 * Setup information needed for a specific process.
 * TV assumes that this will hang something onto the process,
 * if nothing is attached to it, then TV will believe that this process
 * has no message queue information.
 */
int mqs_setup_process (mqs_process *process, const mqs_process_callbacks *pcb)
{ 
    /* Extract the addresses of the global variables we need and save them away */
    mpi_process_info *p_info = (mpi_process_info *)mqs_malloc (sizeof (mpi_process_info));

    if (p_info) {
        mqs_image        *image;
        mpi_image_info *i_info;

        p_info->process_callbacks = pcb;

        /* Now we can get the rest of the info ! */
        image  = mqs_get_image (process);
        i_info = (mpi_image_info *)mqs_get_image_info (image);

        /* Library starts at zero, so this ensures we go look to start with */
        p_info->communicator_sequence = -1;
        /* We have no communicators yet */
        p_info->communicator_list     = NULL;
        mqs_get_type_sizes (process, &p_info->sizes);

        mqs_put_process_info (process, (mqs_process_info *)p_info);
      
        return mqs_ok;
    }
    return err_no_store;
} /* mqs_setup_process */

/***********************************************************************
 * Check the process for message queues.
 */
int mqs_process_has_queues (mqs_process *proc, char **msg)
{
    mpi_process_info *p_info = (mpi_process_info *)mqs_get_process_info (proc);
    mqs_image * image          = mqs_get_image (proc);
    mpi_image_info *i_info   = (mpi_image_info *)mqs_get_image_info (image);

    /* Don't bother with a pop up here, it's unlikely to be helpful */
    *msg = 0;

    if (mqs_find_symbol (image, "ompi_mpi_communicators", &p_info->commlist_base) != mqs_ok)
        return err_all_communicators;
  
    if (mqs_find_symbol (image, "mca_pml_base_send_requests", &p_info->send_queue_base) != mqs_ok)
        return err_mpid_recvs;
  
    if (mqs_find_symbol (image, "mca_pml_base_recv_requests", &p_info->recv_queue_base) != mqs_ok)
        return err_mpid_recvs;
  
    return mqs_ok;
} /* mqs_setup_process_info */

/***********************************************************************
 * Check if the communicators have changed by looking at the 
 * sequence number.
 */
static int communicators_changed (mqs_process *proc)
{
#if 0  /* TODO: how do we figure out which communicators have changed ? */
    mpi_process_info *p_info = (mpi_process_info *)mqs_get_process_info (proc);
    mqs_image * image          = mqs_get_image (proc);
    mpi_image_info *i_info   = (mpi_image_info *)mqs_get_image_info (image);
    mqs_tword_t new_seq = fetch_int (proc, 
                                     p_info->commlist_base+i_info->sequence_number_offs,
                                     p_info);
    int  res = (new_seq != p_info->communicator_sequence);
      
    /* Save the sequence number for next time */
    p_info->communicator_sequence = new_seq;

    return res;
#endif
    return 1;
} /* mqs_communicators_changed */

/***********************************************************************
 * Find a matching communicator on our list. We check the recv context
 * as well as the address since the communicator structures may be
 * being re-allocated from a free list, in which case the same
 * address will be re-used a lot, which could confuse us.
 */
static communicator_t * find_communicator (mpi_process_info *p_info,
					   int recv_ctx)
{
    communicator_t * comm = p_info->communicator_list;

    for (; comm; comm=comm->next) {
        if (comm->recv_context == recv_ctx)
            return comm;
    }

    return NULL;
} /* find_communicator */

/***********************************************************************
 * Comparison function for sorting communicators.
 */
static int compare_comms (const void *a, const void *b)
{
    communicator_t * ca = *(communicator_t **)a;
    communicator_t * cb = *(communicator_t **)b;

    return cb->recv_context - ca->recv_context;
} /* compare_comms */

/***********************************************************************
 * Rebuild our list of communicators because something has changed 
 */
static int rebuild_communicator_list (mqs_process *proc)
{
    mpi_process_info *p_info = (mpi_process_info *)mqs_get_process_info (proc);
    mqs_image * image          = mqs_get_image (proc);
    mpi_image_info *i_info   = (mpi_image_info *)mqs_get_image_info (image);
    communicator_t **commp, *old;
    int commcount = 0, i;
    mqs_tword_t comm_size;
    mqs_taddr_t comm_addr_base = p_info->commlist_base + i_info->ompi_pointer_array_t.offset.addr;
    mqs_taddr_t comm_ptr;
    mqs_communicator remote_comm;

    /**
     * Start by getting the number of registered communicators in the
     * global communicator array.
     */
    comm_size = fetch_int( proc,
                           p_info->commlist_base + i_info->ompi_pointer_array_t.offset.size,
                           p_info );
    /* Now get the pointer to the first communicator pointer */
    comm_addr_base = fetch_pointer( proc, comm_addr_base, p_info );

    for( i = 0; i < comm_size; i++ ) {
        /* Get the communicator pointer */
        comm_ptr = 
            fetch_pointer( proc,
                           comm_addr_base + i * p_info->sizes.pointer_size,
                           p_info );
        if( 0 == comm_ptr ) continue;

        /* Now let's grab the data we want from inside */
        remote_comm.unique_id = fetch_int( proc,
                                           comm_ptr + i_info->ompi_communicator_t.offset.c_contextid,
                                           p_info );
        remote_comm.local_rank = fetch_int( proc,
                                            comm_ptr + i_info->ompi_communicator_t.offset.c_my_rank,
                                            p_info );
        mqs_fetch_data( proc, comm_ptr + i_info->ompi_communicator_t.offset.c_name,
                        64, remote_comm.name );

        /* Do we already have this communicator ? */
        old = find_communicator(p_info, remote_comm.unique_id);
        if( NULL == old ) {
            mqs_taddr_t group_base;

            old = (communicator_t *)mqs_malloc (sizeof (communicator_t));
            /* Save the results */
            old->next = p_info->communicator_list;
            p_info->communicator_list = old;
            old->comm_ptr  = comm_ptr;
            old->recv_context = remote_comm.unique_id;

            /* Now get the information about the group */
            group_base =
                fetch_pointer( proc, comm_ptr + i_info->ompi_communicator_t.offset.c_local_group,
                               p_info );
            old->group = find_or_create_group( proc, group_base );
            
        }
        strncpy(old->comm_info.name, remote_comm.name, 64);
        old->comm_info.unique_id = remote_comm.unique_id;
        old->comm_info.local_rank = remote_comm.local_rank;
        if( NULL != old->group ) {
            old->comm_info.size = old->group->entries;
        }
        old->present = TRUE;
    }

    /* Now iterate over the list tidying up any communicators which
     * no longer exist, and cleaning the flags on any which do.
     */
    commp = &p_info->communicator_list;

    for (; *commp; commp = &(*commp)->next) {
        communicator_t *comm = *commp;
        if (comm->present) {
            comm->present = FALSE;
            commcount++;
        } else { /* It needs to be deleted */
            *commp = comm->next;			/* Remove from the list */
            group_decref (comm->group);		/* Group is no longer referenced from here */
            mqs_free (comm);
            if( *commp == NULL ) break;
        }
    }

    if (commcount) {
        /* Sort the list so that it is displayed in some semi-sane order. */
        communicator_t ** comm_array =
            (communicator_t **) mqs_malloc(commcount * sizeof (communicator_t *));
        communicator_t *comm = p_info->communicator_list;
        int i;
        for (i=0; i<commcount; i++, comm=comm->next)
            comm_array [i] = comm;

        /* Do the sort */
        qsort (comm_array, commcount, sizeof (communicator_t *), compare_comms);

        /* Rebuild the list */
        p_info->communicator_list = NULL;
        for (i=0; i<commcount; i++) {
            comm = comm_array[i];
            comm->next = p_info->communicator_list;
            p_info->communicator_list = comm;
        }

        mqs_free (comm_array);
    }

    return mqs_ok;
} /* rebuild_communicator_list */

/***********************************************************************
 * Update the list of communicators in the process if it has changed.
 */
int mqs_update_communicator_list (mqs_process *proc)
{
    if (communicators_changed (proc))
        return rebuild_communicator_list (proc);
    else
        return mqs_ok;
} /* mqs_update_communicator_list */

/***********************************************************************
 * Setup to iterate over communicators.
 * This is where we check whether our internal communicator list needs
 * updating and if so do it.
 */
int mqs_setup_communicator_iterator (mqs_process *proc)
{
    mpi_process_info *p_info = (mpi_process_info *)mqs_get_process_info (proc);

    /* Start at the front of the list again */
    p_info->current_communicator = p_info->communicator_list;
    /* Reset the operation iterator too */
    p_info->next_msg.free_list            = 0;
    p_info->next_msg.current_item         = 0;
    p_info->next_msg.opal_list_t_pos.list = 0;

    return p_info->current_communicator == NULL ? mqs_end_of_list : mqs_ok;
} /* mqs_setup_communicator_iterator */

/***********************************************************************
 * Fetch information about the current communicator.
 */
int mqs_get_communicator (mqs_process *proc, mqs_communicator *comm)
{
    mpi_process_info *p_info = (mpi_process_info *)mqs_get_process_info (proc);

    if (p_info->current_communicator) {
        *comm = p_info->current_communicator->comm_info;
  
        return mqs_ok;
    }
    return err_no_current_communicator;
} /* mqs_get_communicator */

/***********************************************************************
 * Get the group information about the current communicator.
 */
int mqs_get_comm_group (mqs_process *proc, int *group_members)
{
    mpi_process_info *p_info = (mpi_process_info *)mqs_get_process_info (proc);
    communicator_t     *comm   = p_info->current_communicator;

    if (comm && comm->group) {
        group_t * g = comm->group;
        int i;

        for (i=0; i<g->entries; i++)
            group_members[i] = g->local_to_global[i];

        return mqs_ok;
    }
    return err_no_current_communicator;
} /* mqs_get_comm_group */

/***********************************************************************
 * Step to the next communicator.
 */
int mqs_next_communicator (mqs_process *proc)
{
    mpi_process_info *p_info = (mpi_process_info *)mqs_get_process_info (proc);

    p_info->current_communicator = p_info->current_communicator->next;
  
    return (p_info->current_communicator != NULL) ? mqs_ok : mqs_end_of_list;
} /* mqs_next_communicator */

/**
 * Parsing the opal_list_t.
 */
static int opal_list_t_init_parser( mqs_process *proc, mpi_process_info *p_info,
                                    mqs_opal_list_t_pos* position, mqs_taddr_t list )
{
    mqs_image * image        = mqs_get_image (proc);
    mpi_image_info *i_info   = (mpi_image_info *)mqs_get_image_info (image);

    position->list = list;
    position->sentinel = position->list + i_info->opal_list_t.offset.opal_list_sentinel;
    position->current_item =
        fetch_pointer( proc, position->sentinel + i_info->opal_list_item_t.offset.opal_list_next,
                       p_info );
    if( position->current_item == position->sentinel )
        position->current_item = 0;
    return mqs_ok;
}

static int next_item_opal_list_t( mqs_process *proc, mpi_process_info *p_info,
                                  mqs_opal_list_t_pos* position, mqs_taddr_t* active_item )
{
    mqs_image * image        = mqs_get_image (proc);
    mpi_image_info *i_info   = (mpi_image_info *)mqs_get_image_info (image);

    *active_item = position->current_item;
    if( 0 == position->current_item )
        return mqs_end_of_list;

    position->current_item =
        fetch_pointer( proc,
                       position->current_item + i_info->opal_list_item_t.offset.opal_list_next,
                       p_info );
    if( position->current_item == position->sentinel )
        position->current_item = 0;
    return mqs_ok;
}

#if defined(CODE_NOT_USED)
/**
 * Parsing the ompi_free_list lists.
 *
 *
 *
 *
 */
static void ompi_free_list_t_dump_position( mqs_ompi_free_list_t_pos* position )
{
    printf( "position->opal_list_t_pos.current_item = 0x%llx\n", (long long)position->opal_list_t_pos.current_item );
    printf( "position->opal_list_t_pos.list         = 0x%llx\n", (long long)position->opal_list_t_pos.list );
    printf( "position->opal_list_t_pos.sentinel     = 0x%llx\n", (long long)position->opal_list_t_pos.sentinel );
    printf( "position->current_item                 = 0x%llx\n", (long long)position->current_item );
    printf( "position->upper_bound                  = 0x%llx\n", (long long)position->upper_bound );
    printf( "position->free_list                    = 0x%llx\n", (long long)position->free_list );
    printf( "position->fl_elem_size                 = %ld\n", (long)position->fl_elem_size );
    printf( "position->fl_header_space              = %ld\n", (long)position->fl_header_space );
    printf( "position->fl_alignment                 = %ld\n", (long)position->fl_alignment );
    printf( "position->fl_num_per_alloc             = %ld\n", (long)position->fl_num_per_alloc );
    printf( "position->fl_num_allocated             = %ld\n", (long)position->fl_num_allocated );
    printf( "position->fl_num_initial_alloc         = %ld\n", (long)position->fl_num_initial_alloc );
}
#endif  /* CODE_NOT_USED */

static int ompi_free_list_t_init_parser( mqs_process *proc, mpi_process_info *p_info,
                                         mqs_ompi_free_list_t_pos* position, mqs_taddr_t free_list )
{
    mqs_image * image          = mqs_get_image (proc);
    mpi_image_info *i_info   = (mpi_image_info *)mqs_get_image_info (image);
    mqs_taddr_t active_allocation;

    position->free_list = free_list;

    position->fl_elem_size =
        fetch_int( proc, position->free_list + i_info->ompi_free_list_t.offset.fl_elem_size,
                   p_info );
    position->fl_header_space =
        fetch_int( proc, position->free_list + i_info->ompi_free_list_t.offset.fl_header_space,
                   p_info );
    position->fl_alignment =
        fetch_int( proc, position->free_list + i_info->ompi_free_list_t.offset.fl_alignment,
                   p_info );
    position->fl_num_per_alloc =
        fetch_int( proc, position->free_list + i_info->ompi_free_list_t.offset.fl_num_per_alloc,
                   p_info );
    position->fl_num_allocated =
        fetch_int( proc, position->free_list + i_info->ompi_free_list_t.offset.fl_num_allocated,
                   p_info );
    /**
     * Work around the strange ompi_free_list_t way to allocate elements. The first chunk is
     * not required to have the same size as the others.
     * A similar work around should be set for the last chunk of allocations too !!! But how
     * can we solve ONE equation with 2 unknowns ?
     */
    if( position->fl_num_allocated <= position->fl_num_per_alloc ) {
        position->fl_num_initial_alloc = position->fl_num_allocated;
    } else {
        position->fl_num_initial_alloc = position->fl_num_allocated % position->fl_num_per_alloc;
        if( 0 == position->fl_num_initial_alloc )
            position->fl_num_initial_alloc = position->fl_num_per_alloc;
    }

    /**
     * Initialize the pointer to the opal_list_t.
     */
    opal_list_t_init_parser( proc, p_info, &position->opal_list_t_pos,
                             position->free_list + i_info->ompi_free_list_t.offset.fl_allocations );
    next_item_opal_list_t( proc, p_info, &position->opal_list_t_pos, &active_allocation );

    if( 0 == active_allocation ) {  /* the end of the list */
        position->upper_bound = 0;
    } else {
        /**
         * Handle alignment issues...
         */
        active_allocation += i_info->ompi_free_list_memory_t.size;
        if( 0 != position->fl_alignment ) {
            mqs_tword_t modulo;
            
            active_allocation += position->fl_header_space;
            modulo = active_allocation % position->fl_alignment;
            active_allocation += (position->fl_alignment - modulo);
            active_allocation -= position->fl_header_space;
        }
        /**
         * Now let's try to compute the upper bound ...
         */
        position->upper_bound =
            position->fl_num_initial_alloc * position->fl_elem_size + active_allocation;
    }
    position->current_item = active_allocation;
    
    /*ompi_free_list_t_dump_position( position );*/
    return mqs_ok;
}

/**
 * Return the current position and move the internal counter to the next element.
 */
static int ompi_free_list_t_next_item( mqs_process *proc, mpi_process_info *p_info,
                                       mqs_ompi_free_list_t_pos* position, mqs_taddr_t* active_item )
{
    mqs_image * image          = mqs_get_image (proc);
    mpi_image_info *i_info   = (mpi_image_info *)mqs_get_image_info (image);
    mqs_taddr_t active_allocation;

    *active_item = position->current_item;
    if( 0 == position->current_item )  /* the end ... */
        return mqs_ok;

    position->current_item += position->fl_elem_size;
    if( position->current_item >= position->upper_bound ) {
        /*printf( "Reach the end of one of the ompi_free_list_t allocations. Go to the next one\n" );*/
        /* we should go to the next allocation */
        next_item_opal_list_t( proc, p_info,
                               &position->opal_list_t_pos, &active_allocation );
        if( 0 == active_allocation ) { /* we're at the end */
            position->current_item = 0;
            return mqs_ok;
        }
        /**
         * Handle alignment issues...
         */
        active_allocation += i_info->ompi_free_list_memory_t.size;
        if( 0 != position->fl_alignment ) {
            mqs_tword_t modulo;

            active_allocation += position->fl_header_space;
            modulo = active_allocation % position->fl_alignment;
            active_allocation += (position->fl_alignment - modulo);
            active_allocation -= position->fl_header_space;
        }
        /**
         * Now let's try to compute the upper bound ...
         */
        position->upper_bound =
            position->fl_num_per_alloc * position->fl_elem_size + active_allocation;
        position->current_item = active_allocation;
        /*ompi_free_list_t_dump_position( position );*/
    }
    /*printf( "Free list actual position %p next element at %p\n", (void*)*active_item,
      (void*)position->current_item );*/
    return mqs_ok;
}

#if defined(CODE_NOT_USED)
static void dump_request( mqs_taddr_t current_item, mqs_pending_operation *res )
{
    printf( "\n+===============================================+\n" );
    printf(   "|Request 0x%llx contain \n", (long long)current_item );
    printf(   "|    res->status              = %d\n", res->status );
    printf(   "|    res->desired_local_rank  = %ld\n", (long)res->desired_local_rank );
    printf(   "|    res->desired_global_rank = %ld\n", (long)res->desired_global_rank );
    printf(   "|    res->tag_wild            = %ld\n", (long)res->tag_wild );
    printf(   "|    res->desired_tag         = %ld\n", (long)res->desired_tag );
    printf(   "|    res->system_buffer       = %s\n", (TRUE == res->system_buffer ? "TRUE" : "FALSE") );
    printf(   "|    res->buffer              = 0x%llx\n", (long long)res->buffer );
    printf(   "|    res->desired_length      = %ld\n", (long)res->desired_length );
    if( res->status != mqs_st_pending ) {
        printf(   "|    res->actual_length       = %ld\n", (long)res->actual_length );
        printf(   "|    res->actual_tag          = %ld\n", (long)res->actual_tag );
        printf(   "|    res->actual_local_rank   = %ld\n", (long)res->actual_local_rank );
        printf(   "|    res->actual_global_rank  = %ld\n", (long)res->actual_global_rank );
    }
    printf(   "+===============================================+\n\n" );
}
#endif  /* CODE_NOT_USED */

/**
 * TODO: ompi_request_completed can be used to detect any changes in the request handles.
 */

/**
 * Handle the send queue as well as the receive queue. The unexpected queue
 * is a whole different story ...
 */
static int fetch_request( mqs_process *proc, mpi_process_info *p_info,
			  mqs_pending_operation *res, int look_for_user_buffer )
{
    mqs_image * image        = mqs_get_image (proc);
    mpi_image_info *i_info   = (mpi_image_info *)mqs_get_image_info (image);
    mqs_taddr_t current_item;
    mqs_tword_t req_complete, req_valid, req_type;
    mqs_taddr_t req_buffer, req_comm;

    while( 1 ) {
        ompi_free_list_t_next_item( proc, p_info,
                                    &p_info->next_msg, &current_item );
        if( 0 == current_item )
            return mqs_end_of_list;
        req_valid = fetch_int( proc, current_item + i_info->ompi_request_t.offset.req_state, p_info );
        if( OMPI_REQUEST_INVALID == req_valid ) continue;
        req_comm = fetch_pointer( proc, current_item + i_info->mca_pml_base_request_t.offset.req_comm, p_info );
        if( p_info->current_communicator->comm_ptr == req_comm ) break;
    }

    res->extra_text[0][0] = 0; res->extra_text[1][0] = 0; res->extra_text[2][0] = 0;
    res->extra_text[3][0] = 0; res->extra_text[4][0] = 0;

    req_type = fetch_int( proc, current_item + i_info->ompi_request_t.offset.req_type, p_info );
    if( OMPI_REQUEST_PML == req_type ) {
        req_type =
            fetch_int( proc, current_item + i_info->mca_pml_base_request_t.offset.req_type,
                       p_info);
        req_complete = fetch_bool( proc, current_item + i_info->ompi_request_t.offset.req_complete, p_info );
        res->status = (req_complete == 0 ? mqs_st_pending : mqs_st_complete);

        res->desired_local_rank  =
            fetch_int( proc, current_item + i_info->mca_pml_base_request_t.offset.req_peer, p_info );
        res->desired_global_rank = res->desired_local_rank;
        res->desired_tag         =
            fetch_int( proc, current_item + i_info->mca_pml_base_request_t.offset.req_tag, p_info );
        res->tag_wild            = (MPI_ANY_TAG == res->desired_tag ? TRUE : FALSE);
        
        res->buffer = fetch_pointer( proc, current_item + i_info->mca_pml_base_request_t.offset.req_addr,
                                    p_info );
        res->system_buffer = FALSE;
        if( MCA_PML_REQUEST_SEND == req_type ) {
            snprintf( (char *)res->extra_text[0], 64, "Non-blocking send 0x%llx", (long long)current_item );
            req_buffer =
                fetch_pointer( proc, current_item + i_info->mca_pml_base_send_request_t.offset.req_addr,
                               p_info );
            res->system_buffer = ( req_buffer == res->buffer ? FALSE : TRUE );
            res->desired_length      =
                fetch_int( proc,
                           current_item + i_info->mca_pml_base_send_request_t.offset.req_bytes_packed, p_info );
        } else if( MCA_PML_REQUEST_RECV == req_type ) {
            snprintf( (char *)res->extra_text[0], 64, "Non-blocking recv 0x%llx", (long long)current_item );
        } else {
            snprintf( (char *)res->extra_text[0], 64, "Unknown type of request 0x%llx", (long long)current_item );
        }
        res->desired_length      =
            fetch_int( proc, current_item + i_info->mca_pml_base_request_t.offset.req_count, p_info );
        
        if( mqs_st_pending != res->status ) {  /* The real data from the status */
            res->actual_length       =
                fetch_int( proc, current_item + i_info->ompi_request_t.offset.req_status +
                           i_info->ompi_status_public_t.offset._count, p_info );
            res->actual_tag          =
                fetch_int( proc, current_item + i_info->ompi_request_t.offset.req_status +
                           i_info->ompi_status_public_t.offset.MPI_TAG, p_info );
            res->actual_local_rank   =
                fetch_int( proc, current_item + i_info->ompi_request_t.offset.req_status +
                           i_info->ompi_status_public_t.offset.MPI_SOURCE, p_info );
            res->actual_global_rank  = res->actual_local_rank;  /* TODO: what's the global rank ? */
        }
        /*dump_request( current_item, res );*/
    }
    return mqs_ok;
}

#if 0
/***********************************************************************
 * Handle the unexpected queue and the pending receive queue.
 * They're very similar.
 */
static int fetch_receive (mqs_process *proc, mpi_process_info *p_info,
			  mqs_pending_operation *res, int look_for_user_buffer)
{
    mqs_image * image          = mqs_get_image (proc);
    mpi_image_info *i_info     = (mpi_image_info *)mqs_get_image_info (image);
    communicator_t   *comm     = p_info->current_communicator;
    mqs_tword_t wanted_context = comm->recv_context;
    mqs_taddr_t base           = fetch_pointer (proc, p_info->next_msg, p_info);

    while (base != 0) { /* Well, there's a queue, at least ! */
        mqs_tword_t actual_context = fetch_int (proc, base + i_info->context_id_offs, p_info);
      
        if (actual_context == wanted_context) { /* Found a good one */
            mqs_tword_t tag     = fetch_int (proc, base + i_info->tag_offs, p_info);
            mqs_tword_t tagmask = fetch_int (proc, base + i_info->tagmask_offs, p_info);
            mqs_tword_t lsrc    = fetch_int (proc, base + i_info->lsrc_offs, p_info);
            mqs_tword_t srcmask = fetch_int (proc, base + i_info->srcmask_offs, p_info);
            mqs_taddr_t ptr     = fetch_pointer (proc, base + i_info->ptr_offs, p_info);
	  
            /* Fetch the fields from the MPIR_RHANDLE */
            int is_complete = fetch_int (proc, ptr + i_info->is_complete_offs, p_info);
            mqs_taddr_t buf     = fetch_pointer (proc, ptr + i_info->buf_offs, p_info);
            mqs_tword_t len     = fetch_int (proc, ptr + i_info->len_offs, p_info);
            mqs_tword_t count   = fetch_int (proc, ptr + i_info->count_offs, p_info);

            /* If we don't have start, then use buf instead... */
            mqs_taddr_t start;
            if (i_info->start_offs < 0)
                start = buf;
            else
                start = fetch_pointer (proc, ptr + i_info->start_offs, p_info);

            /* Hurrah, we should now be able to fill in all the necessary fields in the
             * result !
             */
            res->status = is_complete ? mqs_st_complete : mqs_st_pending; /* We can't discern matched */
            if (srcmask == 0) {
                res->desired_local_rank  = -1;
                res->desired_global_rank = -1;
            } else {
                res->desired_local_rank  = lsrc;
                res->desired_global_rank = translate (comm->group, lsrc);
	      
            }
            res->tag_wild       = (tagmask == 0);
            res->desired_tag    = tag;
	  
            if (look_for_user_buffer) {
                res->system_buffer  = FALSE;
                res->buffer         = buf;
                res->desired_length = len;
            } else {
                res->system_buffer  = TRUE;
                /* Correct an oddity. If the buffer length is zero then no buffer
                 * is allocated, but the descriptor is left with random data.
                 */
                if (count == 0)
                    start = 0;
	      
                res->buffer         = start;
                res->desired_length = count;
            }

            if (is_complete) { /* Fill in the actual results, rather than what we were looking for */
                mqs_tword_t mpi_source  = fetch_int (proc, ptr + i_info->MPI_SOURCE_offs, p_info);
                mqs_tword_t mpi_tag  = fetch_int (proc, ptr + i_info->MPI_TAG_offs, p_info);

                res->actual_length     = count;
                res->actual_tag        = mpi_tag;
                res->actual_local_rank = mpi_source;
                res->actual_global_rank= translate (comm->group, mpi_source);
            }

            /* Don't forget to step the queue ! */
            p_info->next_msg = base + i_info->next_offs;
            return mqs_ok;
        } else { /* Try the next one */
            base = fetch_pointer (proc, base + i_info->next_offs, p_info);
        }
    }
  
    p_info->next_msg = 0;
    return mqs_end_of_list;
}  /* fetch_receive */

/***********************************************************************
 * Handle the send queue, somewhat different.
 */
static int fetch_send (mqs_process *proc, mpi_process_info *p_info,
		       mqs_pending_operation *res)
{
    mqs_image * image        = mqs_get_image (proc);
    mpi_image_info *i_info = (mpi_image_info *)mqs_get_image_info (image);
    communicator_t   *comm   = p_info->current_communicator;
    mqs_taddr_t base         = fetch_pointer (proc, p_info->next_msg, p_info);

    /* Say what operation it is. We can only see non blocking send operations
     * in MPICH. Other MPI systems may be able to show more here. 
     */
    strcpy ((char *)res->extra_text[0],"Non-blocking send");
    res->extra_text[1][0] = 0;

    while (base != 0) { /* Well, there's a queue, at least ! */
        /* Check if it's one we're interested in ? */
        mqs_taddr_t commp = fetch_pointer (proc, base+i_info->db_comm_offs, p_info);
        int recv_ctx = fetch_int (proc, commp+i_info->recv_context_offs, p_info);

        mqs_taddr_t next  = base+i_info->db_next_offs;

        if (recv_ctx == comm->recv_context) { /* Found one */
            mqs_tword_t target = fetch_int (proc, base+i_info->db_target_offs,      p_info);
            mqs_tword_t tag    = fetch_int (proc, base+i_info->db_tag_offs,         p_info);
            mqs_tword_t length = fetch_int (proc, base+i_info->db_byte_length_offs, p_info);
            mqs_taddr_t data   = fetch_pointer (proc, base+i_info->db_data_offs,    p_info);
            mqs_taddr_t shandle= fetch_pointer (proc, base+i_info->db_shandle_offs, p_info);
            mqs_tword_t complete=fetch_int (proc, shandle+i_info->is_complete_offs, p_info);

            /* Ok, fill in the results */
            res->status = complete ? mqs_st_complete : mqs_st_pending; /* We can't discern matched */
            res->actual_local_rank = res->desired_local_rank = target;
            res->actual_global_rank= res->desired_global_rank= translate (comm->group, target);
            res->tag_wild   = FALSE;
            res->actual_tag = res->desired_tag = tag;
            res->desired_length = res->actual_length = length;
            res->system_buffer  = FALSE;
            res->buffer = data;

            p_info->next_msg = next;
            return mqs_ok;
        }
      
        base = fetch_pointer (proc, next, p_info);
    }

    p_info->next_msg = 0;
    return mqs_end_of_list;
} /* fetch_send */
#endif

/***********************************************************************
 * Setup to iterate over pending operations 
 */
int mqs_setup_operation_iterator (mqs_process *proc, int op)
{
    mpi_process_info *p_info = (mpi_process_info *)mqs_get_process_info (proc);

    p_info->what = (mqs_op_class)op;

    switch (op) {
    case mqs_pending_sends:
        ompi_free_list_t_init_parser( proc, p_info, &p_info->next_msg, p_info->send_queue_base );
        return mqs_ok;

    case mqs_pending_receives:
        ompi_free_list_t_init_parser( proc, p_info, &p_info->next_msg, p_info->recv_queue_base );
        return mqs_ok;

    case mqs_unexpected_messages:  /* TODO */
        return mqs_no_information;

    default:
        return err_bad_request;
    }
} /* mqs_setup_operation_iterator */

/***********************************************************************
 * Fetch the next valid operation. 
 * Since MPICH only maintains a single queue of each type of operation,
 * we have to run over it and filter out the operations which
 * match the active communicator.
 */
int mqs_next_operation (mqs_process *proc, mqs_pending_operation *op)
{
    mpi_process_info *p_info = (mpi_process_info *)mqs_get_process_info (proc);

    switch (p_info->what) {
    case mqs_pending_receives:
        return fetch_request( proc, p_info, op, TRUE );
    case mqs_unexpected_messages:
        /* TODO: not handled yet */
        return err_bad_request;
    case mqs_pending_sends:
        return fetch_request( proc, p_info, op, FALSE );
    default: return err_bad_request;
    }
} /* mqs_next_operation */

/***********************************************************************
 * Destroy the info.
 */
void mqs_destroy_process_info (mqs_process_info *mp_info)
{
    mpi_process_info *p_info = (mpi_process_info *)mp_info;
    /* Need to handle the communicators and groups too */
    communicator_t *comm = p_info->communicator_list;

    while (comm) {
        communicator_t *next = comm->next;

        if( NULL != comm->group )
            group_decref (comm->group);  /* Group is no longer referenced from here */
        mqs_free (comm);
      
        comm = next;
    }
    mqs_free (p_info);
} /* mqs_destroy_process_info */

/***********************************************************************
 * Free off the data we associated with an image. Since we malloced it
 * we just free it.
 */
void mqs_destroy_image_info (mqs_image_info *info)
{
    mqs_free (info);
} /* mqs_destroy_image_info */

/***********************************************************************/
static mqs_taddr_t fetch_pointer (mqs_process * proc, mqs_taddr_t addr, mpi_process_info *p_info)
{
    int asize = p_info->sizes.pointer_size;
    char data [8];				/* ASSUME a pointer fits in 8 bytes */
    mqs_taddr_t res = 0;

    if (mqs_ok == mqs_fetch_data (proc, addr, asize, data))
        mqs_target_to_host (proc, data, 
                            ((char *)&res) + (host_is_big_endian ? sizeof(mqs_taddr_t)-asize : 0), 
                            asize);

    return res;
} /* fetch_pointer */

/***********************************************************************/
static mqs_tword_t fetch_int (mqs_process * proc, mqs_taddr_t addr, mpi_process_info *p_info)
{
    int isize = p_info->sizes.int_size;
    char buffer[8];				/* ASSUME an integer fits in 8 bytes */
    mqs_tword_t res = 0;

    if (mqs_ok == mqs_fetch_data (proc, addr, isize, buffer))
        mqs_target_to_host (proc, buffer, 
                            ((char *)&res) + (host_is_big_endian ? sizeof(mqs_tword_t)-isize : 0), 
                            isize);
  
    return res;
} /* fetch_int */

/***********************************************************************/
static mqs_tword_t fetch_bool(mqs_process * proc, mqs_taddr_t addr, mpi_process_info *p_info)
{
    int isize = 1;
    char buffer;				/* ASSUME an integer fits in 8 bytes */
    mqs_tword_t res = 0;

    if (mqs_ok == mqs_fetch_data (proc, addr, isize, &buffer))
        res = (mqs_tword_t)buffer;
  
    return res;
} /* fetch_bool */

/***********************************************************************/
/* Convert an error code into a printable string */
char * mqs_dll_error_string (int errcode)
{
    switch (errcode) {
    case err_silent_failure:
        return "";
    case err_no_current_communicator: 
        return "No current communicator in the communicator iterator";
    case err_bad_request:    
        return "Attempting to setup to iterate over an unknown queue of operations";
    case err_no_store: 
        return "Unable to allocate store";
    case err_failed_qhdr: 
        return "Failed to find type MPID_QHDR";
    case err_unexpected: 
        return "Failed to find field 'unexpected' in MPID_QHDR";
    case err_posted: 
        return "Failed to find field 'posted' in MPID_QHDR";
    case err_failed_queue: 
        return "Failed to find type MPID_QUEUE";
    case err_first: 
        return "Failed to find field 'first' in MPID_QUEUE";
    case err_context_id: 
        return "Failed to find field 'context_id' in MPID_QEL";
    case err_tag: 
        return "Failed to find field 'tag' in MPID_QEL";
    case err_tagmask: 
        return "Failed to find field 'tagmask' in MPID_QEL";
    case err_lsrc: 
        return "Failed to find field 'lsrc' in MPID_QEL";
    case err_srcmask: 
        return "Failed to find field 'srcmask' in MPID_QEL";
    case err_next: 
        return "Failed to find field 'next' in MPID_QEL";
    case err_ptr: 
        return "Failed to find field 'ptr' in MPID_QEL";
    case err_missing_type: 
        return "Failed to find some type";
    case err_missing_symbol: 
        return "Failed to find field the global symbol";
    case err_db_shandle: 
        return "Failed to find field 'db_shandle' in MPIR_SQEL";
    case err_db_comm: 
        return "Failed to find field 'db_comm' in MPIR_SQEL";
    case err_db_target: 
        return "Failed to find field 'db_target' in MPIR_SQEL";
    case err_db_tag: 
        return "Failed to find field 'db_tag' in MPIR_SQEL";
    case err_db_data: 
        return "Failed to find field 'db_data' in MPIR_SQEL";
    case err_db_byte_length: 
        return "Failed to find field 'db_byte_length' in MPIR_SQEL";
    case err_db_next: 
        return "Failed to find field 'db_next' in MPIR_SQEL";
    case err_failed_rhandle: 
        return "Failed to find type MPIR_RHANDLE";
    case err_is_complete: 
        return "Failed to find field 'is_complete' in MPIR_RHANDLE";
    case err_buf: 
        return "Failed to find field 'buf' in MPIR_RHANDLE";
    case err_len: 
        return "Failed to find field 'len' in MPIR_RHANDLE";
    case err_s: 
        return "Failed to find field 's' in MPIR_RHANDLE";
    case err_failed_status: 
        return "Failed to find type MPI_Status";
    case err_count: 
        return "Failed to find field 'count' in MPIR_Status";
    case err_MPI_SOURCE: 
        return "Failed to find field 'MPI_SOURCE' in MPIR_Status";
    case err_MPI_TAG: 
        return "Failed to find field 'MPI_TAG' in MPIR_Status";
    case err_failed_commlist: 
        return "Failed to find type MPIR_Comm_list";
    case err_sequence_number: 
        return "Failed to find field 'sequence_number' in MPIR_Comm_list";
    case err_comm_first: 
        return "Failed to find field 'comm_first' in MPIR_Comm_list";
    case err_failed_communicator: 
        return "Failed to find type MPIR_Communicator";
    case err_np: 
        return "Failed to find field 'np' in MPIR_Communicator";
    case err_lrank_to_grank: 
        return "Failed to find field 'lrank_to_grank' in MPIR_Communicator";
    case err_send_context: 
        return "Failed to find field 'send_context' in MPIR_Communicator";
    case err_recv_context: 
        return "Failed to find field 'recv_context' in MPIR_Communicator";
    case err_comm_next: 
        return "Failed to find field 'comm_next' in MPIR_Communicator";
    case err_comm_name: 
        return "Failed to find field 'comm_name' in MPIR_Communicator";
    case err_all_communicators: 
        return "Failed to find the global symbol MPIR_All_communicators";
    case err_mpid_recvs: 
        return "Failed to find the global symbol MPID_recvs";
    case err_group_corrupt:
        return "Could not read a communicator's group from the process (probably a store corruption)";

    default: return "Unknown error code";
    }
} /* mqs_dll_error_string */
