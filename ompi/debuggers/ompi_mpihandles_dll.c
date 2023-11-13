/*
 * Copyright (c) 2007-2008 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2004-2020 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2008      Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2012-2013 Inria.  All rights reserved.
 * Copyright (c) 2016      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2017      IBM Corp.  All rights reserved.
 * Copyright (c) 2023      NVIDIA Corporation. All rights reserved.
 * Copyright (c) 2023      Jeffrey M. Squyres.  All rights reserved.
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

#include <string.h>
#include <stdlib.h>

#include "ompi/mca/pml/base/pml_base_request.h"
#include "mpihandles_interface.h"
#include "ompi_mpihandles_dll_defs.h"
#include "ompi/communicator/communicator.h"


#define OPAL_ALIGN(x,a,t) (((x)+((t)(a)-1)) & ~(((t)(a)-1)))

/* Globals that the debugger expects to find in the DLL */
#if defined(WORDS_BIGENDIAN)
char mpidbg_dll_is_big_endian = 1;
#else
char mpidbg_dll_is_big_endian = 0;
#endif
char mpidbg_dll_bitness = (char) (sizeof(void*) * 8);
enum mpidbg_comm_capabilities_t mpidbg_comm_capabilities = 0;
struct mpidbg_name_map_t *mpidbg_comm_name_map = NULL;
enum mpidbg_errhandler_capabilities_t mpidbg_errhandler_capabilities = 0;
struct mpidbg_name_map_t *mpidbg_errhandler_name_map = NULL;
enum mpidbg_request_capabilities_t mpidbg_request_capabilities = 0;
struct mpidbg_name_map_t *mpidbg_request_name_map = NULL;
enum mpidbg_status_capabilities_t mpidbg_status_capabilities = 0;
struct mpidbg_name_map_t *mpidbg_status_name_map = NULL;

#if defined(__SUNPRO_C)
/*
 * These symbols are defined here because of the different way compilers
 * may handle extern definitions. The particular case that is causing
 * problems is when there is an extern variable that is accessed in a
 * static inline function. For example, here is the code we often see in
 * a header file.
 *
 * extern int request_complete;
 * static inline check_request(void) {
 *    request_complete = 1;
 * }
 *
 * If this code exists in a header file and gets included in a source
 * file, then some compilers expect to have request_complete defined
 * somewhere even if request_complete is never referenced and
 * check_request is never called. Other compilers do not need them defined
 * if they are never referenced in the source file. Therefore, to handle
 * cases like the above with compilers that require the symbol (like
 * Sun Studio) we add in these definitions here.
 */
opal_mutex_t opal_event_lock;
int opal_progress_spin_count;
bool opal_mutex_check_locks;
bool opal_uses_threads;
#endif /* defined(__SUNPRO_C) */

/*---------------------------------------------------------------------*/

/* Small helper function: allocate a map of a given length */
static struct mpidbg_name_map_t *alloc_map(mqs_image *image, int len)
{
    mpi_image_info *i_info = (mpi_image_info *) mqs_get_image_info(image);
    struct mpidbg_name_map_t *m = NULL;

    if (NULL != i_info) {
        m = mqs_malloc(len * sizeof(struct mpidbg_name_map_t));
    }

    return m;
}

/* Small helper function: look up a symbol, and if we find it, put it
   in a map entry */
static void fill_map(mqs_image *image,
                     char *public_name, char *private_name,
                     struct mpidbg_name_map_t *map)
{
    mqs_taddr_t value;
    mpi_image_info *i_info = (mpi_image_info *) mqs_get_image_info(image);

    if (NULL != i_info) {
        map->map_name = strdup(public_name);
        if (NULL != private_name) {
            if (mqs_ok == mqs_find_symbol(image, private_name, &value)) {
                map->map_handle = value;
                return;
            }
        } else {
            map->map_handle = 0;
            return;
        }
    }

    printf("OMPI MPI handles DLL: fill_map: Unable to find symbol: %s\n",
           private_name);
}

/* Helper function to lookup MPI attributes and fill an
   mpidbg_attribute_pair_t array with their keys/values */
static int fill_attributes(int *num_attrs,
                           struct mpidbg_attribute_pair_t **attrs,
                           mqs_taddr_t table)
{
    /* JMS fill me in */
    return mqs_ok;
}

/*---------------------------------------------------------------------*/

int mpidbg_init_once(const mqs_basic_callbacks *cb)
{
    mqs_basic_entrypoints = cb;
    printf("mpidbg_init_once\n");
    return MPIDBG_SUCCESS;
}

/*---------------------------------------------------------------------*/

/* Returns the fixed value */
int mpidbg_interface_version_compatibility(void)
{
    printf("mpidbg_interface_version_compatibility\n");
    return MPIDBG_INTERFACE_VERSION;
}


static char mpidbg_version_str[OMPI_MAX_VER_SIZE];

/* Returns a string specific to OMPI */
char *mpidbg_version_string(void)
{
    int offset;
    printf("mpidbg_version_string\n");
    offset = snprintf(mpidbg_version_str, OMPI_MAX_VER_SIZE-1,  
                      "Open MPI handle interpretation support for parallel debuggers ");
    ompi_get_lib_version(mpidbg_version_str+offset, OMPI_MAX_VER_SIZE-offset);
    return mpidbg_version_str;
}


/* So the debugger can tell what interface width the library was
   compiled with */
int mpidbg_dll_taddr_width(void)
{
    printf("mpidbg_dll_taddr_width\n");
    return sizeof(mqs_taddr_t);
}

/*---------------------------------------------------------------------*/

/* Once-per-image setup */
int mpidbg_init_per_image(mqs_image *image, const mqs_image_callbacks *icb,
                          struct mpidbg_handle_info_t *handle_types)
{
    char **message;
    mpi_image_info *i_info =
        (mpi_image_info *) mqs_malloc(sizeof(mpi_image_info));
    printf("mpidbg_init_per_image\n");

    if (NULL == i_info) {
        printf("mpidbg_init_per_image: malloc failed!\n");
        return MPIDBG_ERR_NO_MEM;
    }

    memset((void *)i_info, 0, sizeof(mpi_image_info));
    /* Before we do *ANYTHING* else */
    i_info->image_callbacks = icb;

    /* Nothing extra (yet) */
    i_info->extra = NULL;

    /* Save the info */
    mqs_put_image_info(image, (mqs_image_info *)i_info);

    /* Fill in the OMPI type information */
    if (mqs_ok != ompi_fill_in_type_info(image, message)) {
        printf("mpidbg_init_per_image: failed to get all type info\n");
        return MPIDBG_ERR_NOT_SUPPORTED;
    }

    /* Fill in the handle_types struct with our types */
    /* JMS: "MPI_Aint" is a typedef -- is that enough?  (the actual
       type is a #define, so it's not easy to put into the
       mqs_find_type call as a string) */
    handle_types->hi_c_aint = mqs_find_type(image, "MPI_Aint", mqs_lang_c);
    /* JMS: these ompi types are just the "foo" types; but OMPI MPI
       types are all "foo*"'s -- is this right?  If this is wrong, I
       *suspect* that something like the following may be right:

       handle_types->hi_c_comm = mqs_find_type(image, "ompi_communicator_t*", mqs_lang_c);

       Need to confirm this with the DDT guys...
    */
    handle_types->hi_c_comm = i_info->ompi_communicator_t.type;
    handle_types->hi_c_datatype = i_info->ompi_datatype_t.type;
    handle_types->hi_c_errhandler =
        mqs_find_type(image, "ompi_errhandler_t", mqs_lang_c);
    handle_types->hi_c_file =
        mqs_find_type(image, "ompi_file_t", mqs_lang_c);
    handle_types->hi_c_group = i_info->ompi_group_t.type;
    handle_types->hi_c_info =
        mqs_find_type(image, "opal_info_t", mqs_lang_c);
    /* JMS: "MPI_Offset" is a typedef (see comment about MPI_Aint above) */
    handle_types->hi_c_offset =
        mqs_find_type(image, "MPI_Offset", mqs_lang_c);
    handle_types->hi_c_op =
        mqs_find_type(image, "ompi_op_t", mqs_lang_c);
    handle_types->hi_c_request = i_info->ompi_request_t.type;
    handle_types->hi_c_status = i_info->ompi_status_public_t.type;
    handle_types->hi_c_win =
        mqs_find_type(image, "ompi_win_t", mqs_lang_c);

    /* MPI::Aint is a typedef to MPI_Aint */
    handle_types->hi_cxx_aint = handle_types->hi_cxx_aint;
    handle_types->hi_cxx_comm =
        mqs_find_type(image, "MPI::Comm", mqs_lang_cplus);
    handle_types->hi_cxx_intracomm =
        mqs_find_type(image, "MPI::Intracomm", mqs_lang_cplus);
    handle_types->hi_cxx_intercomm =
        mqs_find_type(image, "MPI::Intercomm", mqs_lang_cplus);
    handle_types->hi_cxx_graphcomm =
        mqs_find_type(image, "MPI::Graphcomm", mqs_lang_cplus);
    handle_types->hi_cxx_cartcomm =
        mqs_find_type(image, "MPI::Cartcomm", mqs_lang_cplus);
    handle_types->hi_cxx_datatype =
        mqs_find_type(image, "MPI::Datatype", mqs_lang_cplus);
    handle_types->hi_cxx_errhandler =
        mqs_find_type(image, "MPI::Errhandler", mqs_lang_cplus);
    handle_types->hi_cxx_file =
        mqs_find_type(image, "MPI::File", mqs_lang_cplus);
    handle_types->hi_cxx_group =
        mqs_find_type(image, "MPI::Group", mqs_lang_cplus);
    handle_types->hi_cxx_info =
        mqs_find_type(image, "MPI::Info", mqs_lang_cplus);
    /* MPI::Offset is a typedef to MPI_Offset */
    handle_types->hi_cxx_offset = handle_types->hi_c_offset;
    handle_types->hi_cxx_op =
        mqs_find_type(image, "MPI::Op", mqs_lang_cplus);
    handle_types->hi_cxx_request =
        mqs_find_type(image, "MPI::Request", mqs_lang_cplus);
    handle_types->hi_cxx_prequest =
        mqs_find_type(image, "MPI::Prequest", mqs_lang_cplus);
    handle_types->hi_cxx_grequest =
        mqs_find_type(image, "MPI::Grequest", mqs_lang_cplus);
    handle_types->hi_cxx_status =
        mqs_find_type(image, "MPI::Status", mqs_lang_cplus);
    handle_types->hi_cxx_win =
        mqs_find_type(image, "MPI::Win", mqs_lang_cplus);

    /* Tell the debugger what capabilities we have */
    mpidbg_comm_capabilities =
        MPIDBG_COMM_CAP_BASIC |
        MPIDBG_COMM_CAP_STRING_NAMES |
        MPIDBG_COMM_CAP_FREED_HANDLE |
        MPIDBG_COMM_CAP_FREED_OBJECT;
    mpidbg_errhandler_capabilities =
        MPIDBG_ERRH_CAP_BASIC |
        MPIDBG_ERRH_CAP_STRING_NAMES |
        MPIDBG_ERRH_CAP_FREED_HANDLE |
        MPIDBG_ERRH_CAP_FREED_OBJECT;
    mpidbg_request_capabilities =
        MPIDBG_REQUEST_CAP_BASIC;
    mpidbg_status_capabilities =
        MPIDBG_STATUS_CAP_BASIC;

    /* All done */
    printf("mpidbg_init_per_image: init succeeded -- ready!\n");
    return MPIDBG_SUCCESS;
}


/* This image is now dead; free all the state associated with it */
void mpidbg_finalize_per_image(mqs_image *image, mqs_image_info *info)
{
    mpi_image_info *i_info = (mpi_image_info *)info;

    printf("mpidbg_finalize_per_image\n");
    if (NULL != i_info->extra) {
        mqs_free(i_info->extra);
    }
    mqs_free(info);
}

/*---------------------------------------------------------------------*/

/* Setup information needed for a specific process.  The debugger
 * assumes that this will hang something onto the process, if nothing
 * is attached to it, then TV will believe that this process has no
 * message queue information.
 */
int mpidbg_init_per_process(mqs_process *process,
                            const mqs_process_callbacks *pcb,
                            struct mpidbg_handle_info_t *handle_types)
{
    mqs_image *image;
    mpi_image_info *i_info;

    /* Extract the addresses of the global variables we need and save
       them away */
    mpi_process_info *p_info =
        (mpi_process_info *) mqs_malloc(sizeof(mpi_process_info));
    printf("mpidbg_init_per_process\n");

    if (NULL == p_info) {
        return MPIDBG_ERR_NO_MEM;
    }

    /* Setup the callbacks first */
    p_info->process_callbacks = pcb;

    /* Nothing extra (yet) */
    p_info->extra = NULL;

    /* Now we can get the rest of the info */
    image = mqs_get_image(process);
    i_info = (mpi_image_info *) mqs_get_image_info(image);

    /* Get process info sizes */
    mqs_get_type_sizes (process, &p_info->sizes);

    /* Save the info */
    mqs_put_process_info(process, (mqs_process_info *) p_info);

    /* Fill in pre-defined MPI handle name mappings (because OMPI uses
       #define's for the pre-defined names, such as "#define
       MPI_COMM_WORLD &ompi_mpi_comm_world"). */
    /* Communicators */
    mpidbg_comm_name_map = alloc_map(image, 4);
    if (NULL != mpidbg_comm_name_map) {
        int i = 0;
        fill_map(image, "MPI_COMM_WORLD", "ompi_mpi_comm_world",
                 &mpidbg_comm_name_map[i++]);
        fill_map(image, "MPI_COMM_SELF", "ompi_mpi_comm_self",
                 &mpidbg_comm_name_map[i++]);
        fill_map(image, "MPI_COMM_NULL", "ompi_mpi_comm_null",
                 &mpidbg_comm_name_map[i++]);

        /* Sentinel value */
        mpidbg_comm_name_map[i].map_name = NULL;
    }

    /* Error handlers */
    mpidbg_errhandler_name_map = alloc_map(image, 4);
    if (NULL != mpidbg_errhandler_name_map) {
        int i = 0;
        fill_map(image, "MPI_ERRORS_ARE_FATAL", "ompi_mpi_errors_are_fatal",
                 &mpidbg_errhandler_name_map[i++]);
        fill_map(image, "MPI_ERRORS_ABORT", "ompi_mpi_errors_abort",
                 &mpidbg_errhandler_name_map[i++]);
        fill_map(image, "MPI_ERRORS_RETURN", "ompi_mpi_errors_return",
                 &mpidbg_errhandler_name_map[i++]);
        fill_map(image, "MPI_ERRHANDLER_NULL", "ompi_mpi_errhandler_null",
                 &mpidbg_errhandler_name_map[i++]);

        /* Sentinel value */
        mpidbg_errhandler_name_map[i].map_name = NULL;
    }

    /* Requests */
    mpidbg_request_name_map = alloc_map(image, 2);
    if (NULL != mpidbg_request_name_map) {
        int i = 0;
        fill_map(image, "MPI_REQUEST_NULL", "ompi_request_null",
                 &mpidbg_request_name_map[i++]);

        /* Sentinel value */
        mpidbg_request_name_map[i].map_name = NULL;
    }

    /* Statuses */
    mpidbg_status_name_map = alloc_map(image, 2);
    if (NULL != mpidbg_status_name_map) {
        int i = 0;
        fill_map(image, "MPI_STATUS_IGNORE", NULL,
                 &mpidbg_status_name_map[i++]);

        /* Sentinel value */
        mpidbg_status_name_map[i].map_name = NULL;
    }

    /* All done */
    return MPIDBG_SUCCESS;
}


/* This process is now done; free all the state associated with it */
void mpidbg_finalize_per_process(mqs_process *process, mqs_process_info *info)
{
    mpi_process_info *p_info = (mpi_process_info *)info;

    printf("mpidbg_finalize_per_process\n");
    if (NULL != p_info->extra) {
        mqs_free(p_info->extra);
    }
    mqs_free(info);
}


/*---------------------------------------------------------------------*/

int mpidbg_comm_query(mqs_image *image, mqs_image_info *image_info,
                      mqs_process *process, mqs_process_info *process_info,
                      mqs_taddr_t c_comm, struct mpidbg_comm_info_t **info)
{
    int flags;
    mpi_image_info *i_info = (mpi_image_info*) image_info;
    mpi_process_info *p_info = (mpi_process_info*) process_info;
    mqs_taddr_t group, topo, keyhash;

    /* Get the comm name */

    *info = mqs_malloc(sizeof(struct mpidbg_comm_info_t));
    if (NULL == *info) {
        return MPIDBG_ERR_NO_MEM;
    }
    /* JMS temporarily zero everything out.  Remove this when we fill
       in all the fields */
    memset(*info, 0, sizeof(struct mpidbg_comm_info_t));
    (*info)->comm_c_handle = c_comm;

    printf("mpidbg_comm_query: %p\n", (void*) c_comm);
    mqs_taddr_t name_addr = ompi_fetch_pointer( process,
                                                c_comm + i_info->ompi_communicator_t.offset.c_name,
                                                p_info );
    mqs_fetch_data(process, name_addr,
                   MPIDBG_MAX_OBJECT_NAME, (*info)->comm_name);
    (*info)->comm_name[MPIDBG_MAX_OBJECT_NAME-1] = '\0';
    /* Defensively zero anything beyond the actual name.

       We know that MPIDBG_MAX_OBJECT_NAME == MPI_MAX_OBJECT_NAME (per
       mpihandles_interface.h), and OMPI *guarantees* that
       (*info)->comm_name will be both \0-terminated, and have a
       maximum of (MPI_MAX_OBJECT_NAME-1) non-NULL characters.  So the
       memset length expression below is guaranted be >=0. */
    memset((*info)->comm_name + strlen((*info)->comm_name), 0,
           MPIDBG_MAX_OBJECT_NAME - 1 - strlen((*info)->comm_name));

    /* Get this process' rank in the comm */
    (*info)->comm_rank = ompi_fetch_int(process,
                                        c_comm + i_info->ompi_communicator_t.offset.c_my_rank,
                                        p_info);

    /* Analyze the flags on the comm */
    flags = ompi_fetch_int(process,
                           c_comm + i_info->ompi_communicator_t.offset.c_flags,
                           p_info);
    (*info)->comm_bitflags = 0;
    if (MPI_PROC_NULL == (*info)->comm_rank) {
        /* This communicator is MPI_COMM_NULL */
        (*info)->comm_rank = (*info)->comm_size = 0;
        (*info)->comm_bitflags |= MPIDBG_COMM_INFO_COMM_NULL;
    } else if (0 != (flags & OMPI_COMM_INTER)) {
        (*info)->comm_bitflags |= MPIDBG_COMM_INFO_INTERCOMM;
    } else {
        if (0 != (flags & OMPI_COMM_CART)) {
            (*info)->comm_bitflags |= MPIDBG_COMM_INFO_CARTESIAN;
        } else if (0 != (flags & OMPI_COMM_GRAPH)) {
            (*info)->comm_bitflags |= MPIDBG_COMM_INFO_GRAPH;
        } else if (0 != (flags & OMPI_COMM_DIST_GRAPH)) {
            (*info)->comm_bitflags |= MPIDBG_COMM_INFO_DIST_GRAPH;
        }
    }
    if (0 != (flags & OMPI_COMM_ISFREED)) {
        (*info)->comm_bitflags |= MPIDBG_COMM_INFO_FREED_HANDLE;
    }
    if (0 != (flags & OMPI_COMM_INTRINSIC)) {
        (*info)->comm_bitflags |= MPIDBG_COMM_INFO_PREDEFINED;
    }
    if (0 != (flags & OMPI_COMM_INVALID)) {
        (*info)->comm_bitflags |= MPIDBG_COMM_INFO_FREED_OBJECT;
    }

    /* Look up the local group */
    group = ompi_fetch_pointer(process,
                               c_comm + i_info->ompi_communicator_t.offset.c_local_group,
                               p_info);
    (*info)->comm_rank = ompi_fetch_int(process,
                                        group + i_info->ompi_group_t.offset.grp_my_rank,
                                        p_info);
    (*info)->comm_num_local_procs = ompi_fetch_int(process,
                                                   group + i_info->ompi_group_t.offset.grp_proc_count,
                                                   p_info);

    /* Fill in the comm_size with the size of the local group.  We'll
       override below if this is an intercommunicator. */
    (*info)->comm_size = (*info)->comm_num_local_procs;

    /* JMS fill this in: waiting to decide between mpidbg_process_t
       and mqs_process_location */
    (*info)->comm_local_procs = NULL;

    /* Look up the remote group (if relevant) */
    if (0 != (flags & OMPI_COMM_INTER)) {
        group = ompi_fetch_pointer(process,
                                   c_comm + i_info->ompi_communicator_t.offset.c_remote_group,
                                   p_info);
        (*info)->comm_num_remote_procs = ompi_fetch_int(process,
                                                        group + i_info->ompi_group_t.offset.grp_proc_count,
                                                        p_info);
        (*info)->comm_size = (*info)->comm_num_remote_procs;

        /* JMS fill this in: waiting to decide between
           mpidbg_process_t and mqs_process_location */
        (*info)->comm_remote_procs = NULL;
    } else {
        (*info)->comm_num_remote_procs = 0;
        (*info)->comm_remote_procs = NULL;
    }

    /* Fill in cartesian/graph info, if relevant.  The cartesian and
       graph data is just slightly different from each other; it's
       [slightly] easier (and less confusing!) to have separate
       retrieval code blocks. */
    topo = ompi_fetch_pointer(process,
                              c_comm + i_info->ompi_communicator_t.offset.c_topo,
                              p_info);
    if (0 != topo &&
        0 != ((*info)->comm_bitflags & MPIDBG_COMM_INFO_CARTESIAN)) {
        int i, ndims, tmp;
        mqs_taddr_t dims, periods;

        /* Alloc space for copying arrays */
        (*info)->comm_cart_num_dims = ndims =
            ompi_fetch_int(process,
                           topo + i_info->mca_topo_base_module_t.offset.mtc.cart.ndims,
                           p_info);
        (*info)->comm_cart_dims = mqs_malloc(ndims * sizeof(int));
        if (NULL == (*info)->comm_cart_dims) {
            return MPIDBG_ERR_NO_MEM;
        }
        (*info)->comm_cart_periods = mqs_malloc(ndims * sizeof(int8_t));
        if (NULL == (*info)->comm_cart_periods) {
            mqs_free((*info)->comm_cart_dims); (*info)->comm_cart_dims = NULL;
            return MPIDBG_ERR_NO_MEM;
        }
        (*info)->comm_cart_coords = mqs_malloc(ndims * sizeof(int8_t));
        if (NULL == (*info)->comm_cart_coords) {
            mqs_free((*info)->comm_cart_periods); (*info)->comm_cart_periods = NULL;
            mqs_free((*info)->comm_cart_dims);    (*info)->comm_cart_dims = NULL;
            return MPIDBG_ERR_NO_MEM;
        }

        /* Retrieve the dimension and periodic description data from
           the two arrays on the image's communicator */
        dims = ompi_fetch_pointer(process,
                                 topo + i_info->mca_topo_base_module_t.offset.mtc.cart.dims,
                                 p_info);
        periods = ompi_fetch_pointer(process,
                                 topo + i_info->mca_topo_base_module_t.offset.mtc.cart.periods,
                                 p_info);
        coords = ompi_fetch_pointer(process,
                                 topo + i_info->mca_topo_base_module_t.offset.mtc.cart.coords,
                                 p_info);

        for (i = 0; i < ndims; ++i) {
            (*info)->comm_cart_dims[i] =
                ompi_fetch_int(process, dims + (sizeof(int) * i), p_info);
            tmp = ompi_fetch_int(process, periods + (sizeof(int) * i), p_info);
            (*info)->comm_cart_periods[i] = (int8_t) tmp;
            printf("mpidbg: cart comm: dimension %d: (length %d, periodic: %d)\n", i, (*info)->comm_cart_dims[i], tmp);
        }
    } else if (0 != topo &&
               0 != ((*info)->comm_bitflags & MPIDBG_COMM_INFO_GRAPH)) {
        int i, nnodes;
        mqs_taddr_t index, edges;

        /* Alloc space for copying the indexes */
        (*info)->comm_graph_num_nodes = nnodes =
            ompi_fetch_int(process,
                           topo + i_info->mca_topo_base_module_t.offset.mtc.graph.nnodes,
                           p_info);
        (*info)->comm_graph_index = mqs_malloc(nnodes * sizeof(int));
        if (NULL == (*info)->comm_graph_index) {
            return MPIDBG_ERR_NO_MEM;
        }

        /* Retrieve the index data */
        index = ompi_fetch_pointer(process,
                                 topo + i_info->mca_topo_base_module_t.offset.mtc.graph.index,
                                 p_info);
        for (i = 0; i < nnodes; ++i) {
            (*info)->comm_graph_index[i] =
                ompi_fetch_int(process, index + (sizeof(int) * i), p_info);
        }

        /* Allocate space for the edges */
        (*info)->comm_graph_edges = mqs_malloc((*info)->comm_graph_index[(*info)->comm_graph_num_nodes - 1] * sizeof(int));
        if (NULL == (*info)->comm_graph_edges) {
            mqs_free((*info)->comm_graph_index);
            (*info)->comm_graph_index = NULL;
            return MPIDBG_ERR_NO_MEM;
        }

        /* Retrieve the edge data */
        edges = ompi_fetch_pointer(process,
                                 topo + i_info->mca_topo_base_module_t.offset.mtc.graph.edges,
                                 p_info);
        for (i = 0;
             i < (*info)->comm_graph_index[(*info)->comm_graph_num_nodes - 1];
             ++i) {
            (*info)->comm_graph_edges[i] =
                ompi_fetch_int(process, edges + (sizeof(int) * i), p_info);
        }
    } else if (0 != topo &&
               0 != ((*info)->comm_bitflags & MPIDBG_COMM_INFO_DIST_GRAPH)) {
        /* TODO: Complete the info if the communicator has a distributed graph topology */
    }

    /* Fortran handle */
    (*info)->comm_fortran_handle =
        ompi_fetch_int(process,
                       c_comm + i_info->ompi_communicator_t.offset.c_f_to_c_index,
                       p_info);
    printf("mpdbg: comm fortran handle: %d\n", (*info)->comm_fortran_handle);

    /* Fill in attributes */
    keyhash = ompi_fetch_pointer(process,
                                 c_comm + i_info->ompi_communicator_t.offset.c_keyhash,
                                 p_info);
    fill_attributes(&((*info)->comm_num_attrs), &((*info)->comm_attrs),
                    keyhash);

    /* JMS temporary */
    (*info)->comm_num_pending_requests = MPIDBG_ERR_NOT_SUPPORTED;
    (*info)->comm_pending_requests = NULL;
    (*info)->comm_num_derived_windows = MPIDBG_ERR_NOT_SUPPORTED;
    (*info)->comm_derived_windows = NULL;
    (*info)->comm_num_derived_files = MPIDBG_ERR_NOT_SUPPORTED;
    (*info)->comm_derived_files = NULL;

    return MPIDBG_SUCCESS;
}

int mpidbg_comm_f2c(mqs_image *image, mqs_image_info *image_info,
                    mqs_process *process, mqs_process_info *process_info,
                    mqs_taddr_t f77_comm, mqs_taddr_t *c_comm)
{
    mqs_taddr_t comm_list;
    mpi_image_info *i_info = (mpi_image_info *) image_info;
    mpi_process_info *p_info = (mpi_process_info*) process_info;

    mqs_find_symbol(image, "ompi_mpi_communicators", &comm_list);
    if (mqs_ok != ompi_fetch_opal_pointer_array_item(process, comm_list,
                                                     p_info, f77_comm,
                                                     c_comm) ||
        NULL == c_comm) {
        printf("mpidbg_comm_f2c: %lu -> not found\n",
               (long unsigned int) f77_comm);
        return MPIDBG_ERR_NOT_FOUND;
    }
    printf("mpidbg_comm_f2c: %lu -> %lu\n",
           (long unsigned int) f77_comm, (long unsigned int) c_comm);
    return MPIDBG_SUCCESS;
}

int mpidbg_comm_cxx2c(mqs_image *image, mqs_image_info *image_info,
                      mqs_process *process, mqs_process_info *process_info,
                      mqs_taddr_t cxx_comm,
                      enum mpidbg_comm_info_bitmap_t comm_type,
                      mqs_taddr_t *c_comm)
{
    /* David tells me that any type of communicator (MPI::Comm,
       MPI::Intracomm, etc.) should have the offset to the mpi_comm
       member in the same place. */
    printf("mpidbg_comm_cxx2c: %p\n", (void*) cxx_comm);
    return MPIDBG_ERR_NOT_FOUND;
}

/*---------------------------------------------------------------------*/

int mpidbg_errhandler_query(mqs_image *image, mqs_image_info *image_info,
                            mqs_process *process, mqs_process_info *process_info,
                            mqs_taddr_t c_errhandler,
                            struct mpidbg_errhandler_info_t **info)
{
    printf("mpidbg_errhandler_query: %p\n", (void*) c_errhandler);
    printf("mpidbg_errhandler_query: not [yet] found\n");
    return MPIDBG_ERR_NOT_FOUND;
}

int mpidbg_errhandler_f2c(mqs_image *image, mqs_image_info *image_info,
                          mqs_process *process, mqs_process_info *process_info,
                          mqs_taddr_t f77_errhandler, mqs_taddr_t *c_errhandler)
{
    printf("mpidbg_errhandler_f2c: %lu\n", (long unsigned int) f77_errhandler);
    printf("mpidbg_errhandler_f2c: not [yet] found\n");
    return MPIDBG_ERR_NOT_FOUND;
}

int mpidbg_errhandler_cxx2c(mqs_image *image, mqs_image_info *image_info,
                            mqs_process *process, mqs_process_info *process_info,
                            mqs_taddr_t cxx_errhandler,
                            mqs_taddr_t *c_errhandler)
{
    printf("mpidbg_errhandler_cxx2c: %p\n", (void*) cxx_errhandler);
    printf("mpidbg_errhandler_cxx2c: not [yet] found\n");
    return MPIDBG_ERR_NOT_FOUND;
}

/*---------------------------------------------------------------------*/

int mpidbg_request_query(mqs_image *image, mqs_image_info *image_info,
                         mqs_process *process, mqs_process_info *process_info,
                         mqs_taddr_t c_request,
                         struct mpidbg_request_info_t **info)
{
    printf("mpidbg_request_query: %p\n", (void*) c_request);
    printf("mpidbg_request_query: not [yet] found\n");
    return MPIDBG_ERR_NOT_FOUND;
}

int mpidbg_request_f2c(mqs_image *image, mqs_image_info *image_info,
                       mqs_process *process, mqs_process_info *process_info,
                       mqs_taddr_t f77_request, mqs_taddr_t *c_request)
{
    printf("mpidbg_request_f2c: %lu\n", (long unsigned int) f77_request);
    printf("mpidbg_request_f2c: not [yet] found\n");
    return MPIDBG_ERR_NOT_FOUND;
}

int mpidbg_request_cxx2c(mqs_image *image, mqs_image_info *image_info,
                         mqs_process *process, mqs_process_info *process_info,
                         mqs_taddr_t cxx_request,
                         enum mpidbg_request_info_bitmap_t request_type,
                         mqs_taddr_t *c_request)
{
    printf("mpidbg_request_cxx2c: %p\n", (void*) cxx_request);
    printf("mpidbg_request_cxx2c: not [yet] found\n");
    return MPIDBG_ERR_NOT_FOUND;
}

/*---------------------------------------------------------------------*/

int mpidbg_status_query(mqs_image *image, mqs_image_info *image_info,
                        mqs_process *process, mqs_process_info *process_info,
                        mqs_taddr_t c_status,
                        struct mpidbg_status_info_t **info)
{
    printf("mpidbg_status_query: %p\n", (void*) c_status);
    printf("mpidbg_status_query: not [yet] found\n");
    return MPIDBG_ERR_NOT_FOUND;
}

int mpidbg_status_f2c(mqs_image *image, mqs_image_info *image_info,
                      mqs_process *process, mqs_process_info *process_info,
                      mqs_taddr_t f77_status, mqs_taddr_t *c_status)
{
    printf("mpidbg_status_f2c: %lu\n", (long unsigned int) f77_status);
    printf("mpidbg_status_f2c: not [yet] found\n");
    return MPIDBG_ERR_NOT_FOUND;
}

int mpidbg_status_cxx2c(mqs_image *image, mqs_image_info *image_info,
                        mqs_process *process, mqs_process_info *process_info,
                        mqs_taddr_t cxx_status,
                        mqs_taddr_t *c_status)
{
    printf("mpidbg_status_cxx2c: %p\n", (void*) cxx_status);
    printf("mpidbg_status_cxx2c: not [yet] found\n");
    return MPIDBG_ERR_NOT_FOUND;
}
