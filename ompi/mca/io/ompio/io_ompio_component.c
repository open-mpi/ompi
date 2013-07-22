/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008-2011 University of Houston. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "mpi.h"
#include "opal/class/opal_list.h"
#include "opal/threads/mutex.h"
#include "opal/mca/base/base.h"
#include "ompi/mca/io/io.h"
#include "io_ompio.h"

int mca_io_ompio_cycle_buffer_size = OMPIO_PREALLOC_MAX_BUF_SIZE;
int mca_io_ompio_bytes_per_agg = OMPIO_PREALLOC_MAX_BUF_SIZE;
int mca_io_ompio_record_offset_info = 0;
int mca_io_ompio_coll_timing_info = 0;
int mca_io_ompio_sharedfp_lazy_open = 1;

/*
 * Private functions
 */
static int register_component(void);
static int open_component(void);
static int close_component(void);
static int init_query(bool enable_progress_threads,
                      bool enable_mpi_threads);
static const struct mca_io_base_module_2_0_0_t * 
file_query (struct ompi_file_t *file, 
            struct mca_io_base_file_t **private_data,
            int *priority);
static int file_unquery(struct ompi_file_t *file, 
                        struct mca_io_base_file_t *private_data);

static int delete_query(char *filename, struct ompi_info_t *info, 
                        struct mca_io_base_delete_t **private_data,
                        bool *usable, int *priorty);

static int delete_select(char *filename, struct ompi_info_t *info,
                         struct mca_io_base_delete_t *private_data);
/*
static int io_progress(void);

static int register_datarep(char *,
                            MPI_Datarep_conversion_function*,
                            MPI_Datarep_conversion_function*,
                            MPI_Datarep_extent_function*,
                            void*);
*/

/*
 * Private variables
 */
static int priority_param = 10;
static int delete_priority_param = 10;


/*
 * Global, component-wide OMPIO mutex because OMPIO is not thread safe
 */
opal_mutex_t mca_io_ompio_mutex;


/*
 * Global list of requests for this component
 */
opal_list_t mca_io_ompio_pending_requests;


/*
 * Public string showing this component's version number
 */
const char *mca_io_ompio_component_version_string =
"OMPI/MPI OMPIO io MCA component version " OMPI_VERSION;


mca_io_base_component_2_0_0_t mca_io_ompio_component = {
    /* First, the mca_base_component_t struct containing meta information
       about the component itself */

    {
        MCA_IO_BASE_VERSION_2_0_0,
        "ompio",
        OMPI_MAJOR_VERSION,
        OMPI_MINOR_VERSION,
        OMPI_RELEASE_VERSION,
        open_component,
        close_component,
        .mca_register_component_params = register_component,
    },
    {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    },

    /* Initial configuration / Open a new file */

    init_query,
    file_query,
    file_unquery,

    /* Delete a file */

    delete_query,
    NULL, /* delete_unquery */
    delete_select, /* delete_select */

    NULL  /* io_register_datarep */
};

static int register_component(void)
{
    priority_param = 10;
    (void) mca_base_component_var_register(&mca_io_ompio_component.io_version,
                                           "priority", "Priority of the io ompio component",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &priority_param);
    delete_priority_param = 10;
    (void) mca_base_component_var_register(&mca_io_ompio_component.io_version,
                                           "delete_priority", "Delete priority of the io ompio component",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &delete_priority_param);

    /* NTH: Don't bother registering a version if there is none */
/*     reg_string(&mca_io_ompio_component.io_version, */
/*                               "version",  */
/*                               "Version of OMPIO", */
/*                               false, true, NULL, NULL); */

    mca_io_ompio_record_offset_info = 0;
    (void) mca_base_component_var_register(&mca_io_ompio_component.io_version,
                                           "record_file_offset_info",
                                           "The information of the file offset/length",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_io_ompio_record_offset_info);

    mca_io_ompio_coll_timing_info = 0;
    (void) mca_base_component_var_register(&mca_io_ompio_component.io_version,
                                           "coll_timing_info",
                                           "Enable collective algorithm timing information",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_io_ompio_coll_timing_info);

    mca_io_ompio_cycle_buffer_size = OMPIO_PREALLOC_MAX_BUF_SIZE;
    (void) mca_base_component_var_register(&mca_io_ompio_component.io_version,
                                           "cycle_buffer_size",
                                           "Cycle Buffer Size of individual reads/writes",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_io_ompio_cycle_buffer_size);

    mca_io_ompio_bytes_per_agg = OMPIO_PREALLOC_MAX_BUF_SIZE;
    (void) mca_base_component_var_register(&mca_io_ompio_component.io_version,
                                           "bytes_per_agg",
                                           "Bytes per aggregator process for automatic selection",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_io_ompio_bytes_per_agg);

    mca_io_ompio_sharedfp_lazy_open = 1;
    (void) mca_base_component_var_register(&mca_io_ompio_component.io_version,
                                           "sharedfp_lazy_open",
                                           "lazy allocation of internal shared file pointer structures",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_io_ompio_sharedfp_lazy_open);

  
    return OMPI_SUCCESS;
}

static int open_component(void)
{
    /* Create the mutex */
    OBJ_CONSTRUCT(&mca_io_ompio_mutex, opal_mutex_t);

    /* Create the list of pending requests */

    OBJ_CONSTRUCT(&mca_io_ompio_pending_requests, opal_list_t);

    return OMPI_SUCCESS;
}


static int close_component(void)
{
    /* Destroy the list of pending requests */
    /* JMS: Good opprotunity here to list out all the IO requests that
       were not destroyed / completed upon MPI_FINALIZE */

    OBJ_DESTRUCT(&mca_io_ompio_pending_requests);

    OBJ_DESTRUCT(&mca_io_ompio_mutex);

    return OMPI_SUCCESS;
}


static int init_query(bool enable_progress_threads,
                      bool enable_mpi_threads)
{
    return OMPI_SUCCESS;
}


static const struct mca_io_base_module_2_0_0_t *
file_query(struct ompi_file_t *file, 
           struct mca_io_base_file_t **private_data,
           int *priority)
{
    mca_io_ompio_data_t *data;

    *priority = priority_param;

    /* Allocate a space for this module to hang private data (e.g.,
       the OMPIO file handle) */

    data = malloc(sizeof(mca_io_ompio_data_t));
    if (NULL == data) {
        return NULL;
    }

    *private_data = (struct mca_io_base_file_t*) data;

    /* All done */

    return &mca_io_ompio_module;
}


static int file_unquery(struct ompi_file_t *file, 
                        struct mca_io_base_file_t *private_data)
{
    /* Free the ompio module-specific data that was allocated in
       _file_query(), above */

    if (NULL != private_data) {
        free(private_data);
    }

    return OMPI_SUCCESS;
}


static int delete_query(char *filename, struct ompi_info_t *info, 
                        struct mca_io_base_delete_t **private_data,
                        bool *usable, int *priority)
{
    *priority = delete_priority_param;
    *usable = true;
    *private_data = NULL;

    return OMPI_SUCCESS;
}

static int delete_select(char *filename, struct ompi_info_t *info,
                         struct mca_io_base_delete_t *private_data)
{
    int ret;

    OPAL_THREAD_LOCK (&mca_io_ompio_mutex);
    ret = mca_io_ompio_file_delete (filename, info);
    OPAL_THREAD_UNLOCK (&mca_io_ompio_mutex);

    return ret;
}
/*
static int io_progress (void)
{
    return OMPI_SUCCESS;
}
*/
