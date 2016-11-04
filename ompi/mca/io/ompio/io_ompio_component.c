/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
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
 * Copyright (c) 2008-2015 University of Houston. All rights reserved.
 * Copyright (c) 2015      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
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

int mca_io_ompio_cycle_buffer_size = -1;
int mca_io_ompio_bytes_per_agg = OMPIO_PREALLOC_MAX_BUF_SIZE;
int mca_io_ompio_num_aggregators = -1;
int mca_io_ompio_record_offset_info = 0;
int mca_io_ompio_coll_timing_info = 0;
int mca_io_ompio_sharedfp_lazy_open = 1;

int mca_io_ompio_grouping_option=5;

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

static int delete_query(const char *filename, struct ompi_info_t *info,
                        struct mca_io_base_delete_t **private_data,
                        bool *usable, int *priorty);

static int delete_select(const char *filename, struct ompi_info_t *info,
                         struct mca_io_base_delete_t *private_data);

static int register_datarep(const char *,
                            MPI_Datarep_conversion_function*,
                            MPI_Datarep_conversion_function*,
                            MPI_Datarep_extent_function*,
                            void*);
/*
static int io_progress(void);

*/

/*
 * Private variables
 */
static int priority_param = 30;
static int delete_priority_param = 30;


/*
 * Global, component-wide OMPIO mutex because OMPIO is not thread safe
 */
opal_mutex_t mca_io_ompio_mutex = {{0}};


/*
 * Global list of requests for this component
 */
opal_list_t mca_io_ompio_pending_requests = {{0}};


/*
 * Public string showing this component's version number
 */
const char *mca_io_ompio_component_version_string =
"OMPI/MPI OMPIO io MCA component version " OMPI_VERSION;


mca_io_base_component_2_0_0_t mca_io_ompio_component = {
    /* First, the mca_base_component_t struct containing meta information
       about the component itself */

    .io_version = {
        MCA_IO_BASE_VERSION_2_0_0,
        .mca_component_name = "ompio",
        MCA_BASE_MAKE_VERSION(component, OMPI_MAJOR_VERSION, OMPI_MINOR_VERSION,
                              OMPI_RELEASE_VERSION),
        .mca_open_component = open_component,
        .mca_close_component = close_component,
        .mca_register_component_params = register_component,
    },
    .io_data = {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    },

    /* Initial configuration / Open a new file */

    .io_init_query = init_query,
    .io_file_query = file_query,
    .io_file_unquery = file_unquery,

    /* Delete a file */

    .io_delete_query = delete_query,
    .io_delete_select = delete_select,

    .io_register_datarep = register_datarep,
};

static int register_component(void)
{
    priority_param = 30;
    (void) mca_base_component_var_register(&mca_io_ompio_component.io_version,
                                           "priority", "Priority of the io ompio component",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &priority_param);
    delete_priority_param = 30;
    (void) mca_base_component_var_register(&mca_io_ompio_component.io_version,
                                           "delete_priority", "Delete priority of the io ompio component",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &delete_priority_param);

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

    mca_io_ompio_cycle_buffer_size = -1;
    (void) mca_base_component_var_register(&mca_io_ompio_component.io_version,
                                           "cycle_buffer_size",
                                           "Data size issued by individual reads/writes per call",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_io_ompio_cycle_buffer_size);

    mca_io_ompio_bytes_per_agg = OMPIO_PREALLOC_MAX_BUF_SIZE;
    (void) mca_base_component_var_register(&mca_io_ompio_component.io_version,
                                           "bytes_per_agg",
                                           "Size of temporary buffer for collective I/O operations",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_io_ompio_bytes_per_agg);

    mca_io_ompio_num_aggregators = -1;
    (void) mca_base_component_var_register(&mca_io_ompio_component.io_version,
                                           "num_aggregators",
                                           "number of aggregators for collective I/O operations",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_io_ompio_num_aggregators);


    mca_io_ompio_sharedfp_lazy_open = 1;
    (void) mca_base_component_var_register(&mca_io_ompio_component.io_version,
                                           "sharedfp_lazy_open",
                                           "lazy allocation of internal shared file pointer structures",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_io_ompio_sharedfp_lazy_open);

    mca_io_ompio_grouping_option = 5;
    (void) mca_base_component_var_register(&mca_io_ompio_component.io_version,
                                           "grouping_option",
                                           "Option for grouping of processes in the aggregator selection "
                                           "1: Data volume based grouping 2: maximizing group size uniformity 3: maximimze "
                                           "data contiguity 4: hybrid optimization  5: simple (default) "
                                           "6: skip refinement step",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_io_ompio_grouping_option);

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

    data = calloc(1, sizeof(mca_io_ompio_data_t));
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


static int delete_query(const char *filename, struct ompi_info_t *info,
                        struct mca_io_base_delete_t **private_data,
                        bool *usable, int *priority)
{
    *priority = delete_priority_param;
    *usable = true;
    *private_data = NULL;

    return OMPI_SUCCESS;
}

static int delete_select(const char *filename, struct ompi_info_t *info,
                         struct mca_io_base_delete_t *private_data)
{
    int ret;

    OPAL_THREAD_LOCK (&mca_io_ompio_mutex);
    ret = mca_io_ompio_file_delete (filename, info);
    OPAL_THREAD_UNLOCK (&mca_io_ompio_mutex);

    return ret;
}

static int register_datarep(const char * datarep,
                            MPI_Datarep_conversion_function* read_fn,
                            MPI_Datarep_conversion_function* write_fn,
                            MPI_Datarep_extent_function* extent_fn,
                            void* state)
{
    return OMPI_ERROR;
}

/*
static int io_progress (void)
{
    return OMPI_SUCCESS;
}
*/
