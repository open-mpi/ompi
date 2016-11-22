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
 * Copyright (c) 2008-2015 Cisco Systems, Inc.  All rights reserved.
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
#include "io_romio314.h"

#define ROMIO_VERSION_STRING "from MPICH v3.1.4"

/*
 * Private functions
 */
static int register_component(void);
static int open_component(void);
static int close_component(void);
static int init_query(bool enable_progress_threads,
                      bool enable_mpi_threads);
static const struct mca_io_base_module_2_0_0_t *
  file_query(struct ompi_file_t *file,
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
 * Private variables
 */
static int priority_param = 20;
static int delete_priority_param = 20;


/*
 * Global, component-wide ROMIO mutex because ROMIO is not thread safe
 */
opal_mutex_t mca_io_romio314_mutex = {{0}};


/*
 * Public string showing this component's version number
 */
const char *mca_io_romio314_component_version_string =
"OMPI/MPI ROMIO io MCA component version " OMPI_VERSION ", " ROMIO_VERSION_STRING;


mca_io_base_component_2_0_0_t mca_io_romio314_component = {
    /* First, the mca_base_component_t struct containing meta information
       about the component itself */

    .io_version = {
        MCA_IO_BASE_VERSION_2_0_0,
        .mca_component_name = "romio314",
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

static char *ompi_io_romio314_version = ROMIO_VERSION_STRING;
static char *ompi_io_romio314_user_configure_params = MCA_io_romio314_USER_CONFIGURE_FLAGS;
static char *ompi_io_romio314_complete_configure_params = MCA_io_romio314_COMPLETE_CONFIGURE_FLAGS;

static int register_component(void)
{
    /* Use a low priority, but allow other components to be lower */
    priority_param = 10;
    (void) mca_base_component_var_register(&mca_io_romio314_component.io_version,
                                           "priority", "Priority of the io romio component",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY, &priority_param);
    delete_priority_param = 10;
    (void) mca_base_component_var_register(&mca_io_romio314_component.io_version,
                                           "delete_priority", "Delete priority of the io romio component",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY, &delete_priority_param);
    (void) mca_base_component_var_register(&mca_io_romio314_component.io_version,
                                           "version", "Version of ROMIO", MCA_BASE_VAR_TYPE_STRING,
                                           NULL, 0, MCA_BASE_VAR_FLAG_DEFAULT_ONLY,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY, &ompi_io_romio314_version);
    (void) mca_base_component_var_register(&mca_io_romio314_component.io_version,
                                           "user_configure_params",
                                           "User-specified command line parameters passed to ROMIO's configure script",
                                           MCA_BASE_VAR_TYPE_STRING, NULL, 0,
                                           MCA_BASE_VAR_FLAG_DEFAULT_ONLY,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY, &ompi_io_romio314_user_configure_params);
    (void) mca_base_component_var_register(&mca_io_romio314_component.io_version,
                                           "complete_configure_params",
                                           "Complete set of command line parameters passed to ROMIO's configure script",
                                           MCA_BASE_VAR_TYPE_STRING, NULL, 0,
                                           MCA_BASE_VAR_FLAG_DEFAULT_ONLY,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY, &ompi_io_romio314_complete_configure_params);

    return OMPI_SUCCESS;
}

static int open_component(void)
{
    /* Create the mutex */
    OBJ_CONSTRUCT(&mca_io_romio314_mutex, opal_mutex_t);

    return OMPI_SUCCESS;
}


static int close_component(void)
{
    OBJ_DESTRUCT(&mca_io_romio314_mutex);

    return OMPI_SUCCESS;
}


static int init_query(bool enable_progress_threads,
                      bool enable_mpi_threads)
{
    /* Note that it's ok if mpi_enable_threads==true here because we
       self-enforce only allowing one user thread into ROMIO at a time
       -- this fact will be clearly documented for users (ROMIO itself
       is not thread safe). */

    return OMPI_SUCCESS;
}


static const struct mca_io_base_module_2_0_0_t *
file_query(struct ompi_file_t *file,
           struct mca_io_base_file_t **private_data,
           int *priority)
{
    mca_io_romio314_data_t *data;

    *priority = priority_param;

    /* Allocate a space for this module to hang private data (e.g.,
       the ROMIO file handle) */

    data = malloc(sizeof(mca_io_romio314_data_t));
    if (NULL == data) {
        return NULL;
    }
    data->romio_fh = NULL;
    *private_data = (struct mca_io_base_file_t*) data;

    /* All done */

    return &mca_io_romio314_module;
}


static int file_unquery(struct ompi_file_t *file,
                        struct mca_io_base_file_t *private_data)
{
    /* Free the romio module-specific data that was allocated in
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

    OPAL_THREAD_LOCK (&mca_io_romio314_mutex);
    ret = ROMIO_PREFIX(MPI_File_delete)(filename, info);
    OPAL_THREAD_UNLOCK (&mca_io_romio314_mutex);

    return ret;
}


static int
register_datarep(const char * datarep,
                 MPI_Datarep_conversion_function* read_fn,
                 MPI_Datarep_conversion_function* write_fn,
                 MPI_Datarep_extent_function* extent_fn,
                 void* state)
{
    int ret;

    OPAL_THREAD_LOCK(&mca_io_romio314_mutex);
    ret = ROMIO_PREFIX(MPI_Register_datarep(datarep, read_fn, write_fn,
                                            extent_fn, state));
    OPAL_THREAD_UNLOCK(&mca_io_romio314_mutex);

    return ret;
}
