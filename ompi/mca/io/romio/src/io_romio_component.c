/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
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
#include "io_romio.h"


/*
 * Private functions
 */
static int open_component(void);
static int close_component(void);
static int init_query(bool enable_progress_threads,
                      bool enable_mpi_threads);
static const struct mca_io_base_module_1_0_0_t *
  file_query(struct ompi_file_t *file, 
             struct mca_io_base_file_t **private_data,
             int *priority);
static int file_unquery(struct ompi_file_t *file, 
                        struct mca_io_base_file_t *private_data);

static int delete_query(char *filename, struct ompi_info_t *info, 
                        struct mca_io_base_delete_t **private_data,
                        bool *usable, int *priorty);
static int delete_select(char *filename, struct ompi_info_t *info,
                         struct mca_io_base_delete_t *private_data);
static int progress(void);

static int register_datarep(char *,
                            MPI_Datarep_conversion_function*,
                            MPI_Datarep_conversion_function*,
                            MPI_Datarep_extent_function*,
                            void*);

/*
 * Private variables
 */
static int priority_param = -1;
static int delete_priority_param = -1;


/*
 * Global, component-wide ROMIO mutex because ROMIO is not thread safe
 */
opal_mutex_t mca_io_romio_mutex;


/*
 * Global list of requests for this component
 */
opal_list_t mca_io_romio_pending_requests;


/*
 * Public string showing the coll ompi_demo component version number
 */
const char *mca_io_romio_component_version_string =
  "OMPI/MPI ROMIO io MCA component version " OMPI_VERSION;


mca_io_base_component_1_0_0_t mca_io_romio_component = {
    /* First, the mca_base_component_t struct containing meta information
       about the component itself */

    {
        /* Indicate that we are a io v1.0.0 component (which also implies a
           specific MCA version) */
        MCA_IO_BASE_VERSION_1_0_0,
        "romio",
        OMPI_MAJOR_VERSION,
        OMPI_MINOR_VERSION,
        OMPI_RELEASE_VERSION,
        open_component,
        close_component,
    },

    /* Next the MCA v1.0.0 component meta data */

    {
        /* Whether the component is checkpointable or not */
        false
    },

    /* Additional number of bytes required for this component's
       requests */

    sizeof(mca_io_romio_request_t) - sizeof(mca_io_base_request_t),

    /* Initial configuration / Open a new file */

    init_query,
    file_query,
    file_unquery,

    /* Delete a file */

    delete_query,
    NULL,
    delete_select,

    /* Progression of non-blocking requests */

    (mca_io_base_component_progress_fn_t) progress,
    register_datarep
};


static int open_component(void)
{
    /* Use a low priority, but allow other components to be lower */
    
    priority_param = 
        mca_base_param_reg_int(&mca_io_romio_component.io_version, 
                               "priority",
                               "Priority of the io romio component",
                               false, false, 10, NULL);
    delete_priority_param = 
        mca_base_param_reg_int(&mca_io_romio_component.io_version,
                               "delete_priority", 
                               "Delete priority of the io romio component",
                               false, false, 10, NULL);

    mca_base_param_reg_int(&mca_io_romio_component.io_version,
                           "enable_parallel_optimizations",
                           "Enable set of Open MPI-added options to improve collective file i/o performance",
                           false, false, 0, NULL);

    /* Create the mutex */
    OBJ_CONSTRUCT(&mca_io_romio_mutex, opal_mutex_t);

    /* Create the list of pending requests */

    OBJ_CONSTRUCT(&mca_io_romio_pending_requests, opal_list_t);

    return OMPI_SUCCESS;
}


static int close_component(void)
{
    /* Destroy the list of pending requests */
    /* JMS: Good opprotunity here to list out all the IO requests that
       were not destroyed / completed upon MPI_FINALIZE */

    OBJ_DESTRUCT(&mca_io_romio_pending_requests);

    OBJ_DESTRUCT(&mca_io_romio_mutex);

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


static const struct mca_io_base_module_1_0_0_t *
file_query(struct ompi_file_t *file, 
           struct mca_io_base_file_t **private_data,
           int *priority)
{
    mca_io_romio_data_t *data;

    /* Lookup our priority */

    if (OMPI_SUCCESS != mca_base_param_lookup_int(priority_param,
                                                  priority)) {
        return NULL;
    }

    /* Allocate a space for this module to hang private data (e.g.,
       the ROMIO file handle) */

    data = malloc(sizeof(mca_io_romio_data_t));
    if (NULL == data) {
        return NULL;
    }
    data->romio_fh = NULL;
    *private_data = (struct mca_io_base_file_t*) data;

    /* All done */

    return &mca_io_romio_module;
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


static int delete_query(char *filename, struct ompi_info_t *info, 
                        struct mca_io_base_delete_t **private_data,
                        bool *usable, int *priority)
{
    /* Lookup our priority */

    if (OMPI_SUCCESS != mca_base_param_lookup_int(delete_priority_param,
                                                  priority)) {
        return OMPI_ERROR;
    }

    *usable = true;
    *private_data = NULL;

    return OMPI_SUCCESS;
}


static int delete_select(char *filename, struct ompi_info_t *info,
                         struct mca_io_base_delete_t *private_data)
{
    int ret;

    OPAL_THREAD_LOCK (&mca_io_romio_mutex);
    ret = ROMIO_PREFIX(MPI_File_delete)(filename, info);
    OPAL_THREAD_UNLOCK (&mca_io_romio_mutex);

    return ret;
}


static int progress()
{
    opal_list_item_t *item, *next;
    int ret, flag, count;
    ROMIO_PREFIX(MPIO_Request) romio_rq;
    mca_io_base_request_t *ioreq;

    /* Troll through all pending requests and try to progress them.
       If a request finishes, remove it from the list. */

    count = 0;
    OPAL_THREAD_LOCK (&mca_io_romio_mutex);
    for (item = opal_list_get_first(&mca_io_romio_pending_requests);
         item != opal_list_get_end(&mca_io_romio_pending_requests); 
         item = next) {
        next = opal_list_get_next(item);

        ioreq = (mca_io_base_request_t*) item;
        romio_rq = ((mca_io_romio_request_t *) item)->romio_rq;
        ret = ROMIO_PREFIX(MPIO_Test)(&romio_rq, &flag, 
                                      &(((ompi_request_t *) item)->req_status));
        if ((0 != ret) || (0 != flag)) {
            ioreq->super.req_status.MPI_ERROR = ret;
            ++count;
            /* we're done, so remove us from the pending list */
            opal_list_remove_item(&mca_io_romio_pending_requests, item);
            /* mark as complete (and make sure to wake up any waiters */
            ompi_request_complete((ompi_request_t*) item);
            mca_io_base_request_progress_del();
            /* if the request has been freed already, the user isn't
             * going to call test or wait on us, so we need to do it
             * here
             */
            if (ioreq->free_called) {
                ret = ompi_request_free((ompi_request_t**) &ioreq);
                if (OMPI_SUCCESS != ret) {
                    OPAL_THREAD_UNLOCK(&mca_io_romio_mutex);
                    return count;
                }
            }
        }
    }
    OPAL_THREAD_UNLOCK (&mca_io_romio_mutex);

    /* Return how many requests completed */

    return count;
}



static int
register_datarep(char * datarep,
                 MPI_Datarep_conversion_function* read_fn,
                 MPI_Datarep_conversion_function* write_fn,
                 MPI_Datarep_extent_function* extent_fn,
                 void* state)
{
    int ret;

    OPAL_THREAD_LOCK(&mca_io_romio_mutex);
    ret = ROMIO_PREFIX(MPI_Register_datarep(datarep, read_fn, write_fn,
                                            extent_fn, state));
    OPAL_THREAD_UNLOCK(&mca_io_romio_mutex);

    return ret;
}
