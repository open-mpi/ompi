/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"
#include "mpi.h"
#include "class/ompi_list.h"
#include "threads/mutex.h"
#include "mca/base/base.h"
#include "mca/io/io.h"
#include "io_romio.h"
#include "io-romio-version.h"


/*
 * Private functions
 */
static int open_component(void);
static int close_component(void);
static int init_query(bool *enable_multi_user_threads,
                      bool *have_hidden_threads);
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

/*
 * Private variables
 */
static int priority_param = -1;
static int delete_priority_param = -1;


/*
 * Global, component-wide ROMIO mutex because ROMIO is not thread safe
 */
ompi_mutex_t mca_io_romio_mutex;


/*
 * Global list of requests for this component
 */
ompi_list_t mca_io_romio_pending_requests;


/*
 * Public string showing the coll ompi_demo component version number
 */
const char *mca_io_romio_component_version_string =
  "OMPI/MPI ROMIO io MCA component version " MCA_io_romio_VERSION;


mca_io_base_component_1_0_0_t mca_io_romio_component = {
    /* First, the mca_base_component_t struct containing meta information
       about the component itself */

    {
        /* Indicate that we are a io v1.0.0 component (which also implies a
           specific MCA version) */
        MCA_IO_BASE_VERSION_1_0_0,
        "romio",
        MCA_io_romio_MAJOR_VERSION,
        MCA_io_romio_MINOR_VERSION,
        MCA_io_romio_RELEASE_VERSION,
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

    progress
};


static int open_component(void)
{
    /* Use a low priority, but allow other components to be lower */
    
    priority_param = 
        mca_base_param_register_int("io", "romio", "priority", NULL, 10);
    delete_priority_param = 
        mca_base_param_register_int("io", "romio", "delete_priority", 
                                    NULL, 10);

    /* Create the list of pending requests */

    OBJ_CONSTRUCT(&mca_io_romio_pending_requests, ompi_list_t);

    return OMPI_SUCCESS;
}


static int close_component(void)
{
    /* Destroy the list of pending requests */
    /* JMS: Good opprotunity here to list out all the IO requests that
       were not destroyed / completed upon MPI_FINALIZE */

    OBJ_DESTRUCT(&mca_io_romio_pending_requests);

    return OMPI_SUCCESS;
}


static int init_query(bool *allow_multi_user_threads,
                      bool *have_hidden_threads)
{
    /* Note that we say "true" for multi user threads here because we
       self-enforce only allowing one user thread into ROMIO at a time
       -- this fact will be clearly documented for users (ROMIO itself
       is not thread safe). */

    *allow_multi_user_threads = true;
    *have_hidden_threads = ompi_using_threads();

    /* Don't launch a progress thread here -- we'll launch one the
       first time a ROMIO module is initialized */

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

    OMPI_THREAD_LOCK (&mca_io_romio_mutex);
    ret = ROMIO_PREFIX(MPI_File_delete)(filename, info);
    OMPI_THREAD_UNLOCK (&mca_io_romio_mutex);

    return ret;
}


static int progress(void)
{
    ompi_list_item_t *item, *next;
    int ret, flag, count = 0;
    ROMIO_PREFIX(MPIO_Request) romio_rq;

    /* Troll through all pending requests and try to progress them.
       If a request finishes, remove it from the list. */

    OMPI_THREAD_LOCK (&mca_io_romio_mutex);
    for (item = ompi_list_get_first(&mca_io_romio_pending_requests);
         item != ompi_list_get_end(&mca_io_romio_pending_requests); 
         item = next) {
        next = ompi_list_get_next(item);

        romio_rq = ((mca_io_romio_request_t *) item)->romio_rq;
        ret = ROMIO_PREFIX(MPIO_Test)(&romio_rq, &flag, 
                                      &(((ompi_request_t *) item)->req_status));
        if (ret < 0) {
            return ret;
        } else if (1 == flag) {
            ++count;
            ompi_request_complete((ompi_request_t*) item);
            OMPI_REQUEST_FINI((ompi_request_t*) item);
            ompi_list_remove_item(&mca_io_romio_pending_requests, item);
            mca_io_base_request_free(((mca_io_base_request_t *) item)->req_file,
                                     (mca_io_base_request_t *) item);
        }
    }
    OMPI_THREAD_UNLOCK (&mca_io_romio_mutex);

    /* Return how many requests completed */

    return count;
}
