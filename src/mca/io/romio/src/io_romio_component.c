/*
 * $HEADER$
 */

#include "mpi.h"
#include "mca/io/io.h"
#include "io_romio.h"


/*
 * Private functions
 */
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


OMPI_EXPORT
mca_io_base_component_1_0_0_t mca_io_romio_component = {
    /* First, the mca_base_component_t struct containing meta information
       about the component itself */

    {
        /* Indicate that we are a io v1.0.0 component (which also implies a
           specific MCA version) */
        MCA_IO_BASE_VERSION_1_0_0,
        "romio",                   /* MCA component name */
        1,                         /* MCA component major version */
        0,                         /* MCA component minor version */
        0,                         /* MCA component release version */
        NULL,
        NULL
    },

    /* Next the MCA v1.0.0 component meta data */

    {
        /* Whether the component is checkpointable or not */
        false
    },

    /* Initial configuration / Open a new file */

    init_query,
    file_query,
    file_unquery,

    /* Delete a file */

    delete_query,
    NULL,
    delete_select
};


static int init_query(bool *allow_multi_user_threads,
                      bool *have_hidden_threads)
{
    *allow_multi_user_threads = false;
    *have_hidden_threads = false;

    return OMPI_SUCCESS;
}


static const struct mca_io_base_module_1_0_0_t *
file_query(struct ompi_file_t *file, 
           struct mca_io_base_file_t **private_data,
           int *priority)
{
    mca_io_romio_data_t *data;

    /* Allocate a space for this module to hang private data (e.g.,
       the ROMIO file handle) */

    data = malloc(sizeof(mca_io_romio_data_t));
    if (NULL == data) {
        return NULL;
    }
    data->romio_fh = NULL;
    *private_data = (struct mca_io_base_file_t*) data;

    /* The priority level of 20 is pretty arbitrary, since this
       component is likely to be the only one in io v1.x */

    *priority = 20;
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
    *usable = true;
    *priority = 10;
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
