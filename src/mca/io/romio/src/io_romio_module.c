/*
 * $HEADER$
 */

#include "mpi.h"
#include "mca/io/io.h"
#include "io_romio.h"


mca_io_1_0_0_t *mca_io_romio_module_init (int *priority,
                                          int *min_thread,
                                          int *max_thread);

mca_io_base_module_1_0_0_t mca_io_romio_module = {
    /* First, the mca_base_module_t struct containing meta information
       about the module itself */

    {
        /* Indicate that we are a io v1.0.0 module (which also implies a
           specific MCA version) */
        MCA_IO_BASE_VERSION_1_0_0,
        "romio",                   /* MCA module name */
        1,                         /* MCA module major version */
        0,                         /* MCA module minor version */
        0,                         /* MCA module release version */
        NULL,
        NULL
    },

    /* Next the MCA v1.0.0 module meta data */

    {
        /* Whether the module is checkpointable or not */
        false
    },

    mca_io_romio_module_init    /* module init */
};


mca_io_1_0_0_t *
mca_io_romio_module_init (int *priority,
                          int *min_thread,
                          int *max_thread)
{
    *priority = 10;
    *min_thread = MPI_THREAD_SINGLE;
    *max_thread = MPI_THREAD_SERIALIZED;

    return &mca_io_romio_ops;
}
