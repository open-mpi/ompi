/*
 * $HEADER$
 */

#include "mpi.h"
#include "mca/io/io.h"
#include "io_romio.h"


mca_io_1_0_0_t *mca_io_romio_component_init (int *priority,
                                          int *min_thread,
                                          int *max_thread);

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

    mca_io_romio_component_init    /* component init */
};


mca_io_1_0_0_t *
mca_io_romio_component_init (int *priority,
                             int *min_thread,
                             int *max_thread)
{
    *priority = 10;
    *min_thread = MPI_THREAD_SINGLE;
    *max_thread = MPI_THREAD_SERIALIZED;

    return &mca_io_romio_ops;
}
