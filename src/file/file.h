/*
 * $HEADER$
 */

#ifndef LAM_FILE_H
#define LAM_FILE_H

#include "mpi.h"
#include "mca/mpi/io/io.h"

typedef enum {
    LAM_IO_1_0_0,
    LAM_IO_2_0_0
} lam_io_version_t;


struct lam_file_t {
   char f_name[MPI_MAX_OBJECT_NAME];
    lam_io_version_t lam_io_version;

    /* Hooks for io modules to hang things */
    union {
        mca_io_1_0_0_t f_io;
    } mca_io_functions;


};
typedef struct lam_file_t lam_file_t;

#endif /* LAM_FILE_H */
