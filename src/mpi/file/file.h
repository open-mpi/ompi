/*
 * $HEADER$
 */

#ifndef LAM_FILE_H
#define LAM_FILE_H

#include "mpi.h"
#include "mca/mpi/io/io.h"

struct lam_file_t {
   char f_name[MPI_MAX_OBJECT_NAME];

    /* Hooks for io modules to hang things */

    mca_io_1_0_0_t f_io;
    struct mca_io_file_t* f_io_file;

};
typedef struct lam_file_t lam_file_t;

#endif /* LAM_FILE_H */
