/*
 * $HEADER
 */

#ifndef MCA_IO_ROMIO_H
#define MCA_IO_ROMIO_H

#include "mpi/request/request.h"
#include "lam/threads/mutex.h"


#include "romio-1.2.5.1/include/mpio.h" 


struct mca_io_romio_request_t {
    lam_request_t  super;
    mca_io_romio_MPIO_Request romio_rq; 
};
typedef struct mca_io_romio_request_t  mca_io_romio_request_t;


//version numbers
//actions struct

struct mca_io_file_t {
    char * romio_fh;
};
typedef struct mca_io_file_t mca_io_file_t;



// global variables, instantiated in global.c
extern lam_mutex_t mca_io_romio_mutex;




#endif /* MCA_IO_ROMIO_H */
