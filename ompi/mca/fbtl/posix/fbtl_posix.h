/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008-2021 University of Houston. All rights reserved.
 * Copyright (c) 2018      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2022      IBM Corporation. All rights reserved
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_FBTL_POSIX_H
#define MCA_FBTL_POSIX_H

#include "ompi_config.h"
#include "ompi/mca/mca.h"
#include "ompi/mca/fbtl/fbtl.h"
#include "ompi/mca/common/ompio/common_ompio.h"
#include "ompi/mca/common/ompio/common_ompio_request.h"

extern int mca_fbtl_posix_priority;
extern bool mca_fbtl_posix_read_datasieving;
extern bool mca_fbtl_posix_write_datasieving;
extern size_t mca_fbtl_posix_max_block_size;
extern size_t mca_fbtl_posix_max_gap_size;
extern size_t mca_fbtl_posix_max_tmpbuf_size;

BEGIN_C_DECLS

int mca_fbtl_posix_component_init_query(bool enable_progress_threads,
                                        bool enable_mpi_threads);
struct mca_fbtl_base_module_1_0_0_t *
mca_fbtl_posix_component_file_query (ompio_file_t *file, int *priority);
int mca_fbtl_posix_component_file_unquery (ompio_file_t *file);

int mca_fbtl_posix_module_init (ompio_file_t *file);
int mca_fbtl_posix_module_finalize (ompio_file_t *file);

extern int ompi_fbtl_posix_max_prd_active_reqs;

OMPI_DECLSPEC extern mca_fbtl_base_component_2_0_0_t mca_fbtl_posix_component;
/*
 * ******************************************************************
 * ********* functions which are implemented in this module *********
 * ******************************************************************
 */

ssize_t mca_fbtl_posix_preadv (ompio_file_t *file );
ssize_t mca_fbtl_posix_pwritev (ompio_file_t *file );
ssize_t mca_fbtl_posix_ipreadv (ompio_file_t *file,
                               ompi_request_t *request);
ssize_t mca_fbtl_posix_ipwritev (ompio_file_t *file,
                                ompi_request_t *request);

bool mca_fbtl_posix_progress     ( mca_ompio_request_t *req);
void mca_fbtl_posix_request_free ( mca_ompio_request_t *req);
bool mca_fbtl_posix_check_atomicity ( ompio_file_t *file);

int mca_fbtl_posix_lock ( struct flock *lock, ompio_file_t *fh, int op, 
                          OMPI_MPI_OFFSET_TYPE iov_offset, off_t len, int flags,
                          int *lock_counter);
void  mca_fbtl_posix_unlock ( struct flock *lock, ompio_file_t *fh, int *lock_counter );

/* Right now statically defined, will become a configure check */
#define FBTL_POSIX_HAVE_AIO 1

struct mca_fbtl_posix_request_data_t {
    int            prd_req_count;        /* total number of sub reqs */
    int            prd_open_reqs;        /* number of unfinished reqs */
    int            prd_req_type;         /* read or write */
    int            prd_req_chunks;       /* max. no. of sub reqs that can be posted at once*/
    int            prd_first_active_req; /* first active posted req */
    int            prd_last_active_req;  /* last currently active poted req */
    ssize_t        prd_total_len;        /* total amount of data written */
    struct flock   prd_lock;             /* lock used for certain file systems */
    int            prd_lock_counter;     /* to keep track of no. of lock calls */
    ompio_file_t  *prd_fh;               /* pointer to the ompio_fh structure */
    union {
#if defined (FBTL_POSIX_HAVE_AIO)
        struct {
          struct aiocb  *aio_reqs;            /* pointer array of req structures */
          int           *aio_req_status;      /* array of statuses */
      } prd_aio;
#endif
    };

};
typedef struct mca_fbtl_posix_request_data_t mca_fbtl_posix_request_data_t;


/* define constants for AIO requests */
#define FBTL_POSIX_AIO_READ   1
#define FBTL_POSIX_AIO_WRITE  2

#define OMPIO_SET_ATOMICITY_LOCK(_fh, _lock, _lock_counter, _op) {     \
     int32_t _orig_flags = _fh->f_flags;                               \
     _fh->f_flags &= ~OMPIO_LOCK_NEVER;                                \
     _fh->f_flags &= ~OMPIO_LOCK_NOT_THIS_OP;                          \
     off_t _end_offset = (off_t)_fh->f_io_array[_fh->f_num_of_io_entries-1].offset +         \
            (off_t)_fh->f_io_array[_fh->f_num_of_io_entries-1].length;                       \
     off_t _len = _end_offset - (off_t)_fh->f_io_array[0].offset;                            \
     int _ret = mca_fbtl_posix_lock ( &_lock, _fh, _op, (off_t)_fh->f_io_array[0].offset,    \
                                    _len, OMPIO_LOCK_ENTIRE_REGION, &_lock_counter);         \
     if ( _ret == -1 ) {                                                                     \
          opal_output(1, "mca_fbtl_posix: error in mca_fbtl_posix_lock():%s",                \
                      strerror(errno));                                                      \
          return OMPI_ERROR;                                                                 \
     }                                                                                       \
     _fh->f_flags = _orig_flags; }


/*
 * ******************************************************************
 * ************ functions implemented in this module end ************
 * ******************************************************************
 */

END_C_DECLS

#endif /* MCA_FBTL_POSIX_H */
