/*
 * Copyright (c) 2018      DataDirect Networks. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_FBTL_IME_H
#define MCA_FBTL_IME_H

#include "ime_native.h"

#include "ompi_config.h"
#include "ompi/mca/mca.h"
#include "ompi/mca/fbtl/fbtl.h"
#include "ompi/mca/common/ompio/common_ompio.h"
#include "ompi/mca/common/ompio/common_ompio_request.h"

extern int mca_fbtl_ime_priority;
extern int mca_fbtl_ime_iov_max;
extern int mca_fbtl_ime_aio_reqs_max;

#define FBTL_IME_BASE_PRIORITY      0
#define FBTL_IME_INCREASED_PRIORITY 50
#define FBTL_IME_AIO_REQS_MAX       128

#ifdef IME_IOV_MAX
#define FBTL_IME_IOV_MAX IME_IOV_MAX
#else
#define FBTL_IME_IOV_MAX 1024
#endif


BEGIN_C_DECLS

int mca_fbtl_ime_component_init_query(bool enable_progress_threads,
                                        bool enable_mpi_threads);
struct mca_fbtl_base_module_1_0_0_t *
mca_fbtl_ime_component_file_query (ompio_file_t *file, int *priority);
int mca_fbtl_ime_component_file_unquery (ompio_file_t *file);

int mca_fbtl_ime_module_init (ompio_file_t *file);
int mca_fbtl_ime_module_finalize (ompio_file_t *file);

OMPI_MODULE_DECLSPEC extern mca_fbtl_base_component_2_0_0_t mca_fbtl_ime_component;
/*
 * ******************************************************************
 * ********* functions which are implemented in this module *********
 * ******************************************************************
 */

ssize_t mca_fbtl_ime_preadv (ompio_file_t *file );
ssize_t mca_fbtl_ime_pwritev (ompio_file_t *file );
ssize_t mca_fbtl_ime_ipreadv (ompio_file_t *file,
                               ompi_request_t *request);
ssize_t mca_fbtl_ime_ipwritev (ompio_file_t *file,
                                ompi_request_t *request);

bool mca_fbtl_ime_progress     (mca_ompio_request_t *req);
void mca_fbtl_ime_request_free (mca_ompio_request_t *req);
void mca_fbtl_ime_complete_cb  (struct ime_aiocb *aiocb, int err, ssize_t bytes);

struct mca_fbtl_ime_request_data_t {
    int            aio_req_count;       /* total number of aio reqs */
    int            aio_open_reqs;       /* number of unfinished reqs */
    int            aio_req_type;        /* read or write */
    int            aio_req_chunks;      /* max. no. of aio reqs that can be posted at once*/
    int            aio_first_active_req; /* first active posted req */
    int            aio_last_active_req;  /* last currently active poted req */
    int            aio_req_fail_count;   /* number of requests that failed*/
    struct iovec      *aio_iovecs;       /* array of iovecs copied from the file handle */
    struct ime_aiocb  *aio_reqs;         /* array of aio requests that will be sent to IME */
    ssize_t           *aio_req_status;  /* array of status for the IME requests */
    ssize_t        aio_total_len;       /* total amount of data written */
    ompio_file_t  *aio_fh;       /* pointer back to the mca_io_ompio_fh structure */
    void          *allocated_data; /* pointer to the allocated space
                            that will contain all the necessary iovecs,
                            IME requests and their statuses */
};
typedef struct mca_fbtl_ime_request_data_t mca_fbtl_ime_request_data_t;

/* define constants for read/write operations */
#define FBTL_IME_READ 1
#define FBTL_IME_WRITE 2
#define FBTL_IME_IN_PROGRESS -1
#define FBTL_IME_REQ_ERROR   -2
#define FBTL_IME_REQ_CLOSED  -3

/*
 * ******************************************************************
 * ************ functions implemented in this module end ************
 * ******************************************************************
 */

END_C_DECLS

#endif /* MCA_FBTL_IME_H */
