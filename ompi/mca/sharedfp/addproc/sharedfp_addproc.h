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
 * Copyright (c) 2015      University of Houston. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_SHAREDFP_ADDPROC_H
#define MCA_SHAREDFP_ADDPROC_H

#include "ompi_config.h"
#include "ompi/mca/mca.h"
#include "ompi/mca/sharedfp/sharedfp.h"
#include "ompi/mca/io/ompio/io_ompio.h"
#include <semaphore.h>

BEGIN_C_DECLS

int mca_sharedfp_addproc_component_init_query(bool enable_progress_threads,
                                                 bool enable_mpi_threads);
struct mca_sharedfp_base_module_1_0_0_t *
        mca_sharedfp_addproc_component_file_query (mca_io_ompio_file_t *file, int *priority);
int mca_sharedfp_addproc_component_file_unquery (mca_io_ompio_file_t *file);

int mca_sharedfp_addproc_module_init (mca_io_ompio_file_t *file);
int mca_sharedfp_addproc_module_finalize (mca_io_ompio_file_t *file);

extern int mca_sharedfp_addproc_priority;
extern int mca_sharedfp_addproc_verbose;
#if 0
extern char[MPI_MAX_HOSTNAME_LEN] mca_sharedfp_addproc_control_host;
#endif

OMPI_MODULE_DECLSPEC extern mca_sharedfp_base_component_2_0_0_t mca_sharedfp_addproc_component;
/*
 * ******************************************************************
 * ********* functions which are implemented in this module *********
 * ******************************************************************
 */
/*IMPORANT: Update here when implementing functions from sharedfp API*/

int mca_sharedfp_addproc_seek (mca_io_ompio_file_t *fh,
                                  OMPI_MPI_OFFSET_TYPE offset, int whence);
int mca_sharedfp_addproc_get_position (mca_io_ompio_file_t *fh,
                                          OMPI_MPI_OFFSET_TYPE * offset);
int mca_sharedfp_addproc_file_open (struct ompi_communicator_t *comm,
                                       const char* filename,
                                       int amode,
                                       struct ompi_info_t *info,
                                       mca_io_ompio_file_t *fh);
int mca_sharedfp_addproc_file_close (mca_io_ompio_file_t *fh);
int mca_sharedfp_addproc_read (mca_io_ompio_file_t *fh,
                                  void *buf, int count, MPI_Datatype datatype, MPI_Status *status);
int mca_sharedfp_addproc_read_ordered (mca_io_ompio_file_t *fh,
                                          void *buf, int count, struct ompi_datatype_t *datatype,
                                          ompi_status_public_t *status
                                          );
int mca_sharedfp_addproc_read_ordered_begin (mca_io_ompio_file_t *fh,
                                                 void *buf,
                                                 int count,
                                                 struct ompi_datatype_t *datatype);
int mca_sharedfp_addproc_read_ordered_end (mca_io_ompio_file_t *fh,
                                               void *buf,
                                               ompi_status_public_t *status);
int mca_sharedfp_addproc_iread (mca_io_ompio_file_t *fh,
                                    void *buf,
                                    int count,
                                    struct ompi_datatype_t *datatype,
                                    ompi_request_t **request);
int mca_sharedfp_addproc_write (mca_io_ompio_file_t *fh,
                                   const void *buf,
                                   int count,
                                   struct ompi_datatype_t *datatype,
                                   ompi_status_public_t *status);
int mca_sharedfp_addproc_write_ordered (mca_io_ompio_file_t *fh,
                                           const void *buf,
                                           int count,
                                           struct ompi_datatype_t *datatype,
                                           ompi_status_public_t *status);
int mca_sharedfp_addproc_write_ordered_begin (mca_io_ompio_file_t *fh,
                                                 const void *buf,
                                                 int count,
                                                 struct ompi_datatype_t *datatype);
int mca_sharedfp_addproc_write_ordered_end (mca_io_ompio_file_t *fh,
                                               const void *buf,
                                               ompi_status_public_t *status);
int mca_sharedfp_addproc_iwrite (mca_io_ompio_file_t *fh,
                                    const void *buf,
                                    int count,
                                    struct ompi_datatype_t *datatype,
                                    ompi_request_t **request);
/****************************************************/
/*The following are structures and definitions      *
 * copied over directly from uhio codebase          */
/****************************************************/

/*This structure will hang off of the mca_sharedfp_base_data_t's
 *selected_module_data attribute
 */
struct mca_sharedfp_addproc_data
{
    MPI_Comm   intercom;
};

typedef struct mca_sharedfp_addproc_data addproc_data;


int mca_sharedfp_addproc_request_position (struct mca_sharedfp_base_data_t * sh,
                                              int bytes_requested,
                                              OMPI_MPI_OFFSET_TYPE * offset);

#define DO_ACK 0         /* To be set by the Environment Variable*/
#define REQUEST_TAG 99
#define ACK_TAG 1
#define OFFSET_TAG 98
#define END_TAG 97

#define SEEK_END_TAG 91
#define SEEK_SET_TAG 92
#define SEEK_CUR_TAG 93
#define GET_POSITION_TAG 94

#define NUM_OF_SPAWNS 1

struct list {

        int procNo;
        long numBytesArrAddr;
        struct list *Next;
};

struct Stat {
        int tag;
        int source;
        long* recvBuff;
};


double uhio_shared_gettime(void);

typedef struct list node;
typedef struct Stat statusStruct;

/*
 * ******************************************************************
 * ************ functions implemented in this module end ************
 * ******************************************************************
 */

END_C_DECLS

#endif /* MCA_SHAREDFP_ADDPROC_H */
