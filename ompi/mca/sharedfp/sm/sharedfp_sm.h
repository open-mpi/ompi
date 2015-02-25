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
 * Copyright (c) 2008-2015 University of Houston. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_SHAREDFP_sm_H
#define MCA_SHAREDFP_sm_H

#include "ompi_config.h"
#include "opal/mca/mca.h"
#include "ompi/mca/sharedfp/sharedfp.h"
#include "ompi/mca/io/ompio/io_ompio.h"
#include <semaphore.h>

BEGIN_C_DECLS

int mca_sharedfp_sm_component_init_query(bool enable_progress_threads,
                                                 bool enable_mpi_threads);
struct mca_sharedfp_base_module_1_0_0_t *
        mca_sharedfp_sm_component_file_query (mca_io_ompio_file_t *file, int *priority);
int mca_sharedfp_sm_component_file_unquery (mca_io_ompio_file_t *file);

int mca_sharedfp_sm_module_init (mca_io_ompio_file_t *file);
int mca_sharedfp_sm_module_finalize (mca_io_ompio_file_t *file);

extern int mca_sharedfp_sm_priority;
extern int mca_sharedfp_sm_verbose;

OMPI_MODULE_DECLSPEC extern mca_sharedfp_base_component_2_0_0_t mca_sharedfp_sm_component;
/*
 * ******************************************************************
 * ********* functions which are implemented in this module *********
 * ******************************************************************
 */
/*IMPORANT: Update here when implementing functions from sharedfp API*/
int mca_sharedfp_sm_seek (mca_io_ompio_file_t *fh, 
                                  OMPI_MPI_OFFSET_TYPE offset, int whence);
int mca_sharedfp_sm_get_position (mca_io_ompio_file_t *fh,
                                          OMPI_MPI_OFFSET_TYPE * offset);
int mca_sharedfp_sm_file_open (struct ompi_communicator_t *comm,
                                       char* filename,
                                       int amode,
                                       struct ompi_info_t *info,
                                       mca_io_ompio_file_t *fh);
int mca_sharedfp_sm_file_close (mca_io_ompio_file_t *fh);
int mca_sharedfp_sm_read (mca_io_ompio_file_t *fh,
                                  void *buf, int count, MPI_Datatype datatype, MPI_Status *status);
int mca_sharedfp_sm_read_ordered (mca_io_ompio_file_t *fh,
                                          void *buf, int count, struct ompi_datatype_t *datatype,
                                          ompi_status_public_t *status
                                          );
int mca_sharedfp_sm_read_ordered_begin (mca_io_ompio_file_t *fh,
                                                 void *buf,
                                                 int count,
                                                 struct ompi_datatype_t *datatype);
int mca_sharedfp_sm_read_ordered_end (mca_io_ompio_file_t *fh,
                                               void *buf,
                                               ompi_status_public_t *status);
int mca_sharedfp_sm_iread (mca_io_ompio_file_t *fh,
                                    void *buf,
                                    int count,
                                    struct ompi_datatype_t *datatype,
                                    ompi_request_t **request);
int mca_sharedfp_sm_write (mca_io_ompio_file_t *fh,
                                   void *buf,
                                   int count,
                                   struct ompi_datatype_t *datatype,
                                   ompi_status_public_t *status);
int mca_sharedfp_sm_write_ordered (mca_io_ompio_file_t *fh,
                                           void *buf,
                                           int count,
                                           struct ompi_datatype_t *datatype,
                                           ompi_status_public_t *status);
int mca_sharedfp_sm_write_ordered_begin (mca_io_ompio_file_t *fh,
                                                 void *buf,
                                                 int count,
                                                 struct ompi_datatype_t *datatype);
int mca_sharedfp_sm_write_ordered_end (mca_io_ompio_file_t *fh,
                                               void *buf,
                                               ompi_status_public_t *status);
int mca_sharedfp_sm_iwrite (mca_io_ompio_file_t *fh,
                                    void *buf,
                                    int count,
                                    struct ompi_datatype_t *datatype,
                                    ompi_request_t **request);
/*--------------------------------------------------------------*
 *Structures and definitions only for this component
 *--------------------------------------------------------------*/

struct sm_offset{
    sem_t *mutex;      /* the mutex: a Posix memory-based unnamed semaphore */
    long long offset;  /* and the shared file pointer offset */
};

/*This structure will hang off of the mca_sharedfp_base_data_t's
 *selected_module_data attribute
 */
struct mca_sharedfp_sm_data
{
    struct sm_offset * sm_offset_ptr;
    /*save filename so that we can remove the file on close*/
    char * sm_filename;
    sem_t *mutex;      /* the mutex: a Posix memory-based named semaphore */
    char *sem_name;    /* Name of the semaphore */
};

typedef struct mca_sharedfp_sm_data sm_data;


int mca_sharedfp_sm_request_position (struct mca_sharedfp_base_data_t * sh,
                                              int bytes_requested,
                                              OMPI_MPI_OFFSET_TYPE * offset);
/*
 * ******************************************************************
 * ************ functions implemented in this module end ************
 * ******************************************************************
 */

END_C_DECLS

#endif /* MCA_SHAREDFP_sm_H */
