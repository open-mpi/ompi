/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2007 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008-2016 University of Houston. All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_COMMON_OMPIO_PRINT_QUEUE_H
#define MCA_COMMON_OMPIO_PRINT_QUEUE_H


#include "mpi.h"
#include "ompi/mca/io/ompio/io_ompio.h"

OMPI_DECLSPEC extern int mca_io_ompio_coll_timing_info;

#define COMMON_OMPIO_QUEUESIZE 2048

/* PRINT QUEUES*/
#define WRITE_PRINT_QUEUE 1809
#define READ_PRINT_QUEUE 2178
/*---------------------------*/

/*To extract time-information */
typedef struct {
    double time[3];
    int nprocs_for_coll;
    int aggregator;
}mca_common_ompio_print_entry;

typedef struct {
    mca_common_ompio_print_entry entry[COMMON_OMPIO_QUEUESIZE + 1];
    int first;
    int last;
    int count;
} mca_common_ompio_print_queue;


OMPI_DECLSPEC extern mca_common_ompio_print_queue *coll_write_time;
OMPI_DECLSPEC extern mca_common_ompio_print_queue *coll_read_time;


OMPI_DECLSPEC int common_ompio_register_print_entry (int queue_type,
                                                   mca_common_ompio_print_entry x);

OMPI_DECLSPEC int common_ompio_unregister_print_entry (int queue_type, 
                                                     mca_common_ompio_print_entry *x);

OMPI_DECLSPEC int common_ompio_empty_print_queue(int queue_type);

OMPI_DECLSPEC int common_ompio_full_print_queue(int queue_type);

OMPI_DECLSPEC int common_ompio_initialize_print_queue(mca_common_ompio_print_queue *q);

OMPI_DECLSPEC int common_ompio_print_time_info(int queue_type,
                                             char *name_operation,
                                             mca_io_ompio_file_t *fh);
int common_ompio_set_print_queue (mca_common_ompio_print_queue **q,
                                int queue_type);


END_C_DECLS

#endif /* MCA_COMMON_OMPIO_PRINT_QUEUE_H */
