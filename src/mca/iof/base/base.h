/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/**
 * @file
 *
 * I/O Forwarding Service
 */
                                                                                         
#ifndef MCA_IOF_BASE_H
#define MCA_IOF_BASE_H

#include "ompi_config.h"
#include "class/ompi_free_list.h"
#include "threads/condition.h"
#include "mca/mca.h"
#include "mca/iof/iof.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif


struct mca_iof_base_t {
   int                  iof_output;
   ompi_list_t          iof_components_opened;
   ompi_list_t          iof_endpoints;
   ompi_mutex_t         iof_lock;
   ompi_condition_t     iof_condition;
   size_t               iof_waiting;
   ompi_free_list_t     iof_fragments;
   size_t               iof_window_size;
   ompi_process_name_t* iof_service;
};
typedef struct mca_iof_base_t mca_iof_base_t;



int mca_iof_base_open(void);
int mca_iof_base_close(void);
int mca_iof_base_select(bool* allow_multi_user_threads, bool* have_hidden_threads);
int mca_iof_base_flush(void);


extern mca_iof_base_t mca_iof_base;

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif /* MCA_PML_H */
