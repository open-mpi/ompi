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

#include "orte_config.h"
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_UIO_H
#include <sys/uio.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "opal/class/opal_free_list.h"
#include "opal/threads/condition.h"
#include "opal/mca/mca.h"
#include "orte/mca/iof/iof.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif


struct orte_iof_base_t {
   int                  iof_output;
   opal_list_t          iof_components_opened;
   bool                 iof_flush;
   opal_list_t          iof_endpoints;
   opal_mutex_t         iof_lock;
   opal_condition_t     iof_condition;
   size_t               iof_waiting;
   opal_free_list_t     iof_fragments;
   size_t               iof_window_size;
   orte_process_name_t* iof_service;
};
typedef struct orte_iof_base_t orte_iof_base_t;



ORTE_DECLSPEC int orte_iof_base_open(void);
ORTE_DECLSPEC int orte_iof_base_close(void);
ORTE_DECLSPEC int orte_iof_base_select(void);
ORTE_DECLSPEC int orte_iof_base_flush(void);

ORTE_DECLSPEC extern orte_iof_base_t orte_iof_base;

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif /* MCA_PML_H */
