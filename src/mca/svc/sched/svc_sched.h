/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/**
 * @file
 */
#ifndef _MCA_SVC_SCHED_
#define _MCA_SVC_SCHED_

#include "mca/svc/svc.h"
#include "class/ompi_list.h"
#include "class/ompi_rb_tree.h"

#define OMPI_NAME_ARGS(n)  (n).cellid,(n).jobid,(n).vpid

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
/**
 * Component open/close/init
 */
int mca_svc_sched_component_open(void);
int mca_svc_sched_component_close(void);
mca_svc_base_module_t* mca_svc_sched_component_init(void);

/**
 * Module init/fini
 */
int mca_svc_sched_module_init(mca_svc_base_module_t*);
int mca_svc_sched_module_fini(mca_svc_base_module_t*);

struct mca_svc_sched_component_t {
    mca_svc_base_component_t sched_base;
    int sched_debug;                               /* debug flag */
    ompi_list_t sched_node_list;                   /* list of nodes for round-robin scheduling */
    ompi_rb_tree_t sched_node_tree;                /* tree of nodes for quick lookup by name */
    struct mca_svc_sched_node_t* sched_node_next;  /* iterator for round-robin scheduling */
    ompi_mutex_t sched_lock;                       /* lock to protected global variables */
};
typedef struct mca_svc_sched_component_t mca_svc_sched_component_t;

extern mca_svc_base_module_t mca_svc_sched_module;
extern mca_svc_sched_component_t mca_svc_sched_component;


#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif

