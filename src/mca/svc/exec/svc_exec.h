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
#ifndef _MCA_SVC_EXEC_
#define _MCA_SVC_EXEC_

#include "mca/svc/svc.h"

/**
 * Command definitions
 */
#define OMPID_EXEC_CMD  1
#define OMPID_KILL_CMD  2
#define OMPID_PING_CMD  3
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif


/**
 * Component open/close/init
 */
int mca_svc_exec_component_open(void);
int mca_svc_exec_component_close(void);
mca_svc_base_module_t* mca_svc_exec_component_init(void);

/**
 * Module init/fini
 */
int mca_svc_exec_module_init(mca_svc_base_module_t*);
int mca_svc_exec_module_fini(mca_svc_base_module_t*);

struct mca_svc_exec_component_t {
    mca_svc_base_component_t exec_base;
    int exec_debug;
};
typedef struct mca_svc_exec_component_t mca_svc_exec_component_t;                                                                                                                                               

extern mca_svc_base_module_t mca_svc_exec_module;
extern mca_svc_exec_component_t mca_svc_exec_component;

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif

