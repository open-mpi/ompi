/*
 * $HEADER$
 */
/**
 * @file
 */
#ifndef _MCA_SVC_EXEC_
#define _MCA_SVC_EXEC_

#include "mca/svc/svc.h"

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

extern mca_svc_base_module_t mca_svc_exec_module;
extern mca_svc_base_component_t mca_svc_exec_component;

#endif

