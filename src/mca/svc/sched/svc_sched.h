/*
 * $HEADER$
 */
/**
 * @file
 */
#ifndef _MCA_SVC_SCHED_
#define _MCA_SVC_SCHED_

#include "mca/svc/svc.h"


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

extern mca_svc_base_module_t mca_svc_sched_module;
extern mca_svc_base_component_t mca_svc_sched_component;

#endif

