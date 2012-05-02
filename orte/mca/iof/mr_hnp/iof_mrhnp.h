/*
 * Copyright (c) 2012      Los Alamos National Security, LLC.
 *                         All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef ORTE_IOF_MRHNP_H
#define ORTE_IOF_MRHNP_H

#include "orte_config.h"

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif  /* HAVE_SYS_TYPES_H */
#ifdef HAVE_SYS_UIO_H
#include <sys/uio.h>
#endif  /* HAVE_SYS_UIO_H */
#ifdef HAVE_NET_UIO_H
#include <net/uio.h>
#endif  /* HAVE_NET_UIO_H */

#include "orte/mca/iof/iof.h"
#include "orte/mca/iof/base/base.h"


BEGIN_C_DECLS

/**
 * IOF HNP Component 
 */
typedef struct { 
    orte_iof_base_component_t super;
    opal_list_t sinks;
    opal_list_t procs;
    orte_iof_read_event_t *stdinev;
    opal_event_t stdinsig;
    char **input_files;
    opal_pointer_array_t stdin_jobs;
} orte_iof_mrhnp_component_t;

ORTE_MODULE_DECLSPEC extern orte_iof_mrhnp_component_t mca_iof_mr_hnp_component;
extern orte_iof_base_module_t orte_iof_mrhnp_module;

void orte_iof_mrhnp_recv(int status, orte_process_name_t* sender,
                       opal_buffer_t* buffer, orte_rml_tag_t tag,
                       void* cbdata);

void orte_iof_mrhnp_read_local_handler(int fd, short event, void *cbdata);
void orte_iof_mrhnp_stdin_cb(int fd, short event, void *cbdata);
bool orte_iof_mrhnp_stdin_check(int fd);

int orte_iof_hnp_send_data_to_endpoint(orte_process_name_t *host,
                                       orte_process_name_t *target,
                                       orte_iof_tag_t tag,
                                       unsigned char *data, int numbytes);

END_C_DECLS
    
#endif
