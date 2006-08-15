/* -*- C -*-
 * 
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
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
 *
 */
#ifndef NS_PROXY_H
#define NS_PROXY_H


#include "orte_config.h"
#include "opal/types.h"
#include "orte/orte_constants.h"
#include "opal/class/opal_list.h"
#include "orte/dss/dss.h"

#include "orte/mca/ns/base/base.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

struct orte_ns_proxy_cell_info_t {
    opal_object_t super;
    orte_cellid_t cellid;
    char *site;
    char *resource;
};
typedef struct orte_ns_proxy_cell_info_t orte_ns_proxy_cell_info_t;

OBJ_CLASS_DECLARATION(orte_ns_proxy_cell_info_t);

struct orte_ns_proxy_tagitem_t {
    opal_object_t super;
    orte_rml_tag_t tag;  /**< OOB tag */
    char *name;      /**< Name associated with tag */
};
typedef struct orte_ns_proxy_tagitem_t orte_ns_proxy_tagitem_t;

OBJ_CLASS_DECLARATION(orte_ns_proxy_tagitem_t);

struct orte_ns_proxy_dti_t {
    opal_object_t super;
    orte_data_type_t id;  /**< data type id */
    char *name;      /**< Name associated with data type */
};
typedef struct orte_ns_proxy_dti_t orte_ns_proxy_dti_t;

OBJ_CLASS_DECLARATION(orte_ns_proxy_dti_t);


/*
 * Module open / close
 */
int orte_ns_proxy_open(void);
int orte_ns_proxy_close(void);


/*
 * Startup / Shutdown
 */
mca_ns_base_module_t* orte_ns_proxy_init(int *priority);
int orte_ns_proxy_module_init(void);
int orte_ns_proxy_finalize(void);

/*
 * globals used within proxy component
 */
typedef struct {
    size_t max_size, block_size;
    orte_process_name_t *my_replica;
    int debug;
    orte_cellid_t num_cells;
    orte_pointer_array_t *cells;
    orte_pointer_array_t *tags;
    orte_rml_tag_t num_tags;
    orte_pointer_array_t *dts;
    orte_data_type_t num_dts;
    opal_mutex_t mutex;
} orte_ns_proxy_globals_t;

extern orte_ns_proxy_globals_t orte_ns_proxy;

/*
 * proxy function prototypes
 */
int orte_ns_proxy_create_cellid(orte_cellid_t *cellid, char *site, char *resource);

int orte_ns_proxy_get_cell_info(orte_cellid_t cellid, char **site, char **resource);

int orte_ns_proxy_create_jobid(orte_jobid_t *jobid);

int orte_ns_proxy_reserve_range(orte_jobid_t job, orte_vpid_t range,
                                orte_vpid_t *startvpid);

int orte_ns_proxy_get_job_peers(orte_process_name_t **procs, 
                                  orte_std_cntr_t *num_procs, orte_jobid_t job);

int orte_ns_proxy_assign_rml_tag(orte_rml_tag_t *tag, char *name);

int orte_ns_proxy_define_data_type(const char *name,
                                   orte_data_type_t *type);

int orte_ns_proxy_create_my_name(void);

/*
 * Diagnostic functions
 */
int orte_ns_proxy_dump_cells(void);

int orte_ns_proxy_dump_jobs(void);

int orte_ns_proxy_dump_tags(void);

int orte_ns_proxy_dump_datatypes(void);



#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif
