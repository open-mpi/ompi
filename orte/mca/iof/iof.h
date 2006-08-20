/*
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
 */
/**
 * @file
 *
 * I/O Forwarding Service
 */
                                                                                         
#ifndef ORTE_IOF_H
#define ORTE_IOF_H

#include "orte_config.h"
#include "opal/class/opal_list.h"
#include "opal/mca/mca.h"
#include "orte/mca/ns/ns.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/* suggested tag values */
enum {
    ORTE_IOF_ANY = -1,
    ORTE_IOF_STDIN = 0,
    ORTE_IOF_STDOUT = 1,
    ORTE_IOF_STDERR = 2
};
typedef int orte_iof_base_tag_t;

/* endpoint mode */
enum {
    ORTE_IOF_SOURCE = 0,
    ORTE_IOF_SINK
};
typedef int orte_iof_base_mode_t;


/**
 * Publish a local file descriptor as an endpoint that is logically
 * associated with the specified process name (e.g. file descriptor
 * corresponding to the master side of a pipe/pty connected to a 
 * child process)
 *
 * @param name  Process name associated with the endpoint.
 * @param mode  Is the endpoint an input or output.
 * @param tag   The logical tag associated with this file descriptor.
 * @param fd    Local file descriptor
 *
 */

typedef int (*orte_iof_base_publish_fn_t)(
    const orte_process_name_t* name,
    orte_iof_base_mode_t mode,
    orte_iof_base_tag_t tag,
    int fd
);

/**
 * Remove all endpoints matching the specified process
 * name, mask and tag values.
 *
 * @param name  Process name associated with the endpoint.
 * @param mask  A mask indicating the set of processes to unpublish.
 * @param tag   The endpoint tag.
 *
 */

typedef int (*orte_iof_base_unpublish_fn_t)(
    const orte_process_name_t* name,
    orte_ns_cmp_bitmask_t mask,
    orte_iof_base_tag_t tag
);

/**
 * Explicitly push data from the specified input file 
 * descriptor to the indicated set of peers.
 * 
 * @param dst_name  Name used to qualify set of peers.
 * @param dst_mask  Mask that specified how name is interpreted.
 * @param dst_tag   Match a specific peer endpoint.
 * @param fd        Local file descriptor for input.
 */

typedef int (*orte_iof_base_push_fn_t)(
    const orte_process_name_t* dst_name,
    orte_ns_cmp_bitmask_t dst_mask,
    orte_iof_base_tag_t dst_tag,
    int fd
);

/**
 * Explicitly pull data from the specified set of peers
 * and dump to the indicated output file descriptor.
 * 
 * @param dst_name  Name used to qualify set of peers.
 * @param dst_mask  Mask that specified how name is interpreted.
 * @param dst_tag   Match a specific peer endpoint.
 * @param fd        Local file descriptor for output.
 */

typedef int (*orte_iof_base_pull_fn_t)(
    const orte_process_name_t* src_name,
    orte_ns_cmp_bitmask_t src_mask,
    orte_iof_base_tag_t src_tag,
    int fd
);

/**
 * Setup buffering for a specified set of endpoints.
 */

typedef int (*orte_iof_base_buffer_fn_t)(
    const orte_process_name_t* src_name,
    orte_ns_cmp_bitmask_t src_mask,
    orte_iof_base_tag_t src_tag,
    size_t buffer_size
);

/*
 * Subscribe to receive a callback on receipt of data
 * from a specified set of peers.
 */

typedef void (*orte_iof_base_callback_fn_t)(
    orte_process_name_t* src_name, 
    orte_iof_base_tag_t src_tag,
    void *cbdata,
    const unsigned char* data,
    size_t count
);

typedef int (*orte_iof_base_subscribe_fn_t)(
    const orte_process_name_t* src_name,  
    orte_ns_cmp_bitmask_t src_mask,
    orte_iof_base_tag_t src_tag,
    orte_iof_base_callback_fn_t cb,
    void* cbdata
);

typedef int (*orte_iof_base_unsubscribe_fn_t)(
    const orte_process_name_t* src_name,
    orte_ns_cmp_bitmask_t src_mask,
    orte_iof_base_tag_t src_tag
);


/*
 * Flush all output and block until output is delivered.
 */

typedef int (*orte_iof_base_flush_fn_t)(void);

typedef int (*orte_iof_base_finalize_fn_t)(void);

/**
 *  IOF module.
 */

struct orte_iof_base_module_1_0_0_t {
    orte_iof_base_publish_fn_t iof_publish;
    orte_iof_base_unpublish_fn_t iof_unpublish;
    orte_iof_base_push_fn_t iof_push;
    orte_iof_base_pull_fn_t iof_pull;
    orte_iof_base_subscribe_fn_t iof_subscribe;
    orte_iof_base_unsubscribe_fn_t iof_unsubscribe;
    orte_iof_base_flush_fn_t iof_flush;
    orte_iof_base_finalize_fn_t iof_finalize;
};

typedef struct orte_iof_base_module_1_0_0_t orte_iof_base_module_1_0_0_t;
typedef orte_iof_base_module_1_0_0_t orte_iof_base_module_t;
ORTE_DECLSPEC extern orte_iof_base_module_t orte_iof;

/**
 *  IOF component descriptor. Contains component version information
 *  and component open/close/init functions.
 */
                                                                                                                 
typedef orte_iof_base_module_t* (*orte_iof_base_component_init_fn_t)(
    int *priority,
    bool *allow_user_threads,
    bool *have_hidden_threads
);

struct orte_iof_base_component_1_0_0_t {
  mca_base_component_t iof_version;
  mca_base_component_data_1_0_0_t iof_data;
  orte_iof_base_component_init_fn_t iof_init;
};
typedef struct orte_iof_base_component_1_0_0_t orte_iof_base_component_1_0_0_t;
typedef struct orte_iof_base_component_1_0_0_t orte_iof_base_component_t;
                                                                                                                 
/*
 * Macro for use in components that are of type iof v1.0.0
 */
#define ORTE_IOF_BASE_VERSION_1_0_0 \
  /* iof v1.0 is chained to MCA v1.0 */ \
  MCA_BASE_VERSION_1_0_0, \
  /* iof v1.0 */ \
  "iof", 1, 0, 0

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif /* ORTE_IOF_H */
