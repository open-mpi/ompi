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
                                                                                         
#ifndef MCA_IOF_H
#define MCA_IOF_H

#include "ompi_config.h"
#include "include/ompi.h"
#include "class/ompi_list.h"
#include "mca/mca.h"
#include "mca/ns/ns.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/* suggested tag values */
enum {
    MCA_IOF_ANY = -1,
    MCA_IOF_STDIN = 0,
    MCA_IOF_STDOUT = 1,
    MCA_IOF_STDERR = 2
};
typedef int mca_iof_base_tag_t;

/* endpoint mode */
typedef enum {
    MCA_IOF_SOURCE,
    MCA_IOF_SINK
} mca_iof_base_mode_t;


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

typedef int (*mca_iof_base_publish_fn_t)(
    const ompi_process_name_t* name,
    mca_iof_base_mode_t mode,
    mca_iof_base_tag_t tag,
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

typedef int (*mca_iof_base_unpublish_fn_t)(
    const ompi_process_name_t* name,
    ompi_ns_cmp_bitmask_t mask,
    mca_iof_base_tag_t tag
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

typedef int (*mca_iof_base_push_fn_t)(
    const ompi_process_name_t* dst_name,
    ompi_ns_cmp_bitmask_t dst_mask,
    mca_iof_base_tag_t dst_tag,
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

typedef int (*mca_iof_base_pull_fn_t)(
    const ompi_process_name_t* src_name,
    ompi_ns_cmp_bitmask_t src_mask,
    mca_iof_base_tag_t src_tag,
    int fd
);

/**
 * Setup buffering for a specified set of endpoints.
 */

typedef int (*mca_iof_base_buffer_fn_t)(
    const ompi_process_name_t* src_name,
    ompi_ns_cmp_bitmask_t src_mask,
    mca_iof_base_tag_t src_tag,
    size_t buffer_size
);

/*
 * Subscribe to receive a callback on receipt of data
 * from a specified set of peers.
 */

typedef int (*mca_iof_base_callback_fn_t)(
    ompi_process_name_t* src_name, 
    mca_iof_base_tag_t src_tag,
    void *cbdata,
    const unsigned char* data,
    size_t count
);

typedef int (*mca_iof_base_subscribe_fn_t)(
    const ompi_process_name_t* src_name,  
    ompi_ns_cmp_bitmask_t src_mask,
    mca_iof_base_tag_t src_tag,
    mca_iof_base_callback_fn_t cb,
    void* cbdata
);

typedef int (*mca_iof_base_unsubscribe_fn_t)(
    const ompi_process_name_t* src_name,
    ompi_ns_cmp_bitmask_t src_mask,
    mca_iof_base_tag_t src_tag
);

/**
 *  IOF module.
 */

struct mca_iof_base_module_1_0_0_t {
    mca_iof_base_publish_fn_t iof_publish;
    mca_iof_base_unpublish_fn_t iof_unpublish;
    mca_iof_base_push_fn_t iof_push;
    mca_iof_base_pull_fn_t iof_pull;
    mca_iof_base_subscribe_fn_t iof_subscribe;
    mca_iof_base_unsubscribe_fn_t iof_unsubscribe;
};

typedef struct mca_iof_base_module_1_0_0_t mca_iof_base_module_1_0_0_t;
typedef mca_iof_base_module_1_0_0_t mca_iof_base_module_t;
OMPI_DECLSPEC extern mca_iof_base_module_t mca_iof;

/**
 *  IOF component descriptor. Contains component version information
 *  and component open/close/init functions.
 */
                                                                                                                 
typedef mca_iof_base_module_t* (*mca_iof_base_component_init_fn_t)(
    int *priority,
    bool *thread_support
);

struct mca_iof_base_component_1_0_0_t {
  mca_base_component_t iof_version;
  mca_base_component_data_1_0_0_t iof_data;
  mca_iof_base_component_init_fn_t iof_init;
};
typedef struct mca_iof_base_component_1_0_0_t mca_iof_base_component_1_0_0_t;
typedef struct mca_iof_base_component_1_0_0_t mca_iof_base_component_t;
                                                                                                                 
/*
 * Macro for use in components that are of type iof v1.0.0
 */
#define MCA_IOF_BASE_VERSION_1_0_0 \
  /* iof v1.0 is chained to MCA v1.0 */ \
  MCA_BASE_VERSION_1_0_0, \
  /* iof v1.0 */ \
  "iof", 1, 0, 0

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif /* MCA_PML_H */
