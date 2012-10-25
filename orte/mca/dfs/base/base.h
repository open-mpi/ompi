/*
 * Copyright (c) 2012      Los Alamos National Security, Inc.  All rights reserved. 
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/** @file:
 */

#ifndef ORTE_MCA_DFS_BASE_H
#define ORTE_MCA_DFS_BASE_H

/*
 * includes
 */
#include "orte_config.h"
#include "orte/types.h"
#include "orte/constants.h"

#include "opal/class/opal_list.h"
#include "opal/mca/event/event.h"

#include "opal/mca/mca.h"
#include "orte/mca/dfs/dfs.h"


BEGIN_C_DECLS

/*
 * MCA Framework functions
 */
ORTE_DECLSPEC    int orte_dfs_base_open(void);
ORTE_DECLSPEC    int orte_dfs_base_select(void);
ORTE_DECLSPEC    int orte_dfs_base_close(void);

/* define a struct to hold framework-global values */
typedef struct {
    int output;
    bool initialized;
    opal_list_t components_available;
} orte_dfs_base_t;

ORTE_DECLSPEC extern orte_dfs_base_t orte_dfs_base;

/* tracker for active files */
typedef struct {
    opal_list_item_t super;
    orte_process_name_t requestor;
    orte_process_name_t host_daemon;
    char *filename;  /* for debug purposes */
    int local_fd;
    int remote_fd;
} orte_dfs_tracker_t;
OBJ_CLASS_DECLARATION(orte_dfs_tracker_t);

/* requests */
typedef struct {
    opal_list_item_t super;
    opal_event_t ev;
    uint64_t id;
    orte_dfs_cmd_t cmd;
    char *uri;
    int local_fd;
    int remote_fd;
    uint8_t *read_buffer;
    long read_length;
    orte_dfs_open_callback_fn_t open_cbfunc;
    orte_dfs_size_callback_fn_t size_cbfunc;
    orte_dfs_read_callback_fn_t read_cbfunc;
    void *cbdata;
} orte_dfs_request_t;
OBJ_CLASS_DECLARATION(orte_dfs_request_t);

#define ORTE_DFS_POST_REQUEST(d, cb)                                    \
    do {                                                                \
        opal_event_set(orte_event_base, &((d)->ev),                     \
                   -1, OPAL_EV_WRITE, (cb), (d));                       \
        opal_event_set_priority(&((d)->ev), ORTE_MSG_PRI);              \
        opal_event_active(&((d)->ev), OPAL_EV_WRITE, 1);                \
    } while(0);

END_C_DECLS

#endif
