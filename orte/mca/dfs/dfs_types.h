/*
 * Copyright (c) 2012      Los Alamos National Security, LLC.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef ORTE_MCA_DFS_TYPES_H
#define ORTE_MCA_DFS_TYPES_H

#include "orte_config.h"

BEGIN_C_DECLS

typedef uint8_t orte_dfs_cmd_t;
#define ORTE_DFS_CMD_T OPAL_UINT8

#define ORTE_DFS_OPEN_CMD   1
#define ORTE_DFS_CLOSE_CMD  2
#define ORTE_DFS_SEEK_CMD   3
#define ORTE_DFS_READ_CMD   4

typedef void (*orte_dfs_open_callback_fn_t)(int fd, void *cbdata);

typedef void (*orte_dfs_read_callback_fn_t)(long status,
                                            uint8_t *buffer,
                                            void *cbdata);

END_C_DECLS

#endif
