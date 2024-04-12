/*
 * Copyright (c) 2018      Mellanox Technologies, Inc.
 *                         All rights reserved.
 *
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * Copyright (c) 2021      Triad National Security, LLC
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#ifndef TEST_SERVER_C
#define TEST_SERVER_C

#include "pmix_server.h"
#include "test_common.h"

#define PMIXT_PIPE_SZ 64000
#define PMIXT_MAX_FENCES 16
#define PMIXT_MAX_PROCS 128
#define PMIXT_MAX_NODES 64
typedef enum {
    CMD_BARRIER_REQUEST,
    CMD_BARRIER_RESPONSE,
    CMD_FENCE_CONTRIB,
    CMD_FENCE_COMPLETE,
    CMD_DMDX_REQUEST,
    CMD_DMDX_RESPONSE
} server_cmd_t;

typedef struct {
    int dst_id;
    int src_id;
    int cmd;
    size_t size;
    // limit number of procs that can participate, to simplify code
    pmix_proc_t procs[PMIXT_MAX_PROCS];
    size_t nprocs;
    int fence_index;
} msg_hdr_t;

typedef struct {
    int node;
    bool contributed;
} fence_nodes_t;

typedef struct {
    pmix_proc_t procs[PMIXT_MAX_PROCS];
    size_t nprocs;
} fence_sig_t;
struct server_info_t {
    pmix_list_item_t super;
    char *hostname;
    pid_t pid;
    int idx;
    int rd_fd;
    int wr_fd;
    pmix_event_t *evread;
    pmix_lock_t lock;
    int num_fences;
    pmix_modex_cbfunc_t modex_cbfunc[PMIXT_MAX_FENCES];
    void *cbdata[PMIXT_MAX_FENCES];
    // fence-specific proc list for fence signature
    pmix_proc_t procs[PMIXT_MAX_FENCES][PMIXT_MAX_PROCS];
    size_t nprocs[PMIXT_MAX_FENCES];
};
typedef struct server_info_t server_info_t;
PMIX_EXPORT PMIX_CLASS_DECLARATION(server_info_t);

struct server_nspace_t {
    pmix_list_item_t super;
    char name[PMIX_MAX_NSLEN + 1];
    size_t ntasks; /* total number of tasks in this namespace */
    size_t ltasks; /* local */
    int *task_map;
};
typedef struct server_nspace_t server_nspace_t;
PMIX_EXPORT PMIX_CLASS_DECLARATION(server_nspace_t);

extern int my_server_id;
extern pmix_list_t *server_list;
extern server_info_t *my_server_info;
extern pmix_list_t *server_nspace;

int server_init(validation_params *v_params);
int server_finalize(validation_params *v_params, int local_fail);
int server_barrier(void);
int server_fence_contrib(const pmix_proc_t procs[], size_t nprocs, char *data, size_t ndata, pmix_modex_cbfunc_t cbfunc, void *cbdata);
int server_dmdx_get(const char *nspace, int rank, pmix_modex_cbfunc_t cbfunc, void *cbdata);
int server_launch_clients(test_params *params, validation_params *v_params, char ***client_env,
                          char ***base_argv);

#endif // TEST_SERVER_C
