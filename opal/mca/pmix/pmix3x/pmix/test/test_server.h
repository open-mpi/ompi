/*
 * Copyright (c) 2018      Mellanox Technologies, Inc.
 *                         All rights reserved.
 *
 * Copyright (c) 2018      Intel, Inc.  All rights reserved.
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
} msg_hdr_t;

struct server_info_t
{
    pmix_list_item_t super;
    pid_t pid;
    int idx;
    int rd_fd;
    int wr_fd;
    pmix_event_t *evread;
    pmix_lock_t lock;
    pmix_modex_cbfunc_t modex_cbfunc;
    void *cbdata;
};
typedef struct server_info_t server_info_t;
PMIX_EXPORT PMIX_CLASS_DECLARATION(server_info_t);

struct server_nspace_t
{
    pmix_list_item_t super;
    char name[PMIX_MAX_NSLEN+1];
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

int server_init(test_params *params);
int server_finalize(test_params *params);
int server_barrier(void);
int server_fence_contrib(char *data, size_t ndata,
                         pmix_modex_cbfunc_t cbfunc, void *cbdata);
int server_dmdx_get(const char *nspace, int rank,
                    pmix_modex_cbfunc_t cbfunc, void *cbdata);
int server_launch_clients(int local_size, int univ_size, int base_rank,
                   test_params *params, char *** client_env, char ***base_argv);


#endif // TEST_SERVER_C

