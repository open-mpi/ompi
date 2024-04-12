/*
 * Copyright (c) 2015-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2016      IBM Corporation.  All rights reserved.
 *
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "src/include/pmix_config.h"

#include <string.h>
#ifdef HAVE_UNISTD_H
#    include <unistd.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#    include <sys/types.h>
#endif
#ifdef HAVE_SYS_STAT_H
#    include <sys/stat.h>
#endif
#ifdef HAVE_FCNTL_H
#    include <fcntl.h>
#endif
#include <time.h>

#include "pmix_common.h"

#include "src/class/pmix_list.h"
#include "src/include/pmix_globals.h"
#include "src/include/pmix_socket_errno.h"
#include "src/mca/base/pmix_mca_base_var.h"
#include "src/mca/preg/preg.h"
#include "src/util/pmix_alfg.h"
#include "src/util/pmix_argv.h"
#include "src/util/pmix_error.h"
#include "src/util/pmix_output.h"
#include "src/util/pmix_environ.h"
#include "src/util/pmix_printf.h"

#include "pstrg_vfs.h"
#include "src/mca/pstrg/base/base.h"
#include "src/mca/pstrg/pstrg.h"

static pmix_status_t vfs_init(void);
static void vfs_finalize(void);
static pmix_status_t query(pmix_query_t queries[], size_t nqueries, pmix_list_t *results,
                           pmix_pstrg_query_cbfunc_t cbfunc, void *cbdata);

pmix_pstrg_base_module_t pmix_pstrg_vfs_module = {.name = "vfs",
                                                  .init = vfs_init,
                                                  .finalize = vfs_finalize,
                                                  .query = query};

#if 0

typedef struct {
    char *name;
    char *mountpt;
    uint64_t cap;
    uint64_t free;
    uint64_t avail;
    uint64_t bw;
    uint64_t availbw;
} vfs_storage_t;

static vfs_storage_t availsys[] = {
    {.name = "vfs1", .mountpt = "/scratch1", .cap = 123456, .free = 5678,
     .avail = 131, .bw = 100.0, .availbw = 13.2},

    {.name = "vfs2", .mountpt = "/scratch2", .cap = 789, .free = 178,
     .avail = 100, .bw = 10.0, .availbw = 2.2},

    {.name = "vfs3", .mountpt = "/usr/tmp", .cap = 91, .free = 35,
     .avail = 81, .bw = 5.0, .availbw = 42},
    {.name = NULL}
};
#endif

static pmix_status_t vfs_init(void)
{
    pmix_output_verbose(2, pmix_pstrg_base_framework.framework_output, "pstrg: vfs init");

    /* ADD HERE:
     *
     * Discover/connect to any available Vfs systems. Return an error
     * if none are preset, or you are unable to connect to them
     */
    return PMIX_SUCCESS;
}

static void vfs_finalize(void)
{
    pmix_output_verbose(2, pmix_pstrg_base_framework.framework_output, "pstrg: vfs finalize");

    /* ADD HERE:
     *
     * Disconnect from any Vfs systems to which you have connected
     */
}

static pmix_status_t query(pmix_query_t queries[], size_t nqueries, pmix_list_t *results,
                           pmix_pstrg_query_cbfunc_t cbfunc, void *cbdata)
{
    PMIX_HIDE_UNUSED_PARAMS(queries, nqueries, results, cbfunc, cbdata) ;
    
#if 0
    size_t n, m, k;
    char **sid, **mountpt;
    bool takeus;
    uid_t uid = UINT32_MAX;
    gid_t gid = UINT32_MAX;

    pmix_output_verbose(2, pmix_pstrg_base_framework.framework_output,
                        "pstrg: vfs query");

    /* just put something here so that Travis will pass its tests
     * because it treats warnings as errors, and wants to warn about
     * unused variables */
    sid = PMIx_Argv_split("foo,bar", ',');
    PMIx_Argv_free(sid);
    sid = NULL;
    mountpt = PMIx_Argv_split("foo,bar", ',');
    PMIx_Argv_free(mountpt);
    mountpt = NULL;
    if (availsys[0].cap != 123456) {
        return PMIX_ERROR;
    }

#endif

    return PMIX_ERR_NOT_FOUND;
}
