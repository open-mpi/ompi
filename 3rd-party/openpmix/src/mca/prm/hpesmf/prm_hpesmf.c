/*
 * Copyright (c) 2018-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2018      Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
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
#include "src/mca/preg/preg.h"
#include "src/util/pmix_alfg.h"
#include "src/util/pmix_argv.h"
#include "src/util/pmix_error.h"
#include "src/util/pmix_output.h"
#include "src/util/pmix_parse_options.h"
#include "src/util/pmix_if.h"
#include "src/util/pmix_environ.h"

#include "prm_hpesmf.h"
#include "src/mca/prm/base/base.h"

static pmix_status_t hpesmf_notify(pmix_status_t status, const pmix_proc_t *source,
                                   pmix_data_range_t range, const pmix_info_t info[], size_t ninfo,
                                   pmix_op_cbfunc_t cbfunc, void *cbdata);

pmix_prm_module_t pmix_prm_hpesmf_module = {
    .name = "hpesmf",
    .notify = hpesmf_notify
};

static pmix_status_t hpesmf_notify(pmix_status_t status, const pmix_proc_t *source,
                                   pmix_data_range_t range, const pmix_info_t info[], size_t ninfo,
                                   pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    PMIX_HIDE_UNUSED_PARAMS(status, source, range, info, ninfo, cbfunc, cbdata);
    return PMIX_ERR_NOT_SUPPORTED;
}
