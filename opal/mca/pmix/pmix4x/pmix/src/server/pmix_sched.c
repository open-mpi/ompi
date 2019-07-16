/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014-2019 Intel, Inc.  All rights reserved.
 * Copyright (c) 2014-2015 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2014-2015 Artem Y. Polyakov <artpol84@gmail.com>.
 *                         All rights reserved.
 * Copyright (c) 2016      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2016      IBM Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include <src/include/pmix_config.h>
#include <pmix_sched.h>

#include <src/include/types.h>
#include <src/include/pmix_stdint.h>
#include <src/include/pmix_socket_errno.h>

#include <pmix_server.h>
#include <pmix_rename.h>
#include "src/include/pmix_globals.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif
#include <fcntl.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SYS_SOCKET_H
#include <sys/socket.h>
#endif
#ifdef HAVE_SYS_UN_H
#include <sys/un.h>
#endif
#ifdef HAVE_SYS_UIO_H
#include <sys/uio.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#include PMIX_EVENT_HEADER

#include "src/class/pmix_list.h"
#include "src/mca/pnet/base/base.h"
#include "src/util/argv.h"
#include "src/util/error.h"
#include "src/util/output.h"
#include "src/util/pmix_environ.h"

#include "pmix_server_ops.h"

PMIX_EXPORT pmix_status_t PMIx_server_register_fabric(pmix_fabric_t *fabric,
                                                      const pmix_info_t directives[],
                                                      size_t ndirs)
{
    pmix_pnet_base_active_module_t *active;
    pmix_status_t rc;

    /* ensure our fields of the fabric object are initialized */
    fabric->commcost = NULL;
    fabric->nverts = 0;
    fabric->module = NULL;

    PMIX_ACQUIRE_THREAD(&pmix_pnet_globals.lock);

    if (0 == pmix_list_get_size(&pmix_pnet_globals.actives)) {
        PMIX_RELEASE_THREAD(&pmix_pnet_globals.lock);
        return PMIX_ERR_NOT_SUPPORTED;
    }

    /* scan across active modules until one returns success */
    PMIX_LIST_FOREACH(active, &pmix_pnet_globals.actives, pmix_pnet_base_active_module_t) {
        if (NULL != active->module->register_fabric) {
            rc = active->module->register_fabric(fabric, directives, ndirs);
            if (PMIX_SUCCESS == rc || PMIX_ERR_TAKE_NEXT_OPTION != rc) {
                PMIX_RELEASE_THREAD(&pmix_pnet_globals.lock);
                return rc;
            }
        }
    }

    /* unlock prior to return */
    PMIX_RELEASE_THREAD(&pmix_pnet_globals.lock);

    return PMIX_ERR_NOT_FOUND;
}

PMIX_EXPORT pmix_status_t PMIx_server_deregister_fabric(pmix_fabric_t *fabric)
{
    pmix_status_t rc = PMIX_SUCCESS;
    pmix_pnet_fabric_t *active;
    pmix_pnet_module_t *module;

    /* it is possible that multiple threads could call this, so
     * check to see if it has already been initialized - if so,
     * then just return success */
    if (NULL == fabric || NULL == fabric->module) {
        return PMIX_SUCCESS;
    }
    active = (pmix_pnet_fabric_t*)fabric->module;
    module = (pmix_pnet_module_t*)active->module;

    if (NULL != module->deregister_fabric) {
        rc = module->deregister_fabric(fabric);
    }
    return rc;
}

PMIX_EXPORT pmix_status_t PMIx_server_get_vertex_info(pmix_fabric_t *fabric,
                                                      uint32_t i, pmix_value_t *vertex,
                                                      char **nodename)
{
    pmix_status_t ret;
    pmix_pnet_fabric_t *ft;
    pmix_pnet_module_t *module;

    if (NULL == fabric || NULL == fabric->module) {
        return PMIX_ERR_BAD_PARAM;
    }
    ft = (pmix_pnet_fabric_t*)fabric->module;
    module = (pmix_pnet_module_t*)ft->module;

    if (NULL == module->get_vertex) {
        return PMIX_ERR_NOT_SUPPORTED;
    }

    ret = module->get_vertex(fabric, i, vertex, nodename);

    return ret;
}

PMIX_EXPORT pmix_status_t PMIx_server_get_index(pmix_fabric_t *fabric,
                                                pmix_value_t *vertex, uint32_t *i)
{
    pmix_status_t ret;
    pmix_pnet_fabric_t *ft;
    pmix_pnet_module_t *module;

    if (NULL == fabric || NULL == fabric->module) {
        return PMIX_ERR_BAD_PARAM;
    }
    ft = (pmix_pnet_fabric_t*)fabric->module;
    module = (pmix_pnet_module_t*)ft->module;

    if (NULL == module->get_index) {
        return PMIX_ERR_NOT_SUPPORTED;
    }

    ret = module->get_index(fabric, vertex, i);

    return ret;
}
