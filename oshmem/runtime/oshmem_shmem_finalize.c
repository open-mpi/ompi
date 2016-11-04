/*
 * Copyright (c) 2013      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "oshmem_config.h"

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif
#ifdef HAVE_NETDB_H
#include <netdb.h>
#endif

#include "opal/util/output.h"
#include "opal/runtime/opal_progress.h"
#include "opal/mca/base/base.h"
#include "opal/sys/atomic.h"
#include "opal/runtime/opal.h"

#include "orte/util/show_help.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/grpcomm/grpcomm.h"
#include "orte/runtime/runtime.h"
#include "orte/runtime/orte_globals.h"

#include "opal/mca/rcache/base/base.h"
#include "opal/mca/mpool/base/base.h"
#include "opal/mca/allocator/base/base.h"
#include "ompi/runtime/mpiruntime.h"

#include "oshmem/constants.h"
#include "oshmem/runtime/runtime.h"
#include "oshmem/runtime/params.h"
#include "oshmem/mca/spml/base/base.h"
#include "oshmem/mca/scoll/base/base.h"
#include "oshmem/mca/atomic/base/base.h"
#include "oshmem/mca/memheap/base/base.h"
#include "oshmem/mca/sshmem/base/base.h"
#include "oshmem/info/info.h"
#include "oshmem/proc/proc.h"
#include "oshmem/proc/proc_group_cache.h"
#include "oshmem/op/op.h"
#include "oshmem/request/request.h"
#include "oshmem/shmem/shmem_lock.h"
#include "oshmem/runtime/oshmem_shmem_preconnect.h"

static int _shmem_finalize(void);

int oshmem_shmem_finalize(void)
{
    int ret = OSHMEM_SUCCESS;
    static int32_t finalize_has_already_started = 0;

    if (opal_atomic_cmpset_32(&finalize_has_already_started, 0, 1)
            && oshmem_shmem_initialized && !oshmem_shmem_aborted) {
        /* Should be called first because ompi_mpi_finalize makes orte and opal finalization */
        ret = _shmem_finalize();

        if ((OSHMEM_SUCCESS == ret) && ompi_mpi_initialized
                && !ompi_mpi_finalized) {
            PMPI_Comm_free(&oshmem_comm_world);
            ret = ompi_mpi_finalize();
        }

        if (OSHMEM_SUCCESS == ret) {
            oshmem_shmem_initialized = false;
        }
    }

    return ret;
}

static int _shmem_finalize(void)
{
    int ret = OSHMEM_SUCCESS;

    shmem_barrier_all();

    shmem_lock_finalize();

    /* Finalize preconnect framework */
    if (OSHMEM_SUCCESS != (ret = oshmem_shmem_preconnect_all_finalize())) {
        return ret;
    }

    /* free requests */
    if (OSHMEM_SUCCESS != (ret = oshmem_request_finalize())) {
        return ret;
    }
    /* must free cached groups before we kill collectives */
    if (OSHMEM_SUCCESS != (ret = oshmem_group_cache_list_free())) {
        return ret;
    }
    /* We need to call mca_scoll_base_group_unselect explicitly for each group
     * that are not freed by oshmem_group_cache_list_free. We can only release its collectives at this point */
    mca_scoll_base_group_unselect(oshmem_group_all);
    mca_scoll_base_group_unselect(oshmem_group_self);

    /* Close down MCA modules */

    if (OSHMEM_SUCCESS != (ret = mca_base_framework_close(&oshmem_atomic_base_framework) ) ) {
        return ret;
    }

    if (OSHMEM_SUCCESS != (ret = mca_base_framework_close(&oshmem_scoll_base_framework) ) ) {
        return ret;
    }

    if (OSHMEM_SUCCESS != (ret = mca_base_framework_close(&oshmem_memheap_base_framework) ) ) {
        return ret;
    }

    if (OSHMEM_SUCCESS != (ret = mca_base_framework_close(&oshmem_sshmem_base_framework) ) ) {
        return ret;
    }

    if (OSHMEM_SUCCESS
            != (ret =
                    MCA_SPML_CALL(del_procs(oshmem_group_all->proc_array, oshmem_group_all->proc_count)))) {
        return ret;
    }

    oshmem_shmem_barrier();

    /* free spml resource */
    if (OSHMEM_SUCCESS != (ret = mca_spml_base_finalize())) {
        return ret;
    }

    if (OSHMEM_SUCCESS != (ret = mca_base_framework_close(&oshmem_spml_base_framework) ) ) {
        return ret;
    }

    /* free op resources */
    if (OSHMEM_SUCCESS != (ret = oshmem_op_finalize())) {
        return ret;
    }

    /* free proc_group resources */
    if (OSHMEM_SUCCESS != (ret = oshmem_proc_group_finalize())) {
        return ret;
    }

    /* free proc resources */
    if (OSHMEM_SUCCESS != (ret = oshmem_proc_finalize())) {
        return ret;
    }

    /* free info resources */
    if (OSHMEM_SUCCESS != (ret = oshmem_info_finalize())) {
        return ret;
    }

    return ret;
}

