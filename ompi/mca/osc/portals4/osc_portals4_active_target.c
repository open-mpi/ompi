/*
 * Copyright (c) 2011      Sandia National Laboratories.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/mca/osc/osc.h"
#include "ompi/mca/osc/base/base.h"
#include "ompi/mca/osc/base/osc_base_obj_convert.h"

#include "osc_portals4.h"

#include "ompi/mca/mtl/portals4/mtl_portals4_endpoint.h"


int
ompi_osc_portals4_fence(int assert, struct ompi_win_t *win)
{
    ompi_osc_portals4_module_t *module =
        (ompi_osc_portals4_module_t*) win->w_osc_module;
    int comm_ret, ret;

    /* can't enter an active target epoch when in a passive target epoch */
    if (module->passive_target_access_epoch) {
        return OMPI_ERR_RMA_SYNC;
    }

    comm_ret = ompi_osc_portals4_complete_all(module);

    ret = module->comm->c_coll.coll_barrier(module->comm,
                                            module->comm->c_coll.coll_barrier_module);

    return (OMPI_SUCCESS == comm_ret) ? ret : comm_ret;
}


int
ompi_osc_portals4_start(struct ompi_group_t *group,
                        int assert,
                        struct ompi_win_t *win)
{
    ompi_osc_portals4_module_t *module =
        (ompi_osc_portals4_module_t*) win->w_osc_module;

    /* can't enter an active target epoch when in a passive target epoch */
    if (module->passive_target_access_epoch) {
        return OMPI_ERR_RMA_SYNC;
    }

    if (0 == (assert & MPI_MODE_NOCHECK)) {
        int size;

        OBJ_RETAIN(group);
        module->start_group = group;
        size = ompi_group_size(module->start_group);

        while (module->state.post_count != size) opal_progress();
    } else {
        module->start_group = NULL;
    }

    return OMPI_SUCCESS;
}


int
ompi_osc_portals4_complete(struct ompi_win_t *win)
{
    ompi_osc_portals4_module_t *module =
        (ompi_osc_portals4_module_t*) win->w_osc_module;
    int ret, i, size;
    ptl_handle_md_t md_h;
    void *base;

    ret = ompi_osc_portals4_complete_all(module);
    if (ret != OMPI_SUCCESS) return ret;

    if (NULL != module->start_group) {
        module->state.post_count = 0;
        PtlAtomicSync();

        ompi_osc_portals4_get_md(&module->one, module->md_h, &md_h, &base);

        size = ompi_group_size(module->start_group);
        for (i = 0 ; i < size ; ++i) {

            ret = PtlAtomic(md_h,
                            (ptl_size_t) ((char*) &module->one - (char*) base),
                            sizeof(module->one),
                            PTL_ACK_REQ,
                            ompi_osc_portals4_get_peer_group(module->start_group, i),
                            module->pt_idx,
                            module->match_bits | OSC_PORTALS4_MB_CONTROL,
                            offsetof(ompi_osc_portals4_node_state_t, complete_count),
                            NULL,
                            0,
                            PTL_SUM,
                            PTL_INT32_T);
            if (ret != OMPI_SUCCESS) return ret;
            OPAL_THREAD_ADD64(&module->opcount, 1);
        }

        ret = ompi_osc_portals4_complete_all(module);
        if (ret != OMPI_SUCCESS) return ret;

        OBJ_RELEASE(module->start_group);
        module->start_group = NULL;
    }

    return OMPI_SUCCESS;
}


int
ompi_osc_portals4_post(struct ompi_group_t *group,
                       int assert,
                       struct ompi_win_t *win)
{
    ompi_osc_portals4_module_t *module =
        (ompi_osc_portals4_module_t*) win->w_osc_module;
    int ret, i, size;
    ptl_handle_md_t md_h;
    void *base;

    if (0 == (assert & MPI_MODE_NOCHECK)) {
        OBJ_RETAIN(group);
        module->post_group = group;

        module->state.complete_count = 0;
        PtlAtomicSync();

        ompi_osc_portals4_get_md(&module->one, module->md_h, &md_h, &base);

        size = ompi_group_size(module->post_group);
        for (i = 0 ; i < size ; ++i) {
            ret = PtlAtomic(md_h,
                            (ptl_size_t) ((char*) &module->one - (char*) base),
                            sizeof(module->one),
                            PTL_ACK_REQ,
                            ompi_osc_portals4_get_peer_group(module->post_group, i),
                            module->pt_idx,
                            module->match_bits | OSC_PORTALS4_MB_CONTROL,
                            offsetof(ompi_osc_portals4_node_state_t, post_count),
                            NULL,
                            0,
                            PTL_SUM,
                            PTL_INT32_T);
            if (ret != OMPI_SUCCESS) return ret;
            OPAL_THREAD_ADD64(&module->opcount, 1);
        }
    } else {
        module->post_group = NULL;
    }

    return OMPI_SUCCESS;
}


int
ompi_osc_portals4_wait(struct ompi_win_t *win)
{
    ompi_osc_portals4_module_t *module =
        (ompi_osc_portals4_module_t*) win->w_osc_module;

    if (NULL != module->post_group) {
        int size = ompi_group_size(module->post_group);

        while (module->state.complete_count != size) opal_progress();

        OBJ_RELEASE(module->post_group);
        module->post_group = NULL;
    }

    return OMPI_SUCCESS;
}


int
ompi_osc_portals4_test(struct ompi_win_t *win,
                       int *flag)
{
    ompi_osc_portals4_module_t *module =
        (ompi_osc_portals4_module_t*) win->w_osc_module;

    if (NULL != module->post_group) {
        int size = ompi_group_size(module->post_group);

        if (module->state.complete_count == size) {
            OBJ_RELEASE(module->post_group);
            module->post_group = NULL;
            *flag = 1;
        }
    } else {
        *flag = 0;
    }

    return OMPI_SUCCESS;
}
