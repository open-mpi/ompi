/*
 * Copyright (c) 2021-2024 Computer Architecture and VLSI Systems (CARV)
 *                         Laboratory, ICS Forth. All rights reserved.
 * Copyright (c) 2024      Jeffrey M. Squyres.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>
#include <string.h>

#include "mpi.h"

#include "ompi/mca/coll/coll.h"
#include "ompi/mca/coll/base/base.h"
#include "opal/mca/smsc/smsc.h"

#include "opal/util/arch.h"
#include "opal/util/show_help.h"
#include "opal/util/minmax.h"

#include "coll_xhc.h"

// -----------------------------

static void mca_coll_xhc_module_construct(mca_coll_xhc_module_t *module);
static void mca_coll_xhc_module_destruct(mca_coll_xhc_module_t *module);

OBJ_CLASS_INSTANCE(mca_coll_xhc_module_t, mca_coll_base_module_t,
    mca_coll_xhc_module_construct, mca_coll_xhc_module_destruct);

// -----------------------------

static size_t xhc_colltype_to_c_coll_fn_offset_map[XHC_COLLCOUNT] = {
    [XHC_BCAST] = offsetof(mca_coll_base_comm_coll_t, coll_bcast),
    [XHC_BARRIER] = offsetof(mca_coll_base_comm_coll_t, coll_barrier),
    [XHC_REDUCE] = offsetof(mca_coll_base_comm_coll_t, coll_reduce),
    [XHC_ALLREDUCE] = offsetof(mca_coll_base_comm_coll_t, coll_allreduce)
};

static size_t xhc_colltype_to_c_coll_module_offset_map[XHC_COLLCOUNT] = {
    [XHC_BCAST] = offsetof(mca_coll_base_comm_coll_t, coll_bcast_module),
    [XHC_BARRIER] = offsetof(mca_coll_base_comm_coll_t, coll_barrier_module),
    [XHC_REDUCE] = offsetof(mca_coll_base_comm_coll_t, coll_reduce_module),
    [XHC_ALLREDUCE] = offsetof(mca_coll_base_comm_coll_t, coll_allreduce_module)
};

static size_t xhc_colltype_to_base_module_fn_offset_map[XHC_COLLCOUNT] = {
    [XHC_BCAST] = offsetof(mca_coll_base_module_t, coll_bcast),
    [XHC_BARRIER] = offsetof(mca_coll_base_module_t, coll_barrier),
    [XHC_REDUCE] = offsetof(mca_coll_base_module_t, coll_reduce),
    [XHC_ALLREDUCE] = offsetof(mca_coll_base_module_t, coll_allreduce)
};

static inline void (*MODULE_COLL_FN(xhc_module_t *module,
        XHC_COLLTYPE_T colltype))(void) {

    return * (void (**)(void)) ((uintptr_t) &module->super
        + xhc_colltype_to_base_module_fn_offset_map[colltype]);
}

static inline void INSTALL_COLL_API(ompi_communicator_t *comm,
        XHC_COLLTYPE_T colltype, void (*coll_fn)(void), void *coll_module) {

    * (void (**)(void)) ((uintptr_t) comm->c_coll
        + xhc_colltype_to_c_coll_fn_offset_map[colltype]) = coll_fn;
    * (void **) ((uintptr_t) comm->c_coll
        + xhc_colltype_to_c_coll_module_offset_map[colltype]) = coll_module;
}

static inline void GET_COLL_API(ompi_communicator_t *comm, XHC_COLLTYPE_T colltype,
        void (**coll_fn_dst)(void), void **coll_module_dst) {

    *coll_fn_dst = * (void (**)(void)) ((uintptr_t) comm->c_coll
        + xhc_colltype_to_c_coll_fn_offset_map[colltype]);

    *coll_module_dst = * (void **) ((uintptr_t) comm->c_coll
        + xhc_colltype_to_c_coll_module_offset_map[colltype]);
}

// -----------------------------

static void xhc_module_clear(xhc_module_t *module) {
    module->comm_size = 0;
    module->rank = -1;

    module->zcopy_support = false;
    module->zcopy_map_support = false;

    module->rbuf = NULL;
    module->rbuf_size = 0;

    module->peer_info = NULL;

    memset(&module->prev_colls, 0, sizeof(module->prev_colls));
    memset(&module->op_config, 0, sizeof(module->op_config));
    memset(&module->op_data, 0, sizeof(module->op_data));

    module->init = false;
    module->error = false;
}

static void mca_coll_xhc_module_construct(mca_coll_xhc_module_t *module) {
    xhc_module_clear(module);
}

static void mca_coll_xhc_module_destruct(mca_coll_xhc_module_t *module) {
    /* Anything that's allocated during the module's creation/enable, is
     * deallocated here. The stuff that's allocated lazily inside/under
     * xhc_lazy_init and xhc_init_op, is deallocated inside xhc_fini. */

    if(module->init) {
        xhc_fini(module);
    }

    for(int t = 0; t < XHC_COLLCOUNT; t++) {
        free(module->op_config[t].hierarchy_string);
        free(module->op_config[t].chunk_string);
        free(module->op_config[t].chunks);
    }

    xhc_module_clear(module);
}

// -----------------------------

mca_coll_base_module_t *mca_coll_xhc_module_comm_query(ompi_communicator_t *comm,
        int *priority) {

    if((*priority = mca_coll_xhc_component.priority) < 0) {
        return NULL;
    }

    int comm_size = ompi_comm_size(comm);

    if(OMPI_COMM_IS_INTER(comm) || 1 == comm_size
            || ompi_group_have_remote_peers (comm->c_local_group)) {

        opal_output_verbose(MCA_BASE_VERBOSE_COMPONENT,
            ompi_coll_base_framework.framework_output,
            "coll:xhc:comm_query (%s/%s): intercomm, self-comm, "
            "or not all ranks local; disqualifying myself",
            ompi_comm_print_cid(comm), comm->c_name);

        return NULL;
    }

    for(int r = 0; r < comm_size; r++) {
        ompi_proc_t *proc = ompi_comm_peer_lookup(comm, r);

        if(proc->super.proc_arch != opal_local_arch) {
            opal_output_verbose(MCA_BASE_VERBOSE_COMPONENT,
                ompi_coll_base_framework.framework_output,
                "coll:xhc:comm_query (%s/%s): All ranks not of the same arch; "
                "disabling myself", ompi_comm_print_cid(comm), comm->c_name);

            return NULL;
        }
    }

    mca_coll_xhc_module_t *module = OBJ_NEW(mca_coll_xhc_module_t);

    if(NULL == module) {
        return NULL;
    }

    module->zcopy_support = (NULL != mca_smsc);
    module->zcopy_map_support = mca_smsc_base_has_feature(MCA_SMSC_FEATURE_CAN_MAP);

    if(!module->zcopy_support) {
        opal_output_verbose(MCA_BASE_VERBOSE_COMPONENT,
            ompi_coll_base_framework.framework_output,
            "coll:xhc: Warning: No opal/smsc support found; "
            "xhc will only work in CICO mode");
    } else if(!module->zcopy_map_support) {
        opal_output_verbose(MCA_BASE_VERBOSE_COMPONENT,
            ompi_coll_base_framework.framework_output,
            "coll:xhc: Warning: opal/smsc module isn't CAN_MAP "
            "capable; reduced performance is to be expected");
    }

    module->super.coll_module_enable = mca_coll_xhc_module_enable;
    module->super.coll_module_disable = mca_coll_xhc_module_disable;

    module->super.coll_bcast = mca_coll_xhc_bcast;
    module->super.coll_barrier = mca_coll_xhc_barrier;
    module->super.coll_allreduce = mca_coll_xhc_allreduce;
    module->super.coll_reduce = mca_coll_xhc_reduce;

    return &module->super;
}

int mca_coll_xhc_module_enable(mca_coll_base_module_t *ompi_module,
        ompi_communicator_t *comm) {

    xhc_module_t *module = (xhc_module_t *) ompi_module;

    // ---

    /* Assimilate the various MCA parameters. We do this inside module_enable
     * rather than lazy_init, so we may use the values as early as possible
     * (e.g. checking cico and single-copy support at the beginning of ops). */
    for(int t = 0; t < XHC_COLLCOUNT; t++) {
        int err = xhc_read_op_config(module, comm, t);
        if(OMPI_SUCCESS != err) {
            return err;
        }
    }

    // ---

    for(int t = 0; t < XHC_COLLCOUNT; t++) {
        /* Don't want to save a fallback for
         * any op that we won't support */
        if(NULL == MODULE_COLL_FN(module, t)) {
            continue;
        }

        void (*fallback_fn)(void), *fallback_module;
        GET_COLL_API(comm, t, &fallback_fn, &fallback_module);

        if(NULL == fallback_fn || NULL == fallback_module) {
            opal_output_verbose(MCA_BASE_VERBOSE_COMPONENT,
                ompi_coll_base_framework.framework_output,
                "coll:xhc:module_enable (%s/%s): No previous fallback component "
                "found; disabling myself", ompi_comm_print_cid(comm), comm->c_name);

            return OMPI_ERR_NOT_FOUND;
        }

        module->prev_colls.coll_fn[t] = fallback_fn;
        module->prev_colls.coll_module[t] = fallback_module;
    }

    /* We perform the pointer installation last, after we've
     * successfully captured all the fallback pointers we need,
     * and we know xhc_module_enable can no longer fail. */
    for(int t = 0; t < XHC_COLLCOUNT; t++) {
        void (*fn)(void) = MODULE_COLL_FN(module, t);
        if(fn) {INSTALL_COLL_API(comm, t, fn, module);}
    }

    // ---

    return OMPI_SUCCESS;
}

int mca_coll_xhc_module_disable(mca_coll_base_module_t *ompi_module,
        ompi_communicator_t *comm) {

    mca_coll_xhc_module_destruct((xhc_module_t *) ompi_module);
    return OMPI_SUCCESS;
}

// -----------------------------

void mca_coll_xhc_module_set_coll_fns(ompi_communicator_t *comm,
        xhc_coll_fns_t *new_fns, xhc_coll_fns_t *saved_fns_dst) {

    xhc_coll_fns_t saved = {0};

    for(int t = 0; t < XHC_COLLCOUNT; t++) {
        if(!new_fns->coll_fn[t]) {
            continue;
        }

        if(saved_fns_dst) {
            GET_COLL_API(comm, t, &saved.coll_fn[t],
                &saved.coll_module[t]);
        }

        INSTALL_COLL_API(comm, t, new_fns->coll_fn[t],
            new_fns->coll_module[t]);
    }

    if(saved_fns_dst) {
        *saved_fns_dst = saved;
    }
}
