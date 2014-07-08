/*
 * Copyright (c) 2011-2013 Sandia National Laboratories.  All rights reserved.
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
#include "ompi/request/request.h"
#include "ompi/class/ompi_free_list.h"

#include "osc_portals4.h"
#include "osc_portals4_request.h"

static int component_open(void);
static int component_register(void);
static int component_init(bool enable_progress_threads, bool enable_mpi_threads);
static int component_finalize(void);
static int component_query(struct ompi_win_t *win, void **base, size_t size, int disp_unit,
                           struct ompi_communicator_t *comm, struct ompi_info_t *info,
                           int flavor);
static int component_select(struct ompi_win_t *win, void **base, size_t size, int disp_unit,
                            struct ompi_communicator_t *comm, struct ompi_info_t *info,
                            int flavor, int *model);


ompi_osc_portals4_component_t mca_osc_portals4_component = {
    { /* ompi_osc_base_component_t */
        { /* ompi_base_component_t */
            OMPI_OSC_BASE_VERSION_3_0_0,
            "portals4",
            OMPI_MAJOR_VERSION,  /* MCA component major version */
            OMPI_MINOR_VERSION,  /* MCA component minor version */
            OMPI_RELEASE_VERSION,  /* MCA component release version */
            component_open,
            NULL,
            NULL,
            component_register
        },
        { /* mca_base_component_data */
            /* The component is not checkpoint ready */
            MCA_BASE_METADATA_PARAM_NONE
        },
        component_init,
        component_query,
        component_select,
        component_finalize
    }
};


ompi_osc_portals4_module_t ompi_osc_portals4_module_template = {
    {
        NULL, /* shared_query */

        ompi_osc_portals4_attach,
        ompi_osc_portals4_detach,
        ompi_osc_portals4_free,

        ompi_osc_portals4_put,
        ompi_osc_portals4_get,
        ompi_osc_portals4_accumulate,
        ompi_osc_portals4_compare_and_swap,
        ompi_osc_portals4_fetch_and_op,
        ompi_osc_portals4_get_accumulate,

        ompi_osc_portals4_rput,
        ompi_osc_portals4_rget,
        ompi_osc_portals4_raccumulate,
        ompi_osc_portals4_rget_accumulate,

        ompi_osc_portals4_fence,

        ompi_osc_portals4_start,
        ompi_osc_portals4_complete,
        ompi_osc_portals4_post,
        ompi_osc_portals4_wait,
        ompi_osc_portals4_test,

        ompi_osc_portals4_lock,
        ompi_osc_portals4_unlock,
        ompi_osc_portals4_lock_all,
        ompi_osc_portals4_unlock_all,

        ompi_osc_portals4_sync,
        ompi_osc_portals4_flush,
        ompi_osc_portals4_flush_all,
        ompi_osc_portals4_flush_local,
        ompi_osc_portals4_flush_local_all,

        ompi_osc_portals4_set_info,
        ompi_osc_portals4_get_info
    }
};


/* look up parameters for configuring this window.  The code first
   looks in the info structure passed by the user, then through mca
   parameters. */
static bool
check_config_value_bool(char *key, ompi_info_t *info)
{
    char *value_string;
    int value_len, ret, flag, param;
    const bool *flag_value;
    bool result;

    ret = ompi_info_get_valuelen(info, key, &value_len, &flag);
    if (OMPI_SUCCESS != ret) goto info_not_found;
    if (flag == 0) goto info_not_found;
    value_len++;

    value_string = (char*)malloc(sizeof(char) * value_len + 1); /* Should malloc 1 char for NUL-termination */
    if (NULL == value_string) goto info_not_found;

    ret = ompi_info_get(info, key, value_len, value_string, &flag);
    if (OMPI_SUCCESS != ret) {
        free(value_string);
        goto info_not_found;
    }
    assert(flag != 0);
    ret = ompi_info_value_to_bool(value_string, &result);
    free(value_string);
    if (OMPI_SUCCESS != ret) goto info_not_found;
    return result;

 info_not_found:
    param = mca_base_var_find("ompi", "osc", "portals4", key);
    if (0 > param) return false;

    ret = mca_base_var_get_value(param, &flag_value, NULL, NULL);
    if (OMPI_SUCCESS != ret) return false;

    return flag_value[0];
}


static bool
check_config_value_equal(char *key, ompi_info_t *info, char *value)
{
    char *value_string;
    int value_len, ret, flag, param;
    const bool *flag_value;
    bool result = false;

    ret = ompi_info_get_valuelen(info, key, &value_len, &flag);
    if (OMPI_SUCCESS != ret) goto info_not_found;
    if (flag == 0) goto info_not_found;
    value_len++;

    value_string = (char*)malloc(sizeof(char) * value_len + 1); /* Should malloc 1 char for NUL-termination */
    if (NULL == value_string) goto info_not_found;

    ret = ompi_info_get(info, key, value_len, value_string, &flag);
    if (OMPI_SUCCESS != ret) {
        free(value_string);
        goto info_not_found;
    }
    assert(flag != 0);
    if (0 == strcmp(value_string, value)) result = true;
    free(value_string);
    return result;

 info_not_found:
    param = mca_base_var_find("ompi", "osc", "portals4", key);
    if (0 > param) return false;

    ret = mca_base_var_get_value(param, &flag_value, NULL, NULL);
    if (OMPI_SUCCESS != ret) return false;

    if (0 == strcmp(value_string, value)) result = true;

    return result;
}


static int
progress_callback(void)
{
    int ret, count = 0;
    ptl_event_t ev;
    ompi_osc_portals4_request_t *req;
    int32_t ops;

    while (true) {
        ret = PtlEQGet(mca_osc_portals4_component.matching_eq_h, &ev);
        if (PTL_OK == ret) {
            goto process;
        } else if (PTL_EQ_DROPPED == ret) {
            opal_output_verbose(1, ompi_osc_base_framework.framework_output,
                                "%s:%d: PtlEQGet reported dropped event",
                                __FILE__, __LINE__);
            goto process;
        } else if (PTL_EQ_EMPTY) {
            return 0;
        } else {
            opal_output_verbose(1, ompi_osc_base_framework.framework_output,
                                "%s:%d: PtlEQGet failed: %d\n",
                                __FILE__, __LINE__, ret);
            return 0;
        }

process:
        if (ev.ni_fail_type != PTL_OK) {
            opal_output_verbose(1, ompi_osc_base_framework.framework_output,
                                "%s:%d: event failure: %d %d",
                                __FILE__, __LINE__, ev.type, ev.ni_fail_type);
            return 0;
        }

        count++;

        if (NULL != ev.user_ptr) {
            /* can't disable send events, but they don't count in ops */
            if (ev.type == PTL_EVENT_SEND) continue;
            req = (ompi_osc_portals4_request_t*) ev.user_ptr;
            opal_atomic_add_size_t(&req->super.req_status._ucount, ev.mlength);
            ops = opal_atomic_add_32(&req->ops_committed, 1);
            if (ops == req->ops_expected) {
                OPAL_THREAD_LOCK(&ompi_request_lock);
                ompi_request_complete(&req->super, true);
                OPAL_THREAD_UNLOCK(&ompi_request_lock);
            }
        }
    }

    return count;
}


static int
component_open(void)
{
    return OMPI_SUCCESS;
}


static int
component_register(void)
{
    bool ompi_osc_portals4_no_locks = false;
    (void) mca_base_component_var_register(&mca_osc_portals4_component.super.osc_version,
                                           "no_locks",
                                           "Enable optimizations available only if MPI_LOCK is "
                                           "not used.  "
                                           "Info key of same name overrides this value.",
                                           MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &ompi_osc_portals4_no_locks);

    return OMPI_SUCCESS;
}


static int
component_init(bool enable_progress_threads, bool enable_mpi_threads)
{
    int ret;
    ptl_ni_limits_t actual;

    ret = PtlInit();
    if (PTL_OK != ret) {
        opal_output_verbose(1, ompi_osc_base_framework.framework_output,
                            "%s:%d: PtlInit failed: %d\n",
                            __FILE__, __LINE__, ret);
        return OMPI_ERROR;
    }

    ret = PtlNIInit(PTL_IFACE_DEFAULT,
                    PTL_NI_PHYSICAL | PTL_NI_MATCHING,
                    PTL_PID_ANY,
                    NULL,
                    &actual,
                    &mca_osc_portals4_component.matching_ni_h);
    if (PTL_OK != ret) {
        opal_output_verbose(1, ompi_osc_base_framework.framework_output,
                            "%s:%d: PtlNIInit failed: %d\n",
                            __FILE__, __LINE__, ret);
        return ret;
    }

    /* BWB: FIX ME: Need to make sure our ID matches with the MTL... */

    mca_osc_portals4_component.matching_atomic_max = actual.max_atomic_size;
    mca_osc_portals4_component.matching_fetch_atomic_max = actual.max_fetch_atomic_size;
    mca_osc_portals4_component.matching_atomic_ordered_size = 
        MAX(actual.max_waw_ordered_size, actual.max_war_ordered_size);

    ret = PtlEQAlloc(mca_osc_portals4_component.matching_ni_h,
                     4096,
                     &mca_osc_portals4_component.matching_eq_h);
    if (PTL_OK != ret) {
        opal_output_verbose(1, ompi_osc_base_framework.framework_output,
                            "%s:%d: PtlEQAlloc failed: %d\n",
                            __FILE__, __LINE__, ret);
        return ret;
    }

    ret = PtlPTAlloc(mca_osc_portals4_component.matching_ni_h,
                     0,
                     mca_osc_portals4_component.matching_eq_h,
                     4,
                     &mca_osc_portals4_component.matching_pt_idx);
    if (PTL_OK != ret) {
        opal_output_verbose(1, ompi_osc_base_framework.framework_output,
                            "%s:%d: PtlPTAlloc failed: %d\n",
                            __FILE__, __LINE__, ret);
        return ret;
    }

    OBJ_CONSTRUCT(&mca_osc_portals4_component.requests, ompi_free_list_t);
    ret = ompi_free_list_init(&mca_osc_portals4_component.requests,
                              sizeof(ompi_osc_portals4_request_t),
                              OBJ_CLASS(ompi_osc_portals4_request_t),
                              8,
                              0,
                              8,
                              NULL);
    if (OMPI_SUCCESS != ret) {
        opal_output_verbose(1, ompi_osc_base_framework.framework_output,
                            "%s:%d: ompi_free_list_init failed: %d\n",
                            __FILE__, __LINE__, ret);
        return ret;
    }

    ret = opal_progress_register(progress_callback);
    if (OMPI_SUCCESS != ret) {
        opal_output_verbose(1, ompi_osc_base_framework.framework_output,
                            "%s:%d: opal_progress_register failed: %d\n",
                            __FILE__, __LINE__, ret);
        return ret;
    }

    return OMPI_SUCCESS;
}


static int 
component_finalize(void)
{
    PtlNIFini(mca_osc_portals4_component.matching_ni_h);

    return OMPI_SUCCESS;
}


static int
component_query(struct ompi_win_t *win, void **base, size_t size, int disp_unit,
                struct ompi_communicator_t *comm, struct ompi_info_t *info,
                int flavor)
{
    if (MPI_WIN_FLAVOR_SHARED == flavor) return -1;

    return 20;
}


static int
component_select(struct ompi_win_t *win, void **base, size_t size, int disp_unit,
                 struct ompi_communicator_t *comm, struct ompi_info_t *info,
                 int flavor, int *model)
{
    ompi_osc_portals4_module_t *module = NULL;
    int ret = OMPI_ERROR;
    int tmp;
    ptl_md_t md;
    ptl_me_t me;
    char *name;

    if (MPI_WIN_FLAVOR_SHARED == flavor) return OMPI_ERR_NOT_SUPPORTED;

    /* create module structure */
    module = (ompi_osc_portals4_module_t*)
        calloc(1, sizeof(ompi_osc_portals4_module_t));
    if (NULL == module) return OMPI_ERR_TEMP_OUT_OF_RESOURCE;

    /* fill in the function pointer part */
    memcpy(module, &ompi_osc_portals4_module_template, 
           sizeof(ompi_osc_base_module_t));

    /* fill in our part */
    if (MPI_WIN_FLAVOR_ALLOCATE == flavor) {
        module->free_after = *base = malloc(size);
        if (NULL == *base) goto error;
    } else {
        module->free_after = NULL;
    }

    ret = ompi_comm_dup(comm, &module->comm);
    if (OMPI_SUCCESS != ret) goto error;

    opal_output_verbose(1, ompi_osc_base_framework.framework_output,
                        "portals4 component creating window with id %d",
                        ompi_comm_get_cid(module->comm));

    asprintf(&name, "portals4 window %d", ompi_comm_get_cid(module->comm));
    ompi_win_set_name(win, name);
    free(name);

    /* share everyone's displacement units. Only do an allgather if
       strictly necessary, since it requires O(p) state. */
    tmp = disp_unit;
    ret = module->comm->c_coll.coll_bcast(&tmp, 1, MPI_INT, 0, 
                                          module->comm, 
                                          module->comm->c_coll.coll_bcast_module);
    if (OMPI_SUCCESS != ret) {
        opal_output_verbose(1, ompi_osc_base_framework.framework_output,
                            "%s:%d: MPI_Bcast failed: %d\n",
                            __FILE__, __LINE__, ret);
        goto error;
    }
    tmp = (tmp == disp_unit) ? 1 : 0;
    ret = module->comm->c_coll.coll_allreduce(MPI_IN_PLACE, &tmp, 1, MPI_INT, MPI_LAND,
                                              module->comm, module->comm->c_coll.coll_allreduce_module);
    if (OMPI_SUCCESS != ret) goto error;
    if (tmp == 1) {
        module->disp_unit = disp_unit;
        module->disp_units = NULL;
    } else {
        module->disp_unit = -1;
        module->disp_units = malloc(sizeof(int) * ompi_comm_size(module->comm));
        ret = module->comm->c_coll.coll_allgather(&disp_unit, 1, MPI_INT,
                                                  module->disp_units, 1, MPI_INT,
                                                  module->comm,
                                                  module->comm->c_coll.coll_allgather_module);
        if (OMPI_SUCCESS != ret) goto error;
    }

    module->ni_h = mca_osc_portals4_component.matching_ni_h;
    module->pt_idx = mca_osc_portals4_component.matching_pt_idx;

    ret = PtlCTAlloc(module->ni_h, &(module->ct_h));
    if (PTL_OK != ret) {
        opal_output_verbose(1, ompi_osc_base_framework.framework_output,
                            "%s:%d: PtlCTAlloc failed: %d\n",
                            __FILE__, __LINE__, ret);
        goto error;
    }
    
#if OMPI_PORTALS4_MAX_MD_SIZE < OMPI_PORTALS4_MAX_VA_SIZE
    {
        int i;
        int num_mds = ompi_mtl_portals4_get_num_mds();
        ptl_size_t size = 1ULL << OMPI_PORTALS4_MAX_MD_SIZE;
        ptl_size_t offset_unit = (1ULL << OMPI_PORTALS4_MAX_MD_SIZE) / 2;

        module->md_h = malloc(sizeof(ptl_handle_md_t) * num_mds);
        if (NULL == module->md_h) {
            ret = OMPI_ERR_TEMP_OUT_OF_RESOURCE;
            goto error;
        }
        for (i = 0 ; i < num_mds ; ++i) {
            module->md_h[i] = PTL_INVALID_HANDLE;
        }

        module->req_md_h = malloc(sizeof(ptl_handle_md_t) * num_mds);
        if (NULL == module->req_md_h) {
            ret = OMPI_ERR_TEMP_OUT_OF_RESOURCE;
            goto error;
        }
        for (i = 0 ; i < num_mds ; ++i) {
            module->req_md_h[i] = PTL_INVALID_HANDLE;
        }

        for (i = 0 ; i < num_mds ; ++i) {
            md.start = (char*) (offset_unit * i);
            md.length = (i - 1 == num_mds) ? size / 2 : size;

            md.options = PTL_MD_EVENT_SUCCESS_DISABLE | PTL_MD_EVENT_CT_REPLY | PTL_MD_EVENT_CT_ACK;
            md.eq_handle = mca_osc_portals4_component.matching_eq_h;
            md.ct_handle = module->ct_h;
            ret = PtlMDBind(module->ni_h, &md, &module->md_h);
            if (PTL_OK != ret) {
                opal_output_verbose(1, ompi_osc_base_framework.framework_output,
                                    "%s:%d: PtlMDBind failed: %d\n",
                                    __FILE__, __LINE__, ret);
                goto error;
            }

            md.options = PTL_MD_EVENT_CT_REPLY | PTL_MD_EVENT_CT_ACK;
            md.eq_handle = mca_osc_portals4_component.matching_eq_h;
            md.ct_handle = module->ct_h;
            ret = PtlMDBind(module->ni_h, &md, &module->req_md_h);
            if (PTL_OK != ret) {
                opal_output_verbose(1, ompi_osc_base_framework.framework_output,
                                    "%s:%d: PtlMDBind failed: %d\n",
                                    __FILE__, __LINE__, ret);
                goto error;
            }
        }
#else
    md.start = 0;
    md.length = PTL_SIZE_MAX;
    md.options = PTL_MD_EVENT_SUCCESS_DISABLE | PTL_MD_EVENT_CT_REPLY | PTL_MD_EVENT_CT_ACK;
    md.eq_handle = mca_osc_portals4_component.matching_eq_h;
    md.ct_handle = module->ct_h;
    ret = PtlMDBind(module->ni_h, &md, &module->md_h[0]);
    if (PTL_OK != ret) {
        opal_output_verbose(1, ompi_osc_base_framework.framework_output,
                            "%s:%d: PtlMDBind failed: %d\n",
                            __FILE__, __LINE__, ret);
        goto error;
    }

    md.start = 0;
    md.length = PTL_SIZE_MAX;
    md.options = PTL_MD_EVENT_CT_REPLY | PTL_MD_EVENT_CT_ACK;
    md.eq_handle = mca_osc_portals4_component.matching_eq_h;
    md.ct_handle = module->ct_h;
    ret = PtlMDBind(module->ni_h, &md, &module->req_md_h[0]);
    if (PTL_OK != ret) {
        opal_output_verbose(1, ompi_osc_base_framework.framework_output,
                            "%s:%d: PtlMDBind failed: %d\n",
                            __FILE__, __LINE__, ret);
        goto error;
    }
#endif

    if (MPI_WIN_FLAVOR_DYNAMIC == flavor) {
        me.start = 0;
        me.length = SIZE_MAX;
    } else {
        me.start = *base;
        me.length = size;
    }
    me.ct_handle = PTL_CT_NONE;
    me.uid = PTL_UID_ANY;
    me.options = PTL_ME_OP_PUT | PTL_ME_OP_GET | PTL_ME_NO_TRUNCATE | PTL_ME_EVENT_SUCCESS_DISABLE;
    me.match_id.phys.nid = PTL_NID_ANY;
    me.match_id.phys.pid = PTL_PID_ANY;
    me.match_bits = module->comm->c_contextid;
    me.ignore_bits = 0;

    ret = PtlMEAppend(module->ni_h,
                      module->pt_idx,
                      &me,
                      PTL_PRIORITY_LIST,
                      NULL,
                      &module->data_me_h);
    if (PTL_OK != ret) {
        opal_output_verbose(1, ompi_osc_base_framework.framework_output,
                            "%s:%d: PtlMEAppend failed: %d\n",
                            __FILE__, __LINE__, ret);
        goto error;
    }

    me.start = &module->state;
    me.length = sizeof(module->state);
    me.ct_handle = PTL_CT_NONE;
    me.uid = PTL_UID_ANY;
    me.options = PTL_ME_OP_PUT | PTL_ME_OP_GET | PTL_ME_NO_TRUNCATE | PTL_ME_EVENT_SUCCESS_DISABLE;
    me.match_id.phys.nid = PTL_NID_ANY;
    me.match_id.phys.pid = PTL_PID_ANY;
    me.match_bits = module->comm->c_contextid | OSC_PORTALS4_MB_CONTROL;
    me.ignore_bits = 0;

    ret = PtlMEAppend(module->ni_h,
                      module->pt_idx,
                      &me,
                      PTL_PRIORITY_LIST,
                      NULL,
                      &module->control_me_h);
    if (PTL_OK != ret) {
        opal_output_verbose(1, ompi_osc_base_framework.framework_output,
                            "%s:%d: PtlMEAppend failed: %d\n",
                            __FILE__, __LINE__, ret);
        goto error;
    }

    module->opcount = 0;
    module->match_bits = module->comm->c_contextid;
    module->atomic_max = (check_config_value_equal("accumulate_ordering", info, "none")) ?
        mca_osc_portals4_component.matching_atomic_max :
        MIN(mca_osc_portals4_component.matching_atomic_max,
            mca_osc_portals4_component.matching_atomic_ordered_size);
    module->fetch_atomic_max = (check_config_value_equal("accumulate_ordering", info, "none")) ?
        mca_osc_portals4_component.matching_fetch_atomic_max :
        MIN(mca_osc_portals4_component.matching_fetch_atomic_max,
            mca_osc_portals4_component.matching_atomic_ordered_size);

    module->zero = 0;
    module->one = 1;
    module->start_group = NULL;
    module->post_group = NULL;

    module->state.post_count = 0;
    module->state.complete_count = 0;
    if (check_config_value_bool("no_locks", info)) {
        module->state.lock = LOCK_ILLEGAL;
    } else {
        module->state.lock = LOCK_UNLOCKED;
    }

    OBJ_CONSTRUCT(&module->outstanding_locks, opal_list_t);

    module->passive_target_access_epoch = false;

#if OPAL_ASSEMBLY_ARCH == OMPI_AMD64 || OPAL_ASSEMBLY_ARCH == IA32
    *model = MPI_WIN_UNIFIED;
#else
    *model = MPI_WIN_SEPARATE;
#endif

    win->w_osc_module = &module->super;

    PtlAtomicSync();

    /* Make sure that everyone's ready to receive. */
    module->comm->c_coll.coll_barrier(module->comm,
                                      module->comm->c_coll.coll_barrier_module);

    return OMPI_SUCCESS;

 error:
    /* BWB: FIX ME: This is all wrong... */
    if (0 != module->ct_h) PtlCTFree(module->ct_h);
    if (0 != module->data_me_h) PtlMEUnlink(module->data_me_h);
#if OMPI_PORTALS4_MAX_MD_SIZE < OMPI_PORTALS4_MAX_VA_SIZE
    /* BWB: FIX ME */
#else 
    if (0 != module->req_md_h) PtlMDRelease(module->req_md_h[0]);
    if (0 != module->md_h) PtlMDRelease(module->md_h[0]);
#endif
    if (NULL != module->comm) ompi_comm_free(&module->comm);
    if (NULL != module) free(module);

    return ret;
}


int
ompi_osc_portals4_attach(struct ompi_win_t *win, void *base, size_t len)
{
    return OMPI_SUCCESS;
}


int
ompi_osc_portals4_detach(struct ompi_win_t *win, void *base)
{
    return OMPI_SUCCESS;
}


int
ompi_osc_portals4_free(struct ompi_win_t *win)
{
    ompi_osc_portals4_module_t *module =
        (ompi_osc_portals4_module_t*) win->w_osc_module;
    int ret = OMPI_SUCCESS;

    /* synchronize */
    module->comm->c_coll.coll_barrier(module->comm,
                                      module->comm->c_coll.coll_barrier_module);

    /* cleanup */
    PtlMEUnlink(module->data_me_h);
#if OMPI_PORTALS4_MAX_MD_SIZE < OMPI_PORTALS4_MAX_VA_SIZE
    /* BWB: FIX ME */
#else 
    PtlMDRelease(module->md_h[0]);
    PtlMDRelease(module->req_md_h[0]);
#endif
    PtlCTFree(module->ct_h);
    if (NULL != module->disp_units) free(module->disp_units);
    ompi_comm_free(&module->comm);
    if (NULL != module->free_after) free(module->free_after);

    if (!opal_list_is_empty(&module->outstanding_locks)) {
        ret = OMPI_ERR_RMA_SYNC;
    }
    OBJ_DESTRUCT(&module->outstanding_locks);

    free(module);

    return ret;
}


int
ompi_osc_portals4_set_info(struct ompi_win_t *win, struct ompi_info_t *info)
{
    ompi_osc_portals4_module_t *module =
        (ompi_osc_portals4_module_t*) win->w_osc_module;

    /* enforce collectiveness... */
    return module->comm->c_coll.coll_barrier(module->comm,
                                             module->comm->c_coll.coll_barrier_module);
}


int
ompi_osc_portals4_get_info(struct ompi_win_t *win, struct ompi_info_t **info_used)
{
    ompi_osc_portals4_module_t *module =
        (ompi_osc_portals4_module_t*) win->w_osc_module;

    ompi_info_t *info = OBJ_NEW(ompi_info_t);
    if (NULL == info) return OMPI_ERR_TEMP_OUT_OF_RESOURCE;

    ompi_info_set(info, "no_locks",  (module->state.lock == LOCK_ILLEGAL) ? "true" : "false");
    if (module->atomic_max < mca_osc_portals4_component.matching_atomic_max) {
        ompi_info_set(info, "accumulate_ordering", "none");
    } else {
        ompi_info_set(info, "accumulate_ordering", "rar,war,raw,waw");
    }

    *info_used = info;

    return OMPI_SUCCESS;
}
