/*
 * Copyright (c) 2007-2009 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2007-2012 Mellanox Technologies.  All rights reserved.
 *
 * Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
#include "ompi_config.h"

#include <string.h>
#include "base.h"
#include "common_ofacm_oob.h"
#include "common_ofacm_empty.h"
#if HAVE_XRC
#include "common_ofacm_xoob.h"
#endif

#include "ompi/constants.h"
#include "opal/class/opal_list.h"
#include "opal/util/argv.h"
#include "opal/util/output.h"
#include "infiniband/verbs.h"

/* Global variables */
static ompi_common_ofacm_base_component_t **available = NULL;
static int num_available = 0;
int ompi_common_ofacm_base_verbose = 0; /* disabled by default */
char* ompi_common_ofacm_three_dim_torus = NULL;
bool cpc_explicitly_defined = false;
int ompi_common_ofacm_base_output = 1;
bool ompi_common_ofacm_base_register_was_called = false;
bool ompi_common_ofacm_base_init_was_called = false;
/*
 * Array of all possible connection functions
 */
static ompi_common_ofacm_base_component_t *all[] = {
    &ompi_common_ofacm_oob,

    /* Always have an entry here so that the CP indexes will always be
       the same: if XRC is not available, use the "empty" CPC */
#if HAVE_XRC
    &ompi_common_ofacm_xoob,
#else
    &ompi_common_ofacm_empty,
#endif
    NULL
};

static void ofacm_base_proc_contructor (ompi_common_ofacm_base_proc_t *proc)
{
    proc->proc_ompi = NULL;
    OBJ_CONSTRUCT(&proc->all_contexts, opal_list_t);
}

static void ofacm_base_proc_destructor (ompi_common_ofacm_base_proc_t *proc)
{
    OBJ_DESTRUCT(&proc->all_contexts);
}

void ompi_common_ofacm_base_proc_setup(ompi_common_ofacm_base_proc_t *proc, 
        ompi_common_ofacm_base_local_connection_context_t *context,
        ompi_proc_t *proc_ompi) 
{
    if (NULL == proc->proc_ompi) {
        /* first init for the proc, lets set ompi proc */
        proc->proc_ompi = proc_ompi;
    }
    /* put the context on the proc list */
    opal_list_append(&proc->all_contexts, (opal_list_item_t *)context);
}

OBJ_CLASS_INSTANCE(ompi_common_ofacm_base_proc_t,
                   opal_list_item_t,
                   ofacm_base_proc_contructor,
                   ofacm_base_proc_destructor);

/* Constructors / Destructors */
static void base_local_context_constructor
    (ompi_common_ofacm_base_local_connection_context_t *context)
{
    context->proc = NULL;
    context->state = MCA_COMMON_OFACM_CLOSED;
    context->subnet_id = 0;
    context->lid = 0;
    context->num_of_qps = 0;
    context->init_attr = NULL;
    context->attr = NULL;
    context->ib_pd = NULL;
    context->qps = NULL;
    context->user_context = NULL;
    context->initiator = 0;
    context->index = 0;
    context->xrc_recv_qp_num = 0;
    /* remote info we will allocate and fill during qp creation */
    memset(&context->remote_info, 0, sizeof(context->remote_info));
    OBJ_CONSTRUCT(&context->context_lock, opal_mutex_t);
}

static void base_local_context_destructor
    (ompi_common_ofacm_base_local_connection_context_t *context)
{
    /* Release remote data arrays */
    if (NULL != context->remote_info.rem_qps) {
        free(context->remote_info.rem_qps);
    }
    if (NULL != context->remote_info.rem_srqs) {
        free(context->remote_info.rem_srqs);
    }
    OBJ_DESTRUCT(&context->context_lock);
}

OBJ_CLASS_INSTANCE(ompi_common_ofacm_base_local_connection_context_t,
                   opal_list_item_t,
                   base_local_context_constructor,
                   base_local_context_destructor);

int ompi_common_ofacm_base_context_init(ompi_common_ofacm_base_local_connection_context_t *context,
                                         ompi_common_ofacm_base_module_t *cpc,
                                         ompi_common_ofacm_base_context_connect_cb_fn_t connect_cb,
                                         ompi_common_ofacm_base_context_error_cb_fn_t error_cb,
                                         ompi_common_ofacm_base_context_prepare_recv_cb_fn_t prepare_recv_cb,
                                         ompi_common_ofacm_base_proc_t *proc,
                                         ompi_common_ofacm_base_qp_config_t *qp_config,
                                         struct ibv_pd *pd, uint64_t subnet_id, int cpc_type, 
                                         uint16_t lid, uint16_t rem_lid, 
                                         int32_t user_context_index, void *user_context)
{
    context->proc = proc;
    context->cpc = cpc;
    context->subnet_id = subnet_id;
    context->cpc_type = cpc_type;
    context->lid = lid;
    context->rem_lid = rem_lid;
    context->num_of_qps = qp_config->num_qps;
    /* If upper layer defines the QPs we do not want to overwrite it */
    if (NULL == context->qps) {
        context->qps = calloc(context->num_of_qps, sizeof(ompi_common_ofacm_base_qp_t));
        if(NULL == context->qps) {
            OFACM_ERROR(("Failed to allocate memory for qps"));
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
    }

    context->num_of_srqs = qp_config->num_srqs;
    context->srq_num = qp_config->srq_num;
    context->init_attr = qp_config->init_attr;
    context->attr = qp_config->attr;
    context->custom_init_attr_mask = qp_config->init_attr_mask;
    context->custom_rtr_attr_mask = qp_config->rtr_attr_mask;
    context->custom_rts_attr_mask = qp_config->rts_attr_mask;
    context->ib_pd = pd;
    context->connect_cb = connect_cb;
    context->error_cb = error_cb;
    context->prepare_recv_cb = prepare_recv_cb ;
    context->index = user_context_index;
    context->user_context = user_context;
    return OMPI_SUCCESS;
}

int ompi_common_ofacm_base_remote_context_init(ompi_common_ofacm_base_remote_connection_context_t *context,
                                                int num_qps, int num_srqs)
{
    context->rem_qps = (ompi_common_ofacm_base_rem_qp_info_t *)
        calloc(num_qps, sizeof(ompi_common_ofacm_base_rem_qp_info_t));
    if (NULL == context->rem_qps) {
        return OMPI_ERROR;
    }

    context->rem_srqs = (ompi_common_ofacm_base_rem_srq_info_t *)
        calloc(num_qps, sizeof(ompi_common_ofacm_base_rem_srq_info_t));
    if (NULL == context->rem_srqs) {
        return OMPI_ERROR;
    }

    return OMPI_SUCCESS; 
}

ompi_common_ofacm_base_proc_t* ompi_common_ofacm_base_find_proc
        (ompi_common_ofacm_base_component_t *component, ompi_proc_t *proc)
{
    ompi_common_ofacm_base_proc_t *ret = NULL;
    opal_list_item_t *item;
    opal_list_t *list = &component->all_procs;

    for (item = opal_list_get_first(list);
            item != opal_list_get_end(list);
            item = opal_list_get_next(item)) {
        if (proc == ((ompi_common_ofacm_base_proc_t *)item)->proc_ompi){
            ret = (ompi_common_ofacm_base_proc_t *)item;
        }
    }
    return ret;
}
/*
 * Register MCA parameters
 */
int ompi_common_ofacm_base_register(mca_base_component_t *base)
{
    int i, j, save;
    char **temp = NULL, *string = NULL, *all_cpc_names = NULL;
    char *cpc_include = NULL, *cpc_exclude = NULL;

    if (ompi_common_ofacm_base_register_was_called) {
        return OMPI_SUCCESS;
    }

    ompi_common_ofacm_base_register_was_called = true;

    /* Make an MCA parameter to select which connect module to use */
    for (i = 0; NULL != all[i]; ++i) {
        /* The CPC name "empty" is reserved for "fake" CPC modules */
        if (0 != strcmp(all[i]->cbc_name, "empty")) {
            opal_argv_append_nosize(&temp, all[i]->cbc_name);
        }
    }
    all_cpc_names = opal_argv_join(temp, ',');
    opal_argv_free(temp);
    asprintf(&string,
             "Method used to select OpenFabrics connections (valid values: %s)",
             all_cpc_names);

    mca_base_param_reg_string(base, "ofacm_cpc_include", string, false, false,
            NULL, &cpc_include);
    free(string);

    asprintf(&string,
             "Method used to exclude OpenFabrics connections (valid values: %s)",
             all_cpc_names);

    mca_base_param_reg_string(base, "ofacm_cpc_exclude", string, false, false, 
            NULL, &cpc_exclude);
    free(string);

    /* Register the name of the file containing the fabric's Service Levels (SL) */
    mca_base_param_reg_string_name("common", "ofacm_three_dim_torus",
                    "The name of the file contating Service Level (SL) data for 3D-Torus cluster",
                    false, false, NULL, &ompi_common_ofacm_three_dim_torus);


    mca_base_param_reg_int_name("common", 
                                "ofacm_base_verbose", 
                                "Verbosity level of the OFACM framework", 
                                false, false, 
                                0, 
                                &ompi_common_ofacm_base_verbose);


    /* Parse the if_[in|ex]clude paramters to come up with a list of
       CPCs that are available */
    available = calloc(1, sizeof(all));

    /* If we have an "include" list, then find all those CPCs and put
       them in available[] */
    if (NULL != cpc_include) {
        cpc_explicitly_defined = true;
        temp = opal_argv_split(cpc_include, ',');
        for (save = j = 0; NULL != temp[j]; ++j) {
            for (i = 0; NULL != all[i]; ++i) {
                if (0 == strcmp(temp[j], all[i]->cbc_name)) { 
                    OFACM_VERBOSE(("include: saving %s", all[i]->cbc_name));
                    available[save++] = all[i];
                    ++num_available;
                    break;
                }
            }
            if (NULL == all[i]) {
                opal_show_help("help-mpi-common-ofacm-cpc-base.txt",
                               "cpc name not found", true,
                               "include", ompi_process_info.nodename,
                               "include", cpc_include, temp[j], 
                               all_cpc_names);
                opal_argv_free(temp);
                free(all_cpc_names);
                return OMPI_ERR_NOT_FOUND;
            }
        }
        opal_argv_free(temp);
    }

    /* Otherwise, if we have an "exclude" list, take all the CPCs that
       are not in that list and put them in available[] */
    else if (NULL != cpc_exclude) {
        cpc_explicitly_defined = true;
        temp = opal_argv_split(cpc_exclude, ',');
        /* First: error check -- ensure that all the names are valid */
        for (j = 0; NULL != temp[j]; ++j) {
            for (i = 0; NULL != all[i]; ++i) {
                if (0 == strcmp(temp[j], all[i]->cbc_name)) { 
                    break;
                }
            }
            if (NULL == all[i]) {
                opal_show_help("help-mpi-common-ofacm-cpc-base.txt",
                               "cpc name not found", true,
                               "exclude", ompi_process_info.nodename,
                               "exclude", cpc_exclude, temp[j], 
                               all_cpc_names);
                opal_argv_free(temp);
                free(all_cpc_names);
                return OMPI_ERR_NOT_FOUND;
            }
        }

        /* Now do the exclude */
        for (save = i = 0; NULL != all[i]; ++i) {
            for (j = 0; NULL != temp[j]; ++j) {
                if (0 == strcmp(temp[j], all[i]->cbc_name)) {
                    break;
                }
            }
            if (NULL == temp[j]) {
                OFACM_VERBOSE(("exclude: saving %s", all[i]->cbc_name));
                available[save++] = all[i];
                ++num_available;
            }
        }
        opal_argv_free(temp);
    } 

    /* If there's no include/exclude list, copy all[] into available[] */
    else {
        OFACM_VERBOSE(("no include or exclude: saving all"));
        memcpy(available, all, sizeof(all));
        num_available = (sizeof(all) / 
                         sizeof(ompi_common_ofacm_base_module_t *)) - 1;
    }

    /* Call the register function on all the CPCs so that they may
       setup any MCA params specific to the connection type */
    for (i = 0; NULL != available[i]; ++i) {
        if (NULL != available[i]->cbc_register) {
            available[i]->cbc_register();
        }
    }

    return OMPI_SUCCESS;
}

/*
 * Called once during openib BTL component initialization to allow CPC
 * components to initialize.
 */
int ompi_common_ofacm_base_init(void)
{
    int i, rc;

    if (ompi_common_ofacm_base_init_was_called) {
        return OMPI_SUCCESS;
    }

    ompi_common_ofacm_base_init_was_called = true;

    /* Call each available CPC component's open function, if it has
       one.  If the CPC component open function returns OMPI_SUCCESS,
       keep it.  If it returns ERR_NOT_SUPPORTED, remove it from the
       available[] array.  If it returns something else, return that
       error upward. */
    for (i = num_available = 0; NULL != available[i]; ++i) {
        if (NULL == available[i]->cbc_init) {
            available[num_available++] = available[i];
            OFACM_VERBOSE(("found available cpc (NULL init): %s",
                        all[i]->cbc_name));
            continue;
        }

        rc = available[i]->cbc_init();
        if (OMPI_SUCCESS == rc) {
            available[num_available++] = available[i];
            OFACM_VERBOSE(("found available cpc (SUCCESS init): %s",
                        all[i]->cbc_name));
            continue;
        } else if (OMPI_ERR_NOT_SUPPORTED == rc) {
            continue;
        } else {
            return rc;
        }
    }
    available[num_available] = NULL;

    return (num_available > 0) ? OMPI_SUCCESS : OMPI_ERR_NOT_AVAILABLE;
}


/*
 * Find all the CPCs that are eligible for a single local port (i.e.,
 * openib module).
 */
int ompi_common_ofacm_base_select_for_local_port(ompi_common_ofacm_base_dev_desc_t *dev,
        ompi_common_ofacm_base_module_t ***cpcs, int *num_cpcs)
{
    char *msg = NULL;
    int i, rc, cpc_index, len;
    ompi_common_ofacm_base_module_t **tmp_cpcs;

    tmp_cpcs = calloc(num_available, 
                  sizeof(ompi_common_ofacm_base_module_t *));
    if (NULL == tmp_cpcs) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    /* Go through all available CPCs and query them to see if they
       want to run on this module.  If they do, save them to a running
       array. */
    for (len = 1, i = 0; NULL != available[i]; ++i) {
        len += strlen(available[i]->cbc_name) + 2;
    }
    msg = malloc(len);
    if (NULL == msg) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    msg[0] = '\0';
    for (cpc_index = i = 0; NULL != available[i]; ++i) {
        if (i > 0) {
            strcat(msg, ", ");
        }
        strcat(msg, available[i]->cbc_name);

        rc = available[i]->cbc_query(dev, &tmp_cpcs[cpc_index]);
        if (OMPI_ERR_NOT_SUPPORTED == rc || OMPI_ERR_UNREACH == rc) {
            continue;
        } else if (OMPI_SUCCESS != rc) {
            free(tmp_cpcs);
            free(msg);
            return rc;
        }
        OFACM_VERBOSE(("match cpc for local port: %s",
                    available[i]->cbc_name));

        /* If the CPC wants to use the CTS protocol, check to ensure
           that QP 0 is PP; if it's not, we can't use this CPC (or the
           CTS protocol) */
        /* Pasha: Wrong place to check qp type, should be moved to CMs 
        if (cpcs[cpc_index]->cbm_uses_cts &&
            !BTL_OPENIB_QP_TYPE_PP(0)) {
            OFACM_VERBOSE(("this CPC only supports when the first btl_openib_receive_queues QP is a PP QP"));
            continue;
        }
        */

        /* This CPC has indicated that it wants to run on this openib
           BTL module.  Woo hoo! */
        ++cpc_index;
    }

    /* If we got an empty array, then no CPCs were eligible.  Doh! */
    if (0 == cpc_index) {
        opal_show_help("help-mpi-common-ofacm-cpc-base.txt",
                       "no cpcs for port", true,
                       ompi_process_info.nodename,
                       ibv_get_device_name(dev->ib_dev),
                       msg);
        free(tmp_cpcs);
        free(msg);
        return OMPI_ERR_NOT_SUPPORTED;
    }
    free(msg);

    /* We got at least one eligible CPC; save the array into the
       module's port_info */
    *num_cpcs = cpc_index;
    *cpcs = tmp_cpcs;

    return OMPI_SUCCESS;
}

/* 
 * This function is invoked when determining whether we have a CPC in
 * common with a specific remote port.  We already know that the
 * subnet ID is the same between a specific local port and the target
 * remote port; now we need to know if we can find a CPC in common
 * between the two.
 *
 * If yes, be sure to find the *same* CPC on both sides.  We know
 * which CPCs are available on each side, and we know the priorities
 * that were assigned on both sides.  So find a CPC that is common to
 * both sides and has the highest overall priority (between both
 * sides).
 *
 * Return the matching CPC, or NULL if not found.
 */
int
ompi_common_ofacm_base_find_match(ompi_common_ofacm_base_module_t **local_cpcs, int num_local_cpcs,
        ompi_common_ofacm_base_module_data_t *remote_cpc_data, int remote_cpc_data_count,
        ompi_common_ofacm_base_module_t **ret_local_cpc,
        ompi_common_ofacm_base_module_data_t **ret_remote_cpc_data)
{
    int i, j, max = -1;
    ompi_common_ofacm_base_module_t *local_cpc, *local_selected = NULL;
    ompi_common_ofacm_base_module_data_t *local_cpcd, *remote_cpcd,
        *remote_selected = NULL;

    /* Iterate over all the CPCs on the local module */
    for (i = 0; i < num_local_cpcs; ++i) {
        local_cpc = local_cpcs[i];
        local_cpcd = &(local_cpc->data);

        /* Iterate over all the CPCs on the remote port */
        for (j = 0; j < remote_cpc_data_count; ++j) {
            remote_cpcd = &(remote_cpc_data[j]);

            /* Are the components the same? */
            if (local_cpcd->cbm_component == remote_cpcd->cbm_component) {
                /* If so, update the max priority found so far */
                if (max < local_cpcd->cbm_priority) {
                    max = local_cpcd->cbm_priority;
                    local_selected = local_cpc;
                    remote_selected = remote_cpcd;
                }
                if (max < remote_cpcd->cbm_priority) {
                    max = remote_cpcd->cbm_priority;
                    local_selected = local_cpc;
                    remote_selected = remote_cpcd;
                }
            }
        }
    }

    /* All done! */
    if (NULL != local_selected) {
        *ret_local_cpc = local_selected;
        *ret_remote_cpc_data = remote_selected;
        OFACM_VERBOSE(("find_match: found match!"));
        return OMPI_SUCCESS;
    } else {
        OFACM_VERBOSE(("find_match: did NOT find match!"));
        return OMPI_ERR_NOT_FOUND;
    }
}

/*
 * Lookup a CPC component's index in the all[] array so that we can
 * send it int the modex
 */
int ompi_common_ofacm_base_get_cpc_index(ompi_common_ofacm_base_component_t *cpc)
{
    int i;
    for (i = 0; NULL != all[i]; ++i) {
        if (all[i] == cpc) {
            return i;
        }
    }

    /* Not found */
    return -1;
}

/*
 * Lookup a CPC by its index (received from the modex)
 */
ompi_common_ofacm_base_component_t *
ompi_common_ofacm_base_get_cpc_byindex(uint8_t index)
{
    return (index >= (sizeof(all) / 
                      sizeof(ompi_common_ofacm_base_module_t *))) ?
        NULL : all[index];
}

/*
 * This function we never call from BTL - so it is no reason to expose it 
 * in base.
 */
#if 0
int ompi_common_ofacm_base_alloc_cts(mca_btl_base_endpoint_t *endpoint)
{
    ompi_free_list_item_t *fli;
    int length = sizeof(mca_btl_openib_header_t) +
        sizeof(mca_btl_openib_header_coalesced_t) +
        sizeof(mca_btl_openib_control_header_t) +
        sizeof(mca_btl_openib_footer_t) +
        mca_btl_openib_component.qp_infos[mca_btl_openib_component.credits_qp].size;

    /* Explicitly don't use the mpool registration */
    fli = &(endpoint->endpoint_cts_frag.super.super.base.super);
    fli->registration = NULL;
    fli->ptr = malloc(length);
    if (NULL == fli->ptr) {
        BTL_ERROR(("malloc failed"));
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    endpoint->endpoint_cts_mr = 
        ibv_reg_mr(endpoint->endpoint_btl->device->ib_pd, 
                   fli->ptr, length,
                   IBV_ACCESS_LOCAL_WRITE | IBV_ACCESS_REMOTE_WRITE |
                   IBV_ACCESS_REMOTE_READ);
    OPAL_OUTPUT((-1, "registered memory %p, length %d", fli->ptr, length));
    if (NULL == endpoint->endpoint_cts_mr) {
        free(fli->ptr);
        BTL_ERROR(("Failed to reg mr!"));
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    /* Copy the lkey where it needs to go */
    endpoint->endpoint_cts_frag.super.sg_entry.lkey = 
        endpoint->endpoint_cts_frag.super.super.segment.seg_key.key32[0] = 
        endpoint->endpoint_cts_mr->lkey;
    endpoint->endpoint_cts_frag.super.sg_entry.length = length;

    /* Construct the rest of the recv_frag_t */
    OBJ_CONSTRUCT(&(endpoint->endpoint_cts_frag), mca_btl_openib_recv_frag_t);
    endpoint->endpoint_cts_frag.super.super.base.order = 
        mca_btl_openib_component.credits_qp;
    endpoint->endpoint_cts_frag.super.endpoint = endpoint;
    OPAL_OUTPUT((-1, "Got a CTS frag for peer %s, addr %p, length %d, lkey %d",
                 endpoint->endpoint_proc->proc_ompi->proc_hostname,
                 (void*) endpoint->endpoint_cts_frag.super.sg_entry.addr,
                 endpoint->endpoint_cts_frag.super.sg_entry.length,
                 endpoint->endpoint_cts_frag.super.sg_entry.lkey));

    return OMPI_SUCCESS;
}
#endif
/* This function is needed for CTS packet release on completion..
 * and it is bad idea...it is 2 possible solutions:
 * - make the send operation blocking (simple and not optimal).
 * - rdmacm should add own progress function (best but not trivial).
 */
#if 0
int ompi_common_ofacm_base_free_cts(mca_btl_base_endpoint_t *endpoint)
{
    if (NULL != endpoint->endpoint_cts_mr) {
        ibv_dereg_mr(endpoint->endpoint_cts_mr);
        endpoint->endpoint_cts_mr = NULL;
    }
    if (NULL != endpoint->endpoint_cts_frag.super.super.base.super.ptr) {
        free(endpoint->endpoint_cts_frag.super.super.base.super.ptr);
        endpoint->endpoint_cts_frag.super.super.base.super.ptr = NULL;
        OPAL_OUTPUT((-1, "Freeing CTS frag"));
    }

    return OMPI_SUCCESS;
}
#endif

/*
 * Called to start a connection
 */
int ompi_common_ofacm_base_start_connect(
        ompi_common_ofacm_base_local_connection_context_t *context)
{
#if 0
    /* If the CPC uses the CTS protocol, provide a frag buffer for the
       CPC to post.  Must allocate these frags up here in the main
       thread because the FREE_LIST_WAIT is not thread safe. */
    if (cpc->cbm_uses_cts) {
        int rc;
        rc = ompi_common_ofacm_base_alloc_cts(context);
        if (OMPI_SUCCESS != rc) {
            return rc;
        }
    }
#endif

    return context->cpc->cbm_start_connect(context);
}

/*
 * Called during openib btl component close
 */
void ompi_common_ofacm_base_finalize(void)
{
    int i;

    if (NULL != available) {
        for (i = 0; NULL != available[i]; ++i) {
            if (NULL != available[i]->cbc_finalize) {
                available[i]->cbc_finalize();
            }
        }
        free(available);
        available = NULL;
    }
}
