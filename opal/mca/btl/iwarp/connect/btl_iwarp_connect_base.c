/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2007-2013 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2007      Mellanox Technologies, Inc.  All rights reserved.
 * Copyright (c) 2012-2018 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2013-2014 Intel, Inc. All rights reserved
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#include "opal_config.h"

#include "btl_iwarp.h"
#include "btl_iwarp_proc.h"
#include "connect/base.h"
#include "connect/btl_iwarp_connect_empty.h"
#include "connect/btl_iwarp_connect_rdmacm.h"

#include "opal/util/argv.h"
#include "opal/util/output.h"
#include "opal/util/proc.h"
#include "opal/util/show_help.h"

#include "opal/util/sys_limits.h"
#include "opal/align.h"

/*
 * Array of all possible connection functions
 */
static opal_btl_iwarp_connect_base_component_t *all[] = {
    /* Always have an entry here so that the CP indexes will always be
       the same: OOB has been removed, so use the "empty" CPC */
    &opal_btl_iwarp_connect_empty,

    /* Always have an entry here so that the CP indexes will always be
       the same: XOOB has been removed, so use the "empty" CPC */
    &opal_btl_iwarp_connect_empty,

    /* Always have an entry here so that the CP indexes will always be
       the same: if RDMA CM is not available, use the "empty" CPC */
    &opal_btl_iwarp_connect_rdmacm,
    &opal_btl_iwarp_connect_empty,
    NULL
};

/* increase this count if any more cpcs are added */
static opal_btl_iwarp_connect_base_component_t *available[5];
static int num_available = 0;

static char *btl_iwarp_cpc_include;
static char *btl_iwarp_cpc_exclude;

/*
 * Register MCA parameters
 */
int opal_btl_iwarp_connect_base_register(void)
{
    int i, j, save;
    char **temp = NULL, *string = NULL, *all_cpc_names = NULL;

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

    btl_iwarp_cpc_include = NULL;
    (void) mca_base_component_var_register(&mca_btl_iwarp_component.super.btl_version,
                                           "cpc_include", string, MCA_BASE_VAR_TYPE_STRING,
                                           NULL, 0, 0, OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &btl_iwarp_cpc_include);
    free(string);

    asprintf(&string,
             "Method used to exclude OpenFabrics connections (valid values: %s)",
             all_cpc_names);

    btl_iwarp_cpc_exclude = NULL;
    (void) mca_base_component_var_register(&mca_btl_iwarp_component.super.btl_version,
                                           "cpc_exclude", string, MCA_BASE_VAR_TYPE_STRING,
                                           NULL, 0, 0, OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &btl_iwarp_cpc_exclude);
    free(string);

    /* Parse the if_[in|ex]clude paramters to come up with a list of
       CPCs that are available */
    /* If we have an "include" list, then find all those CPCs and put
       them in available[] */
    if (NULL != btl_iwarp_cpc_include) {
        mca_btl_iwarp_component.cpc_explicitly_defined = true;
        temp = opal_argv_split(btl_iwarp_cpc_include, ',');
        for (save = j = 0; NULL != temp[j]; ++j) {
            for (i = 0; NULL != all[i]; ++i) {
                if (0 == strcmp(temp[j], all[i]->cbc_name)) {
                    opal_output(-1, "include: saving %s", all[i]->cbc_name);
                    available[save++] = all[i];
                    ++num_available;
                    break;
                }
            }
            if (NULL == all[i]) {
                opal_show_help("help-mpi-btl-iwarp-cpc-base.txt",
                               "cpc name not found", true,
                               "include", opal_process_info.nodename,
                               "include", btl_iwarp_cpc_include, temp[j],
                               all_cpc_names);
                opal_argv_free(temp);
                free(all_cpc_names);
                return OPAL_ERR_NOT_FOUND;
            }
        }
        opal_argv_free(temp);
    }

    /* Otherwise, if we have an "exclude" list, take all the CPCs that
       are not in that list and put them in available[] */
    else if (NULL != btl_iwarp_cpc_exclude) {
        mca_btl_iwarp_component.cpc_explicitly_defined = true;
        temp = opal_argv_split(btl_iwarp_cpc_exclude, ',');
        /* First: error check -- ensure that all the names are valid */
        for (j = 0; NULL != temp[j]; ++j) {
            for (i = 0; NULL != all[i]; ++i) {
                if (0 == strcmp(temp[j], all[i]->cbc_name)) {
                    break;
                }
            }
            if (NULL == all[i]) {
                opal_show_help("help-mpi-btl-iwarp-cpc-base.txt",
                               "cpc name not found", true,
                               "exclude", opal_process_info.nodename,
                               "exclude", btl_iwarp_cpc_exclude, temp[j],
                               all_cpc_names);
                opal_argv_free(temp);
                free(all_cpc_names);
                return OPAL_ERR_NOT_FOUND;
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
                opal_output(-1, "exclude: saving %s", all[i]->cbc_name);
                available[save++] = all[i];
                ++num_available;
            }
        }
        opal_argv_free(temp);
    }

    /* If there's no include/exclude list, copy all[] into available[] */
    else {
        opal_output(-1, "no include or exclude: saving all");
        memcpy(available, all, sizeof(all));
        num_available = (sizeof(all) /
                         sizeof(opal_btl_iwarp_connect_base_module_t *)) - 1;
    }

    /* Call the register function on all the CPCs so that they may
       setup any MCA params specific to the connection type */
    for (i = 0; NULL != available[i]; ++i) {
        if (NULL != available[i]->cbc_register) {
            available[i]->cbc_register();
        }
    }

    free (all_cpc_names);
    return OPAL_SUCCESS;
}

/*
 * Called once during iwarp BTL component initialization to allow CPC
 * components to initialize.
 */
int opal_btl_iwarp_connect_base_init(void)
{
    int i, rc;

    /* Call each available CPC component's open function, if it has
       one.  If the CPC component open function returns OPAL_SUCCESS,
       keep it.  If it returns ERR_NOT_SUPPORTED, remove it from the
       available[] array.  If it returns something else, return that
       error upward. */
    for (i = num_available = 0; NULL != available[i]; ++i) {
        if (NULL == available[i]->cbc_init) {
            available[num_available++] = available[i];
            opal_output(-1, "found available cpc (NULL init): %s",
                        all[i]->cbc_name);
            continue;
        }

        rc = available[i]->cbc_init();
        if (OPAL_SUCCESS == rc) {
            available[num_available++] = available[i];
            opal_output(-1, "found available cpc (SUCCESS init): %s",
                        all[i]->cbc_name);
            continue;
        } else if (OPAL_ERR_NOT_SUPPORTED == rc) {
            continue;
        } else {
            return rc;
        }
    }
    available[num_available] = NULL;

    return (num_available > 0) ? OPAL_SUCCESS : OPAL_ERR_NOT_AVAILABLE;
}


/*
 * Find all the CPCs that are eligible for a single local port (i.e.,
 * iwarp module).
 */
int opal_btl_iwarp_connect_base_select_for_local_port(mca_btl_iwarp_module_t *btl)
{
    char *msg = NULL;
    int i, rc, cpc_index, len;
    opal_btl_iwarp_connect_base_module_t **cpcs;

    cpcs = (opal_btl_iwarp_connect_base_module_t **) calloc(num_available,
                  sizeof(opal_btl_iwarp_connect_base_module_t *));
    if (NULL == cpcs) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    /* Go through all available CPCs and query them to see if they
       want to run on this module.  If they do, save them to a running
       array. */
    for (len = 1, i = 0; NULL != available[i]; ++i) {
        len += strlen(available[i]->cbc_name) + 2;
    }
    msg = (char *) malloc(len);
    if (NULL == msg) {
        free(cpcs);
        return OPAL_ERR_OUT_OF_RESOURCE;
    }
    msg[0] = '\0';
    for (cpc_index = i = 0; NULL != available[i]; ++i) {
        if (i > 0) {
            strcat(msg, ", ");
        }
        strcat(msg, available[i]->cbc_name);

        rc = available[i]->cbc_query(btl, &cpcs[cpc_index]);
        if (OPAL_ERR_NOT_SUPPORTED == rc || OPAL_ERR_UNREACH == rc) {
            continue;
        } else if (OPAL_SUCCESS != rc) {
            free(cpcs);
            free(msg);
            return rc;
        }
        opal_output(-1, "match cpc for local port: %s",
                    available[i]->cbc_name);

        /* If the CPC wants to use the CTS protocol, check to ensure
           that QP 0 is PP; if it's not, we can't use this CPC (or the
           CTS protocol) */
        if (cpcs[cpc_index]->cbm_uses_cts &&
            !BTL_IWARP_QP_TYPE_PP(0)) {
            BTL_VERBOSE(("this CPC only supports when the first btl_iwarp_receive_queues QP is a PP QP"));
            continue;
        }

        /* This CPC has indicated that it wants to run on this iwarp
           BTL module.  Woo hoo! */
        ++cpc_index;
    }

    /* If we got an empty array, then no CPCs were eligible.  Doh! */
    if (0 == cpc_index) {
        opal_show_help("help-mpi-btl-iwarp-cpc-base.txt",
                       "no cpcs for port", true,
                       opal_process_info.nodename,
                       ibv_get_device_name(btl->device->ib_dev),
                       btl->port_num, msg);
        free(cpcs);
        free(msg);
        return OPAL_ERR_NOT_SUPPORTED;
    }
    free(msg);

    /* We got at least one eligible CPC; save the array into the
       module's port_info */
    btl->cpcs = cpcs;
    btl->num_cpcs = cpc_index;

    return OPAL_SUCCESS;
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
opal_btl_iwarp_connect_base_find_match(mca_btl_iwarp_module_t *btl,
                                        mca_btl_iwarp_proc_modex_t *peer_port,
                                        opal_btl_iwarp_connect_base_module_t **ret_local_cpc,
                                        opal_btl_iwarp_connect_base_module_data_t **ret_remote_cpc_data)
{
    int i, j, max = -1;
    opal_btl_iwarp_connect_base_module_t *local_cpc, *local_selected = NULL;
    opal_btl_iwarp_connect_base_module_data_t *local_cpcd, *remote_cpcd,
        *remote_selected = NULL;

    /* Iterate over all the CPCs on the local module */
    for (i = 0; i < btl->num_cpcs; ++i) {
        local_cpc = btl->cpcs[i];
        local_cpcd = &(local_cpc->data);

        /* Iterate over all the CPCs on the remote port */
        for (j = 0; j < peer_port->pm_cpc_data_count; ++j) {
            remote_cpcd = &(peer_port->pm_cpc_data[j]);

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
        opal_output(-1, "find_match: found match!");
        return OPAL_SUCCESS;
    } else {
        opal_output(-1, "find_match: did NOT find match!");
        return OPAL_ERR_NOT_FOUND;
    }
}

/*
 * Lookup a CPC component's index in the all[] array so that we can
 * send it int the modex
 */
int opal_btl_iwarp_connect_base_get_cpc_index(opal_btl_iwarp_connect_base_component_t *cpc)
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
opal_btl_iwarp_connect_base_component_t *
opal_btl_iwarp_connect_base_get_cpc_byindex(uint8_t index)
{
    return (index >= (sizeof(all) /
                      sizeof(opal_btl_iwarp_connect_base_module_t *))) ?
        NULL : all[index];
}

int opal_btl_iwarp_connect_base_alloc_cts(mca_btl_base_endpoint_t *endpoint)
{
    opal_free_list_item_t *fli;
    int length = sizeof(mca_btl_iwarp_header_t) +
        sizeof(mca_btl_iwarp_header_coalesced_t) +
        sizeof(mca_btl_iwarp_control_header_t) +
        sizeof(mca_btl_iwarp_footer_t) +
        mca_btl_iwarp_component.qp_infos[mca_btl_iwarp_component.credits_qp].size;

    int align_it = 0;
    int page_size;

    page_size = opal_getpagesize();
    if (length >= page_size / 2) { align_it = 1; }
    if (align_it) {
// I think this is only active for ~64k+ buffers anyway, but I'm not
// positive, so I'm only increasing the buffer size and alignment if
// it's not too small. That way we'd avoid wasting excessive memory
// in case this code was active for tiny buffers.
        length = OPAL_ALIGN(length, page_size, int);
    }

    /* Explicitly don't use the mpool registration */
    fli = &(endpoint->endpoint_cts_frag.super.super.base.super);
    fli->registration = NULL;
    if (!align_it) {
        fli->ptr = malloc(length);
    } else {
        posix_memalign((void**)&(fli->ptr), page_size, length);
    }
    if (NULL == fli->ptr) {
        BTL_ERROR(("malloc failed"));
        return OPAL_ERR_OUT_OF_RESOURCE;
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
        return OPAL_ERR_OUT_OF_RESOURCE;
    }
    /* NOTE: We do not need to register this memory with the
       opal_memory subsystem, because this is OMPI-controlled memory
       -- we do not need to worry about this memory being freed out
       from underneath us. */

    /* Copy the lkey where it needs to go */
    endpoint->endpoint_cts_frag.super.sg_entry.lkey =
        endpoint->endpoint_cts_mr->lkey;
    endpoint->endpoint_cts_frag.super.sg_entry.length = length;

    /* Construct the rest of the recv_frag_t */
    OBJ_CONSTRUCT(&(endpoint->endpoint_cts_frag), mca_btl_iwarp_recv_frag_t);
    endpoint->endpoint_cts_frag.super.super.base.order =
        mca_btl_iwarp_component.credits_qp;
    endpoint->endpoint_cts_frag.super.endpoint = endpoint;
    OPAL_OUTPUT((-1, "Got a CTS frag for peer %s, addr %p, length %d, lkey %d",
                 opal_get_proc_hostname(endpoint->endpoint_proc->proc_opal),
                 (void*) endpoint->endpoint_cts_frag.super.sg_entry.addr,
                 endpoint->endpoint_cts_frag.super.sg_entry.length,
                 endpoint->endpoint_cts_frag.super.sg_entry.lkey));

    return OPAL_SUCCESS;
}

int opal_btl_iwarp_connect_base_free_cts(mca_btl_base_endpoint_t *endpoint)
{
    /* NOTE: We don't need to deregister this memory with opal_memory
       because it was not registered there in the first place (see
       comment above, near call to ibv_reg_mr). */
    if (NULL != endpoint->endpoint_cts_mr) {
        ibv_dereg_mr(endpoint->endpoint_cts_mr);
        endpoint->endpoint_cts_mr = NULL;
    }
    if (NULL != endpoint->endpoint_cts_frag.super.super.base.super.ptr) {
        free(endpoint->endpoint_cts_frag.super.super.base.super.ptr);
        endpoint->endpoint_cts_frag.super.super.base.super.ptr = NULL;
        OPAL_OUTPUT((-1, "Freeing CTS frag"));
    }

    return OPAL_SUCCESS;
}

/*
 * Called to start a connection
 */
int opal_btl_iwarp_connect_base_start(
        opal_btl_iwarp_connect_base_module_t *cpc,
        mca_btl_base_endpoint_t *endpoint)
{
    /* If the CPC uses the CTS protocol, provide a frag buffer for the
       CPC to post.  Must allocate these frags up here in the main
       thread because the FREE_LIST_WAIT is not thread safe. */
    if (cpc->cbm_uses_cts) {
        int rc;
        rc = opal_btl_iwarp_connect_base_alloc_cts(endpoint);
        if (OPAL_SUCCESS != rc) {
            return rc;
        }
    }

    return cpc->cbm_start_connect(cpc, endpoint);
}

/*
 * Called during iwarp btl component close
 */
void opal_btl_iwarp_connect_base_finalize(void)
{
    int i;

    for (i = 0 ; i < num_available ; ++i) {
        if (NULL != available[i]->cbc_finalize) {
            available[i]->cbc_finalize();
        }
    }
}
