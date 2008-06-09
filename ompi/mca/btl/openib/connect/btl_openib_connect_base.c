/*
 * Copyright (c) 2007-2008 Cisco, Inc.  All rights reserved.
 * Copyright (c) 2007 Mellanox Technologies, Inc.  All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
#include "ompi_config.h"

#include "btl_openib.h"
#include "btl_openib_proc.h"
#include "connect/base.h"
#include "connect/btl_openib_connect_oob.h"
#include "connect/btl_openib_connect_empty.h"
#if HAVE_XRC
#include "connect/btl_openib_connect_xoob.h"
#endif
#if OMPI_HAVE_RDMACM
#include "connect/btl_openib_connect_rdmacm.h"
#endif
#if OMPI_HAVE_IBCM
#include "connect/btl_openib_connect_ibcm.h"
#endif

#include "orte/util/show_help.h"
#include "opal/util/argv.h"

/*
 * Array of all possible connection functions
 */
static ompi_btl_openib_connect_base_component_t *all[] = {
    &ompi_btl_openib_connect_oob,

    /* Always have an entry here so that the CP indexes will always be
       the same: if XRC is not available, use the "empty" CPC */
#if HAVE_XRC
    &ompi_btl_openib_connect_xoob,
#else
    &ompi_btl_openib_connect_empty,
#endif

    /* Always have an entry here so that the CP indexes will always be
       the same: if RDMA CM is not available, use the "empty" CPC */
#if OMPI_HAVE_RDMACM
    &ompi_btl_openib_connect_rdmacm,
#else
    &ompi_btl_openib_connect_empty,
#endif

    /* Always have an entry here so that the CP indexes will always be
       the same: if IB CM is not available, use the "empty" CPC */
#if OMPI_HAVE_IBCM
    &ompi_btl_openib_connect_ibcm,
#else
    &ompi_btl_openib_connect_empty,
#endif

    NULL
};
static ompi_btl_openib_connect_base_component_t **available = NULL;
static int num_available = 0;

/*
 * Register MCA parameters
 */
int ompi_btl_openib_connect_base_register(void)
{
    int i, j, save;
    char **temp = NULL, *string = NULL, *all_cpc_names = NULL;
    char *cpc_include = NULL, *cpc_exclude = NULL;

    /* Make an MCA parameter to select which connect module to use */
    for (i = 0; NULL != all[i]; ++i) {
        opal_argv_append_nosize(&temp, all[i]->cbc_name);
    }
    all_cpc_names = opal_argv_join(temp, ',');
    opal_argv_free(temp);
    asprintf(&string,
             "Method used to select OpenFabrics connections (valid values: %s)",
             all_cpc_names);

    mca_base_param_reg_string(&mca_btl_openib_component.super.btl_version,
                              "cpc_include", string, false, false, 
                              NULL, &cpc_include);
    free(string);

    asprintf(&string,
             "Method used to exclude OpenFabrics connections (valid values: %s)",
             all_cpc_names);

    mca_base_param_reg_string(&mca_btl_openib_component.super.btl_version,
                              "cpc_exclude", string, false, false, 
                              NULL, &cpc_exclude);
    free(string);

    /* Parse the if_[in|ex]clude paramters to come up with a list of
       CPCs that are available */
    available = calloc(1, sizeof(all));

    /* If we have an "include" list, then find all those CPCs and put
       them in available[] */
    if (NULL != cpc_include) {
        temp = opal_argv_split(cpc_include, ',');
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
                orte_show_help("help-mpi-btl-openib-cpc-base.txt",
                               "cpc name not found", true,
                               "include", orte_process_info.nodename,
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
        temp = opal_argv_split(cpc_exclude, ',');
        /* First: error check -- ensure that all the names are valid */
        for (j = 0; NULL != temp[j]; ++j) {
            for (i = 0; NULL != all[i]; ++i) {
                if (0 == strcmp(temp[j], all[i]->cbc_name)) { 
                    break;
                }
            }
            if (NULL == all[i]) {
                orte_show_help("help-mpi-btl-openib-cpc-base.txt",
                               "cpc name not found", true,
                               "exclude", orte_process_info.nodename,
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
                         sizeof(ompi_btl_openib_connect_base_module_t *)) - 1;
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
int ompi_btl_openib_connect_base_init(void)
{
    int i, rc;

    /* Call each available CPC component's open function, if it has
       one.  If the CPC component open function returns OMPI_SUCCESS,
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
        if (OMPI_SUCCESS == rc) {
            available[num_available++] = available[i];
            opal_output(-1, "found available cpc (SUCCESS init): %s",
                        all[i]->cbc_name);
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
int ompi_btl_openib_connect_base_select_for_local_port(mca_btl_openib_module_t *btl)
{
    char *msg = NULL;
    int i, rc, cpc_index, len;
    ompi_btl_openib_connect_base_module_t **cpcs;

    cpcs = calloc(num_available, 
                  sizeof(ompi_btl_openib_connect_base_module_t *));
    if (NULL == cpcs) {
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

        rc = available[i]->cbc_query(btl, &cpcs[cpc_index]);
        if (OMPI_ERR_NOT_SUPPORTED == rc) {
            continue;
        } else if (OMPI_SUCCESS != rc) {
            free(cpcs);
            free(msg);
            return rc;
        }
        opal_output(-1, "match cpc for local port: %s",
                    available[i]->cbc_name);

        /* This CPC has indicated that it wants to run on this openib
           BTL module.  Woo hoo! */
        ++cpc_index;
    }

    /* If we got an empty array, then no CPCs were eligible.  Doh! */
    if (0 == cpc_index) {
        orte_show_help("help-mpi-btl-openib-cpc-base.txt",
                       "no cpcs for port", true,
                       orte_process_info.nodename,
                       ibv_get_device_name(btl->hca->ib_dev),
                       msg);
        free(cpcs);
        free(msg);
        return OMPI_ERR_NOT_SUPPORTED;
    }
    free(msg);

    /* We got at least one eligible CPC; save the array into the
       module's port_info */
    btl->cpcs = cpcs;
    btl->num_cpcs = cpc_index;

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
ompi_btl_openib_connect_base_find_match(mca_btl_openib_module_t *btl,
                                        mca_btl_openib_proc_modex_t *peer_port,
                                        ompi_btl_openib_connect_base_module_t **ret_local_cpc,
                                        ompi_btl_openib_connect_base_module_data_t **ret_remote_cpc_data)
{
    int i, j, max = -1;
    ompi_btl_openib_connect_base_module_t *local_cpc, *local_selected = NULL;
    ompi_btl_openib_connect_base_module_data_t *local_cpcd, *remote_cpcd,
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
        return OMPI_SUCCESS;
    } else {
        opal_output(-1, "find_match: did NOT find match!");
        return OMPI_ERR_NOT_FOUND;
    }
}

/*
 * Lookup a CPC component's index in the all[] array so that we can
 * send it int the modex
 */
int ompi_btl_openib_connect_base_get_cpc_index(ompi_btl_openib_connect_base_component_t *cpc)
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
ompi_btl_openib_connect_base_component_t *
ompi_btl_openib_connect_base_get_cpc_byindex(uint8_t index)
{
    return (index >= (sizeof(all) / 
                      sizeof(ompi_btl_openib_connect_base_module_t *))) ?
        NULL : all[index];
}

/*
 * Called during openib btl component close
 */
void ompi_btl_openib_connect_base_finalize(void)
{
    int i;

    if (NULL != available) {
        for (i = 0; NULL != available[i]; ++i) {
            if (NULL != available[i]->cbc_finalize) {
                available[i]->cbc_finalize();
            }
        }
        free(available);
    }
}
