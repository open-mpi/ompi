/*
 * Copyright (c) 2007 Cisco, Inc.  All rights reserved.
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
#include "connect/base.h"
#include "connect/btl_openib_connect_oob.h"
#include "connect/btl_openib_connect_xoob.h"
#include "connect/btl_openib_connect_rdma_cm.h"
#include "connect/btl_openib_connect_ibcm.h"

#include "opal/util/argv.h"
#include "opal/util/show_help.h"

/*
 * Global variable with the selected function pointers in it
 */
ompi_btl_openib_connect_base_funcs_t ompi_btl_openib_connect = {
    "",
    /* Compiler fills in the rest with NULL */
};

/*
 * Array of all possible connection functions
 */
static ompi_btl_openib_connect_base_funcs_t *all[] = {
    &ompi_btl_openib_connect_oob,
#if HAVE_XRC
    &ompi_btl_openib_connect_xoob,
#endif
    &ompi_btl_openib_connect_rdma_cm,
    &ompi_btl_openib_connect_ibcm,
    NULL
};

/*
 * MCA parameter value
 */
static char *cpc_include = NULL;
static char *cpc_exclude = NULL;

/*
 * Register MCA parameters
 */
int ompi_btl_openib_connect_base_open(void)
{
    int i;
    char **temp, *list, *string;

    /* Make an MCA parameter to select which connect module to use */
    temp = NULL;
    for (i = 0; NULL != all[i]; ++i) {
        opal_argv_append_nosize(&temp, all[i]->bcf_name);
    }
    list = opal_argv_join(temp, ',');
    opal_argv_free(temp);
    asprintf(&string,
             "Method used to select OpenFabrics connections (valid values: %s)",
             list);

    mca_base_param_reg_string(&mca_btl_openib_component.super.btl_version,
                "cpc_include", string, false, false, NULL, &cpc_include);
    free(string);

    asprintf(&string,
             "Method used to exclude OpenFabrics connections (valid values: %s)",
             list);

    mca_base_param_reg_string(&mca_btl_openib_component.super.btl_version,
                "cpc_exclude", string, false, false, NULL, &cpc_exclude);
    free(list);
    free(string);

    /* Call the open function on all the connect modules so that they
     * may setup any MCA params specific to the connection type
     */
    for (i = 0; NULL != all[i]; ++i) {
        if (NULL != all[i]->bcf_open) {
            all[i]->bcf_open();
        }
    }

    return OMPI_SUCCESS;
}

/* 
 * The connection method is chosen by comparing the lists passed around
 * to all nodes via modex with the list generated locally.  Any
 * non-negative number is a potentially valid connection method.  The
 * method below of determining the optimal connection method is to take
 * the cross-section of the two lists.  The highest single value (and
 * the other side being non-negative) is selected as the cpc method.
 */
int ompi_btl_openib_connect_base_select(char *remotelist, char *locallist)
{
    int i, j, max = -1;
    char **localist_formatted, **remotelist_formatted;
    char *name;

    BTL_VERBOSE(("remotelist = %s locallist = %s", remotelist, locallist));

    localist_formatted = opal_argv_split(locallist, ',');
    remotelist_formatted = opal_argv_split(remotelist, ',');

    for (i = 0; NULL != localist_formatted[i] && NULL != localist_formatted[i+1]; i+=2) {
        for (j = 0; NULL != remotelist_formatted[j] && NULL != remotelist_formatted[j+1]; j+=2) {
            int local_val, remote_val;

            local_val = atoi(localist_formatted[i+1]);
            remote_val = atoi(remotelist_formatted[j+1]);

            if (0 == strcmp(localist_formatted[i], remotelist_formatted[j]) &&
                (-1 != local_val && -1 != remote_val)) {
                if (local_val > max) {
                    max = local_val;
                    name = localist_formatted[i];
                }
                if (remote_val > max) {
                    max = remote_val;
                    name = remotelist_formatted[j];
                }
            }
        }
    }

    if (-1 == max) {
        BTL_ERROR(("Failed to find any working connections"));
        return OMPI_ERROR;
    }

    for (i = 0; NULL != all[i]; i++) {
        if (0 == strcmp(all[i]->bcf_name, name)) {
            int rc;

            rc = all[i]->bcf_init();
            if (OMPI_SUCCESS != rc) {
                BTL_ERROR(("A problem was encountered with %s, ignoring this cpc", all[i]->bcf_name));
                return OMPI_ERROR;
            }

            ompi_btl_openib_connect = *(all[i]);
            break;
        }
    }

    BTL_VERBOSE(("%s selected as transport", all[i]->bcf_name));

    opal_argv_free(localist_formatted);
    opal_argv_free(remotelist_formatted);

    return OMPI_SUCCESS;
}

static inline int cpc_specific_query(char ***cpclist, mca_btl_openib_hca_t *hca, int cpc_counter, bool *valid)
{
    char *temp;
    int rc;

    if (NULL == all[cpc_counter]->bcf_query) {
        return OMPI_SUCCESS;
    }

    rc = all[cpc_counter]->bcf_query(hca);
    if (rc > 0) {
        *valid = 1;
    }

    asprintf(&temp, "%s,%d", all[cpc_counter]->bcf_name, rc);
    opal_argv_append_nosize(cpclist, temp);
    return OMPI_SUCCESS;
}

int ompi_btl_openib_connect_base_query(char **cpclist, mca_btl_openib_hca_t *hca)
{
    int i, rc;
    bool valid = 0;
    char **cpclist_include, **cpclist_exclude, **namepriority_list = NULL;

    cpclist_include = opal_argv_split(cpc_include, ',');
    cpclist_exclude = opal_argv_split(cpc_exclude, ',');

    /* Go through all the CMs to create a list of usable CPCs */
    for (i = 0; NULL != all[i]; ++i) {
        if (NULL != cpclist_include) {
            int j;
            for (j = 0; NULL != cpclist_include[j]; ++j) {
                if (0 == strcmp(cpclist_include[j], all[i]->bcf_name)) { 
                    rc = cpc_specific_query(&namepriority_list, hca, i, &valid);
                    if (OMPI_ERROR == rc) {
                        return OMPI_ERROR;
                    }
                }
            }
        } else if (NULL != cpclist_exclude) {
            int j;
            for (j = 0; NULL != cpclist_exclude[j]; ++j) {
                if (0 != strcmp(cpclist_exclude[j], all[i]->bcf_name)) {
                    rc = cpc_specific_query(&namepriority_list, hca, i, &valid);
                    if (OMPI_ERROR == rc) {
                        return OMPI_ERROR;
                    }
                }
            }
        } else {
            rc = cpc_specific_query(&namepriority_list, hca, i, &valid);
            if (OMPI_ERROR == rc) {
                return OMPI_ERROR;
            }
        }
    }

    if (0 == valid) {
        BTL_ERROR(("Failed to find any valid connections for %s, not "
                   "using it for this run",
                   ibv_get_device_name(hca->ib_dev)));
        return OMPI_ERROR;
    }

    *cpclist = opal_argv_join(namepriority_list, ',');
    opal_argv_free(namepriority_list);

    return OMPI_SUCCESS;
}
