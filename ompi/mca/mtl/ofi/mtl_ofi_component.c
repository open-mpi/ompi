/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2013-2018 Intel, Inc. All rights reserved
 *
 * Copyright (c) 2014-2017 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2015-2016 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "mtl_ofi.h"
#include "opal/util/argv.h"
#include "opal/util/show_help.h"

static int ompi_mtl_ofi_component_open(void);
static int ompi_mtl_ofi_component_query(mca_base_module_t **module, int *priority);
static int ompi_mtl_ofi_component_close(void);
static int ompi_mtl_ofi_component_register(void);

static mca_mtl_base_module_t*
ompi_mtl_ofi_component_init(bool enable_progress_threads,
                            bool enable_mpi_threads);

static int param_priority;
static char *prov_include;
static char *prov_exclude;
static int control_progress;
static int data_progress;
static int av_type;
static int ofi_tag_mode;

/*
 * Enumerators
 */

enum {
    MTL_OFI_PROG_AUTO=1,
    MTL_OFI_PROG_MANUAL,
    MTL_OFI_PROG_UNSPEC,
};

mca_base_var_enum_value_t control_prog_type[] = {
    {MTL_OFI_PROG_AUTO, "auto"},
    {MTL_OFI_PROG_MANUAL, "manual"},
    {MTL_OFI_PROG_UNSPEC, "unspec"},
    {0, NULL}
};

mca_base_var_enum_value_t data_prog_type[] = {
    {MTL_OFI_PROG_AUTO, "auto"},
    {MTL_OFI_PROG_MANUAL, "manual"},
    {MTL_OFI_PROG_UNSPEC, "unspec"},
    {0, NULL}
};

enum {
    MTL_OFI_AV_MAP=1,
    MTL_OFI_AV_TABLE,
    MTL_OFI_AV_UNKNOWN,
};

mca_base_var_enum_value_t av_table_type[] = {
    {MTL_OFI_AV_MAP, "map"},
    {MTL_OFI_AV_TABLE, "table"},
    {0, NULL}
};

enum {
    MTL_OFI_TAG_AUTO=1,
    MTL_OFI_TAG_1,
    MTL_OFI_TAG_2,
    MTL_OFI_TAG_FULL,
};

mca_base_var_enum_value_t ofi_tag_mode_type[] = {
    {MTL_OFI_TAG_AUTO, "auto"},
    {MTL_OFI_TAG_1, "ofi_tag_1"},
    {MTL_OFI_TAG_2, "ofi_tag_2"},
    {MTL_OFI_TAG_FULL, "ofi_tag_full"},
    {0, NULL}
};

mca_mtl_ofi_component_t mca_mtl_ofi_component = {
    {

        /* First, the mca_base_component_t struct containing meta
         * information about the component itself */

        .mtl_version = {
            MCA_MTL_BASE_VERSION_2_0_0,

            .mca_component_name = "ofi",
            OFI_COMPAT_MCA_VERSION,
            .mca_open_component = ompi_mtl_ofi_component_open,
            .mca_close_component = ompi_mtl_ofi_component_close,
            .mca_query_component = ompi_mtl_ofi_component_query,
            .mca_register_component_params = ompi_mtl_ofi_component_register,
        },
        .mtl_data = {
            /* The component is not checkpoint ready */
            MCA_BASE_METADATA_PARAM_NONE
        },

        .mtl_init = ompi_mtl_ofi_component_init,
    }
};

static int
ompi_mtl_ofi_component_register(void)
{
    int ret;
    mca_base_var_enum_t *new_enum = NULL;
    char *desc;

    param_priority = 25;   /* for now give a lower priority than the psm mtl */
    mca_base_component_var_register(&mca_mtl_ofi_component.super.mtl_version,
                                    "priority", "Priority of the OFI MTL component",
                                    MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                    OPAL_INFO_LVL_9,
                                    MCA_BASE_VAR_SCOPE_READONLY,
                                    &param_priority);

    prov_include = NULL;
    mca_base_component_var_register(&mca_mtl_ofi_component.super.mtl_version,
                                    "provider_include",
                                    "Comma-delimited list of OFI providers that are considered for use (e.g., \"psm,psm2\"; an empty value means that all providers will be considered). Mutually exclusive with mtl_ofi_provider_exclude.",
                                    MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0,
                                    OPAL_INFO_LVL_1,
                                    MCA_BASE_VAR_SCOPE_READONLY,
                                    &prov_include);

    prov_exclude = "shm,sockets,tcp,udp,rstream";
    mca_base_component_var_register(&mca_mtl_ofi_component.super.mtl_version,
                                    "provider_exclude",
                                    "Comma-delimited list of OFI providers that are not considered for use (default: \"sockets,mxm\"; empty value means that all providers will be considered). Mutually exclusive with mtl_ofi_provider_include.",
                                    MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0,
                                    OPAL_INFO_LVL_1,
                                    MCA_BASE_VAR_SCOPE_READONLY,
                                    &prov_exclude);

    ompi_mtl_ofi.ofi_progress_event_count = 100;
    asprintf(&desc, "Max number of events to read each call to OFI progress (default: %d events will be read per OFI progress call)", ompi_mtl_ofi.ofi_progress_event_count);
    mca_base_component_var_register(&mca_mtl_ofi_component.super.mtl_version,
                                    "progress_event_cnt",
                                    desc,
                                    MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                    OPAL_INFO_LVL_6,
                                    MCA_BASE_VAR_SCOPE_READONLY,
                                    &ompi_mtl_ofi.ofi_progress_event_count);

    free(desc);

    ret = mca_base_var_enum_create ("ofi_tag_mode_type", ofi_tag_mode_type , &new_enum);
    if (OPAL_SUCCESS != ret) {
        return ret;
    }

    ofi_tag_mode = MTL_OFI_TAG_AUTO;
    asprintf(&desc, "Mode specifying how many bits to use for various MPI values in OFI/Libfabric"
            " communications. Some Libfabric provider network types can support most of Open MPI"
            " needs; others can only supply a limited number of bits, which then must be split"
            " across the MPI communicator ID, MPI source rank, and MPI tag. Three different"
            " splitting schemes are available: ofi_tag_full (%d bits for the communicator, %d bits"
            " for the source rank, and %d bits for the tag), ofi_tag_1 (%d bits for the communicator"
            ", %d bits source rank, %d bits tag), ofi_tag_2 (%d bits for the communicator"
            ", %d bits source rank, %d bits tag). By default, this MCA variable is set to \"auto\","
            " which will first try to use ofi_tag_full, and if that fails, fall back to ofi_tag_1.",
            MTL_OFI_CID_BIT_COUNT_DATA, 32, MTL_OFI_TAG_BIT_COUNT_DATA,
            MTL_OFI_CID_BIT_COUNT_1, MTL_OFI_SOURCE_BIT_COUNT_1, MTL_OFI_TAG_BIT_COUNT_1,
            MTL_OFI_CID_BIT_COUNT_2, MTL_OFI_SOURCE_BIT_COUNT_2, MTL_OFI_TAG_BIT_COUNT_2);

    mca_base_component_var_register (&mca_mtl_ofi_component.super.mtl_version,
                                    "tag_mode",
                                     desc,
                                     MCA_BASE_VAR_TYPE_INT, new_enum, 0, 0,
                                     OPAL_INFO_LVL_6,
                                     MCA_BASE_VAR_SCOPE_READONLY,
                                     &ofi_tag_mode);

    free(desc);
    OBJ_RELEASE(new_enum);

    ret = mca_base_var_enum_create ("control_prog_type", control_prog_type, &new_enum);
    if (OPAL_SUCCESS != ret) {
        return ret;
    }

    control_progress = MTL_OFI_PROG_UNSPEC;
    mca_base_component_var_register (&mca_mtl_ofi_component.super.mtl_version,
                                     "control_progress",
                                     "Specify control progress model (default: unspecificed, use provider's default). Set to auto or manual for auto or manual progress respectively.",
                                     MCA_BASE_VAR_TYPE_INT, new_enum, 0, 0,
                                     OPAL_INFO_LVL_3,
                                     MCA_BASE_VAR_SCOPE_READONLY,
                                     &control_progress);
    OBJ_RELEASE(new_enum);

    ret = mca_base_var_enum_create ("data_prog_type", data_prog_type, &new_enum);
    if (OPAL_SUCCESS != ret) {
        return ret;
    }

    data_progress = MTL_OFI_PROG_UNSPEC;
    mca_base_component_var_register(&mca_mtl_ofi_component.super.mtl_version,
                                    "data_progress",
                                    "Specify data progress model (default: unspecified, use provider's default). Set to auto or manual for auto or manual progress respectively.",
                                    MCA_BASE_VAR_TYPE_INT, new_enum, 0, 0,
                                    OPAL_INFO_LVL_3,
                                    MCA_BASE_VAR_SCOPE_READONLY,
                                    &data_progress);
    OBJ_RELEASE(new_enum);

    ret = mca_base_var_enum_create ("av_type", av_table_type, &new_enum);
    if (OPAL_SUCCESS != ret) {
        return ret;
    }

    av_type = MTL_OFI_AV_MAP;
    mca_base_component_var_register (&mca_mtl_ofi_component.super.mtl_version,
                                     "av",
                                     "Specify AV type to use (default: map). Set to table for FI_AV_TABLE AV type.",
                                     MCA_BASE_VAR_TYPE_INT, new_enum, 0, 0,
                                     OPAL_INFO_LVL_3,
                                     MCA_BASE_VAR_SCOPE_READONLY,
                                     &av_type);
    OBJ_RELEASE(new_enum);

    return OMPI_SUCCESS;
}



static int
ompi_mtl_ofi_component_open(void)
{
    ompi_mtl_ofi.base.mtl_request_size =
        sizeof(ompi_mtl_ofi_request_t) - sizeof(struct mca_mtl_request_t);

    ompi_mtl_ofi.domain =  NULL;
    ompi_mtl_ofi.av     =  NULL;
    ompi_mtl_ofi.cq     =  NULL;
    ompi_mtl_ofi.ep     =  NULL;

    /**
     * Sanity check: provider_include and provider_exclude must be mutually
     * exclusive
     */
    if (OMPI_SUCCESS !=
        mca_base_var_check_exclusive("ompi",
            mca_mtl_ofi_component.super.mtl_version.mca_type_name,
            mca_mtl_ofi_component.super.mtl_version.mca_component_name,
            "provider_include",
            mca_mtl_ofi_component.super.mtl_version.mca_type_name,
            mca_mtl_ofi_component.super.mtl_version.mca_component_name,
            "provider_exclude")) {
        return OMPI_ERR_NOT_AVAILABLE;
    }

    return OMPI_SUCCESS;
}

static int
ompi_mtl_ofi_component_query(mca_base_module_t **module, int *priority)
{
    *priority = param_priority;
    *module = (mca_base_module_t *)&ompi_mtl_ofi.base;
    return OMPI_SUCCESS;
}

static int
ompi_mtl_ofi_component_close(void)
{
    return OMPI_SUCCESS;
}

int
ompi_mtl_ofi_progress_no_inline(void)
{
	return ompi_mtl_ofi_progress();
}

static int
is_in_list(char **list, char *item)
{
    int i = 0;

    if ((NULL == list) || (NULL == item)) {
        return 0;
    }

    while (NULL != list[i]) {
        if (0 == strncmp(item, list[i], strlen(list[i]))) {
            return 1;
        } else {
            i++;
        }
    }

    return 0;
}

static struct fi_info*
select_ofi_provider(struct fi_info *providers)
{
    char **include_list = NULL;
    char **exclude_list = NULL;
    struct fi_info *prov = providers;

    opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                        "%s:%d: mtl:ofi:provider_include = \"%s\"\n",
                        __FILE__, __LINE__, prov_include);
    opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                        "%s:%d: mtl:ofi:provider_exclude = \"%s\"\n",
                        __FILE__, __LINE__, prov_exclude);

    if (NULL != prov_include) {
        include_list = opal_argv_split(prov_include, ',');
        while ((NULL != prov) &&
               (!is_in_list(include_list, prov->fabric_attr->prov_name))) {
            opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                                "%s:%d: mtl:ofi: \"%s\" not in include list\n",
                                __FILE__, __LINE__,
                                prov->fabric_attr->prov_name);
            prov = prov->next;
        }
    } else if (NULL != prov_exclude) {
        exclude_list = opal_argv_split(prov_exclude, ',');
        while ((NULL != prov) &&
               (is_in_list(exclude_list, prov->fabric_attr->prov_name))) {
            opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                                "%s:%d: mtl:ofi: \"%s\" in exclude list\n",
                                __FILE__, __LINE__,
                                prov->fabric_attr->prov_name);
            prov = prov->next;
        }
    }

    opal_argv_free(include_list);
    opal_argv_free(exclude_list);

    opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                        "%s:%d: mtl:ofi:prov: %s\n",
                        __FILE__, __LINE__,
                        (prov ? prov->fabric_attr->prov_name : "none"));

    return prov;
}

/* Check if FI_REMOTE_CQ_DATA is supported, if so send the source rank there
 * FI_DIRECTED_RECV is also needed so receives can discrimate the source
 */
static int
ompi_mtl_ofi_check_fi_remote_cq_data(int fi_version,
                                     struct fi_info *hints,
                                     struct fi_info *provider,
                                     struct fi_info **prov_cq_data)
{
    int ret;
    char *provider_name;
    struct fi_info *hints_dup;
    hints_dup = fi_dupinfo(hints);

    provider_name = strdup(provider->fabric_attr->prov_name);
    hints_dup->fabric_attr->prov_name = provider_name;
    hints_dup->caps |= FI_TAGGED | FI_DIRECTED_RECV;
    /* Ask for the size that OMPI uses for the source rank number */
    hints_dup->domain_attr->cq_data_size = sizeof(int);
    ret = fi_getinfo(fi_version, NULL, NULL, 0ULL, hints_dup, prov_cq_data);

    if ((0 != ret) && (-FI_ENODATA != ret)) {
        opal_show_help("help-mtl-ofi.txt", "OFI call fail", true,
                       "fi_getinfo",
                       ompi_process_info.nodename, __FILE__, __LINE__,
                       fi_strerror(-ret), -ret);
        return ret;
    } else if (-FI_ENODATA == ret) {
        /* The provider does not support  FI_REMOTE_CQ_DATA */
        prov_cq_data = NULL;
    }

    fi_freeinfo(hints_dup);
    return OMPI_SUCCESS;
}

static void
ompi_mtl_ofi_define_tag_mode(int ofi_tag_mode) {
    switch (ofi_tag_mode) {
        case MTL_OFI_TAG_1:
            ompi_mtl_ofi.base.mtl_max_contextid = (int)((1ULL << MTL_OFI_CID_BIT_COUNT_1 ) - 1);
            ompi_mtl_ofi.base.mtl_max_tag = (int)((1ULL << (MTL_OFI_TAG_BIT_COUNT_1 - 1)) - 1);

            ompi_mtl_ofi.source_rank_tag_mask = MTL_OFI_SOURCE_TAG_MASK_1;
            ompi_mtl_ofi.num_bits_source_rank = MTL_OFI_SOURCE_BIT_COUNT_1;
            ompi_mtl_ofi.source_rank_mask = MTL_OFI_SOURCE_MASK_1;

            ompi_mtl_ofi.mpi_tag_mask = MTL_OFI_TAG_MASK_1;
            ompi_mtl_ofi.num_bits_mpi_tag = MTL_OFI_TAG_BIT_COUNT_1;

            ompi_mtl_ofi.sync_send = MTL_OFI_SYNC_SEND_1;
            ompi_mtl_ofi.sync_send_ack = MTL_OFI_SYNC_SEND_ACK_1;
            ompi_mtl_ofi.sync_proto_mask = MTL_OFI_PROTO_MASK_1;
        break;
        case MTL_OFI_TAG_2:
            ompi_mtl_ofi.base.mtl_max_contextid = (int)((1ULL << MTL_OFI_CID_BIT_COUNT_2 ) - 1);
            ompi_mtl_ofi.base.mtl_max_tag = (int)((1ULL << (MTL_OFI_TAG_BIT_COUNT_2 - 1)) - 1);

            ompi_mtl_ofi.source_rank_tag_mask = MTL_OFI_SOURCE_TAG_MASK_2;
            ompi_mtl_ofi.num_bits_source_rank = MTL_OFI_SOURCE_BIT_COUNT_2;
            ompi_mtl_ofi.source_rank_mask = MTL_OFI_SOURCE_MASK_2;

            ompi_mtl_ofi.mpi_tag_mask = MTL_OFI_TAG_MASK_2;
            ompi_mtl_ofi.num_bits_mpi_tag = MTL_OFI_TAG_BIT_COUNT_2;

            ompi_mtl_ofi.sync_send = MTL_OFI_SYNC_SEND_2;
            ompi_mtl_ofi.sync_send_ack = MTL_OFI_SYNC_SEND_ACK_2;
            ompi_mtl_ofi.sync_proto_mask = MTL_OFI_PROTO_MASK_2;
        break;
        default: /* use FI_REMOTE_CQ_DATA */
            ompi_mtl_ofi.base.mtl_max_contextid = (int)((1ULL << MTL_OFI_CID_BIT_COUNT_DATA ) - 1);
            ompi_mtl_ofi.base.mtl_max_tag = (int)((1ULL << (MTL_OFI_TAG_BIT_COUNT_DATA - 1)) - 1);

            ompi_mtl_ofi.mpi_tag_mask = MTL_OFI_TAG_MASK_DATA;

            ompi_mtl_ofi.sync_send = MTL_OFI_SYNC_SEND_DATA;
            ompi_mtl_ofi.sync_send_ack = MTL_OFI_SYNC_SEND_ACK_DATA;
            ompi_mtl_ofi.sync_proto_mask = MTL_OFI_PROTO_MASK_DATA;
    }
}

static mca_mtl_base_module_t*
ompi_mtl_ofi_component_init(bool enable_progress_threads,
                            bool enable_mpi_threads)
{
    int ret, fi_version;
    struct fi_info *hints;
    struct fi_info *providers = NULL;
    struct fi_info *prov = NULL;
    struct fi_info *prov_cq_data = NULL;
    struct fi_cq_attr cq_attr = {0};
    struct fi_av_attr av_attr = {0};
    char ep_name[FI_NAME_MAX] = {0};
    size_t namelen;

    /**
     * Hints to filter providers
     * See man fi_getinfo for a list of all filters
     * mode:  Select capabilities MTL is prepared to support.
     *        In this case, MTL will pass in context into communication calls
     * ep_type:  reliable datagram operation
     * caps:     Capabilities required from the provider.
     *           Tag matching is specified to implement MPI semantics.
     * msg_order: Guarantee that messages with same tag are ordered.
     */
    hints = fi_allocinfo();
    if (!hints) {
        opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                            "%s:%d: Could not allocate fi_info\n",
                            __FILE__, __LINE__);
        goto error;
    }
    hints->mode               = FI_CONTEXT;
    hints->ep_attr->type      = FI_EP_RDM;      /* Reliable datagram         */
    hints->caps               = FI_TAGGED;      /* Tag matching interface    */
    hints->tx_attr->msg_order = FI_ORDER_SAS;
    hints->rx_attr->msg_order = FI_ORDER_SAS;
    hints->rx_attr->op_flags = FI_COMPLETION;
    hints->tx_attr->op_flags = FI_COMPLETION;

    hints->domain_attr->threading        = FI_THREAD_UNSPEC;

    switch (control_progress) {
    case MTL_OFI_PROG_AUTO:
	hints->domain_attr->control_progress = FI_PROGRESS_AUTO;
	break;
    case MTL_OFI_PROG_MANUAL:
        hints->domain_attr->control_progress = FI_PROGRESS_MANUAL;
	break;
    default:
        hints->domain_attr->control_progress = FI_PROGRESS_UNSPEC;
    }

    switch (data_progress) {
    case MTL_OFI_PROG_AUTO:
	hints->domain_attr->data_progress = FI_PROGRESS_AUTO;
	break;
    case MTL_OFI_PROG_MANUAL:
        hints->domain_attr->data_progress = FI_PROGRESS_MANUAL;
	break;
    default:
        hints->domain_attr->data_progress = FI_PROGRESS_UNSPEC;
    }

    if (MTL_OFI_AV_TABLE == av_type) {
        hints->domain_attr->av_type          = FI_AV_TABLE;
    } else {
        hints->domain_attr->av_type          = FI_AV_MAP;
    }

    hints->domain_attr->resource_mgmt    = FI_RM_ENABLED;

    /**
     * FI_VERSION provides binary backward and forward compatibility support
     * Specify the version of OFI is coded to, the provider will select struct
     * layouts that are compatible with this version.
     */
    fi_version = FI_VERSION(1, 0);

    /**
     * fi_getinfo:  returns information about fabric  services for reaching a
     * remote node or service.  this does not necessarily allocate resources.
     * Pass NULL for name/service because we want a list of providers supported.
     */
    ret = fi_getinfo(fi_version,    /* OFI version requested                    */
                     NULL,          /* Optional name or fabric to resolve       */
                     NULL,          /* Optional service name or port to request */
                     0ULL,          /* Optional flag                            */
                     hints,         /* In: Hints to filter providers            */
                     &providers);   /* Out: List of matching providers          */
    if (FI_ENODATA == -ret) {
        // It is not an error if no information is returned.
        goto error;
    } else if (0 != ret) {
        opal_show_help("help-mtl-ofi.txt", "OFI call fail", true,
                       "fi_getinfo",
                       ompi_process_info.nodename, __FILE__, __LINE__,
                       fi_strerror(-ret), -ret);
        goto error;
    }

    /**
     * Select a provider from the list returned by fi_getinfo().
     */
    prov = select_ofi_provider(providers);
    if (!prov) {
        opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                            "%s:%d: select_ofi_provider: no provider found\n",
                            __FILE__, __LINE__);
        goto error;
    }

    /**
     * Select the format of the OFI tag
     */
    if ((MTL_OFI_TAG_AUTO == ofi_tag_mode) ||
        (MTL_OFI_TAG_FULL == ofi_tag_mode)) {
            ret = ompi_mtl_ofi_check_fi_remote_cq_data(fi_version,
                                                       hints, prov,
                                                       &prov_cq_data);
            if (OMPI_SUCCESS != ret) {
                goto error;
            } else if (NULL == prov_cq_data) {
                /* No support for FI_REMTOTE_CQ_DATA */
                fi_freeinfo(prov_cq_data);
                ompi_mtl_ofi.fi_cq_data = false;
                if (MTL_OFI_TAG_AUTO == ofi_tag_mode) {
                   /* Fallback to MTL_OFI_TAG_1 */
                   ompi_mtl_ofi_define_tag_mode(MTL_OFI_TAG_1);
                } else { /* MTL_OFI_TAG_FULL */
                   opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                            "%s:%d: OFI provider %s does not support FI_REMOTE_CQ_DATA\n",
                            __FILE__, __LINE__, prov->fabric_attr->prov_name);
                    goto error;
                }
            } else {
                /* Use FI_REMTOTE_CQ_DATA */
                ompi_mtl_ofi.fi_cq_data = true;
                prov = prov_cq_data;
                ompi_mtl_ofi_define_tag_mode(MTL_OFI_TAG_FULL);
            }
    } else { /* MTL_OFI_TAG_1 or MTL_OFI_TAG_2 */
        ompi_mtl_ofi.fi_cq_data = false;
        ompi_mtl_ofi_define_tag_mode(ofi_tag_mode);
    }

    /**
     * Open fabric
     * The getinfo struct returns a fabric attribute struct that can be used to
     * instantiate the virtual or physical network. This opens a "fabric
     * provider". See man fi_fabric for details.
     */
    ret = fi_fabric(prov->fabric_attr,    /* In:  Fabric attributes             */
                    &ompi_mtl_ofi.fabric, /* Out: Fabric handle                 */
                    NULL);                /* Optional context for fabric events */
    if (0 != ret) {
        opal_show_help("help-mtl-ofi.txt", "OFI call fail", true,
                       "fi_fabric",
                       ompi_process_info.nodename, __FILE__, __LINE__,
                       fi_strerror(-ret), -ret);
        goto error;
    }

    /**
     * Create the access domain, which is the physical or virtual network or
     * hardware port/collection of ports.  Returns a domain object that can be
     * used to create endpoints.  See man fi_domain for details.
     */
    ret = fi_domain(ompi_mtl_ofi.fabric,  /* In:  Fabric object                 */
                    prov,                 /* In:  Provider                      */
                    &ompi_mtl_ofi.domain, /* Out: Domain oject                  */
                    NULL);                /* Optional context for domain events */
    if (0 != ret) {
        opal_show_help("help-mtl-ofi.txt", "OFI call fail", true,
                       "fi_domain",
                       ompi_process_info.nodename, __FILE__, __LINE__,
                       fi_strerror(-ret), -ret);
        goto error;
    }

    /**
     * Create a transport level communication endpoint.  To use the endpoint,
     * it must be bound to completion counters or event queues and enabled,
     * and the resources consumed by it, such as address vectors, counters,
     * completion queues, etc.
     * see man fi_endpoint for more details.
     */
    ret = fi_endpoint(ompi_mtl_ofi.domain, /* In:  Domain object   */
                      prov,                /* In:  Provider        */
                      &ompi_mtl_ofi.ep,    /* Out: Endpoint object */
                      NULL);               /* Optional context     */
    if (0 != ret) {
        opal_show_help("help-mtl-ofi.txt", "OFI call fail", true,
                       "fi_endpoint",
                       ompi_process_info.nodename, __FILE__, __LINE__,
                       fi_strerror(-ret), -ret);
        goto error;
    }

    /**
     * Save the maximum sizes.
     */
    ompi_mtl_ofi.max_inject_size = prov->tx_attr->inject_size;
    ompi_mtl_ofi.max_msg_size = prov->ep_attr->max_msg_size;

    /**
     * Create the objects that will be bound to the endpoint.
     * The objects include:
     *     - completion queue for events
     *     - address vector of other endpoint addresses
     *     - dynamic memory-spanning memory region
     */
    cq_attr.format = FI_CQ_FORMAT_TAGGED;

    /**
     * If a user has set an ofi_progress_event_count > the default, then
     * the CQ size hint is set to the user's desired value such that
     * the CQ created will have enough slots to store up to
     * ofi_progress_event_count events. If a user has not set the
     * ofi_progress_event_count, then the provider is trusted to set a
     * default high CQ size and the CQ size hint is left unspecified.
     */
    if (ompi_mtl_ofi.ofi_progress_event_count > 100) {
        cq_attr.size = ompi_mtl_ofi.ofi_progress_event_count;
    }

    ret = fi_cq_open(ompi_mtl_ofi.domain, &cq_attr, &ompi_mtl_ofi.cq, NULL);
    if (ret) {
        opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                            "%s:%d: fi_cq_open failed: %s\n",
                            __FILE__, __LINE__, fi_strerror(-ret));
        goto error;
    }

    av_attr.type = (MTL_OFI_AV_TABLE == av_type) ? FI_AV_TABLE: FI_AV_MAP;

    ret = fi_av_open(ompi_mtl_ofi.domain, &av_attr, &ompi_mtl_ofi.av, NULL);
    if (ret) {
        opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                            "%s:%d: fi_av_open failed: %s\n",
                            __FILE__, __LINE__, fi_strerror(-ret));
        goto error;
    }

    /**
     * Bind the CQ and AV to the endpoint object.
     */
    ret = fi_ep_bind(ompi_mtl_ofi.ep,
                     (fid_t)ompi_mtl_ofi.cq,
                     FI_TRANSMIT | FI_RECV | FI_SELECTIVE_COMPLETION);
    if (0 != ret) {
        opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                            "%s:%d: fi_bind CQ-EP failed: %s\n",
                            __FILE__, __LINE__, fi_strerror(-ret));
        goto error;
    }

    ret = fi_ep_bind(ompi_mtl_ofi.ep,
                     (fid_t)ompi_mtl_ofi.av,
                     0);
    if (0 != ret) {
        opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                            "%s:%d: fi_bind AV-EP failed: %s\n",
                            __FILE__, __LINE__, fi_strerror(-ret));
        goto error;
    }

    /**
     * Enable the endpoint for communication
     * This commits the bind operations.
     */
    ret = fi_enable(ompi_mtl_ofi.ep);
    if (0 != ret) {
        opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                            "%s:%d: fi_enable failed: %s\n",
                            __FILE__, __LINE__, fi_strerror(-ret));
        goto error;
    }

    /**
     * Free providers info since it's not needed anymore.
     */
    fi_freeinfo(hints);
    hints = NULL;
    fi_freeinfo(providers);
    providers = NULL;

    /**
     * Get our address and publish it with modex.
     */
    namelen = sizeof(ep_name);
    ret = fi_getname((fid_t)ompi_mtl_ofi.ep, &ep_name[0], &namelen);
    if (ret) {
        opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                            "%s:%d: fi_getname failed: %s\n",
                            __FILE__, __LINE__, fi_strerror(-ret));
        goto error;
    }

    OFI_COMPAT_MODEX_SEND(ret,
                          &mca_mtl_ofi_component.super.mtl_version,
                          &ep_name,
                          namelen);
    if (OMPI_SUCCESS != ret) {
        opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                            "%s:%d: modex_send failed: %d\n",
                            __FILE__, __LINE__, ret);
        goto error;
    }

    ompi_mtl_ofi.epnamelen = namelen;

    /**
     * Set the ANY_SRC address.
     */
    ompi_mtl_ofi.any_addr = FI_ADDR_UNSPEC;

    /**
     * Activate progress callback.
     */
    ret = opal_progress_register(ompi_mtl_ofi_progress_no_inline);
    if (OMPI_SUCCESS != ret) {
        opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                            "%s:%d: opal_progress_register failed: %d\n",
                            __FILE__, __LINE__, ret);
        goto error;
    }

    return &ompi_mtl_ofi.base;

error:
    if (providers) {
        (void) fi_freeinfo(providers);
    }
    if (prov_cq_data) {
        (void) fi_freeinfo(prov_cq_data);
    }
    if (hints) {
        (void) fi_freeinfo(hints);
    }
    if (ompi_mtl_ofi.av) {
        (void) fi_close((fid_t)ompi_mtl_ofi.av);
    }
    if (ompi_mtl_ofi.cq) {
        (void) fi_close((fid_t)ompi_mtl_ofi.cq);
    }
    if (ompi_mtl_ofi.ep) {
        (void) fi_close((fid_t)ompi_mtl_ofi.ep);
    }
    if (ompi_mtl_ofi.domain) {
        (void) fi_close((fid_t)ompi_mtl_ofi.domain);
    }
    if (ompi_mtl_ofi.fabric) {
        (void) fi_close((fid_t)ompi_mtl_ofi.fabric);
    }

    return NULL;
}

int
ompi_mtl_ofi_finalize(struct mca_mtl_base_module_t *mtl)
{
    ssize_t ret;

    opal_progress_unregister(ompi_mtl_ofi_progress_no_inline);

    /* Close all the OFI objects */
    if ((ret = fi_close((fid_t)ompi_mtl_ofi.ep))) {
        goto finalize_err;
    }

    if ((ret = fi_close((fid_t)ompi_mtl_ofi.cq))) {
        goto finalize_err;
    }

    if ((ret = fi_close((fid_t)ompi_mtl_ofi.av))) {
        goto finalize_err;
    }

    if ((ret = fi_close((fid_t)ompi_mtl_ofi.domain))) {
        goto finalize_err;
    }

    if ((ret = fi_close((fid_t)ompi_mtl_ofi.fabric))) {
        goto finalize_err;
    }

    return OMPI_SUCCESS;

finalize_err:
    opal_show_help("help-mtl-ofi.txt", "OFI call fail", true,
                   "fi_close",
                   ompi_process_info.nodename, __FILE__, __LINE__,
                   fi_strerror(-ret), -ret);

    return OMPI_ERROR;
}



