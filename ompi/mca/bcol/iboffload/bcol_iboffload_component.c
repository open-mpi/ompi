/*
 * Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
 * Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include <unistd.h>
#include <sys/types.h>
#include <sys/mman.h>
#include <fcntl.h>

#include <infiniband/verbs.h>

#include "ompi/constants.h"
#include "ompi/mca/bcol/bcol.h"
#include "ompi/mca/bcol/base/base.h"
#include "ompi/mca/mpool/base/base.h"
#include "ompi/mca/common/ofacm/connect.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mca/common/ofautils/common_ofautils.h"

#include "orte/mca/rml/rml.h"
#include "orte/util/show_help.c"

#include "opal/util/argv.h"
#include "opal/include/opal/types.h"

#include "bcol_iboffload_mca.h"
#include "bcol_iboffload_frag.h"
#include "bcol_iboffload_task.h"
#include "bcol_iboffload_device.h"
#include "bcol_iboffload_qp_info.h"
#include "bcol_iboffload_collreq.h"
#include "bcol_iboffload_collfrag.h"

/*
 * Public string showing the bcol ompi_sm V2 component version number
 */
const char *mca_bcol_iboffload_component_version_string =
    "Open MPI bcol - iboffload collective MCA component version " OMPI_VERSION;

/*
 * Local functions
 */

static int setup_qps(void);
static int iboffload_open(void);
static int iboffload_close(void);

#define GET_IB_DTYPE_BY_CTYPE(ctype, is_int, ib_dtype)                           \
do {                                                                             \
    switch (sizeof(ctype)) {                                                     \
    case 1:                                                                      \
        ib_dtype = ((is_int) ? IBV_M_DATA_TYPE_INT8 : IBV_M_DATA_TYPE_INVALID);  \
        break;                                                                   \
    case 2:                                                                      \
        ib_dtype = ((is_int) ? IBV_M_DATA_TYPE_INT16 : IBV_M_DATA_TYPE_INVALID); \
        break;                                                                   \
    case 4:                                                                      \
        ib_dtype = ((is_int) ? IBV_M_DATA_TYPE_INT32 : IBV_M_DATA_TYPE_FLOAT32); \
        break;                                                                   \
    case 8:                                                                      \
        ib_dtype = ((is_int) ? IBV_M_DATA_TYPE_INT64 : IBV_M_DATA_TYPE_FLOAT64); \
        break;                                                                   \
    default:                                                                     \
        ib_dtype = IBV_M_DATA_TYPE_INVALID;                                      \
    }                                                                            \
} while (0)

/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */
mca_bcol_iboffload_component_t mca_bcol_iboffload_component = {

    /* First, fill in the super */

    {
        /* First, the mca_component_t struct containing meta
           information about the component itself */

        {
            MCA_BCOL_BASE_VERSION_2_0_0,

            /* Component name and version */

            "iboffload",
            OMPI_MAJOR_VERSION,
            OMPI_MINOR_VERSION,
            OMPI_RELEASE_VERSION,

            /* Component open and close functions */

            iboffload_open,
            iboffload_close,
            NULL, /* mca_register_component_params() */
            NULL, /* reserved */
        },

        mca_bcol_iboffload_init_query,
        mca_bcol_iboffload_comm_query,
        mca_bcol_iboffload_coll_supported,
        mca_bcol_iboffload_coll_support_all_types,
        false,
        true, /* collective calls with iboffload should to be ordered */
    },
    /* iboffload-component specifc information */
    0, /* verbose */
    0, /* number of qps to use */
    false, /* warn_default_gid_prefix */
    false, /* warn_nonexistent_if */
    0, /* free_list_num */
    0, /* free_list_max */
    0, /* free_list_inc */
    NULL, /* mpool_name */
    0, /* cq_size */
    0, /* max_inline_data */
    0, /* pkey_val */
    0, /* qp_ous_rd_atom */
    0, /* mtu */
    0, /* min_rnr_timer */
    0, /* timeout */
    0, /* retry_count */
    0, /* rnr_retry */
    0, /* max_rdma_dst_ops */
    0, /* service_level */
    0, /* bcols_per_lid */
    0, /* max_lmc */
    0, /* max_bcols */
    0, /* use_async_event_thread */
    0, /* buffer_alignment */
    0, /* max_mqe_tasks */
    0, /* max_mq_size */
    0, /* frag_size */
    NULL, /* if_include */
    NULL, /* if_include_list */
    NULL, /* if_exclude */
    NULL, /* if_exclude_list */
    NULL, /* if_list */
    NULL, /* ib_devs */
    0, /* num_devs */
    NULL, /* receive_queues */
};

static int mca_bcol_iboffload_dummy_init_query(
    bool enable_progress_threads, bool enable_mpi_threads)
{
    return OMPI_SUCCESS;
}

static void mca_bcol_iboffload_device_constructor
            (mca_bcol_iboffload_device_t *device)
{
    /* Init OFACM stuf */
    device->dev.ib_dev = NULL;
    device->dev.ib_dev_context = NULL;
    device->dev.capabilities = 0;
    /* device->dev.type = MCA_COMMON_OFACM_COLL;*/
    /* Init other stuff */
    device->ib_pd = NULL;
    device->ib_cq = NULL;
    device->ports = NULL;

    device->mpool = NULL;
    device->ib_mq_cq = NULL;
    device->frags_free = NULL;

    device->activated = false;
    device->num_act_ports = 0;

    memset(&device->ib_dev_attr, 0, sizeof(struct ibv_device_attr));
    memset(&device->dummy_reg, 0, sizeof( mca_bcol_iboffload_reg_t));
}

static void mca_bcol_iboffload_device_destructor
            (mca_bcol_iboffload_device_t *device)
{
    int qp_index, num_qps = mca_bcol_iboffload_component.num_qps;

    IBOFFLOAD_VERBOSE(10, ("Device %s will be destroyed.\n",
                           ibv_get_device_name(device->dev.ib_dev)));

    if (NULL != device->frags_free) {
        for (qp_index = 0; qp_index < num_qps; ++qp_index) {
            mca_bcol_iboffload_dealloc_qps_resource_fn_t dealloc_resource =
                        mca_bcol_iboffload_component.qp_infos[qp_index].dealloc_resource;
            if (NULL != dealloc_resource) {
                dealloc_resource(qp_index, device);
            }
        }

        free(device->frags_free);
    }

    if (NULL != device->mpool) {
        IBOFFLOAD_VERBOSE(10, ("Mpool destroy - %p.\n", device->mpool));
        if (OMPI_SUCCESS != mca_mpool_base_module_destroy(device->mpool)) {
            IBOFFLOAD_ERROR(("Device %s, failed to destroy mpool",
                              ibv_get_device_name(device->dev.ib_dev)));
        }
    }

    if (NULL != device->dummy_reg.mr) {
        IBOFFLOAD_VERBOSE(10, ("Dummy memory MR unregister - %p.\n", device->dummy_reg.mr));
        if (OMPI_SUCCESS !=
            mca_bcol_iboffload_deregister_mr((void *) device, &device->dummy_reg.base)) {
            IBOFFLOAD_ERROR(("Device %s: failed to unregister dummy memory MR.",
                              ibv_get_device_name(device->dev.ib_dev)));
        }
    }

    if (NULL != device->ib_cq) {
        if (ibv_destroy_cq(device->ib_cq)) {
            IBOFFLOAD_ERROR(("Device %s, failed to destroy CQ, errno says %s",
                        ibv_get_device_name(device->dev.ib_dev), strerror(errno)));
        }
    }

    if (NULL != device->ib_mq_cq) {
        if (ibv_destroy_cq(device->ib_mq_cq)) {
            IBOFFLOAD_ERROR(("Device %s, failed to destroy mq CQ, errno says %s",
                        ibv_get_device_name(device->dev.ib_dev), strerror(errno)));
        }
    }

    /* Release IB PD if we have one */
    if (NULL != device->ib_pd) {
        if(ibv_dealloc_pd(device->ib_pd)){
            IBOFFLOAD_ERROR(("Device %s, failed to release PD, errno says %s",
                ibv_get_device_name(device->dev.ib_dev), strerror(errno)));
        }
    }

    /* close the device */
    if (NULL != device->dev.ib_dev_context) {
        if (ibv_close_device(device->dev.ib_dev_context)) {
            IBOFFLOAD_ERROR(("Device %s "
                        ", failed to close the device, errno says %s",
                        ibv_get_device_name(device->dev.ib_dev), strerror(errno)));
        }
    }

    /* release memory */
    if (NULL != device->ports) {
        free(device->ports);
    }
}

OBJ_CLASS_INSTANCE(mca_bcol_iboffload_device_t,
                   opal_list_item_t,
                   mca_bcol_iboffload_device_constructor,
                   mca_bcol_iboffload_device_destructor);

int mca_bcol_iboffload_coll_supported(int op, int dtype, bcol_elem_type elem_type)
{
    mca_bcol_iboffload_component_t *cm = &mca_bcol_iboffload_component;

    return  (IBV_M_DATA_TYPE_INVALID != cm->map_ompi_to_ib_dt[dtype]) &&
            (IBV_M_CALC_OP_INVALID != cm->map_ompi_to_ib_calcs[op]) &&
            (BCOL_SINGLE_ELEM_TYPE == elem_type);
}

int mca_bcol_iboffload_coll_support_all_types(bcol_coll coll_name)
{
    return BCOL_ALLREDUCE ^ coll_name;
}

/* Unload devices */
static int iboffload_release_devices(void)
{
    int i;
    mca_bcol_iboffload_device_t *device = NULL;

    mca_bcol_iboffload_component_t *cm = &mca_bcol_iboffload_component;
    opal_pointer_array_t *devs = &cm->devices;

    IBOFFLOAD_VERBOSE(10, ("Destroy all devices.\n"));

    for (i = 0; i < cm->num_devs; i++) {
        device = opal_pointer_array_get_item(devs, i);

        IBOFFLOAD_VERBOSE(10, ("Device %s with index %d will be destroyed.\n",
                               ibv_get_device_name(device->dev.ib_dev), i));
        if (NULL != device) {
            OBJ_RELEASE(device);
        }
    }

    IBOFFLOAD_VERBOSE(10, ("All devices were destroyed.\n"));

    opal_pointer_array_remove_all(devs);
    OBJ_DESTRUCT(devs);

    /* release device list */
    /*ibv_free_device_list_compat(cm->ib_devs);*/
    ompi_ibv_free_device_list(cm->ib_devs);
    cm->ib_devs = NULL;

    IBOFFLOAD_VERBOSE(10, ("All devices destroyed.\n"));

    return OMPI_SUCCESS;
}

/* Create list of IB HCA that have active port */
static int iboffload_load_devices(void)
{
    int num_devs = 0, i;
    mca_bcol_iboffload_device_t *device = NULL;
    mca_bcol_iboffload_component_t *cm = &mca_bcol_iboffload_component;

    IBOFFLOAD_VERBOSE(10, ("Entering to iboffload_load_devices"));

    /* Get list of devices */
    /*cm->ib_devs = ibv_get_device_list_compat(&num_devs);*/
    cm->ib_devs = ompi_ibv_get_device_list(&num_devs);
    if (0 == num_devs || NULL == cm->ib_devs) {
        IBOFFLOAD_ERROR(("No IB devices found"));
        /* No hca error*/
        orte_show_help("help-mpi-btl-openib.txt", "no-nics", true);
        return OMPI_ERROR;
    }

    cm->num_devs = num_devs;

    for (i = 0; i < num_devs; i++) {
        device = OBJ_NEW(mca_bcol_iboffload_device_t);
        if (NULL != device) {
            opal_pointer_array_set_item(&cm->devices, i, (void *) device);
            device->dev.ib_dev = cm->ib_devs[i];

            IBOFFLOAD_VERBOSE(10, ("Device %s with index %d was appended.\n",
                                    ibv_get_device_name(device->dev.ib_dev), i));
        }
    }

    if (0 == opal_pointer_array_get_size(&cm->devices)) {
        /* No relevand devices were found, return error */
        IBOFFLOAD_ERROR(("No active devices found.\n"));

        return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
}

static void map_ompi_to_ib_dtype(void)
{
    int dt;
    mca_bcol_iboffload_component_t *cm = &mca_bcol_iboffload_component;

    for (dt = 0; dt < OMPI_DATATYPE_MAX_PREDEFINED; ++dt) {
        cm->map_ompi_to_ib_dt[dt] = IBV_M_DATA_TYPE_INVALID;
    }

    GET_IB_DTYPE_BY_CTYPE(char,      true,  cm->map_ompi_to_ib_dt[OMPI_DATATYPE_MPI_SIGNED_CHAR]);

    GET_IB_DTYPE_BY_CTYPE(short,     true,  cm->map_ompi_to_ib_dt[OMPI_DATATYPE_MPI_SHORT]);
    GET_IB_DTYPE_BY_CTYPE(int,       true,  cm->map_ompi_to_ib_dt[OMPI_DATATYPE_MPI_INT]);
    GET_IB_DTYPE_BY_CTYPE(long,      true,  cm->map_ompi_to_ib_dt[OMPI_DATATYPE_MPI_LONG]);
    GET_IB_DTYPE_BY_CTYPE(long long, true,  cm->map_ompi_to_ib_dt[OMPI_DATATYPE_MPI_LONG_LONG]);
    GET_IB_DTYPE_BY_CTYPE(float,     false, cm->map_ompi_to_ib_dt[OMPI_DATATYPE_MPI_FLOAT]);
    GET_IB_DTYPE_BY_CTYPE(double,    false, cm->map_ompi_to_ib_dt[OMPI_DATATYPE_MPI_DOUBLE]);

    /* Check (only in DEBUG mode) if size of double equal to 64 bit */
    assert(8 == sizeof(double));
}

static void map_ompi_to_ib_op_type(void)
{
    int op;
    mca_bcol_iboffload_component_t *cm = &mca_bcol_iboffload_component;

    for (op = 0; op < OMPI_OP_NUM_OF_TYPES; ++op) {
        cm->map_ompi_to_ib_calcs[op] = IBV_M_CALC_OP_INVALID;
    }

    cm->map_ompi_to_ib_calcs[OMPI_OP_MAX]  = IBV_M_CALC_OP_MAX;
    cm->map_ompi_to_ib_calcs[OMPI_OP_MIN]  = IBV_M_CALC_OP_MIN;
    cm->map_ompi_to_ib_calcs[OMPI_OP_SUM]  = IBV_M_CALC_OP_ADD;

    cm->map_ompi_to_ib_calcs[OMPI_OP_LAND] = IBV_M_CALC_OP_LAND;
    cm->map_ompi_to_ib_calcs[OMPI_OP_BAND] = IBV_M_CALC_OP_BAND;
    cm->map_ompi_to_ib_calcs[OMPI_OP_LOR]  = IBV_M_CALC_OP_LOR;
    cm->map_ompi_to_ib_calcs[OMPI_OP_BOR]  = IBV_M_CALC_OP_BOR;
    cm->map_ompi_to_ib_calcs[OMPI_OP_LXOR] = IBV_M_CALC_OP_LXOR;
    cm->map_ompi_to_ib_calcs[OMPI_OP_BXOR] = IBV_M_CALC_OP_BXOR;
}

/*
 * Open the component
 */
static int iboffload_open(void)
{
    int rc;

    /* local variables */
    mca_bcol_iboffload_component_t *cm = &mca_bcol_iboffload_component;

    IBOFFLOAD_VERBOSE(10, ("Open Iboffload component.\n"));

    cm->super.priority = 100;
    cm->super.n_net_contexts = 0;
    cm->super.network_contexts = NULL;

    OBJ_CONSTRUCT(&cm->recv_wrs.lock, opal_mutex_t);

    /* construct lists */
    OBJ_CONSTRUCT(&cm->devices, opal_pointer_array_t);
    rc = opal_pointer_array_init(&cm->devices, 10, INT_MAX, 10);
    if (OMPI_SUCCESS != rc) {
        goto close_device;
    }

    /* load mca parametres */
    rc = mca_bcol_iboffload_register_params();
    if (OMPI_SUCCESS != rc) {
        goto close_device;
    }

    /* Register the progress function */
    rc = opal_progress_register(mca_bcol_iboffload_component_progress);
    if (OMPI_SUCCESS != rc) {
        IBOFFLOAD_ERROR(("Failed to register the progress function"
                         " for iboffload component.\n"));
        goto close_device;
    }

    map_ompi_to_ib_dtype();
    map_ompi_to_ib_op_type();

    /* The init_done set to true on first component usage */
    cm->init_done = false;

    return OMPI_SUCCESS;

close_device:
    OBJ_DESTRUCT(&cm->devices);
    OBJ_DESTRUCT(&cm->recv_wrs.lock);
    return rc;
}

/*
 * Close the component
 */
static int iboffload_close(void)
{
    int rc;

    mca_bcol_iboffload_component_t *cm = &mca_bcol_iboffload_component;

    IBOFFLOAD_VERBOSE(10, ("Destroy component free lists.\n"));

    if (true == cm->init_done) {
        OBJ_DESTRUCT(&cm->tasks_free);
        OBJ_DESTRUCT(&cm->collreqs_free);
        OBJ_DESTRUCT(&cm->collfrags_free);
        OBJ_DESTRUCT(&cm->calc_tasks_free);
    }

    /* Unregister the progress function */
    rc = opal_progress_unregister(mca_bcol_iboffload_component_progress);
    if (OMPI_SUCCESS != rc) {
        IBOFFLOAD_ERROR(("Failed to unregister the progress function"
                         " for iboffload component.\n"));
    }

    rc = iboffload_release_devices();
    if (OMPI_SUCCESS != rc) {
        return rc;
    }

    if (NULL != cm->receive_queues) {
        free(cm->receive_queues);
    }

    OBJ_DESTRUCT(&cm->recv_wrs.lock);

    IBOFFLOAD_VERBOSE(10, ("The component closed.\n"));

    return OMPI_SUCCESS;
}

/* query to see if the component is available for use, and can
 * satisfy the thread and progress requirements
 */
int mca_bcol_iboffload_init_query(bool enable_progress_threads,
        bool enable_mpi_threads)
{
    int rc;
    mca_bcol_iboffload_component_t *cm = &mca_bcol_iboffload_component;

    IBOFFLOAD_VERBOSE(10, ("Init Iboffload component.\n"));

    /* Get list of HCAs and ports */
    rc = iboffload_load_devices();
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        IBOFFLOAD_ERROR(("Load devices error.\n"));
        goto unload_devices;
    }

    /* Setup the BSRQ QP's based on the final value of
       mca_bcol_iboffload_component.receive_queues. */
    rc = setup_qps();
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        IBOFFLOAD_ERROR(("QPs setup error.\n"));
        goto unload_devices;
    }

    cm->super.collm_init_query = mca_bcol_iboffload_dummy_init_query;

    return OMPI_SUCCESS;

    /* done */
unload_devices:
    IBOFFLOAD_ERROR(("Release devices: an error occured.\n"));

    iboffload_release_devices();

    return rc;
}

static int32_t atoi_param(char *param, int32_t dflt)
{
    if (NULL == param || '\0' == param[0]) {
        return dflt ? dflt : 1;
    }

    return atoi(param);
}

static int setup_qps(void)
{
    int ret = OMPI_SUCCESS, qp = 0;
    int rd_num, rd_low, size, rd_win, rd_rsv, sd_max;

    mca_bcol_iboffload_qp_type_t type;

    char **queues = NULL, **params = NULL;

    queues = opal_argv_split(mca_bcol_iboffload_component.receive_queues, ':');
    if (0 == opal_argv_count(queues)) {
        orte_show_help("help-mpi-btl-openib.txt",
                       "no qps in receive_queues", true,
                       orte_process_info.nodename,
                       mca_bcol_iboffload_component.receive_queues);

        ret = OMPI_ERROR;

        goto exit;
    }

    while (queues[qp] != NULL) {
        if (0 == strncmp("P,", queues[qp], 2)) {
            type = MCA_BCOL_IBOFFLOAD_PP_QP;
        } else if (0 == strncmp("S,", queues[qp], 2)) {
            type = MCA_BCOL_IBOFFLOAD_SRQ_QP;
        } else if (0 == strncmp("X,", queues[qp], 2)) {
#if HAVE_XRC
        type = MCA_BCOL_IBOFFLOAD_XRC_QP;
#else
            orte_show_help("help-mpi-btl-openib.txt", "No XRC support", true,
                           orte_process_info.nodename,
                           mca_bcol_iboffload_component.receive_queues);
            ret = OMPI_ERR_NOT_AVAILABLE;
            goto exit;
#endif
        } else {
            orte_show_help("help-mpi-btl-openib.txt",
                           "invalid qp type in receive_queues", true,
                           orte_process_info.nodename,
                           mca_bcol_iboffload_component.receive_queues,
                           queues[qp]);

            ret = OMPI_ERR_BAD_PARAM;

            goto exit;
        }

        ++qp;
    }

    mca_bcol_iboffload_component.num_qps = MCA_BCOL_IBOFFLOAD_QP_LAST;

    qp = 0;
#define P(N) (((N) > count) ? NULL : params[(N)])
    while (NULL != queues[qp]) {
        int count;

        params = opal_argv_split_with_empty(queues[qp], ',');
        count = opal_argv_count(params);

        if ('P' == params[0][0]) {
            if (count < 3 || count > 6) {
                orte_show_help("help-mpi-btl-openib.txt",
                               "invalid pp qp specification", true,
                               orte_process_info.nodename, queues[qp]);

                ret = OMPI_ERR_BAD_PARAM;

                goto exit;
            }

            size = atoi_param(P(1), 0);

            rd_num = atoi_param(P(2), 256);

            /* by default set rd_low to be 3/4 of rd_num */
            rd_low = atoi_param(P(3), rd_num - (rd_num / 4));
            rd_win = atoi_param(P(4), (rd_num - rd_low) * 2);
            rd_rsv = atoi_param(P(5), (rd_num * 2) / rd_win);


            if ((rd_num - rd_low) > rd_win) {
                orte_show_help("help-mpi-btl-openib.txt", "non optimal rd_win",
                        true, rd_win, rd_num - rd_low);
            }
        } else {
            if (count < 3 || count > 5) {
                orte_show_help("help-mpi-btl-openib.txt",
                               "invalid srq specification", true,
                               orte_process_info.nodename, queues[qp]);

                ret = OMPI_ERR_BAD_PARAM;

                goto exit;
            }

            size = atoi_param(P(1), 0);
            rd_num = atoi_param(P(2), 256);

            /* by default set rd_low to be 3/4 of rd_num */
            rd_low = atoi_param(P(3), rd_num - (rd_num / 4));
            sd_max = atoi_param(P(4), rd_low / 4);

            IBOFFLOAD_VERBOSE(10, ("srq: rd_num is %d rd_low is %d sd_max is %d",
                         rd_num, rd_low, sd_max));

        }

        if (rd_num <= rd_low) {
            orte_show_help("help-mpi-btl-openib.txt", "rd_num must be > rd_low",
                    true, orte_process_info.nodename, queues[qp]);
            ret = OMPI_ERR_BAD_PARAM;

            goto exit;
        }

        opal_argv_free(params);

        ++qp;
    }

    params = NULL;

    for (qp = 0; qp < MCA_BCOL_IBOFFLOAD_QP_LAST; ++qp) {
        mca_bcol_iboffload_component.qp_infos[qp].qp_index = qp;

        mca_bcol_iboffload_component.qp_infos[qp].type = type;
        mca_bcol_iboffload_component.qp_infos[qp].size = size;

        mca_bcol_iboffload_component.qp_infos[qp].rd_num = rd_num;
        mca_bcol_iboffload_component.qp_infos[qp].rd_low = rd_low;

        mca_bcol_iboffload_component.qp_infos[qp].rd_pp_win = rd_num - rd_low;

        if (MCA_BCOL_IBOFFLOAD_PP_QP == type) {
            mca_bcol_iboffload_component.qp_infos[qp].u.pp_qp.rd_win = rd_win;
            mca_bcol_iboffload_component.qp_infos[qp].u.pp_qp.rd_rsv = rd_rsv;
        } else {
            mca_bcol_iboffload_component.qp_infos[qp].u.srq_qp.sd_max = sd_max;
        }

        if (NULL != setup_qps_fn[qp]) {
            setup_qps_fn[qp](&mca_bcol_iboffload_component.qp_infos[qp]);
        }
    }

exit:
    if (NULL != params) {
        opal_argv_free(params);
    }

    if (NULL != queues) {
        opal_argv_free(queues);
    }

    return ret;
}

static int progress_pending_collfrags(mca_bcol_iboffload_module_t *iboffload)
{
    mca_bcol_iboffload_collfrag_t *pending_collfrag;
    int rc, size = opal_list_get_size(&iboffload->collfrag_pending);

    IBOFFLOAD_VERBOSE(10, ("Calling progress_pending_collfrags"));

    do {
        pending_collfrag = (mca_bcol_iboffload_collfrag_t *)
                opal_list_remove_first(&iboffload->collfrag_pending);

        IBOFFLOAD_VERBOSE(10, ("Get pending_collfrag - %p, iboffload - %p, "
                              "pending list size - %d.", pending_collfrag, iboffload,
                               opal_list_get_size(&iboffload->collfrag_pending)));

        /* Return back coll frag to coll request opal_list */
        opal_list_append(&pending_collfrag->coll_full_req->work_requests,
                         (opal_list_item_t *) pending_collfrag);

        rc = pending_collfrag->coll_full_req->progress_fn
                            (iboffload, pending_collfrag->coll_full_req);
        if (OPAL_UNLIKELY(BCOL_FN_STARTED != rc && OMPI_SUCCESS != rc)) {
            return OMPI_ERROR;
        }
    } while (--size > 0);

    return OMPI_SUCCESS;
}


/**
 * Test - if we finished with the coll fragment descriptor,
 * and free all resouces if so.
 **/
int
mca_bcol_iboffload_free_tasks_frags_resources(
                  mca_bcol_iboffload_collfrag_t *collfrag,
                  ompi_free_list_t *frags_free)
{
    int rc;

    mca_bcol_iboffload_task_t *task = collfrag->tasks_to_release;
    mca_bcol_iboffload_component_t *cm = &mca_bcol_iboffload_component;

    /* Support for multiple frags we will add later
     * n_outstanding_frags = coll_req->n_frags_sent - coll_req->n_frag_net_complete; */

    while (NULL != task) {
        /* Return frag (is the reference counter is zero)*/
        rc = release_frags_on_task(task, frags_free);
        if (OMPI_SUCCESS != rc) {
            return OMPI_ERROR;
        }

        /* Return task: if the pointer is NULL => we assume the task
           is a member of the common task list (tasks_free) */
        if (NULL == task->task_list) {
            OMPI_FREE_LIST_RETURN(&cm->tasks_free,
                    (ompi_free_list_item_t *) task);
        } else {
            OMPI_FREE_LIST_RETURN(task->task_list,
                    (ompi_free_list_item_t *) task);
        }

        task = task->next_task;
    }

    return OMPI_SUCCESS;
}

static void fatal_error(char *mesg)
{
    IBOFFLOAD_ERROR(("FATAL ERROR: %s", mesg));
    ompi_mpi_abort(&ompi_mpi_comm_world.comm, MPI_ERR_INTERN, true);
}

#define RELEASE_COLLFRAG(cf)                                                    \
    do {                                                                        \
        opal_list_remove_item(&(cf)->coll_full_req->work_requests,              \
                               (opal_list_item_t *) (cf));                      \
        if (&(cf)->coll_full_req->first_collfrag != (cf)) {                     \
            OMPI_FREE_LIST_RETURN(&mca_bcol_iboffload_component.collfrags_free, \
                                 (ompi_free_list_item_t *) (cf));               \
        }                                                                       \
    } while (0)

#define COLLFRAG_IS_DONE(cf) ((cf)->complete && (cf)->n_sends_completed == (cf)->n_sends)

/* Pasha: Need to modify the code to progress pending queue only if relevant
* resource was released */
#define PROGRESS_PENDING_COLLFRAG(cf)                                                            \
    if (OPAL_UNLIKELY(opal_list_get_size(&(cf)->coll_full_req->module->collfrag_pending) > 0)) { \
        int rc;                                                                                  \
        IBOFFLOAD_VERBOSE(10, ("Calling for PROGRESS_PENDING_COLLFRAG"));                        \
        rc = progress_pending_collfrags((cf)->coll_full_req->module);                            \
        if (OPAL_UNLIKELY(OMPI_ERROR == rc)) {                                                   \
            fatal_error("failed to progress_pending_collfrags\n");                               \
            return 0;                                                                            \
        }                                                                                        \
    }


static inline __opal_attribute_always_inline__ int
                  handle_collfrag_done(mca_bcol_iboffload_collfrag_t *coll_frag,
                                       mca_bcol_iboffload_collreq_t *coll_request,
                                       mca_bcol_iboffload_device_t *device)
{
    int rc;

    if (COLLFRAG_IS_DONE(coll_frag)) {
        IBOFFLOAD_VERBOSE(10, ("Coll frag - %p already done.\n", coll_frag));

        coll_request->n_frag_net_complete++;
        IBOFFLOAD_VERBOSE(10, ("Free tasks resourse.\n"));
        /* Check if we are done with this coll_frag and release resources if so.  */
        rc = mca_bcol_iboffload_free_tasks_frags_resources(coll_frag, device->frags_free);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
            IBOFFLOAD_ERROR(("mca_bcol_iboffload_free_tasks_frags_resources FAILED"));
            fatal_error("Failed to mca_bcol_iboffload_free_tasks_frags_resources\n");
            return -1;
        }

        BCOL_IBOFFLOAD_MQ_RETURN_CREDITS(coll_request->module, coll_frag->mq_index, coll_frag->mq_credits);

        RELEASE_COLLFRAG(coll_frag);

        PROGRESS_PENDING_COLLFRAG(coll_frag);

        IBOFFLOAD_VERBOSE(10, ("Alg %d: user_handle_freed - %d, n_frag_mpi_complete - %d, "
                               "n_fragments- %d, n_frag_net_complete - %d, n_fragments - %d.\n",
                                coll_frag->alg,
                                coll_request->user_handle_freed,
                                coll_request->n_frag_mpi_complete,
                                coll_request->n_fragments,
                                coll_request->n_frag_net_complete,
                                coll_request->n_fragments));

        /* check for full message completion */
        if (COLLREQ_IS_DONE(coll_request)) {
            IBOFFLOAD_VERBOSE(10, ("Coll request already done.\n"));
            RELEASE_COLLREQ(coll_request);
        }
    }

    IBOFFLOAD_VERBOSE(10, ("Exit with success.\n"));

    return 0;
}

/*
 *  IBOFFLOAD component progress.
 */

static int progress_one_device(mca_bcol_iboffload_device_t *device)
{
    int ne, rc, count = 0;

    mca_bcol_iboffload_collfrag_t *coll_frag;
    mca_bcol_iboffload_collreq_t *coll_request;

    struct ibv_wc wc;
    memset(&wc, 0, sizeof(struct ibv_wc));

    /*
     * poll for collective completion - does not mean resources can
     * be freed, as incomplete network level sends may still be pending
     */

    /* Poll for completion on completion on wait MQEs */
    if(0 != (ne = ibv_poll_cq(device->ib_mq_cq, 1, &wc))) {
        do {
            if (OPAL_UNLIKELY(0 > ne)) {
                IBOFFLOAD_ERROR(("Device %s: "
                    "failed to poll MQ completion queue\n",
                    ibv_get_device_name(device->dev.ib_dev)));
                fatal_error("failed to poll MQ completion queue\n");
                return count;
            }

            if (OPAL_UNLIKELY(IBV_WC_SUCCESS != wc.status)) {
                IBOFFLOAD_ERROR(("Device %s: "
                    "the completion with error on wait was gotten, status %d, opcode %d, "
                    "vendor_err 0x%x, qp %x, id 0x%x\n", ibv_get_device_name(device->dev.ib_dev),
                    wc.status, wc.opcode, wc.vendor_err, wc.qp_num, wc.wr_id));
                fatal_error("wc.status \n");
                return count;
            }

            IBOFFLOAD_VERBOSE(10, ("The MQ completion was polled.\n"));

            ++count;

            /* get pointer to mca_bcol_iboffload_collfrag_t */
            coll_frag = (mca_bcol_iboffload_collfrag_t*)
                                    (uint64_t) (uintptr_t) wc.wr_id;

            /* Only last MQ task of collective frag
               sends completion signal, so if we got it =>
               all MQEs were done. */
            coll_frag->complete = true;

            IBOFFLOAD_VERBOSE(10, ("MQ completion for algorithm %d coll_frag_addr %p ml buffer index %d",
                        coll_frag->alg, (void *)coll_frag, coll_frag->coll_full_req->ml_buffer_index));

            /* full request descriptor */
            coll_request = coll_frag->coll_full_req;

            coll_request->n_frag_mpi_complete++;

            /*
             * at this stage all receives have been completed, so
             * unpack the data to user buffer, the resources will be released when we will done with all
             * element in the task list
             */

            if (NULL != coll_request->completion_cb_fn) {
                if (OMPI_SUCCESS !=
                     coll_request->completion_cb_fn(coll_frag)) {
                    fatal_error("coll_request->completion_cb_fn\n");
                    return count;
                }
            }

            if (coll_request->n_frag_mpi_complete ==
                            coll_request->n_fragments) {
                coll_request->super.req_complete = true;
                opal_condition_broadcast(&ompi_request_cond);
                IBOFFLOAD_VERBOSE(10, ("After opal_condition_broadcast.\n"));
            }

            rc = handle_collfrag_done(coll_frag, coll_request, device);
            if (0 != rc) {
                return count;
            }
        } while(0 != (ne = ibv_poll_cq(device->ib_mq_cq, 1, &wc)));

        return count;
    }

    /* poll the send completion queue */
    do {
        ne = ibv_poll_cq(device->ib_cq, 1, &wc);
        if (0 < ne) {
            if (OPAL_UNLIKELY(IBV_WC_SUCCESS != wc.status)) {
                IBOFFLOAD_ERROR(("Device %s, "
                        "the completion with error on send was gotten, status %d, opcode %d, "
                        "vendor_err 0x%x, qp %x, id 0x%x\n", ibv_get_device_name(device->dev.ib_dev),
                         wc.status, wc.opcode, wc.vendor_err, wc.qp_num, wc.wr_id));

#if OPAL_ENABLE_DEBUG
                {
                    mca_bcol_iboffload_module_t *iboffload;
                    int i, qp_index, num_qps = mca_bcol_iboffload_component.num_qps;

                    coll_frag = (mca_bcol_iboffload_collfrag_t*)
                                         (uint64_t) (uintptr_t) wc.wr_id;

                    iboffload = coll_frag->coll_full_req->module;

                    for (i = 0; i < iboffload->num_endpoints; ++i) {
                        mca_bcol_iboffload_endpoint_t *ep = iboffload->endpoints[i];

                        for (qp_index = 0; qp_index < num_qps; ++qp_index) {
                            if (NULL != ep->qps[qp_index].qp->lcl_qp &&
                                wc.qp_num == ep->qps[qp_index].qp->lcl_qp->qp_num) {
                                IBOFFLOAD_ERROR(("Module - %p, coll_frag - %p, "
                                                 "destination %d, qp index - %d.",
                                                  iboffload, coll_frag, i, qp_index));
                            }
                        }
                    }
                }
#endif
                fatal_error("Failed to ibv_poll_cq\n");
                return count;
            }

            ++count;

            /* get pointer to mca_bcol_iboffload_collfrag_t */
            coll_frag = (mca_bcol_iboffload_collfrag_t*)
                                         (uint64_t) (uintptr_t) wc.wr_id;

            /* update the number of completed sends */
            coll_frag->n_sends_completed++;

            IBOFFLOAD_VERBOSE(10, ("Send CQ completion for algorithm %d coll_frag_addr %p ml buffer index %d",
                        coll_frag->alg, (void *)coll_frag, coll_frag->coll_full_req->ml_buffer_index));

            IBOFFLOAD_VERBOSE(10, ("Alg %d coll_frag_addr %p: n_sends_completed - %d, n_sends - %d.\n",
                                    coll_frag->alg, (void *)coll_frag,
                                    coll_frag->n_sends_completed,
                                    coll_frag->n_sends));

            assert(coll_frag->n_sends_completed <= coll_frag->n_sends);

            /* full message descriptor */
            coll_request = coll_frag->coll_full_req;

            /* check to see if all sends are complete from the network
             * perspective */
            rc = handle_collfrag_done(coll_frag, coll_request, device);
            if (0 != rc) {
                return count;
            }
        } else if (OPAL_UNLIKELY(0 > ne)) {
            IBOFFLOAD_ERROR(("Device %s: "
                "failed to poll send completion queue\n",
                ibv_get_device_name(device->dev.ib_dev)));
                fatal_error("failed to poll send completion queue\n");
            return count;
        }
    } while (0 != ne);

    return count;
}

int mca_bcol_iboffload_component_progress(void)
{
    int i, count = 0;

    mca_bcol_iboffload_component_t *cm = &mca_bcol_iboffload_component;
    opal_pointer_array_t *devs = &cm->devices;

    int devices_count = cm->num_devs;

    for(i = 0; i < devices_count; ++i) {
        mca_bcol_iboffload_device_t *device =
            opal_pointer_array_get_item(devs, i);

        if (OPAL_LIKELY(device->activated)) {
            count += progress_one_device(device);
        }
    }

    return count;
}

#if OPAL_ENABLE_DEBUG /* debug code */
int task_to_rank(mca_bcol_iboffload_module_t *iboffload, struct mqe_task *task)
{
    int i, j, num_qps = mca_bcol_iboffload_component.num_qps;
    for (i = 0; i < iboffload->num_endpoints; i++) {
        for (j = 0; j < num_qps; j++) {
            if (task->post.qp == iboffload->endpoints[i]->qps[j].qp->lcl_qp) {
                return i;
            }
        }
    }

    return -1; /* not found ! */
}

int wait_to_rank(mca_bcol_iboffload_module_t *iboffload, struct mqe_task *task)
{
    int i, j;
    for (i = 0; i < iboffload->num_endpoints; i++) {
        for (j = 0; j < IBOFFLOAD_CQ_LAST; j++) {
            if (task->wait.cq == iboffload->endpoints[i]->recv_cq[j]) {
                return i;
            }
        }
    }

    return -1; /* not found ! */
}

#endif /* debug code */
