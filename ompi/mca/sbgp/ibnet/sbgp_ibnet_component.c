/*
 * Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
 * Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
 * Copyright (c) 2012 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/**
 * @file
 *
 */

#include "ompi_config.h"
#include "infiniband/verbs.h"
#include <unistd.h>
#include <sys/types.h>
#include <sys/mman.h>
#include <fcntl.h>

#include "ompi/constants.h"
#include "ompi/communicator/communicator.h"
#include "opal/util/argv.h"
#include "opal/include/opal/types.h"
#include "opal_stdint.h"
#include "sbgp_ibnet.h"
#include "sbgp_ibnet_mca.h"
#include "ompi/mca/common/ofacm/base.h"
#include "ompi/mca/common/ofacm/connect.h"
#include "ompi/mca/common/verbs/common_verbs.h"

/*
 * Public string showing the coll ompi_sm V2 component version number
 */
const char *mca_sbgp_ibnet_component_version_string =
    "Open MPI sbgp - ibnet collective MCA component version " OMPI_VERSION;

/*
 * Local functions
 */

static int mca_sbgp_ibnet_open(void);
static int mca_sbgp_ibnet_close(void);
static int mca_sbgp_ibnet_init_query(bool enable_progress_threads,
        bool enable_mpi_threads);

/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */

mca_sbgp_ibnet_component_t mca_sbgp_ibnet_component = {

    /* First, fill in the super */

    {
        /* First, the mca_component_t struct containing meta
           information about the component itself */

        {
            MCA_SBGP_BASE_VERSION_2_0_0,

            /* Component name and version */

            "ibnet",
            OMPI_MAJOR_VERSION,
            OMPI_MINOR_VERSION,
            OMPI_RELEASE_VERSION,

            /* Component open and close functions */

            mca_sbgp_ibnet_open,            /* mca_open_component */
            mca_sbgp_ibnet_close,           /* mca_close_component */
            NULL,                           /* mca_query_component */
            mca_sbgp_ibnet_register_params, /* mca_register_component_params */
        },

        mca_sbgp_ibnet_init_query,          /* sbgp_init_query */
        mca_sbgp_ibnet_select_procs,        /* select_procs */
        0,                                  /* (default) priority */
    },

    /* verbose mode */
    false,

    /* Maximum allowed number of subroups*/
    0,

    /* Enable disable default subnet id warning */
    false,
    false,

    /* IB MTU requested by user */
    0,

    /* IB partition definition */
    0,

    /* Keeping hca data */
    NULL,
    NULL,
    NULL,
    NULL,

    /** Dummy argv-style list; a copy of names from the
      if_[in|ex]clude list that we use for error checking (to ensure
      that they all exist) */
    NULL,
};

static int mca_sbgp_ibnet_dummy_init_query(
    bool enable_progress_threads, bool enable_mpi_threads)
{
    return OMPI_SUCCESS;
}

/*
 * Open the component
 */
static int mca_sbgp_ibnet_open(void)
{
    /* local variables */
    int rc;
    mca_sbgp_ibnet_component_t *cs = &mca_sbgp_ibnet_component;

    cs->total_active_ports = 0;
    cs->curr_max_group_id = 100;

    OBJ_CONSTRUCT(&cs->devices, opal_list_t);

    /* register all parameters including priority */
    rc = mca_sbgp_ibnet_register_params();
    if (OMPI_SUCCESS != rc) {
        return rc;
    }

    return OMPI_SUCCESS;
}

/*
 * Close the component
 */
static int mca_sbgp_ibnet_close(void)
{
    mca_sbgp_ibnet_component_t *cs = &mca_sbgp_ibnet_component;

    OBJ_DESTRUCT(&cs->devices);

    return OMPI_SUCCESS;
}

static void mca_sbgp_ibnet_device_constructor
            (mca_sbgp_ibnet_device_t *device)
{
    /* Init OFACM stuf */
    device->ib_dev = NULL;
    device->device_index = -1;
    device->num_act_ports = 0;
    memset(&device->ib_dev_attr, 0, sizeof(struct ibv_device_attr));
    device->cpcs= NULL;
    device->num_cpcs = 0;
    device->ports = NULL;
}

static void mca_sbgp_ibnet_device_destructor
            (mca_sbgp_ibnet_device_t *device)
{
    /* release memory */
    if (NULL != device->ports) {
        free(device->ports);
    }
}

OBJ_CLASS_INSTANCE(mca_sbgp_ibnet_device_t,
                   opal_list_item_t,
                   mca_sbgp_ibnet_device_constructor,
                   mca_sbgp_ibnet_device_destructor);

static int
get_port_list(mca_sbgp_ibnet_device_t *device, int *allowed_ports)
{
    char *name;
    const char *dev_name;
    int i, j, k, num_ports = 0;

    dev_name = ibv_get_device_name(device->ib_dev);
    name = (char*) malloc(strlen(dev_name) + 4);
    if (NULL == name) {
        return 0;
    }

    num_ports = 0;
    if (NULL != mca_sbgp_ibnet_component.if_include_list) {
        /* If only the device name is given (eg. mtdevice0,mtdevice1) use all
           ports */
        i = 0;

        while (mca_sbgp_ibnet_component.if_include_list[i]) {
            if (0 == strcmp(dev_name,
                            mca_sbgp_ibnet_component.if_include_list[i])) {
                num_ports = device->ib_dev_attr.phys_port_cnt;

                IBNET_VERBOSE(10, ("if_include_list - %s.\n", mca_sbgp_ibnet_component.if_include_list[i]));
                goto done;
            }
            ++i;
        }

        /* Include only requested ports on the device */
        for (i = 1; i <= device->ib_dev_attr.phys_port_cnt; ++i) {
            sprintf(name, "%s:%d", dev_name, i);

            for (j = 0;
                 NULL != mca_sbgp_ibnet_component.if_include_list[j]; ++j) {
                if (0 == strcmp(name,
                                mca_sbgp_ibnet_component.if_include_list[j])) {

                    IBNET_VERBOSE(10, ("Allowed port %d: idx %d; if_include_list - %s\n",
                                       i, num_ports, mca_sbgp_ibnet_component.if_include_list[j]));

                    allowed_ports[num_ports++] = i;
                    break;
                }
            }
        }
    } else if (NULL != mca_sbgp_ibnet_component.if_exclude_list) {
        /* If only the device name is given (eg. mtdevice0,mtdevice1) exclude
           all ports */
        i = 0;
        while (mca_sbgp_ibnet_component.if_exclude_list[i]) {
            if (0 == strcmp(dev_name,
                            mca_sbgp_ibnet_component.if_exclude_list[i])) {
                num_ports = 0;
                goto done;
            }
            ++i;
        }
        /* Exclude the specified ports on this device */
        for (i = 1; i <= device->ib_dev_attr.phys_port_cnt; ++i) {
            sprintf(name,"%s:%d",dev_name,i);
            for (j = 0;
                 NULL != mca_sbgp_ibnet_component.if_exclude_list[j]; ++j) {
                if (0 == strcmp(name,
                                mca_sbgp_ibnet_component.if_exclude_list[j])) {
                    /* If found, set a sentinel value */
                    j = -1;
                    break;
                }
            }
            /* If we didn't find it, it's ok to include in the list */
            if (-1 != j) {
                allowed_ports[num_ports++] = i;
            }
        }
    } else {
        /* Assume that all ports are allowed.  num_ports will be adjusted
           below to reflect whether this is true or not. */
        for (i = 1; i <= device->ib_dev_attr.phys_port_cnt; ++i) {
            allowed_ports[num_ports++] = i;
        }
    }

done:

    /* Remove the following from the error-checking if_list:
       - bare device name
       - device name suffixed with port number */
    if (NULL != mca_sbgp_ibnet_component.if_list) {
        for (i = 0; NULL != mca_sbgp_ibnet_component.if_list[i]; ++i) {
            /* Look for raw device name */
            if (0 == strcmp(mca_sbgp_ibnet_component.if_list[i], dev_name)) {
                j = opal_argv_count(mca_sbgp_ibnet_component.if_list);
                opal_argv_delete(&j, &(mca_sbgp_ibnet_component.if_list),
                                 i, 1);
                --i;
            }
        }

        for (i = 1; i <= device->ib_dev_attr.phys_port_cnt; ++i) {
            sprintf(name, "%s:%d", dev_name, i);
            for (j = 0; NULL != mca_sbgp_ibnet_component.if_list[j]; ++j) {
                if (0 == strcmp(mca_sbgp_ibnet_component.if_list[j], name)) {
                    k = opal_argv_count(mca_sbgp_ibnet_component.if_list);
                    opal_argv_delete(&k, &(mca_sbgp_ibnet_component.if_list),
                                     j, 1);
                    --j;
                    break;
                }
            }
        }
    }

    free(name);

    return num_ports;
}

static int ibnet_init_port(struct mca_sbgp_ibnet_device_t *device,
                           int port_index,  struct ibv_port_attr *ib_port_attr,
                           struct ibv_context *ib_dev_context)
{
    union ibv_gid gid;
    struct mca_sbgp_ibnet_port_t *p = &device->ports[port_index];

    /* Set port data */
    p->lmc  = (1 << ib_port_attr->lmc);
    p->lid  = ib_port_attr->lid;
    p->stat = ib_port_attr->state;
    p->mtu  = ib_port_attr->active_mtu;

    IBNET_VERBOSE(10, ("Setting port data (%s:%d) lid=%d, lmc=%d, stat=%d, mtu=%d\n",
                ibv_get_device_name(device->ib_dev), p->id, p->lid,
                p->lmc, p->stat, p->mtu));

    if (0 != ibv_query_gid(ib_dev_context, p->id, 0, &gid)) {
        IBNET_ERROR(("ibv_query_gid failed (%s:%d)\n",
                    ibv_get_device_name(device->ib_dev), p->id));
        return OMPI_ERR_NOT_FOUND;
    }
    /* set subnet data */
     p->subnet_id = ntoh64(gid.global.subnet_prefix);

/* p->subnet_id = gid.global.subnet_prefix; */

    IBNET_VERBOSE(10, ("my IB-only subnet_id for HCA %d %s port %d is %lx\n" PRIx64,
                gid.global.subnet_prefix,ibv_get_device_name(device->ib_dev), p->id, p->subnet_id));

    return OMPI_SUCCESS;
}

/* Find active port */
static mca_sbgp_ibnet_device_t* ibnet_load_ports(struct ibv_device *ib_dev, int device_index)
{
    struct ibv_context *ib_dev_context = NULL;
    mca_sbgp_ibnet_device_t *device = NULL;
    int *allowed_ports = NULL;
    int rc, port_cnt, port, i, ret, p = 0;

#if defined(HAVE_STRUCT_IBV_DEVICE_TRANSPORT_TYPE)
    if (IBV_TRANSPORT_IB != ib_dev->transport_type) {
        IBNET_VERBOSE(10, ("Skipping non IB device %s",
                    ibv_get_device_name(ib_dev)));
        goto error;
    }
#endif

    device = OBJ_NEW(mca_sbgp_ibnet_device_t);
    device->ib_dev = ib_dev;
    device->device_index = device_index;
    ib_dev_context = ibv_open_device(ib_dev);

    if(NULL == ib_dev_context) {
        IBNET_ERROR(("Error obtaining device context for %s errno says %s",
                    ibv_get_device_name(device->ib_dev), strerror(errno)));
        goto error;
    }

    if(ibv_query_device(ib_dev_context, &device->ib_dev_attr)) {
        IBNET_ERROR(("error obtaining device attributes for %s errno says %s",
                    ibv_get_device_name(ib_dev), strerror(errno)));
        goto error;
    }

    allowed_ports = (int *) calloc(device->ib_dev_attr.phys_port_cnt, sizeof(int));
    if (NULL == allowed_ports) {
        goto error;
    }

    port_cnt = get_port_list(device, allowed_ports);
    if (0 == port_cnt) {
        goto error;
    }

#if OPAL_ENABLE_DEBUG
    for (i = 0; i < port_cnt; ++i) {
        IBNET_VERBOSE(10, ("allowed port %d with idx %d.\n", allowed_ports[i], i));
    }
#endif

    device->num_allowed_ports = port_cnt;
    device->ports = (mca_sbgp_ibnet_port_t *) calloc(port_cnt, sizeof(mca_sbgp_ibnet_port_t));
    if (NULL == device->ports) {
        goto error;
    }

    /* Note ports are 1 based (i >= 1) */
    for(port = 0; port < port_cnt; port++) {
        struct ibv_port_attr ib_port_attr;

        i = allowed_ports[port];
        if(ibv_query_port(ib_dev_context, i, &ib_port_attr)){
            IBNET_ERROR(("Error getting port attributes for device %s "
                        "port number %d errno says %s",
                        ibv_get_device_name(device->ib_dev), i, strerror(errno)));
            continue;
        }

        if(IBV_PORT_ACTIVE == ib_port_attr.state) {
            /* Pasha: Need to think how we want to handle MTUs
            if (ib_port_attr.active_mtu < mca_bcol_iboffload_component.mtu){
                device->mtu = ib_port_attr.active_mtu;
            }
            */
            /* start to put port info */
            device->ports[p].id = i;
            device->ports[p].stat = ib_port_attr.state;
            device->ports[p].mtu = ib_port_attr.active_mtu;

            device->ports[p].used = true;

            if (0 == mca_sbgp_ibnet_component.pkey_val) {
                ret = ibnet_init_port(device, p, &ib_port_attr, ib_dev_context);
                if (OMPI_SUCCESS != ret) {
                    IBNET_ERROR(("Device %s "
                                "port number %d , failed to init port, errno says %s",
                                ibv_get_device_name(device->ib_dev),
                                i, strerror(errno)));
                    continue;
                }
            } else {
                uint16_t pkey,j;
                device->ports[p].used = false;

                for (j = 0; j < device->ib_dev_attr.max_pkeys; j++) {
                    if(ibv_query_pkey(ib_dev_context, i, j, &pkey)){
                        IBNET_ERROR(("error getting pkey for index %d, device %s "
                                    "port number %d errno says %s",
                                    j, ibv_get_device_name(device->ib_dev), i, strerror(errno)));
                        continue;
                    }

                    pkey = ntohs(pkey) & MCA_SBGP_IBNET_PKEY_MASK;
                    if (pkey == mca_sbgp_ibnet_component.pkey_val){
                        ret = ibnet_init_port(device, p, &ib_port_attr, ib_dev_context);
                        if (OMPI_SUCCESS != ret) {
                            IBNET_ERROR(("Device %s "
                                        "port number %d , failed to init port, errno says %s",
                                        ibv_get_device_name(device->ib_dev),
                                        i, strerror(errno)));
                            continue;
                        }
                    }
                }
            }

            p++; /* One port was loaded, go to the next one */
        }
    }

    device->num_act_ports = p;
    /* Update total number of active ports */
    mca_sbgp_ibnet_component.total_active_ports += p;

    if (0 != device->num_act_ports) {
        ompi_common_ofacm_base_dev_desc_t dev;
        /* Init dev */
        dev.ib_dev = ib_dev;
        dev.ib_dev_context = ib_dev_context;
        dev.capabilities = 0;

        rc = ompi_common_ofacm_base_select_for_local_port(
                &dev, &device->cpcs, (int *)&device->num_cpcs);
        /* If we get NOT_SUPPORTED, then no CPC was found for this
           port.  But that's not a fatal error -- just keep going;
           let's see if we find any usable openib modules or not. */
        if (OMPI_SUCCESS != rc) {
            /* All others *are* fatal.  Note that we already did a
               show_help in the lower layer */
            IBNET_VERBOSE(10, ("Device %s, no CPC found",
                        ibv_get_device_name(device->ib_dev)));
            goto error;
        }
    }

    /* we do not continue to use the device we just collect data,
     * so close it for now. We will open it later in iboffload coll*/
    if(ibv_close_device(ib_dev_context)) {
        IBNET_ERROR(("Device %s, failed to close the device %s",
                    ibv_get_device_name(device->ib_dev), strerror(errno)));
    }

    if (0 == device->num_act_ports) {
        goto error;
    }

    /* Pasha - I do not like the error flow here */
    free(allowed_ports);

    return device;

error:

    if (NULL != allowed_ports) {
        free(allowed_ports);
    }

    OBJ_DESTRUCT(device);

    return NULL;
}

/* Create list of IB hca that have active port */
static int ibnet_load_devices(void)
{
    int num_devs, i;
    struct ibv_device **ib_devs = NULL;

    mca_sbgp_ibnet_device_t *device = NULL;
    mca_sbgp_ibnet_component_t *cs = &mca_sbgp_ibnet_component;

    IBNET_VERBOSE(7, ("Entering to ibnet_load_devices"));

    /* Get list of devices */
    ib_devs = ompi_ibv_get_device_list(&num_devs);

    if(0 == num_devs || NULL == ib_devs) {
        IBNET_VERBOSE(10, ("No ib devices found"));
        /* No hca error*/
        ompi_show_help("help-mpi-sbgp-ibnet.txt", "no-nics", true);
        return OMPI_ERROR;
    }

    for (i = 0; i < num_devs; i++) {
        device = ibnet_load_ports(ib_devs[i], i);
        if (NULL != device) {
            IBNET_VERBOSE(10, ("Device %s was appended to device list with index %d.\n",
                          ibv_get_device_name(device->ib_dev), i));
            opal_list_append(&cs->devices,
                    (opal_list_item_t *) device);
        }
    }

    if (opal_list_is_empty(&cs->devices)) {
        /* No relevand devices were found, return error */
        IBNET_ERROR(("No active devices found"));
        return OMPI_ERROR;
        /* Maybe need to add error here*/
    }

    ompi_ibv_free_device_list(ib_devs);

    return OMPI_SUCCESS;
}

/* query to see if the component is available for use, and can
 * satisfy the thread and progress requirements
 */
int mca_sbgp_ibnet_init_query(bool enable_progress_threads,
        bool enable_mpi_threads)
{
    int rc, list_count = 0;

    /* Parse the include and exclude lists, checking for errors */
    mca_sbgp_ibnet_component.if_list = NULL;
    mca_sbgp_ibnet_component.if_include_list = NULL;
    mca_sbgp_ibnet_component.if_exclude_list = NULL;

    IBNET_VERBOSE(7, ("Calling mca_sbgp_ibnet_init_query"));

    if (NULL != mca_sbgp_ibnet_component.if_include) {
      list_count++;
    }

    if (NULL != mca_sbgp_ibnet_component.if_exclude) {
      list_count++;
    }

    if (list_count > 1) {
        IBNET_ERROR(("Bad --mca (if_include, if_exclude) parameters !"));
        return OMPI_ERROR;
    } else if (NULL != mca_sbgp_ibnet_component.if_include) {
        mca_sbgp_ibnet_component.if_include_list =
            opal_argv_split(mca_sbgp_ibnet_component.if_include, ',');
        mca_sbgp_ibnet_component.if_list =
            opal_argv_copy(mca_sbgp_ibnet_component.if_include_list);
    } else if (NULL != mca_sbgp_ibnet_component.if_exclude) {
        mca_sbgp_ibnet_component.if_exclude_list =
            opal_argv_split(mca_sbgp_ibnet_component.if_exclude, ',');
        mca_sbgp_ibnet_component.if_list =
            opal_argv_copy(mca_sbgp_ibnet_component.if_exclude_list);
    }

    /* Init CPC components */
    rc = ompi_common_ofacm_base_init();
    if (OMPI_SUCCESS != rc) {
        return rc;
    }

    /* Load all devices and active ports */
    rc = ibnet_load_devices();
    if (OMPI_SUCCESS != rc) {
        return rc;
    }

    mca_sbgp_ibnet_component.super.sbgp_init_query =
                           mca_sbgp_ibnet_dummy_init_query;

    return OMPI_SUCCESS;
}
