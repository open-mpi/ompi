/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2008 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2014 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2006-2012 Mellanox Technologies.  All rights reserved.
 * Copyright (c) 2006-2007 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2006-2007 Voltaire All rights reserved.
 * Copyright (c) 2009      Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>
#include <string.h>
#include <infiniband/verbs.h>
#include <stdint.h>

#include "opal_stdint.h"
#include "opal/types.h"
#include "opal/util/output.h"
#include "opal/util/argv.h"
#include "opal/class/opal_object.h"
#include "opal/util/show_help.h"

#include "ompi/constants.h"
#include "ompi/mca/rte/rte.h"

#include "common_verbs.h"

/***********************************************************************/

static void device_item_construct(ompi_common_verbs_device_item_t *di)
{
    di->device = NULL;
    di->device_name = NULL;
    di->context = NULL;
    di->destructor_free_context = true;
    memset(&di->device_attr, 0, sizeof(di->device_attr));
}


static void device_item_destruct(ompi_common_verbs_device_item_t *di)
{
    if (NULL != di->device_name) {
        free(di->device_name);
    }

    /* Only free the context if a) the device is open, and b) the
       upper layer didn't tell us not to */
    if (NULL != di->context && di->destructor_free_context) {
        ibv_close_device(di->context);
    }

    /* Zero out all the fields */
    device_item_construct(di);
}


OBJ_CLASS_INSTANCE(ompi_common_verbs_device_item_t,
                   opal_object_t,
                   device_item_construct,
                   device_item_destruct);

/***********************************************************************/

static void port_item_construct(ompi_common_verbs_port_item_t *pi)
{
    pi->device = NULL;
    pi->port_num = 0;
    memset(&pi->port_attr, 0, sizeof(pi->port_attr));
}


static void port_item_destruct(ompi_common_verbs_port_item_t *pi)
{
    OBJ_RELEASE(pi->device);
    /* Zero out all the fields */
    port_item_construct(pi);
}


OBJ_CLASS_INSTANCE(ompi_common_verbs_port_item_t,
                   opal_list_item_t,
                   port_item_construct,
                   port_item_destruct);

/***********************************************************************/

/*
 * Given a list of include or exclude items (never both), determine
 * whether we want the current port or not.
 */
static bool want_this_port(char **include_list, char **exclude_list, 
                           ompi_common_verbs_device_item_t *di, int port)
{
    int i;
    char name[1024];

    /* If we have no include or exclude list, then we unconditionally
       want the port */
    if (NULL == include_list && NULL == exclude_list) {
        return true;
    }

    /* Search the include list */
    if (NULL != include_list) {
        for (i = 0; NULL != include_list[i]; ++i) {
            /* First check if we can find the naked device name */
            if (strcmp(di->device_name, include_list[i]) == 0) {
                return true;
            }

            /* Now check for the specific port number */
            snprintf(name, sizeof(name), "%s:%d", di->device_name, port);
            if (strcmp(name, include_list[i]) == 0) {
                return true;
            }
        }

        /* Didn't find it.  So we don't want it. */
        return false;
    } 

    /* Search the exclude list */
    else {
        for (i = 0; NULL != exclude_list[i]; ++i) {
            /* First check if we can find the naked device name */
            if (strcmp(di->device_name, exclude_list[i]) == 0) {
                return false;
            }

            /* Now check for the specific port number */
            snprintf(name, sizeof(name), "%s:%d", di->device_name, port);
            if (strcmp(name, exclude_list[i]) == 0) {
                return false;
            }
        }

        /* Didn't find it.  So we want it. */
        return true;
    }

    /* Will never get here */
}

/***********************************************************************/

#if HAVE_DECL_IBV_LINK_LAYER_ETHERNET
static const char *link_layer_to_str(int link_type)
{
    switch(link_type) {
    case IBV_LINK_LAYER_INFINIBAND:   return "IB";
    case IBV_LINK_LAYER_ETHERNET:     return "IWARP";
    case IBV_LINK_LAYER_UNSPECIFIED: 
    default:                          return "unspecified";
    }
}
#endif

/* Helper routine to detect Cisco usNIC devices (these are non-IB, non-RoCE,
 * non-iWARP devices).  See the usnic BTL for more information.
 *
 * Once usNIC is no longer new and the IBV_TRANSPORT_USNIC constant is widely
 * available in the wild, all calls to it can be replaced with a simple check
 * against said constant.
 */
static bool device_is_usnic(struct ibv_device *device)
{
    /* A usNIC-capable VIC will present as one of:
     *   1. _IWARP -- any libibverbs and old kernel
     *   2. _UNKNOWN -- old libibverbs and new kernel
     *   3. _USNIC -- new libibverbs and new kernel
     *
     * Where an "old kernel" is one that does not have this commit:
     * http://bit.ly/kernel-180771a3
     */
#if HAVE_DECL_IBV_TRANSPORT_USNIC
    if (IBV_TRANSPORT_USNIC == device->transport_type) {
        return true;
    }
#endif
#if HAVE_DECL_IBV_TRANSPORT_USNIC_UDP
    if (IBV_TRANSPORT_USNIC_UDP == device->transport_type) {
        return true;
    }
#endif
    if ((IBV_TRANSPORT_IWARP == device->transport_type ||
         IBV_TRANSPORT_UNKNOWN == device->transport_type) &&
        0 == strncmp(device->name, "usnic_", strlen("usnic_"))) {
        /* if we are willing to open the device, query its attributes, then
         * close it again, we could also check for Cisco's vendor ID (0x1137) */
        return true;
    }

    return false;
}

enum {
    USNIC_L2,
    USNIC_UDP,
    USNIC_UNKNOWN
};

/* See comment in btl_usnic_ext.c about why we must check the return
   from the usnic verbs extensions probe for a magic number (which
   means we must also copy the usnic extension struct and magic number
   value down here into common/verbs.  Bummer). */
typedef union {
    struct {
        int lookup_version;
        uint64_t magic;
    } qpt;
    struct ibv_port_attr attr;
} port_query_u;

#define USNIC_PORT_QUERY_MAGIC (0x43494e7375534355ULL)

/*
 * Probe for the magic number to see if the userspace side of verbs is
 * new enough to include UDP transport support.
 *
 * If the userspace side is too old to include UDP support, then it
 * will fail the magic probe.  If somehow we eneded up with a "new"
 * userspace (e.g., that supports UDP) and an "old" kernel module
 * (e.g., that does not support UDP), then the userspace will fail the
 * ABI check with the kernel module and we won't get this far at all.
 *
 * NB: it will be complicated if we ever need to extend this scheme
 * (e.g., if we support something other than UDP someday), because the
 * real way to know what the actual transport is will be to call a
 * usnic verbs extension, and that code is all currently over in the
 * usnic BTL, which we can't call from here.
 */
static int usnic_magic_probe(struct ibv_context *context)
{
    int rc;
    port_query_u u;

    rc = ibv_query_port(context, 42, &u.attr);
    /* See comment in btl_usnic_ext.c about why we have to check
       for rc==0 *and* the magic number. */
    if (0 == rc && USNIC_PORT_QUERY_MAGIC == u.qpt.magic) {
        /* We only support version 1 of the lookup function in
           this particular version of Open MPI */
        if (1 == u.qpt.lookup_version) {
            return USNIC_UDP;
        } else {
            return USNIC_UNKNOWN;
        }
    } else {
        return USNIC_L2;
    }
}

/*
 * usNIC devices will always return one of these
 * device->transport_type values:
 *
 * 1. TRANSPORT_IWARP: for older kernels (e.g., on systems such as
 * RHEL 6.x (x>=4) with the drivers downloaded from cisco.com) where
 * the cisco.com drivers could not modify verbs.h to include
 * TRANSPORT_USNIC*.  In this case, it is unknown whether the
 * transport is usNIC/L2 or usNIC/UDP -- you have to do an additional
 * probe to figure it out.
 *
 * 2. TRANSPORT_USNIC: for some systems that updated to include the
 * RDMA_TRANSPORT_USNIC constant, but not the RDMA_TRANSPORT_USNIC_UDP
 * constant, with the drivers downloaded from cisco.com (e.g., RHEL
 * 7.0).  This is just like the TRANSPORT_IWARP case: we have to do an
 * additional probe to figure out whether the transport is usNIC/L2 or
 * usNIC/UDP.
 *
 * 3. TRANSPORT_USNIC_UDP: on systems with new kernels and new
 * libibverbs.  In this case, the transport is guaranteed to be
 * usNIC/UDP.
 *
 * 4. TRANSPORT_UNKNOWN: on systems with a new kernel but an old
 * libibverbs (i.e., kernel understands/returns TRANSPORT_USNIC*
 * values, but libibverbs doesn't understant the TRANSPORT_USNIC*
 * constants, and therefore returns TRANSPORT_UNKNOWN).
 */
static int usnic_transport(struct ibv_device *device,
                           struct ibv_context *context)
{
    if (!device_is_usnic(device)) {
        return USNIC_UNKNOWN;
    }

#if HAVE_DECL_IBV_TRANSPORT_USNIC_UDP
    /* If we got the transport type of USNIC_UDP, then it's definitely
       the UDP transport. */
    if (IBV_TRANSPORT_USNIC_UDP == device->transport_type) {
        return USNIC_UDP;
    }
#endif

    /* All other cases require a secondary check to figure out whether
       the transport is L2 or UDP */
    return usnic_magic_probe(context);
}

/***********************************************************************/

static void check_sanity(char ***if_sanity_list, const char *dev_name, int port)
{
    int i;
    char tmp[BUFSIZ], **list = *if_sanity_list;
    const char *compare;

    if (NULL == if_sanity_list || NULL == *if_sanity_list) {
        return;
    }

    /* A match is found if:
       - "dev_name" is in the list and port == -1, or
       - "dev_name:port" is in the list
       If a match is found, remove that entry from the list. */
    memset(tmp, 0, sizeof(tmp));
    if (port > 0) {
        snprintf(tmp, sizeof(tmp) - 1, "%s:%d", dev_name, port);
        compare = tmp;
    } else {
        compare = dev_name;
    }

    for (i = 0; NULL != list[i]; ++i) {
        if (0 == strcmp(list[i], compare)) {
            int count = opal_argv_count(list);
            opal_argv_delete(&count, &list, i, 1);
            *if_sanity_list = list;
            --i;
        }
    }
}

/***********************************************************************/

/*
 * Find a list of ibv_ports matching a set of criteria.
 */
opal_list_t *ompi_common_verbs_find_ports(const char *if_include, 
                                          const char *if_exclude, 
                                          int flags,
                                          int stream)
{
    int32_t num_devs;
    struct ibv_device **devices;
    struct ibv_device *device;
    struct ibv_context *device_context;
    struct ibv_device_attr device_attr;
    struct ibv_port_attr port_attr;
    char **if_include_list = NULL, **if_exclude_list = NULL, **if_sanity_list = NULL;
    ompi_common_verbs_device_item_t *di;
    ompi_common_verbs_port_item_t *pi;
    int rc;
    uint32_t i, j;
    opal_list_t *port_list = NULL;
    opal_list_item_t *item;
    bool want, dev_is_usnic;

    /* Allocate a list to fill */
    port_list = OBJ_NEW(opal_list_t);
    if (NULL == port_list) {
        goto err_free_argv;
    }

    /* Sanity check the include/exclude params */
    if (NULL != if_include && NULL != if_exclude) {
        return port_list;
    } else if (NULL != if_include) {
        opal_output_verbose(5, stream, "finding verbs interfaces, including %s", 
                            if_include);
        if_include_list = opal_argv_split(if_include, ',');
        if_sanity_list = opal_argv_copy(if_include_list);
    } else if (NULL != if_exclude) {
        opal_output_verbose(5, stream, "finding verbs interfaces, excluding %s", 
                            if_exclude);
        if_exclude_list = opal_argv_split(if_exclude, ',');
        if_sanity_list = opal_argv_copy(if_exclude_list);
    }

    /* Query all the IBV devices on the machine.  Use an ompi
       compatibility function, because how to get this list changed
       over the history of the IBV API. */
    devices = ompi_ibv_get_device_list(&num_devs);
    if (0 == num_devs) {
        opal_output_verbose(5, stream, "no verbs interfaces found");
        goto err_free_argv;
    } else {
        opal_output_verbose(5, stream, "found %d verbs interface%s", 
                            num_devs, (num_devs != 1) ? "s" : "");
    }

    /* Now loop through all the devices.  Get the attributes for each
       port on each device to see if they match our selection
       criteria. */
    for (i = 0; (int32_t) i < num_devs; ++i) {
        /* See if this device is on the include/exclude sanity check
           list.  If it is, remove it from the sanity check list
           (i.e., we should end up with an empty list at the end if
           all entries in the sanity check list exist) */
        device = devices[i];
        check_sanity(&if_sanity_list, ibv_get_device_name(device), -1);

        opal_output_verbose(5, stream, "examining verbs interface: %s",
                            ibv_get_device_name(device));

        dev_is_usnic = false;
        if ((flags & OMPI_COMMON_VERBS_FLAGS_TRANSPORT_USNIC) ||
            (flags & OMPI_COMMON_VERBS_FLAGS_TRANSPORT_USNIC_UDP)) {
            dev_is_usnic = device_is_usnic(device);
        }

        device_context = ibv_open_device(device);
        if (NULL == device_context) {
            opal_show_help("help-ompi-common-verbs.txt",
                           "ibv_open_device fail", true,
                           ompi_process_info.nodename,
                           ibv_get_device_name(device),
                           errno, strerror(errno));
            goto err_free_port_list;
        }

        if (ibv_query_device(device_context, &device_attr)){
            opal_show_help("help-ompi-common-verbs.txt",
                           "ibv_query_device fail", true,
                           ompi_process_info.nodename,
                           ibv_get_device_name(device),
                           errno, strerror(errno));
            goto err_free_port_list;
        }

        /* Now that we have the attributes of this device, remove all
           ports of this device from the sanity check list.  Note that
           IBV ports are indexed from 1, not 0. */
        for (j = 1; j <= device_attr.phys_port_cnt; j++) {
            check_sanity(&if_sanity_list, ibv_get_device_name(device), j);
        }

        /* Check the device-specific flags to see if we want this
           device */
        want = false;

        if (flags & OMPI_COMMON_VERBS_FLAGS_TRANSPORT_USNIC &&
            dev_is_usnic &&
            USNIC_L2 == usnic_transport(device, device_context)) {
            want = true;
            opal_output_verbose(5, stream,
                                "verbs interface %s has the right transport (usNIC/L2)",
                                ibv_get_device_name(device));
        }
        if (flags & OMPI_COMMON_VERBS_FLAGS_TRANSPORT_USNIC_UDP &&
            dev_is_usnic &&
            USNIC_UDP == usnic_transport(device, device_context)) {
            want = true;
            opal_output_verbose(5, stream,
                                "verbs interface %s has the right transport (usNIC/UDP)",
                                ibv_get_device_name(device));
        }
        if (flags & OMPI_COMMON_VERBS_FLAGS_TRANSPORT_IB &&
            IBV_TRANSPORT_IB == device->transport_type) {
            opal_output_verbose(5, stream, "verbs interface %s has right type (IB)",
                                ibv_get_device_name(device));
            want = true;
        }
        if (flags & OMPI_COMMON_VERBS_FLAGS_TRANSPORT_IWARP &&
            IBV_TRANSPORT_IWARP == device->transport_type) {
            opal_output_verbose(5, stream, "verbs interface %s has right type (IWARP)",
                                ibv_get_device_name(device));
            want = true;
        }

        /* Check for RC or UD QP support */
        if (flags & OMPI_COMMON_VERBS_FLAGS_RC) {
            rc = ompi_common_verbs_qp_test(device_context, flags);
            if (OMPI_SUCCESS == rc) {
                want = true;
                opal_output_verbose(5, stream,
                                    "verbs interface %s supports RC QPs",
                                    ibv_get_device_name(device));
            } else {
                opal_output_verbose(5, stream,
                                    "verbs interface %s failed to make RC QP",
                                    ibv_get_device_name(device));
            }
        }
        if (flags & OMPI_COMMON_VERBS_FLAGS_UD) {
            rc = ompi_common_verbs_qp_test(device_context, flags);
            if (OMPI_SUCCESS == rc) {
                want = true;
                opal_output_verbose(5, stream,
                                    "verbs interface %s supports UD QPs",
                                    ibv_get_device_name(device));
            } else if (OMPI_ERR_TYPE_MISMATCH == rc) {
                opal_output_verbose(5, stream,
                                    "verbs interface %s made an RC QP! we don't want RC-capable devices",
                                    ibv_get_device_name(device));
            } else {
                opal_output_verbose(5, stream,
                                    "verbs interface %s failed to make UD QP",
                                    ibv_get_device_name(device));
            }
        }

        /* If we didn't want it, go to the next device */
        if (!want) {
            continue;
        }

        /* Make a device_item_t to hold the device information */
        di = OBJ_NEW(ompi_common_verbs_device_item_t);
        if (NULL == di) {
            goto err_free_port_list;
        }
        di->device = device;
        di->context = device_context;
        di->device_attr = device_attr;
        di->device_name = strdup(ibv_get_device_name(device));

        /* Note IBV ports are 1 based (not 0 based) */
        for (j = 1; j <= device_attr.phys_port_cnt; j++) {

            /* If we don't want this port (based on if_include /
               if_exclude lists), skip it */
            if (!want_this_port(if_include_list, if_exclude_list, di, j)) {
                opal_output_verbose(5, stream, "verbs interface %s:%d: rejected by include/exclude",
                                    ibv_get_device_name(device), j);
                continue;
            }

            /* Query the port */
            if (ibv_query_port(device_context, (uint8_t) j, &port_attr)) {
                opal_show_help("help-ompi-common-verbs.txt",
                               "ibv_query_port fail", true,
                               ompi_process_info.nodename,
                               ibv_get_device_name(device),
                               errno, strerror(errno));
                goto err_free_port_list;
            }

            /* We definitely only want ACTIVE ports */
            if (IBV_PORT_ACTIVE != port_attr.state) {
                opal_output_verbose(5, stream, "verbs interface %s:%d: not ACTIVE",
                                    ibv_get_device_name(device), j);
                continue;
            }

            /* Check the port-specific flags to see if we want this
               port */
            want = false;
            if (0 == flags) {
                want = true;
            }

            if ((flags & (OMPI_COMMON_VERBS_FLAGS_LINK_LAYER_IB |
                          OMPI_COMMON_VERBS_FLAGS_LINK_LAYER_ETHERNET)) ==
                 (OMPI_COMMON_VERBS_FLAGS_LINK_LAYER_IB |
                  OMPI_COMMON_VERBS_FLAGS_LINK_LAYER_ETHERNET)) {
                /* If they specified both link layers, then we want this port */
                want = true;
            } else if ((flags & (OMPI_COMMON_VERBS_FLAGS_LINK_LAYER_IB |
                                 OMPI_COMMON_VERBS_FLAGS_LINK_LAYER_ETHERNET)) == 0) {
                /* If they specified neither link layer, then we want this port */
                want = true;
            } 
#if HAVE_DECL_IBV_LINK_LAYER_ETHERNET
            else if (flags & OMPI_COMMON_VERBS_FLAGS_LINK_LAYER_IB) {
                if (IBV_LINK_LAYER_INFINIBAND == port_attr.link_layer) {
                    want = true;
                } else {
                    opal_output_verbose(5, stream, "verbs interface %s:%d has wrong link layer (has %s, want IB)",
                                        ibv_get_device_name(device), j,
                                        link_layer_to_str(port_attr.link_layer));
                }
            } else if (flags & OMPI_COMMON_VERBS_FLAGS_LINK_LAYER_ETHERNET) {
                if (IBV_LINK_LAYER_ETHERNET == port_attr.link_layer) {
                    want = true;
                } else {
                    opal_output_verbose(5, stream, "verbs interface %s:%d has wrong link layer (has %s, want Ethernet)",
                                        ibv_get_device_name(device), j,
                                        link_layer_to_str(port_attr.link_layer));
                }
            }
#endif

            if (!want) {
                continue;
            }

            /* If we got this far, we want the port.  Make an item for it. */
            pi = OBJ_NEW(ompi_common_verbs_port_item_t);
            if (NULL == pi) {
                goto err_free_port_list;
            }
            pi->device = di;            
            pi->port_num = j;
            pi->port_attr = port_attr;
            OBJ_RETAIN(di);

            /* Add the port item to the list */
            opal_list_append(port_list, &pi->super);
            opal_output_verbose(5, stream, "found acceptable verbs interface %s:%d",
                                ibv_get_device_name(device), j);
        }

        /* We're done with the device; if some ports are using it, its
           ref count will be > 0, and therefore the device won't be
           deleted here. */
        OBJ_RELEASE(di);
    }

    /* Sanity check that the devices specified in the if_include /
       if_exclude lists actually existed.  If this is true, then the
       sanity list will now be empty.  If there are still items left
       on the list, then they didn't exist.  Bad.  Print a warning (if
       the warning is not disabled). */
    if (0 != opal_argv_count(if_sanity_list)) {
        if (ompi_common_verbs_warn_nonexistent_if) {
            char *str = opal_argv_join(if_sanity_list, ',');
            opal_show_help("help-ompi-common-verbs.txt", "nonexistent port",
                           true, ompi_process_info.nodename,
                           ((NULL != if_include) ? "in" : "ex"), str);
            free(str);

            /* Only warn once per process */
            ompi_common_verbs_warn_nonexistent_if = false;
        }
    }
    if (NULL != if_sanity_list) {
        opal_argv_free(if_sanity_list);
    }

    /* All done! */
    ompi_ibv_free_device_list(devices);
    return port_list;

 err_free_port_list:
    for (item = opal_list_remove_first(port_list);
         item != NULL; 
         item = opal_list_remove_first(port_list)) {
        OBJ_RELEASE(item);
    }
    ompi_ibv_free_device_list(devices);

 err_free_argv:
    if (NULL != if_sanity_list) {
        opal_argv_free(if_sanity_list);
        if_sanity_list = NULL;
    }
    opal_argv_free(if_include_list);
    if_include_list = NULL;
    opal_argv_free(if_exclude_list);
    if_exclude_list = NULL;

    return port_list;
}
