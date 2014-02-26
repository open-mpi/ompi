/*
 * Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
 *                         All rights reserved.
 * Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
 * Copyright (c) 2012-2014 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef _COMMON_OFAUTILS_H_
#define _COMMON_OFAUTILS_H_

#include "ompi_config.h"

#include <stdint.h>
#include <infiniband/verbs.h>

#include "opal/mca/mca.h"

#include <infiniband/verbs.h>

#include "opal/class/opal_list.h"

BEGIN_C_DECLS

/*
 * common_verbs_devlist.c
 */
OMPI_DECLSPEC struct ibv_device **ompi_ibv_get_device_list(int *num_devs);
OMPI_DECLSPEC void ompi_ibv_free_device_list(struct ibv_device **ib_devs);

/*
 * common_verbs_mca.c
 */
extern bool ompi_common_verbs_warn_nonexistent_if;
OMPI_DECLSPEC void ompi_common_verbs_mca_register(mca_base_component_t *component);

/*
 * common_verbs_basics.c
 */
bool ompi_common_verbs_check_basics(void);

/*
 * common_verbs_find_ports.c
 */
typedef struct ompi_common_verbs_device_item_t {
    opal_object_t super;

    struct ibv_device *device;
    char *device_name;
    struct ibv_context *context;
    struct ibv_device_attr device_attr;

    /** This field defaults to true, meaning that the destructor for
        ompi_common_verbs_device_item_t will invoke ibv_close_device()
        on the context.  An upper layer can reset this field to false,
        however, indicating that the destructor should *not* invoke
        ibv_close_device() (e.g., if the upper layer has copied the
        context and is using it). */
    bool destructor_free_context;
} ompi_common_verbs_device_item_t;
OBJ_CLASS_DECLARATION(ompi_common_verbs_device_item_t);

typedef struct ompi_common_verbs_port_item_t {
    opal_list_item_t super;

    ompi_common_verbs_device_item_t *device;
    uint8_t port_num;
    struct ibv_port_attr port_attr;
} ompi_common_verbs_port_item_t;
OBJ_CLASS_DECLARATION(ompi_common_verbs_port_item_t);

enum {
    OMPI_COMMON_VERBS_FLAGS_RC = 0x1,
    OMPI_COMMON_VERBS_FLAGS_NOT_RC = 0x2,
    OMPI_COMMON_VERBS_FLAGS_UD = 0x4,
    OMPI_COMMON_VERBS_FLAGS_TRANSPORT_IB = 0x8,
    OMPI_COMMON_VERBS_FLAGS_TRANSPORT_IWARP = 0x10,
    OMPI_COMMON_VERBS_FLAGS_TRANSPORT_USNIC = 0x20,
    OMPI_COMMON_VERBS_FLAGS_TRANSPORT_USNIC_UDP = 0x40,
    /* Note that these 2 link layer flags will only be useful if
       defined(HAVE_IBV_LINK_LAYER_ETHERNET). Otherwise, they will be
       ignored. */
    OMPI_COMMON_VERBS_FLAGS_LINK_LAYER_IB = 0x80,
    OMPI_COMMON_VERBS_FLAGS_LINK_LAYER_ETHERNET = 0x100,
    OMPI_COMMON_VERBS_FLAGS_MAX
};

enum {
    /* a constant used when probing the usNIC transport type (custom L2 vs.
     * UDP/IP) */
    OMPI_COMMON_VERBS_USNIC_PROBE_MAGIC = 42
};

/**
 * Find a list of ibv_device ports that match a specific criteria. 
 *
 * @param if_include (IN): comma-delimited list of interfaces to use
 * @param if_exclude (IN): comma-delimited list of interfaces to NOT use
 * @param flags (IN): bit flags
 * @param verbose_stream (IN): stream to send opal_output_verbose messages to
 *
 * The ports will adhere to the if_include / if_exclude lists (only
 * one can be specified).  The lists are comma-delimited tokens in one
 * of two forms:
 *
 * interface_name
 * interface_name:port
 *
 * Hence, a if_include list could be the follwing: "mlx4_0,mthca0:1".
 *
 * The flags provide logical OR behavior -- a port will be included if
 * it includes any of the capabilities/characteristics listed in the
 * flags.
 *
 * Note that if the verbose_stream is >=0, output will be sent to that
 * stream with a verbose level of 5.
 *
 * A valid list will always be returned.  It will contain zero or more
 * ompi_common_verbs_port_item_t items.  Each item can be individually
 * OBJ_RELEASE'd; the destructor will take care of cleaning up the
 * linked ompi_common_verbs_device_item_t properly (i.e., when all
 * port_items referring to it have been freed).
 */
OMPI_DECLSPEC opal_list_t *
ompi_common_verbs_find_ports(const char *if_include, 
                             const char *if_exclude, 
                             int flags,
                             int verbose_stream);

/*
 * Trivial function to compute the bandwidth on an ibv_port.
 *
 * Will return OMPI_ERR_NOT_FOUND if it can't figure out the bandwidth
 * (and the bandwidth parameter value will be undefined).  Otherwise,
 * will return OMPI_SUCCESS and set bandwidth to an appropriate value.
 */
OMPI_DECLSPEC int
ompi_common_verbs_port_bw(struct ibv_port_attr *port_attr,
                          uint32_t *bandwidth);

/*
 * Trivial function to switch on the verbs MTU enum and return a
 * numeric value.
 */
OMPI_DECLSPEC int
ompi_common_verbs_mtu(struct ibv_port_attr *port_attr);

/*
 * Find the max_inline_data value for a given device
 */
OMPI_DECLSPEC int
ompi_common_verbs_find_max_inline(struct ibv_device *device,
                                  struct ibv_context *context,
                                  struct ibv_pd *pd,
                                  uint32_t *max_inline_arg);

/*
 * Test a device to see if it can handle a specific QP type (RC and/or
 * UD).  Will return the logical AND if multiple types are specified
 * (e.g., if (RC|UD) are in flags, then will return OMPI_SUCCESS only
 * if *both* types can be created on the device).
 *
 * Flags can be the logical OR of OMPI_COMMON_VERBS_FLAGS_RC and/or
 * OMPI_COMMON_VERBS_FLAGS_UD.  All other values are ignored.
 */
OMPI_DECLSPEC int ompi_common_verbs_qp_test(struct ibv_context *device_context, 
                                            int flags);

END_C_DECLS

#endif

