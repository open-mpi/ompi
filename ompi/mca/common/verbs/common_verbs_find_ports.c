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
 * Copyright (c) 2006-2012 Cisco Systems, Inc.  All rights reserved.
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

#include "opal/util/output.h"
#include "opal/util/argv.h"
#include "opal/class/opal_object.h"

#include "orte/util/show_help.h"
#include "orte/util/proc_info.h"
#include "orte/runtime/orte_globals.h"

#include "ompi/constants.h"

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


/*
 * It seems you can't probe a device / port to see if it supports a
 * specific type of QP.  You just have to try to make it and see if it
 * works.  This is a short helper function to try to make a QP of a
 * specific type and return whether it worked.
 */
static bool make_qp(struct ibv_pd *pd, struct ibv_cq *cq, 
                    enum ibv_qp_type type)
{
    struct ibv_qp_init_attr qpia;
    struct ibv_qp *qp;

    qpia.qp_context = NULL;
    qpia.send_cq = cq;
    qpia.recv_cq = cq;
    qpia.srq = NULL;
    qpia.cap.max_send_wr = 1;
    qpia.cap.max_recv_wr = 1;
    qpia.cap.max_send_sge = 1;
    qpia.cap.max_recv_sge = 1;
    qpia.cap.max_inline_data = 0;
    qpia.qp_type = type;
    qpia.sq_sig_all = 0;
    
    qp = ibv_create_qp(pd, &qpia);
    if (NULL != qp) {
        ibv_destroy_qp(qp);
        return true;
    }

    return false;
}

/*
 * Find a list of ibv_ports matching a set of criteria.
 */
opal_list_t *ompi_common_verbs_find_ibv_ports(const char *if_include, 
                                              const char *if_exclude, 
                                              int flags)
{
    int32_t num_devs;
    struct ibv_device **devices;
    struct ibv_device *device;
    struct ibv_context *device_context;
    struct ibv_device_attr device_attr;
    struct ibv_port_attr port_attr;
    struct ibv_pd *pd = NULL;
    struct ibv_cq *cq = NULL;
    char **if_include_list = NULL, **if_exclude_list = NULL;
    ompi_common_verbs_device_item_t *di;
    ompi_common_verbs_port_item_t *pi;
    uint32_t i, j;
    opal_list_t *port_list = NULL;
    opal_list_item_t *item;
    bool want;

    /* Allocate a list to fill */
    port_list = OBJ_NEW(opal_list_t);
    if (NULL == port_list) {
        goto err_free_argv;
    }

    /* Sanity check the include/exclude params */
    if (NULL != if_include && NULL != if_exclude) {
        return port_list;
    } else if (NULL != if_include) {
        if_include_list = opal_argv_split(if_include, ',');
    } else if (NULL != if_exclude) {
        if_exclude_list = opal_argv_split(if_exclude, ',');
    }

    /* Query all the IBV devices on the machine.  Use an ompi
       compatibility function, because how to get this list changed
       over the history of the IBV API. */
    devices = ompi_ibv_get_device_list(&num_devs);
    if (0 == num_devs) {
        goto err_free_argv;
    }

    /* Now loop through all the devices.  Get the attributes for each
       port on each device to see if they match our selection
       criteria. */
    for (i = 0; (int32_t) i < num_devs; ++i) {
        device = devices[i];
        device_context = ibv_open_device(device);
        if (NULL == device_context) {
            orte_show_help("help-ompi-common-verbs.txt",
                           "ibv_open_device fail", true,
                           orte_process_info.nodename,
                           ibv_get_device_name(device),
                           errno, strerror(errno));
            goto err_free_port_list;
        }

        if (ibv_query_device(device_context, &device_attr)){
            orte_show_help("help-ompi-common-verbs.txt",
                           "ibv_query_device fail", true,
                           orte_process_info.nodename,
                           ibv_get_device_name(device),
                           errno, strerror(errno));
            goto err_free_port_list;
        }

        /* Check the the device-specific flags to see if we want this
           device */
        want = true;
        if (flags & OMPI_COMMON_VERBS_FLAGS_TRANSPORT_IB &&
            IBV_TRANSPORT_IB != device->transport_type) {
            want = false;
        }
        if (flags & OMPI_COMMON_VERBS_FLAGS_TRANSPORT_IWARP &&
            IBV_TRANSPORT_IWARP != device->transport_type) {
            want = false;
        }

        /* If we didn't want it, go to the next device */
        if (!want) {
            continue;
        }

        /* If we asked for check for RC or UD support, then we'll need
           a PD and CQ for checking, below.  So alloc one. */
        if (flags & OMPI_COMMON_VERBS_FLAGS_RC ||
            flags & OMPI_COMMON_VERBS_FLAGS_UD) {
            pd = ibv_alloc_pd(device_context);
            cq = ibv_create_cq(device_context, 1, NULL, NULL, 0);
            if (NULL == cq || NULL == pd) {
                goto err_destroy_cq_pd;
            }
        }

        /* Make a device_item_t to hold the device information */
        di = OBJ_NEW(ompi_common_verbs_device_item_t);
        if (NULL == di) {
            goto err_destroy_cq_pd;
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
                continue;
            }

            /* Query the port */
            if (ibv_query_port(device_context, (uint8_t) j, &port_attr)) {
                orte_show_help("help-ompi-common-verbs.txt",
                               "ibv_query_port fail", true,
                               orte_process_info.nodename,
                               ibv_get_device_name(device),
                               errno, strerror(errno));
                goto err_destroy_cq_pd;
            }

            /* We definitely only want ACTIVE ports */
            if (IBV_PORT_ACTIVE != port_attr.state) {
                continue;
            }

            /* Check the port-specific flags to see if we want this
               port */
            want = false;
            if (0 == flags) {
                want = true;
            }
            if (flags & OMPI_COMMON_VERBS_FLAGS_RC) {
                /* It doesn't look like you can query whether a
                   device/port supports RC QP's.  You just have to try
                   to make one.  :-( If it succeeds, the device/port
                   supports it. */
                if (make_qp(pd, cq, IBV_QPT_RC)) {
                    want = true;
                }
            }
            if (flags & OMPI_COMMON_VERBS_FLAGS_UD) {
                /* See above comment about RC QP's -- same rationale holds
                   true here. */
                if (make_qp(pd, cq, IBV_QPT_UD)) {
                    want = true;
                }
            }
#if defined(HAVE_IBV_LINK_LAYER_ETHERNET)
            if (flags & OMPI_COMMON_VERBS_FLAGS_LINK_LAYER_IB &&
                IBV_LINK_LAYER_INFINIBAND == port_attr.link_layer) {
                want = true;
            }
            if (flags & OMPI_COMMON_VERBS_FLAGS_LINK_LAYER_ETHERNET &&
                IBV_LINK_LAYER_ETHERNET == port_attr.link_layer) {
                want = true;
            }
#endif

            if (!want) {
                continue;
            }

            /* If we got this far, we want the port.  Make an item for
               it and add it to the list. */
            pi = OBJ_NEW(ompi_common_verbs_port_item_t);
            if (NULL == pi) {
                goto err_destroy_cq_pd;
            }
            pi->device = di;            
            pi->port_num = j;
            pi->port_attr = port_attr;
            OBJ_RETAIN(di);

            opal_list_append(port_list, &pi->super);
        }

        /* If we allocated a pd for testing RC/UD, free it here */
        if (NULL != pd) {
            ibv_dealloc_pd(pd);
            pd = NULL;
        }
        if (NULL != cq) {
            ibv_destroy_cq(cq);
            cq = NULL;
        }

        /* We're done with the device; if some ports are using it, its
           ref count will be > 0, and therefore the device won't be
           deleted here. */
        OBJ_RELEASE(di);
    }

    /* All done! */
    return port_list;

 err_destroy_cq_pd:
    if (NULL != pd) {
        ibv_dealloc_pd(pd);
        pd = NULL;
    }
    if (NULL != cq) {
        ibv_destroy_cq(cq);
        cq = NULL;
    }

 err_free_port_list:
    for (item = opal_list_remove_first(port_list);
         item != NULL; 
         item = opal_list_remove_first(port_list)) {
        OBJ_RELEASE(item);
    }

 err_free_argv:
    opal_argv_free(if_include_list);
    if_include_list = NULL;
    opal_argv_free(if_exclude_list);
    if_exclude_list = NULL;

    return port_list;
}
