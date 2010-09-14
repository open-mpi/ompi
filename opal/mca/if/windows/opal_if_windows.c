/*
 * Copyright (c) 2004-2007 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2009      High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "opal_config.h"

#include <stdlib.h>
#include <string.h>

#include "opal/mca/if/if.h"
#include "opal/constants.h"

static int if_windows_open(void);

opal_if_base_component_t mca_if_windows_component = {
    /* First, the mca_component_t struct containing meta information
       about the component itself */
    {
        OPAL_IF_BASE_VERSION_2_0_0,

        /* Component name and version */
        "windows",
        OPAL_MAJOR_VERSION,
        OPAL_MINOR_VERSION,
        OPAL_RELEASE_VERSION,

        /* Component open and close functions */
        if_windows_open,
        NULL
    },
    {
        /* This component is checkpointable */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    },
};

    /* 
       1. check if the interface info list is already populated. If so, return
       2. get the interface information which is required using WSAIoctl
       3. construct opal_if_list and populate it with the list of interfaces we have
       CAVEAT: Does not support the following options which are supported in SIOCGIFCONF
       - kernel table index
       - interface name
    */

#define MAX_INTERFACES 10 /* Anju: for now assume there are no more than this */

static int if_windows_open(void)
{
    SOCKET sd; 
    INTERFACE_INFO if_list[MAX_INTERFACES];
    int num_interfaces;
    unsigned long num_bytes_returned;
    int i;
    unsigned int interface_counter = 0;
    opal_if_t *intf;

    /* create a socket */
    sd = WSASocket (AF_INET, SOCK_DGRAM, IPPROTO_UDP, NULL, 0, 0);
    if (sd == SOCKET_ERROR) {
        opal_output(0, "opal_ifinit: WSASocket failed with errno=%d\n",WSAGetLastError());
        return OPAL_ERROR;
    }

    /* get the information about the interfaces */
    if (SOCKET_ERROR == WSAIoctl (sd, 
                                  SIO_GET_INTERFACE_LIST, 
                                  NULL, 
                                  0, 
                                  &if_list,
                                  sizeof (if_list), 
                                  &num_bytes_returned, 
                                  0,
                                  0)) {
        opal_output(0, "opal_ifinit: WSAIoctl failed with errno=%d\n",WSAGetLastError());
        return OPAL_ERROR;
    }

    /* loop through all the interfaces and create the list */
    num_interfaces = num_bytes_returned / sizeof (INTERFACE_INFO);
    for (i = 0; i < num_interfaces; ++i) {
        /* do all this only if the interface is up, and skip loopback interface */
        if (0 != (if_list[i].iiFlags & IFF_UP)
            && (!opal_if_retain_loopback && 0 == (if_list[i].iiFlags & IFF_LOOPBACK))) {

            intf = OBJ_NEW(opal_if_t);
            if (NULL == intf) {
                opal_output (0,"opal_ifinit: Unable to malloc %d bytes",sizeof(opal_list_t));
                return OPAL_ERR_OUT_OF_RESOURCE;
            }
        
            /* fill in the interface address */ 
            memcpy(&intf->if_addr, &(if_list[i].iiAddress),
                   sizeof(intf->if_addr));

            /* fill in the netmask information */
            memcpy(&intf->if_mask, &(if_list[i].iiNetmask),
                   sizeof(intf->if_mask));

            /* fill in the bcast address */
            memcpy(&intf->if_bcast, &(if_list[i].iiBroadcastAddress),
                   sizeof(intf->if_bcast));

            /* fill in the flags */
            intf->if_flags = if_list[i].iiFlags;

            /* fill in the index in the table */
            intf->if_index = opal_list_get_size(&opal_if_list)+1;

            /* fill in the kernel index */
            intf->if_kernel_index = intf->if_index;

            /* generate the interface name, e.g. eth0, eth1, ..... */
            sprintf(intf->if_name, "eth%u", interface_counter++);

            /* copy all this into a persistent form and store it in the list */
            opal_list_append(&opal_if_list, &(intf->super));
        }
    }

    return OPAL_SUCCESS;
}
