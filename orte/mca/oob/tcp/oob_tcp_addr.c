/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_SOCKET_H
#include <sys/socket.h>
#endif
#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif
#ifdef HAVE_ARPA_INET_H
#include <arpa/inet.h>
#endif
#include <string.h>
#include "orte/orte_constants.h"
#include "opal/util/if.h"

#include "orte/mca/ns/ns_types.h"
#include "orte/util/proc_info.h"
#include "orte/dss/dss.h"

#include "oob_tcp.h"
#include "oob_tcp_addr.h"


static void mca_oob_tcp_addr_construct(mca_oob_tcp_addr_t* addr)
{
    memset(&addr->addr_name, 0, sizeof(addr->addr_name));
    addr->addr_count = 0;
    addr->addr_alloc = 0;
    addr->addr_next = 0;
    addr->addr_inet = NULL;
    addr->addr_matched = false;
}

static void mca_oob_tcp_addr_destruct(mca_oob_tcp_addr_t* addr)
{
    if(addr->addr_inet != NULL)
        free(addr->addr_inet);
}

OBJ_CLASS_INSTANCE(
    mca_oob_tcp_addr_t,
    opal_object_t,
    mca_oob_tcp_addr_construct,
    mca_oob_tcp_addr_destruct);


int mca_oob_tcp_addr_pack(orte_buffer_t* buffer)
{
    uint32_t count = 0;
    int rc;
    opal_list_item_t *item;
  
    rc = orte_dss.pack(buffer, ORTE_PROC_MY_NAME, 1, ORTE_NAME);
    if(rc != ORTE_SUCCESS)
        return rc;

    count = opal_list_get_size(&mca_oob_tcp_component.tcp_available_devices);
    rc = orte_dss.pack(buffer, &count, 1, ORTE_INT32);
    if(rc != ORTE_SUCCESS)
        return rc;

    for (item = opal_list_get_first(&mca_oob_tcp_component.tcp_available_devices) ;
         item != opal_list_get_end(&mca_oob_tcp_component.tcp_available_devices) ;
         item = opal_list_get_next(item)) {
        mca_oob_tcp_device_t *dev = (mca_oob_tcp_device_t*) item;
        uint8_t type;
        uint32_t ipaddr;
        uint16_t port;

        switch (dev->if_addr.sin_family) { 
        case AF_INET: 
            type = MCA_OOB_TCP_ADDR_TYPE_AFINET; 
            break; 
        default: 
            /* shouldn't get here, as opal_if shouldn't allow anything 
               but AFINET.  Will need another case once IPv6 code is 
               committed. */ 
            continue; 
        } 
        orte_dss.pack(buffer, &type, 1, ORTE_INT8); 
 	 
        port = mca_oob_tcp_component.tcp_listen_port; 
        orte_dss.pack(buffer, &port, sizeof(port), ORTE_BYTE); 
 	 
        /* This will need to be adjusted for IPv6 */ 
        ipaddr = (uint32_t) dev->if_addr.sin_addr.s_addr; 
        orte_dss.pack(buffer, &ipaddr, sizeof(ipaddr), ORTE_BYTE); 
    }
    return ORTE_SUCCESS;
}


mca_oob_tcp_addr_t* mca_oob_tcp_addr_unpack(orte_buffer_t* buffer)
{
    mca_oob_tcp_addr_t* addr = OBJ_NEW(mca_oob_tcp_addr_t);
    int rc;
    orte_std_cntr_t count;
    if(NULL == addr) 
         return NULL;

    count = 1;
    rc = orte_dss.unpack(buffer, &addr->addr_name, &count, ORTE_NAME);
    if(rc != ORTE_SUCCESS) {
        OBJ_RELEASE(addr);
        return NULL;
    }

    count = 1;
    rc = orte_dss.unpack(buffer, &addr->addr_count, &count, ORTE_INT32);
    if(rc != ORTE_SUCCESS) {
        OBJ_RELEASE(addr);
        return NULL;
    }

    if(addr->addr_count != 0) {
        orte_std_cntr_t i;
        addr->addr_inet = (struct sockaddr_in *)malloc(sizeof(struct sockaddr_in) * addr->addr_count);
        if(NULL == addr->addr_inet) {
             OBJ_RELEASE(addr);
             return NULL;
        }
        addr->addr_alloc = addr->addr_count;
        for(i=0; i<addr->addr_count; i++) {
            uint8_t type;
            uint32_t ipaddr;
            uint16_t port;
 	    /* unpack and expand family */ 
 	    count = 1; 
 	    rc = orte_dss.unpack(buffer, &type, &count, ORTE_INT8);             
            if(rc != ORTE_SUCCESS) {
                OBJ_RELEASE(addr);
                return NULL;
            }
            switch (type) { 
            case MCA_OOB_TCP_ADDR_TYPE_AFINET: 
                addr->addr_inet[i].sin_family = AF_INET; 
                break; 
            default: 
                OBJ_RELEASE(addr); 
                return NULL; 
            } 
 	 
            /* and the listen port */ 
            count = sizeof(port);
            rc = orte_dss.unpack(buffer, &port, &count, ORTE_BYTE); 
            if(rc != ORTE_SUCCESS) { 
                OBJ_RELEASE(addr); 
                return NULL; 
            } 
            addr->addr_inet[i].sin_port = port; 
 
            /* and the address.  need to fix for IPv6 */ 
            count = sizeof(ipaddr);
            rc = orte_dss.unpack(buffer, &ipaddr, &count, ORTE_BYTE); 
            if(rc != ORTE_SUCCESS) { 
                OBJ_RELEASE(addr); 
                return NULL; 
            } 
            addr->addr_inet[i].sin_addr.s_addr = ipaddr; 
        }
    }
    return addr;
}


int mca_oob_tcp_addr_get_next(mca_oob_tcp_addr_t* addr, struct sockaddr_in* retval)
{
    if(addr == NULL || addr->addr_count == 0)
        return ORTE_ERROR;
    if(addr->addr_matched == false) {
        orte_std_cntr_t i=0;
        for(i=0; i<addr->addr_count; i++) {
            opal_list_item_t *item;

            for (item = opal_list_get_first(&mca_oob_tcp_component.tcp_available_devices) ;
                 item != opal_list_get_end(&mca_oob_tcp_component.tcp_available_devices) ;
                 item = opal_list_get_next(item)) {
                mca_oob_tcp_device_t *dev = (mca_oob_tcp_device_t*) item;
                struct sockaddr_in inmask;

                opal_ifindextomask(dev->if_index, (struct sockaddr*)&inmask, sizeof(inmask));

                /* if match on network prefix - start here */
                if((dev->if_addr.sin_addr.s_addr & inmask.sin_addr.s_addr) ==
                   (addr->addr_inet[i].sin_addr.s_addr & inmask.sin_addr.s_addr)) {
                   addr->addr_next = i;
                   goto done;
                }
            }
        }
done:
        addr->addr_matched = true;
    }
    *retval = addr->addr_inet[addr->addr_next];
    if(++addr->addr_next >= addr->addr_count)
        addr->addr_next = 0;
    return ORTE_SUCCESS;
}


int mca_oob_tcp_addr_insert(mca_oob_tcp_addr_t* addr, const struct sockaddr_in* inaddr)
{
    if(addr->addr_alloc == 0) {
        addr->addr_alloc = 2;
        addr->addr_inet = (struct sockaddr_in *)malloc(addr->addr_alloc * sizeof(struct sockaddr_in));
    } else if(addr->addr_count == addr->addr_alloc) {
        addr->addr_alloc <<= 1;
        addr->addr_inet = (struct sockaddr_in *)realloc(addr->addr_inet, addr->addr_alloc * sizeof(struct sockaddr_in));
    }
    if(NULL == addr->addr_inet)
        return ORTE_ERR_OUT_OF_RESOURCE;
    memcpy(addr->addr_inet+addr->addr_count, inaddr, sizeof(struct sockaddr_in));
    addr->addr_count++;
    return ORTE_SUCCESS;
}

