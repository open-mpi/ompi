/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"
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
#include "include/constants.h"
#include "util/if.h"
#include "oob_tcp.h"
#include "oob_tcp_addr.h"


static void mca_oob_tcp_addr_construct(mca_oob_tcp_addr_t* addr)
{
    memset(&addr->addr_name, 0, sizeof(addr->addr_name));
    addr->addr_count = 0;
    addr->addr_alloc = 0;
    addr->addr_next = 0;
    addr->addr_inet = NULL;
}

static void mca_oob_tcp_addr_destruct(mca_oob_tcp_addr_t* addr)
{
    if(addr->addr_inet != NULL)
        free(addr->addr_inet);
}

OBJ_CLASS_INSTANCE(
    mca_oob_tcp_addr_t,
    ompi_object_t,
    mca_oob_tcp_addr_construct,
    mca_oob_tcp_addr_destruct);


int mca_oob_tcp_addr_pack(orte_buffer_t* buffer)
{
    uint32_t count = 0;
    int i;
    int rc;
  
    rc = orte_dps.pack(buffer, orte_process_info.my_name, 1, ORTE_NAME);
    if(rc != ORTE_SUCCESS)
        return rc;

    for(i=ompi_ifbegin(); i>0; i=ompi_ifnext(i)) {
        struct sockaddr_in inaddr;
        ompi_ifindextoaddr(i, (struct sockaddr*)&inaddr, sizeof(inaddr));
        if(ompi_ifcount() > 1 && inaddr.sin_addr.s_addr == inet_addr("127.0.0.1"))
            continue;
        count++;
    } 
    rc = orte_dps.pack(buffer, &count, 1, ORTE_INT32);
    if(rc != ORTE_SUCCESS)
        return rc;

    for(i=ompi_ifbegin(); i>0; i=ompi_ifnext(i)) {
        struct sockaddr_in inaddr;
        ompi_ifindextoaddr(i, (struct sockaddr*)&inaddr, sizeof(inaddr));
        if(ompi_ifcount() > 1 && inaddr.sin_addr.s_addr == inet_addr("127.0.0.1"))
            continue;
        inaddr.sin_port = mca_oob_tcp_component.tcp_listen_port;
        orte_dps.pack(buffer,&inaddr,sizeof(inaddr),ORTE_BYTE);
    }
    return ORTE_SUCCESS;
}


mca_oob_tcp_addr_t* mca_oob_tcp_addr_unpack(orte_buffer_t* buffer)
{
    mca_oob_tcp_addr_t* addr = OBJ_NEW(mca_oob_tcp_addr_t);
    int rc;
    size_t count;
    if(NULL == addr) 
         return NULL;

    count = 1;
    rc = orte_dps.unpack(buffer, &addr->addr_name, &count, ORTE_NAME);
    if(rc != OMPI_SUCCESS) {
        OBJ_RELEASE(addr);
        return NULL;
    }

    count = 1;
    rc = orte_dps.unpack(buffer, &addr->addr_count, &count, ORTE_INT32);
    if(rc != OMPI_SUCCESS) {
        OBJ_RELEASE(addr);
        return NULL;
    }

    if(addr->addr_count != 0) {
        addr->addr_inet = (struct sockaddr_in *)malloc(sizeof(struct sockaddr_in) * addr->addr_count);
        if(NULL == addr->addr_inet) {
             OBJ_RELEASE(addr);
             return NULL;
        }
        addr->addr_alloc = addr->addr_count;
        count = addr->addr_count * sizeof(struct sockaddr_in);
        rc = orte_dps.unpack(buffer, addr->addr_inet, &count, ORTE_BYTE);
        if(rc != OMPI_SUCCESS) {
            OBJ_RELEASE(addr);
            return NULL;
        }
    }
    return addr;
}


int mca_oob_tcp_addr_get_next(mca_oob_tcp_addr_t* addr, struct sockaddr_in* inaddr)
{
    if(addr == NULL || addr->addr_count == 0)
        return ORTE_ERROR;
    *inaddr = addr->addr_inet[addr->addr_next];
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

