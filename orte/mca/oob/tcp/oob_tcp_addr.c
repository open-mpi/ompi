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
    addr->addr_matched = MCA_OOB_TCP_ADDR_UNCLASSIFIED;
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
    orte_std_cntr_t count = 0;
    int i;
    int rc;
  
    rc = orte_dss.pack(buffer, ORTE_PROC_MY_NAME, 1, ORTE_NAME);
    if(rc != ORTE_SUCCESS)
        return rc;

    for(i=opal_ifbegin(); i>0; i=opal_ifnext(i)) {
        struct sockaddr_storage inaddr;
        opal_ifindextoaddr(i, &inaddr, sizeof(inaddr));
        if(opal_ifcount() > 1 && 
           opal_ifislocalhost(&inaddr)) {
            continue;
        }
        count++;
    } 
    rc = orte_dss.pack(buffer, &count, 1, ORTE_STD_CNTR);
    if(rc != ORTE_SUCCESS)
        return rc;

    for(i=opal_ifbegin(); i>0; i=opal_ifnext(i)) {
        struct sockaddr_storage inaddr;
        uint8_t type;
        uint32_t ipaddr;
        uint16_t port;

        opal_ifindextoaddr(i, &inaddr, sizeof(inaddr));
        if(opal_ifcount() > 1 && 
           opal_ifislocalhost(&inaddr))
            continue;

        switch (inaddr.ss_family) {
            case AF_INET:
                type = MCA_OOB_TCP_ADDR_TYPE_AFINET; 
                port = mca_oob_tcp_component.tcp_listen_port;
                orte_dss.pack(buffer, &type, 1, ORTE_INT8); 
                orte_dss.pack(buffer, &port, sizeof (port), ORTE_BYTE);
                ipaddr = (uint32_t) ((struct sockaddr_in*)&inaddr)->sin_addr.s_addr;
                orte_dss.pack(buffer, &ipaddr, sizeof (ipaddr), ORTE_BYTE);
                break; 
#if OPAL_WANT_IPV6
            case AF_INET6:
                type = MCA_OOB_TCP_ADDR_TYPE_AFINET6;
                port =mca_oob_tcp_component.tcp6_listen_port;
                orte_dss.pack(buffer, &type, 1, ORTE_INT8); 
                orte_dss.pack(buffer, &port, sizeof (port), ORTE_BYTE);
                orte_dss.pack(buffer, &((struct sockaddr_in6*)&inaddr)->sin6_addr,
                              4, ORTE_BYTE);
                break;
#endif
            default: 
                /* shouldn't get here, as opal_if shouldn't allow anything 
                but AFINET and AF_INET6. */
                continue; 
        } 
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
    rc = orte_dss.unpack(buffer, &addr->addr_count, &count, ORTE_STD_CNTR);
    if(rc != ORTE_SUCCESS) {
        OBJ_RELEASE(addr);
        return NULL;
    }

    if(addr->addr_count != 0) {
        orte_std_cntr_t i;
#if OPAL_WANT_IPV6
        addr->addr_inet = (struct sockaddr_in6 *)malloc(sizeof(struct sockaddr_in6) * addr->addr_count);
#else
        addr->addr_inet = (struct sockaddr_in *)malloc(sizeof(struct sockaddr_in) * addr->addr_count);
#endif
        if(NULL == addr->addr_inet) {
             OBJ_RELEASE(addr);
             return NULL;
        }
        addr->addr_alloc = addr->addr_count;
        for(i=0; i<addr->addr_count; i++) {
            uint8_t type;
            uint16_t port;
            /* unpack and expand family */ 
            count = 1; 
            rc = orte_dss.unpack(buffer, &type, &count, ORTE_INT8);             
            if(rc != ORTE_SUCCESS) {
                OBJ_RELEASE(addr);
                return NULL;
            }
            /* and the listen port */
            count = sizeof (port);
            rc = orte_dss.unpack(buffer, &port, &count, ORTE_BYTE);
            if(rc != ORTE_SUCCESS) {
                OBJ_RELEASE(addr);
                return NULL;
            }
            
            switch (type) {
                case MCA_OOB_TCP_ADDR_TYPE_AFINET:
                {
#if OPAL_WANT_IPV6
                    struct sockaddr_in6* target;
#else
                    struct sockaddr_in* target;
#endif
                    uint32_t ipaddr;
                    count = sizeof (ipaddr);
                    rc = orte_dss.unpack(buffer, &ipaddr, &count, ORTE_BYTE);
                    if(rc != ORTE_SUCCESS) {
                        OBJ_RELEASE(addr);
                        return NULL;
                    }
#if OPAL_WANT_IPV6
                    target = (struct sockaddr_in6*)&(addr->addr_inet[i]);
                    target->sin6_family = AF_INET;
                    target->sin6_port = port;
                    memcpy(&target->sin6_addr, &ipaddr, sizeof (ipaddr));
#else
                    target = (struct sockaddr_in*)&(addr->addr_inet[i]);
                    target->sin_family = AF_INET;
                    target->sin_port = port;
                    target->sin_addr.s_addr = ipaddr;
#endif
                }
                    break;
#if OPAL_WANT_IPV6
                case MCA_OOB_TCP_ADDR_TYPE_AFINET6:
                {
                    uint32_t address[4];
                    struct sockaddr_in6* target;
                    count = 4;
                    rc = orte_dss.unpack(buffer, &address, &count, ORTE_BYTE);
                    if(rc != ORTE_SUCCESS) {
                        OBJ_RELEASE(addr);
                        return NULL;
                    }
                    target = (struct sockaddr_in6*)&(addr->addr_inet[i]);
                    target->sin6_family = AF_INET6;
                    target->sin6_port = port;
                    memcpy(&target->sin6_addr, &address, sizeof (address));
                }
                    break;
#endif
                default:
                    OBJ_RELEASE(addr);
                    return NULL;
            }            
        }
    }
    return addr;
}


int mca_oob_tcp_addr_get_next(mca_oob_tcp_addr_t* addr, struct sockaddr_storage* retval)
{
    static uint32_t i_have = MCA_OOB_TCP_ADDR_UNCLASSIFIED; /* my own capabilities */
    
    if((NULL == addr) || (0 == addr->addr_count)) {
        return ORTE_ERROR;
    }
    
    if(MCA_OOB_TCP_ADDR_UNCLASSIFIED == addr->addr_matched) {
        orte_std_cntr_t i=0;
        for(i=0; i<addr->addr_count; i++) {
            int ifindex;
            for(ifindex=opal_ifbegin(); ifindex>0; ifindex=opal_ifnext(ifindex)) {
                struct sockaddr_storage inaddr;
                uint32_t inmask;
                char name[32];
                opal_ifindextoname(i, name, sizeof(name));
                if (mca_oob_tcp_component.tcp_include != NULL &&
                    strstr(mca_oob_tcp_component.tcp_include,name) == NULL) {
                    continue;
                }
                if (mca_oob_tcp_component.tcp_exclude != NULL &&
                    strstr(mca_oob_tcp_component.tcp_exclude,name) != NULL) {
                    continue;
                }
                opal_ifindextoaddr(ifindex, &inaddr, sizeof(inaddr));
                if(opal_ifcount() > 1 && opal_ifislocalhost(&inaddr)) {
                    continue;
                }
                opal_ifindextomask(ifindex, &inmask, sizeof(inmask));

                /* Decide which address to try first; note that we're
                    called multiple times and each time we need to
                    present a different address
                    
                    Precedence rules:
                    
                    - IPv4public has the highest priority
                    - when IPv4private + IPv6, use IPv6 (this should
                      be changed when there is something like a CellID)
                    */
                if (true == opal_addr_isipv4public (&inaddr)) {
                    i_have |= MCA_OOB_TCP_ADDR_IPV4public;
                }
                
                if (true == opal_addr_isipv4public ((struct sockaddr_storage*)&addr->addr_inet[i])) {
                    addr->addr_matched |= MCA_OOB_TCP_ADDR_IPV4public;
                }
                
                if ((MCA_OOB_TCP_ADDR_IPV4public ==
                     (i_have & MCA_OOB_TCP_ADDR_IPV4public)) &&
                    (MCA_OOB_TCP_ADDR_IPV4public ==
                     (addr->addr_matched & MCA_OOB_TCP_ADDR_IPV4public))) {
                    addr->addr_next = i;
                    goto done;
                }
                
#if OPAL_WANT_IPV6
                if (AF_INET6 == inaddr.ss_family) {
                    i_have |= MCA_OOB_TCP_ADDR_IPV6;
                }
                
                if (AF_INET6 ==
                    ((struct sockaddr_in6*)&addr->addr_inet[i])->sin6_family) {
                    addr->addr_matched |= MCA_OOB_TCP_ADDR_IPV6;
                    addr->addr_next = i;
                    goto done;
                }
                
#endif
                /* if match on network prefix - start here */
                /* Bug, FIXME: This code is dangerous, it will prefer
                    local addresses even if they point to wrong hosts
                    (the multicluster problem).
                    
                    We need more magic to select the best address
                    
                    adi@2006-09-30
                */
                if(opal_samenetwork(&inaddr,
                                    (struct sockaddr_storage*)&addr->addr_inet[i],
                                    inmask)) {
                    addr->addr_matched |= MCA_OOB_TCP_ADDR_MATCHED;
                    addr->addr_next = i;
                   goto done;
                }
            }
        }
done:
        ; /* NOP */
    }
    memcpy (retval, &addr->addr_inet[addr->addr_next],
            sizeof (addr->addr_inet[addr->addr_next]));
    if(++addr->addr_next >= addr->addr_count)
        addr->addr_next = 0;
    return ORTE_SUCCESS;
}


#if OPAL_WANT_IPV6
int mca_oob_tcp_addr_insert(mca_oob_tcp_addr_t* addr, const struct sockaddr_in6* inaddr)
#else
int mca_oob_tcp_addr_insert(mca_oob_tcp_addr_t* addr, const struct sockaddr_in* inaddr)
#endif
{
    if(addr->addr_alloc == 0) {
        addr->addr_alloc = 2;
#if OPAL_WANT_IPV6
        addr->addr_inet = (struct sockaddr_in6 *)malloc(addr->addr_alloc * sizeof(struct sockaddr_in6));
#else
        addr->addr_inet = (struct sockaddr_in *)malloc(addr->addr_alloc * sizeof(struct sockaddr_in));
#endif
    } else if(addr->addr_count == addr->addr_alloc) {
        addr->addr_alloc <<= 1;
#if OPAL_WANT_IPV6
        addr->addr_inet = (struct sockaddr_in6 *)realloc(addr->addr_inet, addr->addr_alloc * sizeof(struct sockaddr_in6));
#else
        addr->addr_inet = (struct sockaddr_in *)realloc(addr->addr_inet, addr->addr_alloc * sizeof(struct sockaddr_in));
#endif
    }
    if(NULL == addr->addr_inet)
        return ORTE_ERR_OUT_OF_RESOURCE;
#if OPAL_WANT_IPV6
    memcpy(addr->addr_inet+addr->addr_count, inaddr, sizeof(struct sockaddr_in6));
#else
    memcpy(addr->addr_inet+addr->addr_count, inaddr, sizeof(struct sockaddr_in));
#endif
    addr->addr_count++;
    return ORTE_SUCCESS;
}
