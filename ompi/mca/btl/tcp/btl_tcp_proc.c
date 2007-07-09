/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2007 The University of Tennessee and The University
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

#include "ompi_config.h"

#ifdef HAVE_ARPA_INET_H
#include <arpa/inet.h>
#endif
#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif

#include "orte/class/orte_proc_table.h"
#include "ompi/mca/btl/base/btl_base_error.h"
#include "ompi/runtime/ompi_module_exchange.h"
#include "ompi/datatype/dt_arch.h"
#include "opal/util/if.h"
#include "opal/util/net.h"
#include "orte/mca/oob/tcp/oob_tcp_addr.h"

#include "btl_tcp.h"
#include "btl_tcp_proc.h"

static void mca_btl_tcp_proc_construct(mca_btl_tcp_proc_t* proc);
static void mca_btl_tcp_proc_destruct(mca_btl_tcp_proc_t* proc);

OBJ_CLASS_INSTANCE( mca_btl_tcp_proc_t, 
                    opal_list_item_t, 
                    mca_btl_tcp_proc_construct, 
                    mca_btl_tcp_proc_destruct );

void mca_btl_tcp_proc_construct(mca_btl_tcp_proc_t* proc)
{
    proc->proc_ompi = 0;
    proc->proc_addrs = NULL;
    proc->proc_addr_count = 0;
    proc->proc_endpoints = NULL;
    proc->proc_endpoint_count = 0;
    OBJ_CONSTRUCT(&proc->proc_lock, opal_mutex_t);
}

/*
 * Cleanup ib proc instance
 */

void mca_btl_tcp_proc_destruct(mca_btl_tcp_proc_t* proc)
{
    /* remove from list of all proc instances */
    OPAL_THREAD_LOCK(&mca_btl_tcp_component.tcp_lock);
    orte_hash_table_remove_proc(&mca_btl_tcp_component.tcp_procs, &proc->proc_name);
    OPAL_THREAD_UNLOCK(&mca_btl_tcp_component.tcp_lock);

    /* release resources */
    if(NULL != proc->proc_endpoints) {
        free(proc->proc_endpoints);
        OBJ_DESTRUCT(&proc->proc_lock);
    }
}


/*
 * Create a TCP process structure. There is a one-to-one correspondence
 * between a ompi_proc_t and a mca_btl_tcp_proc_t instance. We cache
 * additional data (specifically the list of mca_btl_tcp_endpoint_t instances, 
 * and published addresses) associated w/ a given destination on this
 * datastructure.
 */

mca_btl_tcp_proc_t* mca_btl_tcp_proc_create(ompi_proc_t* ompi_proc)
{
    int rc;
    size_t size;
    mca_btl_tcp_proc_t* btl_proc;

    OPAL_THREAD_LOCK(&mca_btl_tcp_component.tcp_lock);
    btl_proc = (mca_btl_tcp_proc_t*)orte_hash_table_get_proc(
         &mca_btl_tcp_component.tcp_procs, &ompi_proc->proc_name);
    if(NULL != btl_proc) {
        OPAL_THREAD_UNLOCK(&mca_btl_tcp_component.tcp_lock);
        return btl_proc;
    }

    btl_proc = OBJ_NEW(mca_btl_tcp_proc_t);
    if(NULL == btl_proc)
        return NULL;
    btl_proc->proc_ompi = ompi_proc;
    btl_proc->proc_name = ompi_proc->proc_name;

    /* add to hash table of all proc instance */
    orte_hash_table_set_proc( &mca_btl_tcp_component.tcp_procs,
                              &btl_proc->proc_name,
                              btl_proc );
    OPAL_THREAD_UNLOCK(&mca_btl_tcp_component.tcp_lock);

    /* lookup tcp parameters exported by this proc */
    rc = ompi_modex_recv( &mca_btl_tcp_component.super.btl_version,
                                  ompi_proc,
                                  (void**)&btl_proc->proc_addrs,
                                  &size );
    if(rc != OMPI_SUCCESS) {
        BTL_ERROR(("mca_base_modex_recv: failed with return value=%d", rc));
        OBJ_RELEASE(btl_proc);
        return NULL;
    }
    if(0 != (size % sizeof(mca_btl_tcp_addr_t))) {
        BTL_ERROR(("mca_base_modex_recv: invalid size %d: btl-size: %d\n",
          size, sizeof(mca_btl_tcp_addr_t)));
        return NULL;
    }
    btl_proc->proc_addr_count = size / sizeof(mca_btl_tcp_addr_t);

    /* allocate space for endpoint array - one for each exported address */
    btl_proc->proc_endpoints = (mca_btl_base_endpoint_t**)
        malloc((1 + btl_proc->proc_addr_count) *
                sizeof(mca_btl_base_endpoint_t*));
    if(NULL == btl_proc->proc_endpoints) {
        OBJ_RELEASE(btl_proc);
        return NULL;
    }
    if(NULL == mca_btl_tcp_component.tcp_local && ompi_proc == ompi_proc_local()) {
        mca_btl_tcp_component.tcp_local = btl_proc;
    }
    {
        /* convert the OMPI addr_family field to OS constants,
         * so we can check for AF_INET (or AF_INET6) and don't have
         * to deal with byte ordering anymore.
         */
        unsigned int i;
        for (i = 0; i < btl_proc->proc_addr_count; i++) {
            if (MCA_BTL_TCP_AF_INET == btl_proc->proc_addrs[i].addr_family) {
                btl_proc->proc_addrs[i].addr_family = AF_INET;
            }
#if OPAL_WANT_IPV6
            if (MCA_BTL_TCP_AF_INET6 == btl_proc->proc_addrs[i].addr_family) {
                btl_proc->proc_addrs[i].addr_family = AF_INET6;
            }
#endif
        }
    }
    return btl_proc;
}


/*
 * Note that this routine must be called with the lock on the process
 * already held.  Insert a btl instance into the proc array and assign 
 * it an address.
 */
int mca_btl_tcp_proc_insert( mca_btl_tcp_proc_t* btl_proc, 
                             mca_btl_base_endpoint_t* btl_endpoint )
{
    size_t i;
    struct sockaddr_storage endpoint_addr_ss;

#ifndef WORDS_BIGENDIAN
    /* if we are little endian and our peer is not so lucky, then we
       need to put all information sent to him in big endian (aka
       Network Byte Order) and expect all information received to
       be in NBO.  Since big endian machines always send and receive
       in NBO, we don't care so much about that case. */
    if (btl_proc->proc_ompi->proc_arch & OMPI_ARCH_ISBIGENDIAN) {
        btl_endpoint->endpoint_nbo = true;
    }
#endif

    /* insert into endpoint array */
    btl_endpoint->endpoint_proc = btl_proc;
    btl_proc->proc_endpoints[btl_proc->proc_endpoint_count++] = btl_endpoint;

    /*
     * Look through the proc instance for an address that is on the
     * directly attached network. If we don't find one, pick the first
     * unused address.
    */
    for( i = 0; i < btl_proc->proc_addr_count; i++ ) {
        mca_btl_tcp_addr_t* endpoint_addr = btl_proc->proc_addrs + i;
        if(endpoint_addr->addr_inuse != 0) {
            continue;
        }
        mca_btl_tcp_proc_tosocks (endpoint_addr, &endpoint_addr_ss);

        /* The best we could get is IPv4 public. So let's check */
        if (true == opal_net_addr_isipv4public((struct sockaddr*) &endpoint_addr_ss)) {
            btl_endpoint->endpoint_addr = endpoint_addr;
            btl_endpoint->endpoint_addr->addr_inuse++;
            return OMPI_SUCCESS;
        }

#if OPAL_WANT_IPV6
        /* Bug, FIXME: this is Thomas' job: if we have IPv6 AND RFC1918,
         * use IPv6, else use IPv4 private */
        /* 
         * adi@2006-11-22: new bug. It's not sufficient to look for
         * remote's IPv6 capabilities, we even need to check if we're
         * able to communicate via IPv6. We might also want to look
         * at mca_btl_tcp_component.tcp_disable_family
         */
        if((AF_INET6 == endpoint_addr->addr_family) &&
           (6 != mca_btl_tcp_component.tcp_disable_family)) {
            btl_endpoint->endpoint_addr = endpoint_addr;
            btl_endpoint->endpoint_addr->addr_inuse++;
            return OMPI_SUCCESS;
        }
#endif
        /* Read:
         * if we are on the same network, accept.
         * Bug, FIXME. May be wrong. That's only a
         * last resort
         */
        
        /* loop over our local addresses and see if we could accept it */
        {
            int index;
            struct sockaddr_storage local_ss;
            uint32_t netmask;
            for (index = opal_ifbegin(); index >= 0; index=opal_ifnext (index)) {
                /* we're only looking for IPv4 (private) */
                if (AF_INET != endpoint_addr->addr_family) {
                    continue;
                }
                if (OPAL_SUCCESS != 
                    opal_ifindextoaddr (index, (struct sockaddr*) &local_ss, sizeof (local_ss))) {
                    opal_output (0,
                            "btl_tcp_proc: problems getting address for index %i (kernel index %i)\n", index, opal_ifindextokindex (index));
                    continue;
                }
                if (OPAL_SUCCESS != 
                        opal_ifindextomask (index, &netmask, sizeof (netmask))) {
                    opal_output (0,
                            "btl_tcp_proc: problems getting netmask for index %i (kernel index %i)\n", index, opal_ifindextokindex (index));
                    continue;
                }

                /* we know that we're only talking about IPv4 now.
                 * Let's talk about IPv4 _private_, so isipv4public must
                 * return false
                 */
                if (false == opal_net_addr_isipv4public((struct sockaddr*) &local_ss)) {
                    if (opal_net_samenetwork((struct sockaddr*) &local_ss,
                                             (struct sockaddr*) &endpoint_addr_ss, 
                                             netmask)) {
                        btl_endpoint->endpoint_addr = endpoint_addr;
                        btl_endpoint->endpoint_addr->addr_inuse++;
                        return OMPI_SUCCESS;
                    }
                }
            }
        }
    } /* end of for btl_proc_proc_addr_count */

    /* Make sure there is a common interface */
    if( NULL != btl_endpoint->endpoint_addr ) {
        btl_endpoint->endpoint_addr->addr_inuse++;
        return OMPI_SUCCESS;
    }
    /* Bug, FIXME: Once upon a time, there was a lot of
     * code in here. I've removed it. There might be better
     * approaches. Thomas will show...
     */
    return OMPI_ERR_UNREACH;
}

/*
 * Remove an endpoint from the proc array and indicate the address is
 * no longer in use.
 */
                                                                                                                 
int mca_btl_tcp_proc_remove(mca_btl_tcp_proc_t* btl_proc, mca_btl_base_endpoint_t* btl_endpoint)
{
    size_t i;
    OPAL_THREAD_LOCK(&btl_proc->proc_lock);
    for(i=0; i<btl_proc->proc_endpoint_count; i++) {
        if(btl_proc->proc_endpoints[i] == btl_endpoint) {
            memmove(btl_proc->proc_endpoints+i, btl_proc->proc_endpoints+i+1,
                (btl_proc->proc_endpoint_count-i-1)*sizeof(mca_btl_base_endpoint_t*));
            if(--btl_proc->proc_endpoint_count == 0) {
                OPAL_THREAD_UNLOCK(&btl_proc->proc_lock);
                OBJ_RELEASE(btl_proc);
                return OMPI_SUCCESS;
            }
            /* The endpoint_addr may still be NULL if this enpoint is
               being removed early in the wireup sequence (e.g., if it
               is unreachable by all other procs) */
            if (NULL != btl_endpoint->endpoint_addr) {
                btl_endpoint->endpoint_addr->addr_inuse--;
            }
            break;
        }
    }
    OPAL_THREAD_UNLOCK(&btl_proc->proc_lock);
    return OMPI_SUCCESS;
}

/*
 * Look for an existing TCP process instance based on the globally unique
 * process identifier.
 */
mca_btl_tcp_proc_t* mca_btl_tcp_proc_lookup(const orte_process_name_t *name)
{
    mca_btl_tcp_proc_t* proc;
    OPAL_THREAD_LOCK(&mca_btl_tcp_component.tcp_lock);
    proc = (mca_btl_tcp_proc_t*)orte_hash_table_get_proc(
         &mca_btl_tcp_component.tcp_procs, name);
    OPAL_THREAD_UNLOCK(&mca_btl_tcp_component.tcp_lock);
    return proc;
}

/*
 * loop through all available BTLs for one matching the source address
 * of the request.
 */
bool mca_btl_tcp_proc_accept(mca_btl_tcp_proc_t* btl_proc, struct sockaddr* addr, int sd)
{
    size_t i;
    OPAL_THREAD_LOCK(&btl_proc->proc_lock);
    for( i = 0; i < btl_proc->proc_endpoint_count; i++ ) {
        mca_btl_base_endpoint_t* btl_endpoint = btl_proc->proc_endpoints[i];
        /* Check all conditions before going to try to accept the connection. */
        if( btl_endpoint->endpoint_addr->addr_family != addr->sa_family ) {
            continue;
        }

        switch (addr->sa_family) {
        case AF_INET:
            if( memcmp( &btl_endpoint->endpoint_addr->addr_inet,
                        &(((struct sockaddr_in*)addr)->sin_addr),
                        sizeof(struct in_addr) ) ) {
                continue;
            }
            break;
#if OPAL_WANT_IPV6
        case AF_INET6:
            if( memcmp( &btl_endpoint->endpoint_addr->addr_inet,
                        &(((struct sockaddr_in6*)addr)->sin6_addr),
                        sizeof(struct in6_addr) ) ) {
                continue;
            }
            break;
#endif
        default:
            ;
        }

        if(mca_btl_tcp_endpoint_accept(btl_endpoint, addr, sd)) {
            OPAL_THREAD_UNLOCK(&btl_proc->proc_lock);
            return true;
        }
    }
    OPAL_THREAD_UNLOCK(&btl_proc->proc_lock);
    return false;
}

/*
 * convert internal data structure (mca_btl_tcp_addr_t) to sockaddr_storage
 *
 */
bool mca_btl_tcp_proc_tosocks(mca_btl_tcp_addr_t* proc_addr,
                              struct sockaddr_storage* output)
{
    memset(output, 0, sizeof (*output));
    switch (proc_addr->addr_family) {
    case AF_INET:
        output->ss_family = AF_INET;
        memcpy(&((struct sockaddr_in*)output)->sin_addr,
               &proc_addr->addr_inet, sizeof(struct in_addr));
        ((struct sockaddr_in*)output)->sin_port = proc_addr->addr_port;
        break;
#if OPAL_WANT_IPV6
    case AF_INET6:
        {
            struct sockaddr_in6* inaddr = (struct sockaddr_in6*)output;
            output->ss_family = AF_INET6;
            memcpy(&inaddr->sin6_addr, &proc_addr->addr_inet,
                   sizeof (proc_addr->addr_inet));
            inaddr->sin6_port = proc_addr->addr_port;
            inaddr->sin6_scope_id = 0;
            inaddr->sin6_flowinfo = 0;
        }
        break;
#endif
    default:
        opal_output( 0, "mca_btl_tcp_proc: unknown af_family received: %d\n",
                     proc_addr->addr_family );
        return false;
    } 
    return true;
}

