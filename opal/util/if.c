/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
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

#include "opal_config.h"
#include <string.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <errno.h>
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_SOCKET_H
#include <sys/socket.h>
#endif
#ifdef HAVE_SYS_SOCKIO_H
#include <sys/sockio.h>
#endif
#ifdef HAVE_SYS_IOCTL_H
#include <sys/ioctl.h>
#endif
#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif
#ifdef HAVE_ARPA_INET_H
#include <arpa/inet.h>
#endif
#ifdef HAVE_NET_IF_H
#if defined(__APPLE__) && defined(_LP64)
/* Apple engineering suggested using options align=power as a
   workaround for a bug in OS X 10.4 (Tiger) that prevented ioctl(...,
   SIOCGIFCONF, ...) from working properly in 64 bit mode on Power PC.
   It turns out that the underlying issue is the size of struct
   ifconf, which the kernel expects to be 12 and natural 64 bit
   alignment would make 16.  The same bug appears in 64 bit mode on
   Intel macs, but align=power is a no-op there, so instead, use the
   pack pragma to instruct the compiler to pack on 4 byte words, which
   has the same effect as align=power for our needs and works on both
   Intel and Power PC Macs. */
#pragma pack(push,4)
#endif
#include <net/if.h>
#if defined(__APPLE__) && defined(_LP64)
#pragma pack(pop)
#endif
#endif
#ifdef HAVE_NETDB_H
#include <netdb.h>
#endif

#include "opal/class/opal_list.h"
#include "opal/util/if.h"
#include "opal/util/output.h"
#include "opal/util/strncpy.h"
#include "opal/constants.h"

#ifdef HAVE_STRUCT_SOCKADDR_IN

#ifndef IF_NAMESIZE
#define IF_NAMESIZE 32
#endif

/*
 * Define INADDR_NONE if we don't have it.  Solaris is the only system
 * where I have found that it does not exist, and the man page for
 * inet_addr() says that it returns -1 upon failure.  On Linux and
 * other systems with INADDR_NONE, it's just a #define to -1 anyway.
 * So just #define it to -1 here if it doesn't already exist.
 */

#if !defined(INADDR_NONE)
#define INADDR_NONE -1
#endif

struct opal_if_t {
    opal_list_item_t     super;
    char                if_name[IF_NAMESIZE];
    int                 if_index;
#ifndef __WINDOWS__
    int                 if_flags;
#else
    u_long              if_flags;
#endif
    int                 if_speed;
    struct sockaddr_in  if_addr;
    struct sockaddr_in  if_mask;
#ifdef __WINDOWS__
    struct sockaddr_in  if_bcast;
#endif
    uint32_t            if_bandwidth;
};
typedef struct opal_if_t opal_if_t;

static opal_list_t opal_if_list;
static bool already_done = false;

#define DEFAULT_NUMBER_INTERFACES 10
#define MAX_IFCONF_SIZE 10 * 1024 * 1024

/*
 *  Discover the list of configured interfaces. Don't care about any
 *  interfaces that are not up or are local loopbacks.
 */

static int opal_ifinit(void) 
{
#ifndef __WINDOWS__
    int sd;
    int lastlen, num, rem;
    char *ptr;
    struct ifconf ifconf;
    int ifc_len;
    bool successful_locate = false;

    if (already_done) {
        return OPAL_SUCCESS;
    }
    already_done = true;

    /* create the internet socket to test off */
    if((sd = socket(AF_INET, SOCK_DGRAM, 0)) < 0) {
        opal_output(0, "opal_ifinit: socket() failed with errno=%d\n", errno);
        return OPAL_ERROR;
    }

    /*
     * Get Network Interface configuration 
     *
     * Some notes on the behavior of ioctl(..., SIOCGIFCONF,...)
     * when not enough space is allocated for all the entries.
     *
     * - Solaris returns -1, errno EINVAL if there is not enough
     *   space 
     * - OS X returns 0, sets .ifc_len to the space used by the
     *   by the entries that did fit.
     * - Linux returns 0, sets .ifc_len to the space required to
     *   hold all the entries (although it only writes what will
     *   fit in the buffer of .ifc_len passed to the function).
     * - FreeBSD returns 0, sets .ifc_len to 0.
     *
     * Everyone else seems to do one of the four.
     */
    lastlen = 0;
    ifc_len = sizeof(struct ifreq) * DEFAULT_NUMBER_INTERFACES;
    do {
        ifconf.ifc_len = ifc_len;
        ifconf.ifc_req = malloc(ifc_len);
        if (NULL == ifconf.ifc_req) {
            close(sd);
            return OPAL_ERROR;
        }

        /* initialize the memory so valgrind and purify won't complain
         * can't use OMPI_DEBUG_ZERO because sizeof(ifconf.ifc_req)
         * isn't userful.  Since this isn't performance critical, just
         * always memset.
         */
        memset(ifconf.ifc_req, 0, ifconf.ifc_len);
        
        if(ioctl(sd, SIOCGIFCONF, &ifconf) < 0) {
            /* if we got an einval, we probably don't have enough
               space.  so we'll fall down and try to expand our
               space */
            if (errno != EINVAL && lastlen != 0) {
                opal_output(0, "opal_ifinit: ioctl(SIOCGIFCONF) failed with errno=%d", 
                            errno);
                close(sd);
                return OPAL_ERROR;
            }
        } else {
            /* if ifc_len is 0 or different than what we set it to at
               call to ioctl, try again with a bigger buffer.  else stop */
            if (ifconf.ifc_len == lastlen && ifconf.ifc_len > 0) {
                /* we didn't expand.  we're done */
                successful_locate = true;
                break;
            }
            lastlen = ifconf.ifc_len;
        }

        /* Yes, we overflowed (or had an EINVAL on the ioctl).  Loop
           back around and try again with a bigger buffer */
        free(ifconf.ifc_req);
        ifc_len = (ifc_len == 0) ? 1 : ifc_len * 2;
    } while (ifc_len < MAX_IFCONF_SIZE);
    if (!successful_locate) {
        opal_output(0, "opal_ifinit: unable to find network interfaces.");
        return OPAL_ERR_FATAL;
    }


    /* 
     * Setup indexes 
     */
    OBJ_CONSTRUCT(&opal_if_list, opal_list_t);
    ptr = (char*) ifconf.ifc_req;
    rem = ifconf.ifc_len;
    num = 0;

    /* loop through all interfaces */
    while (rem > 0) {
        struct ifreq* ifr = (struct ifreq*) ptr;
        opal_if_t intf;
        opal_if_t *intf_ptr;
        int length;

        OMPI_DEBUG_ZERO(intf);
        OBJ_CONSTRUCT(&intf, opal_list_item_t);

        /* compute offset for entries */
#if OMPI_HAVE_SA_LEN
        length = sizeof(struct sockaddr);

        if (ifr->ifr_addr.sa_len > length) {
            length = ifr->ifr_addr.sa_len;
        }

        length += sizeof(ifr->ifr_name);
#else
        length = sizeof(struct ifreq);
#endif

        rem -= length;
        ptr += length;

        /* see if we like this entry */
        if(ifr->ifr_addr.sa_family != AF_INET)
            continue;

        if(ioctl(sd, SIOCGIFFLAGS, ifr) < 0) {
            opal_output(0, "opal_ifinit: ioctl(SIOCGIFFLAGS) failed with errno=%d", errno);
            continue;
        }
        if ((ifr->ifr_flags & IFF_UP) == 0) 
            continue;
#ifdef IFF_SLAVE
        /* Is this a slave to a load balancer or bonded channel?
           If so, don't use it -- pick up the master instead */
        if ((ifr->ifr_flags & IFF_SLAVE) != 0)
           continue;
#endif
#if 0
        if ((ifr->ifr_flags & IFF_LOOPBACK) != 0)
            continue;
#endif
                                                                                
        /* copy entry over into our data structure */
        strcpy(intf.if_name, ifr->ifr_name);
        intf.if_flags = ifr->ifr_flags;

#ifndef SIOCGIFINDEX
        intf.if_index = opal_list_get_size(&opal_if_list)+1;
#else
        if(ioctl(sd, SIOCGIFINDEX, ifr) < 0) {
            opal_output(0,"opal_ifinit: ioctl(SIOCGIFINDEX) failed with errno=%d", errno);
            continue;
        }
#if defined(ifr_ifindex)
        intf.if_index = ifr->ifr_ifindex;
#elif defined(ifr_index)
        intf.if_index = ifr->ifr_index;
#else
        intf.if_index = -1;
#endif
#endif /* SIOCGIFINDEX */

        if(ioctl(sd, SIOCGIFADDR, ifr) < 0) {
            opal_output(0, "opal_ifinit: ioctl(SIOCGIFADDR) failed with errno=%d", errno);
            break;
        }
        if(ifr->ifr_addr.sa_family != AF_INET) 
            continue;

        memcpy(&intf.if_addr, &ifr->ifr_addr, sizeof(intf.if_addr));
        if(ioctl(sd, SIOCGIFNETMASK, ifr) < 0) {
            opal_output(0, "opal_ifinit: ioctl(SIOCGIFNETMASK) failed with errno=%d", errno);
            continue;
        }
        memcpy(&intf.if_mask, &ifr->ifr_addr, sizeof(intf.if_mask));

        intf_ptr = (opal_if_t*) malloc(sizeof(opal_if_t));
        OMPI_DEBUG_ZERO(*intf_ptr);
        if(intf_ptr == 0) {
            opal_output(0, "opal_ifinit: unable to allocated %d bytes\n", sizeof(opal_if_t));
            return OPAL_ERR_OUT_OF_RESOURCE;
        }
        memcpy(intf_ptr, &intf, sizeof(intf));
        opal_list_append(&opal_if_list, (opal_list_item_t*)intf_ptr);
    }
    free(ifconf.ifc_req);
    close(sd);
    
#else /* __WINDOWS__ implementation begins */

    /* 
       1. check if the interface info list is already populated. If so, return
       2. get the interface information which is required using WSAIoctl
       3. construct opal_if_list and populate it with the list of interfaces we have
       CAVEAT: Does not support the following options which are supported in SIOCGIFCONF
            - kernel table index
            - interface name
     */

    #define MAX_INTERFACES 10 /* Anju: for now assume there are no more than this */
    SOCKET sd; 
    INTERFACE_INFO if_list[MAX_INTERFACES];
    int num_interfaces;
    unsigned long num_bytes_returned;
    int i;
    unsigned int interface_counter = 0;
    opal_if_t intf;
    opal_if_t *intf_ptr;

    /* return if this has been done before */
    if (already_done) {
        return OPAL_SUCCESS;
    }
    already_done = true;
  
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

    /* create and populate opal_if_list */
    OBJ_CONSTRUCT (&opal_if_list, opal_list_t);


    /* loop through all the interfaces and create the list */
    num_interfaces = num_bytes_returned / sizeof (INTERFACE_INFO);
    for (i = 0; i < num_interfaces; ++i) {
        /* do all this only if the interface is up */
        if (if_list[i].iiFlags & IFF_UP) {

            OBJ_CONSTRUCT (&intf, opal_list_item_t);
        
            /* fill in the interface address */ 
            memcpy (&intf.if_addr, &(if_list[i].iiAddress), sizeof(intf.if_addr));

            /* fill in the netmask information */
            memcpy (&intf.if_mask, &(if_list[i].iiNetmask), sizeof(intf.if_mask));

            /* fill in the bcast address */
            memcpy (&intf.if_bcast, &(if_list[i].iiBroadcastAddress), sizeof(intf.if_bcast));

            /* fill in the flags */
            intf.if_flags = if_list[i].iiFlags;

            /* fill in the index in the table */
            intf.if_index = (int)opal_list_get_size(&opal_if_list)+1;

            /* generate the interface name on your own ....
               loopback: lo
               Rest:    eth0, eth1, ..... */

            if (if_list[i].iiFlags & IFF_LOOPBACK) {
                sprintf (intf.if_name, "lo");
            } else {
                sprintf (intf.if_name, "eth%u", interface_counter++);
            }

            /* copy all this into a persistent form and store it in the list */
            intf_ptr = (opal_if_t*)malloc(sizeof(opal_if_t));
            if (NULL == intf_ptr) {
                opal_output (0,"opal_ifinit: Unable to malloc %d bytes",sizeof(opal_list_t));
                return OPAL_ERR_OUT_OF_RESOURCE;
            }

            memcpy (intf_ptr, &intf, sizeof(intf));
            opal_list_append(&opal_if_list, (opal_list_item_t *)intf_ptr);
        }
    }
    
#endif
    return OPAL_SUCCESS;
}


/*
 *  Finalize the list of configured interfaces to free malloc'd memory
 */

int opal_iffinalize(void) 
{
    if (already_done) {
#ifndef __WINDOWS__
        opal_if_t *intf_ptr;
    
        while (NULL != 
               (intf_ptr = (opal_if_t*)opal_list_remove_first(&opal_if_list))) {
            OBJ_RELEASE(intf_ptr);
        }
        OBJ_DESTRUCT(&opal_if_list);
#endif
        already_done = false;
    }
    return OPAL_SUCCESS;
}


/*
 *  Look for interface by name and returns its address 
 *  as a dotted decimal formatted string.
 */

int opal_ifnametoaddr(const char* if_name, struct sockaddr* addr, int length)
{
    opal_if_t* intf;
    int rc = opal_ifinit();
    if(rc != OPAL_SUCCESS)
        return rc;

    for(intf =  (opal_if_t*)opal_list_get_first(&opal_if_list);
        intf != (opal_if_t*)opal_list_get_end(&opal_if_list);
        intf =  (opal_if_t*)opal_list_get_next(intf)) {
        if(strcmp(intf->if_name, if_name) == 0) {
            memcpy(addr, &intf->if_addr, length);
            return OPAL_SUCCESS;
        }
    }
    return OPAL_ERROR;
}


/*
 *  Look for interface by name and returns its 
 *  corresponding kernel index.
 */

int opal_ifnametoindex(const char* if_name)
{
    opal_if_t* intf;
    int rc = opal_ifinit();
    if(rc != OPAL_SUCCESS)
        return rc;

    for(intf =  (opal_if_t*)opal_list_get_first(&opal_if_list);
        intf != (opal_if_t*)opal_list_get_end(&opal_if_list);
        intf =  (opal_if_t*)opal_list_get_next(intf)) {
        if(strcmp(intf->if_name, if_name) == 0) {
            return intf->if_index;
        }
    }
    return -1;
}


/*
 *  Attempt to resolve the adddress as either a dotted decimal formated
 *  string or a hostname and lookup corresponding interface.
 */

int opal_ifaddrtoname(const char* if_addr, char* if_name, int length)
{
    opal_if_t* intf;
#ifndef __WINDOWS__
    in_addr_t inaddr;
#else 
    unsigned long inaddr;
#endif
    int rc;
    struct hostent *h;
    
    inaddr = inet_addr(if_addr);

    rc = opal_ifinit();
    if (OPAL_SUCCESS != rc) {
        return rc;
    }

    if(INADDR_NONE == inaddr) {
        h = gethostbyname(if_addr);
        if(0 == h) {
            opal_output(0,"opal_ifaddrtoname: unable to resolve %s\n", if_addr);
            return OPAL_ERR_NOT_FOUND;
        }
        memcpy(&inaddr, h->h_addr, sizeof(inaddr));
    }

    for(intf =  (opal_if_t*)opal_list_get_first(&opal_if_list);
        intf != (opal_if_t*)opal_list_get_end(&opal_if_list);
        intf =  (opal_if_t*)opal_list_get_next(intf)) {
        if(intf->if_addr.sin_addr.s_addr == inaddr) {
            strncpy(if_name, intf->if_name, length);
            return OPAL_SUCCESS;
        }
    }
    return OPAL_ERR_NOT_FOUND;
}

/*
 *  Return the number of discovered interface.
 */

int opal_ifcount(void)
{
    if(opal_ifinit() != OPAL_SUCCESS)
        return (-1);
    return (int)opal_list_get_size(&opal_if_list);
}


/*
 *  Return the kernels interface index for the first
 *  interface in our list.
 */

int opal_ifbegin(void)
{
    opal_if_t *intf;
    if(opal_ifinit() != OPAL_SUCCESS)
        return (-1);
    intf = (opal_if_t*)opal_list_get_first(&opal_if_list);
    if(NULL != intf)
        return intf->if_index;
    return (-1);
}


/*
 *  Located the current position in the list by if_index and
 *  return the interface index of the next element in our list
 *  (if it exists).
 */

int opal_ifnext(int if_index)
{
    opal_if_t *intf;
    if(opal_ifinit() != OPAL_SUCCESS)
        return (-1);

    for(intf =  (opal_if_t*)opal_list_get_first(&opal_if_list);
        intf != (opal_if_t*)opal_list_get_end(&opal_if_list);
        intf =  (opal_if_t*)opal_list_get_next(intf)) {
        if(intf->if_index == if_index) {
            do {
                opal_if_t* if_next = (opal_if_t*)opal_list_get_next(intf);
                opal_if_t* if_end =  (opal_if_t*)opal_list_get_end(&opal_if_list);
                if (if_next == if_end) {
                    return -1;
                }
                intf = if_next;
            } while(intf->if_index == if_index);
            return intf->if_index;
        }
    }
    return (-1);
}


/* 
 *  Lookup the interface by kernel index and return the 
 *  primary address assigned to the interface.
 */

int opal_ifindextoaddr(int if_index, struct sockaddr* if_addr, int length)
{
    opal_if_t* intf;
    int rc = opal_ifinit();
    if(rc != OPAL_SUCCESS)
        return rc;

    for(intf =  (opal_if_t*)opal_list_get_first(&opal_if_list);
        intf != (opal_if_t*)opal_list_get_end(&opal_if_list);
        intf =  (opal_if_t*)opal_list_get_next(intf)) {
        if(intf->if_index == if_index) {
            memcpy(if_addr, &intf->if_addr, length);
            return OPAL_SUCCESS;
        }
    }
    return OPAL_ERROR;
}


/* 
 *  Lookup the interface by kernel index and return the 
 *  network mask assigned to the interface.
 */

int opal_ifindextomask(int if_index, struct sockaddr* if_mask, int length)
{
    opal_if_t* intf;
    int rc = opal_ifinit();
    if(rc != OPAL_SUCCESS)
        return rc;

    for(intf =  (opal_if_t*)opal_list_get_first(&opal_if_list);
        intf != (opal_if_t*)opal_list_get_end(&opal_if_list);
        intf =  (opal_if_t*)opal_list_get_next(intf)) {
        if(intf->if_index == if_index) {
            memcpy(if_mask, &intf->if_mask, length);
            return OPAL_SUCCESS;
        }
    }
    return OPAL_ERROR;
}



/* 
 *  Lookup the interface by kernel index and return
 *  the associated name.
 */

int opal_ifindextoname(int if_index, char* if_name, int length)
{
    opal_if_t *intf;
    int rc = opal_ifinit();
    if(rc != OPAL_SUCCESS)
        return rc;

    for(intf =  (opal_if_t*)opal_list_get_first(&opal_if_list);
        intf != (opal_if_t*)opal_list_get_end(&opal_if_list);
        intf =  (opal_if_t*)opal_list_get_next(intf)) {
        if(intf->if_index == if_index) {
            strncpy(if_name, intf->if_name, length);
            return OPAL_SUCCESS;
        }
    }
    return OPAL_ERROR;
}


bool
opal_ifislocalhost(struct sockaddr *addr)
{
    switch (addr->sa_family) {
    case AF_INET:
        {
            struct sockaddr_in *inaddr = (struct sockaddr_in*) addr;
            /* if it's in the 127. domain, it shouldn't be routed
               (0x7f == 127) */
            if (0x7F000000 == (0x7F000000 & ntohl(inaddr->sin_addr.s_addr))) {
                return true;
            }
            return false;
        }
        break;

    default:
        opal_output(0, "unhandled sa_family %d passed to opal_ifislocalhost",
                    addr->sa_family);
        return false;
        break;
    }
}


#define ADDRLEN 100
bool
opal_ifislocal(char *hostname)
{
    char addrname[ADDRLEN + 1];
    int ret;
    struct hostent *h;

    /* opal_ifaddrtoname will complain (rightly) if hostname is not
       resolveable.  check to make sure it's resolveable.  If not,
       definitely not local... */
    h = gethostbyname(hostname);
    if (NULL == h) return false;

    ret = opal_ifaddrtoname(hostname, addrname, ADDRLEN);
    if (OPAL_SUCCESS == ret) return true;

    return false;
}

#else /* HAVE_STRUCT_SOCKADDR_IN */

/* if we don't have struct sockaddr_in, we don't have traditional
   ethernet devices.  Just make everything a no-op error call (except
   for finailize, which will return successfully) */

int
opal_ifnametoaddr(const char* if_name, 
                  struct sockaddr* if_addr, int size)
{
    return OPAL_ERR_NOT_SUPPORTED;
}

int
opal_ifaddrtoname(const char* if_addr, 
                  char* if_name, int size)
{
    return OPAL_ERR_NOT_SUPPORTED;
}

int
opal_ifnametoindex(const char* if_name)
{
    return OPAL_ERR_NOT_SUPPORTED;
}

int
opal_ifcount(void)
{
    return OPAL_ERR_NOT_SUPPORTED;
}

int
opal_ifbegin(void)
{
    return OPAL_ERR_NOT_SUPPORTED;
}

int
opal_ifnext(int if_index)
{
    return OPAL_ERR_NOT_SUPPORTED;
}

int
opal_ifindextoname(int if_index, char* if_name, int length)
{
    return OPAL_ERR_NOT_SUPPORTED;
}

int
opal_ifindextoaddr(int if_index, struct sockaddr* if_addr, int length)
{
    return OPAL_ERR_NOT_SUPPORTED;
}

int
opal_ifindextomask(int if_index, struct sockaddr* if_addr, int length)
{
    return OPAL_ERR_NOT_SUPPORTED;
}

bool
opal_ifislocal(char *hostname)
{
    return false;
}

bool
opal_ifislocalhost(struct sockaddr *addr)
{
    return false;
}

int
opal_iffinalize(void)
{
    return OPAL_SUCCESS;
}

#endif /* HAVE_STRUCT_SOCKADDR_IN */
