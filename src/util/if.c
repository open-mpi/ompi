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
#include <net/if.h>
#endif
#ifdef HAVE_NETDB_H
#include <netdb.h>
#endif

#include "include/constants.h"
#include "class/ompi_list.h"
#include "util/if.h"
#include "util/output.h"
#include "util/strncpy.h"

#ifndef IF_NAMESIZE
#define IF_NAMESIZE 32
#endif

struct ompi_if_t {
    ompi_list_item_t     super;
    char                if_name[IF_NAMESIZE];
    int                 if_index;
#ifndef WIN32
    int                 if_flags;
#else
    u_long              if_flags;
#endif
    int                 if_speed;
    struct sockaddr_in  if_addr;
    struct sockaddr_in  if_mask;
#ifdef WIN32
    struct sockaddr_in  if_bcast;
#endif
    uint32_t            if_bandwidth;
};
typedef struct ompi_if_t ompi_if_t;

static ompi_list_t ompi_if_list;
static bool already_done = false;

#define DEFAULT_NUMBER_INTERFACES 10

#ifndef WIN32
static int old_ompi_ifinit(void)
{
    char buff[1024];
    char *ptr;
    struct ifconf ifconf;
    int sd;
    ifconf.ifc_len = sizeof(buff);
    ifconf.ifc_buf = buff;

    if (already_done) {
        return OMPI_SUCCESS;
    }
    already_done = true;

    if((sd = socket(AF_INET, SOCK_DGRAM, 0)) < 0) {
        ompi_output(0, "ompi_ifinit: socket() failed with errno=%d\n", errno);
        return OMPI_ERROR;
    }

    if(ioctl(sd, SIOCGIFCONF, &ifconf) < 0) {
        ompi_output(0, "ompi_ifinit: ioctl(SIOCGIFCONF) failed with errno=%d", errno);
        close(sd);
        return OMPI_ERROR;
    }
    OBJ_CONSTRUCT(&ompi_if_list, ompi_list_t);

    for(ptr = buff; ptr < buff + ifconf.ifc_len; ) {
        struct ifreq* ifr = (struct ifreq*)ptr;
        ompi_if_t intf;
        ompi_if_t *intf_ptr;

        OBJ_CONSTRUCT(&intf, ompi_list_item_t);

#if defined(__APPLE__)
        ptr += (sizeof(ifr->ifr_name) + 
               MAX(sizeof(struct sockaddr),ifr->ifr_addr.sa_len));
#else
#if 0
        switch(ifr->ifr_addr.sa_family) {
        case AF_INET6:
            ptr += sizeof(ifr->ifr_name) + sizeof(struct sockaddr_in6);
            break;
        case AF_INET:
        default:
            ptr += sizeof(ifr->ifr_name) + sizeof(struct sockaddr);
            break;
        }
#else

	/* Jeff/George/Graham fix.  According to LAM/MPI and FT-MPI
	   and /usr/include/linux/if.h, you should really be advancing
	   by sizeof(struct ifreq) -- sizeof(ifr->ifr_name) +
	   sizeof(struct sockaddr) may be smaller than sizeof(struct
	   ifreq).  More specifically, the second element of struct
	   ifreq may be a union, of which struct sockaddr is only one
	   element (and may not be the smallest). */

	ptr += sizeof(struct ifreq);
#endif
#endif
        if(ifr->ifr_addr.sa_family != AF_INET)
            continue;

        if(ioctl(sd, SIOCGIFFLAGS, ifr) < 0) {
            ompi_output(0, "ompi_ifinit: ioctl(SIOCGIFFLAGS) failed with errno=%d", errno);
            continue;
        }
        if ((ifr->ifr_flags & IFF_UP) == 0) 
            continue;
#if 0
        if ((ifr->ifr_flags & IFF_LOOPBACK) != 0)
            continue;
#endif
                                                                                
        strcpy(intf.if_name, ifr->ifr_name);
        intf.if_flags = ifr->ifr_flags;

#if defined(__APPLE__)
        intf.if_index = ompi_list_get_size(&ompi_if_list)+1;
#else
        if(ioctl(sd, SIOCGIFINDEX, ifr) < 0) {
            ompi_output(0,"ompi_ifinit: ioctl(SIOCGIFINDEX) failed with errno=%d", errno);
            continue;
        }
#if defined(ifr_ifindex)
        intf.if_index = ifr->ifr_ifindex;
#elif defined(ifr_index)
        intf.if_index = ifr->ifr_index;
#else
        intf.if_index = -1;
#endif
#endif /* __APPLE__ */

        if(ioctl(sd, SIOCGIFADDR, ifr) < 0) {
            ompi_output(0, "ompi_ifinit: ioctl(SIOCGIFADDR) failed with errno=%d", errno);
            break;
        }
        if(ifr->ifr_addr.sa_family != AF_INET) 
            continue;

        memcpy(&intf.if_addr, &ifr->ifr_addr, sizeof(intf.if_addr));
        if(ioctl(sd, SIOCGIFNETMASK, ifr) < 0) {
            ompi_output(0, "ompi_ifinit: ioctl(SIOCGIFNETMASK) failed with errno=%d", errno);
            continue;
        }
        memcpy(&intf.if_mask, &ifr->ifr_addr, sizeof(intf.if_mask));

        intf_ptr = (ompi_if_t*) malloc(sizeof(ompi_if_t));
        if(intf_ptr == 0) {
            ompi_output(0, "ompi_ifinit: unable to allocated %d bytes\n", sizeof(ompi_if_t));
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
        memcpy(intf_ptr, &intf, sizeof(intf));
        ompi_list_append(&ompi_if_list, (ompi_list_item_t*)intf_ptr);
    }
    close(sd);
    return OMPI_SUCCESS;
}
#endif

/*
 *  Discover the list of configured interfaces. Don't care about any
 *  interfaces that are not up or are local loopbacks.
 */

static int ompi_ifinit(void) 
{
#ifndef WIN32
    int sd;
    int lastlen, num, rem;
    char *ptr;
    struct ifconf ifconf;
    int ifc_len;

    /* BWB - remove once this code is better tested */
    if (NULL != getenv("OMPI_orig_if")) {
        return old_ompi_ifinit();
    }

    if (already_done) {
        return OMPI_SUCCESS;
    }
    already_done = true;

    /* create the internet socket to test off */
    if((sd = socket(AF_INET, SOCK_DGRAM, 0)) < 0) {
        ompi_output(0, "ompi_ifinit: socket() failed with errno=%d\n", errno);
        return OMPI_ERROR;
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
            return OMPI_ERROR;
        }

        if(ioctl(sd, SIOCGIFCONF, &ifconf) < 0) {
            /* if we got an einval, we probably don't have enough
               space.  so we'll fall down and try to expand our
               space */
            if (errno != EINVAL && lastlen != 0) {
                ompi_output(0, "ompi_ifinit: ioctl(SIOCGIFCONF) failed with errno=%d", 
                            errno);
                close(sd);
                return OMPI_ERROR;
            }
        } else {
            /* if ifc_len is 0 or different than what we set it to at
               call to ioctl, try again with a bigger buffer.  else stop */
            if (ifconf.ifc_len == lastlen && ifconf.ifc_len > 0) {
                /* we didn't expand.  we're done */
                break;
            }
            lastlen = ifconf.ifc_len;
        }

        /* Yes, we overflowed (or had an EINVAL on the ioctl).  Loop
           back around and try again with a bigger buffer */
        free(ifconf.ifc_req);
        ifc_len = (ifc_len == 0) ? 1 : ifc_len * 2;
    } while (1);


    /* 
     * Setup indexes 
     */
    OBJ_CONSTRUCT(&ompi_if_list, ompi_list_t);
    ptr = (char*) ifconf.ifc_req;
    rem = ifconf.ifc_len;
    num = 0;

    /* loop through all interfaces */
    while (rem > 0) {
        struct ifreq* ifr = (struct ifreq*) ptr;
        ompi_if_t intf;
        ompi_if_t *intf_ptr;
        int length;

        OBJ_CONSTRUCT(&intf, ompi_list_item_t);

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
            ompi_output(0, "ompi_ifinit: ioctl(SIOCGIFFLAGS) failed with errno=%d", errno);
            continue;
        }
        if ((ifr->ifr_flags & IFF_UP) == 0) 
            continue;
#if 0
        if ((ifr->ifr_flags & IFF_LOOPBACK) != 0)
            continue;
#endif
                                                                                
        /* copy entry over into our data structure */
        strcpy(intf.if_name, ifr->ifr_name);
        intf.if_flags = ifr->ifr_flags;

#if defined(__APPLE__)
        intf.if_index = ompi_list_get_size(&ompi_if_list)+1;
#else
        if(ioctl(sd, SIOCGIFINDEX, ifr) < 0) {
            ompi_output(0,"ompi_ifinit: ioctl(SIOCGIFINDEX) failed with errno=%d", errno);
            continue;
        }
#if defined(ifr_ifindex)
        intf.if_index = ifr->ifr_ifindex;
#elif defined(ifr_index)
        intf.if_index = ifr->ifr_index;
#else
        intf.if_index = -1;
#endif
#endif /* __APPLE__ */

        if(ioctl(sd, SIOCGIFADDR, ifr) < 0) {
            ompi_output(0, "ompi_ifinit: ioctl(SIOCGIFADDR) failed with errno=%d", errno);
            break;
        }
        if(ifr->ifr_addr.sa_family != AF_INET) 
            continue;

        memcpy(&intf.if_addr, &ifr->ifr_addr, sizeof(intf.if_addr));
        if(ioctl(sd, SIOCGIFNETMASK, ifr) < 0) {
            ompi_output(0, "ompi_ifinit: ioctl(SIOCGIFNETMASK) failed with errno=%d", errno);
            continue;
        }
        memcpy(&intf.if_mask, &ifr->ifr_addr, sizeof(intf.if_mask));

        intf_ptr = (ompi_if_t*) malloc(sizeof(ompi_if_t));
        if(intf_ptr == 0) {
            ompi_output(0, "ompi_ifinit: unable to allocated %d bytes\n", sizeof(ompi_if_t));
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
        memcpy(intf_ptr, &intf, sizeof(intf));
        ompi_list_append(&ompi_if_list, (ompi_list_item_t*)intf_ptr);
    }
    close(sd);
    
#else /* WIN32 implementation begins */

    /* 
       1. check if the interface info list is already populated. If so, return
       2. get the interface information which is required using WSAIoctl
       3. construct ompi_if_list and populate it with the list of interfaces we have
       CAVEAT: Does not support the following options which are supported in SIOCGIFCONF
            - kernel table index
            - interface name
     */

    #define MAX_INTERFACES 10 /* Anju: for now assume there are no more than this */
    int ret;
    SOCKET sd; 
    INTERFACE_INFO if_list[MAX_INTERFACES];
    int num_interfaces;
    unsigned long num_bytes_returned;
    int i;
    SOCKADDR_IN *sock_address;
    unsigned int interface_counter = 0;
    ompi_if_t intf;
    ompi_if_t *intf_ptr;

    /* return if this has been done before */
    if (already_done) {
        return OMPI_SUCCESS;
    }
    already_done = true;
  
    /* create a socket */
    sd = WSASocket (AF_INET, SOCK_DGRAM, IPPROTO_UDP, NULL, 0, 0);
    if (sd == SOCKET_ERROR) {
        ompi_output(0, "ompi_ifinit: WSASocket failed with errno=%d\n",WSAGetLastError());
        return OMPI_ERROR;
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
        ompi_output(0, "ompi_ifinit: WSAIoctl failed with errno=%d\n",WSAGetLastError());
        return OMPI_ERROR;
    }

    /* create and populate ompi_if_list */
    OBJ_CONSTRUCT (&ompi_if_list, ompi_list_t);


    /* loop through all the interfaces and create the list */
    num_interfaces = num_bytes_returned / sizeof (INTERFACE_INFO);
    for (i = 0; i < num_interfaces; ++i) {
        /* do all this only if the interface is up */
        if (if_list[i].iiFlags & IFF_UP) {

            OBJ_CONSTRUCT (&intf, ompi_list_item_t);
        
            /* fill in the interface address */ 
            memcpy (&intf.if_addr, &(if_list[i].iiAddress), sizeof(intf.if_addr));

            /* fill in the netmask information */
            memcpy (&intf.if_mask, &(if_list[i].iiNetmask), sizeof(intf.if_mask));

            /* fill in the bcast address */
            memcpy (&intf.if_bcast, &(if_list[i].iiBroadcastAddress), sizeof(intf.if_bcast));

            /* fill in the flags */
            intf.if_flags = if_list[i].iiFlags;

            /* fill in the index in the table */
            intf.if_index = ompi_list_get_size(&ompi_if_list)+1;

            /* generate the interface name on your own ....
               loopback: lo
               Rest:    eth0, eth1, ..... */

            if (if_list[i].iiFlags & IFF_LOOPBACK) {
                sprintf (intf.if_name, "lo");
            } else {
                sprintf (intf.if_name, "eth%u", interface_counter++);
            }

            /* copy all this into a persistent form and store it in the list */
            intf_ptr = malloc(sizeof(ompi_if_t));
            if (NULL == intf_ptr) {
                ompi_output (0,"ompi_ifinit: Unable to malloc %d bytes",sizeof(ompi_list_t));
                return OMPI_ERR_OUT_OF_RESOURCE;
            }

            memcpy (intf_ptr, &intf, sizeof(intf));
            ompi_list_append(&ompi_if_list, (ompi_list_item_t *)intf_ptr);
        }
    }
    
#endif
    return OMPI_SUCCESS;
}


/*
 *  Look for interface by name and returns its address 
 *  as a dotted decimal formatted string.
 */

int ompi_ifnametoaddr(const char* if_name, struct sockaddr* addr, int length)
{
    ompi_if_t* intf;
    int rc = ompi_ifinit();
    if(rc != OMPI_SUCCESS)
        return rc;

    for(intf =  (ompi_if_t*)ompi_list_get_first(&ompi_if_list);
        intf != (ompi_if_t*)ompi_list_get_end(&ompi_if_list);
        intf =  (ompi_if_t*)ompi_list_get_next(intf)) {
        if(strcmp(intf->if_name, if_name) == 0) {
            memcpy(addr, &intf->if_addr, length);
            return OMPI_SUCCESS;
        }
    }
    return OMPI_ERROR;
}


/*
 *  Look for interface by name and returns its 
 *  corresponding kernel index.
 */

int ompi_ifnametoindex(const char* if_name)
{
    ompi_if_t* intf;
    int rc = ompi_ifinit();
    if(rc != OMPI_SUCCESS)
        return rc;

    for(intf =  (ompi_if_t*)ompi_list_get_first(&ompi_if_list);
        intf != (ompi_if_t*)ompi_list_get_end(&ompi_if_list);
        intf =  (ompi_if_t*)ompi_list_get_next(intf)) {
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

int ompi_ifaddrtoname(const char* if_addr, char* if_name, int length)
{
    ompi_if_t* intf;
#ifndef WIN32
    in_addr_t inaddr;
#else 
    unsigned long inaddr;
#endif
    int rc;
    struct hostent *h;
    
    inaddr = inet_addr(if_addr);

    rc = ompi_ifinit();
    if(rc != OMPI_SUCCESS)
        return rc;

    if(inaddr == INADDR_NONE) {
        h = gethostbyname(if_addr);
        if(h == 0) {
            ompi_output(0,"ompi_ifaddrtoname: unable to resolve %s\n", if_addr);
            return OMPI_ERR_NOT_FOUND;
        }
        memcpy(&inaddr, h->h_addr, sizeof(inaddr));
    }

    for(intf =  (ompi_if_t*)ompi_list_get_first(&ompi_if_list);
        intf != (ompi_if_t*)ompi_list_get_end(&ompi_if_list);
        intf =  (ompi_if_t*)ompi_list_get_next(intf)) {
        if(intf->if_addr.sin_addr.s_addr == inaddr) {
            strncpy(if_name, intf->if_name, length);
            return OMPI_SUCCESS;
        }
    }
    return OMPI_ERR_NOT_FOUND;
}

/*
 *  Return the number of discovered interface.
 */

int ompi_ifcount(void)
{
    if(ompi_ifinit() != OMPI_SUCCESS)
        return (-1);
    return ompi_list_get_size(&ompi_if_list);
}


/*
 *  Return the kernels interface index for the first
 *  interface in our list.
 */

int ompi_ifbegin(void)
{
    ompi_if_t *intf;
    if(ompi_ifinit() != OMPI_SUCCESS)
        return (-1);
    intf = (ompi_if_t*)ompi_list_get_first(&ompi_if_list);
    if(NULL != intf)
        return intf->if_index;
    return (-1);
}


/*
 *  Located the current position in the list by if_index and
 *  return the interface index of the next element in our list
 *  (if it exists).
 */

int ompi_ifnext(int if_index)
{
    ompi_if_t *intf;
    if(ompi_ifinit() != OMPI_SUCCESS)
        return (-1);

    for(intf =  (ompi_if_t*)ompi_list_get_first(&ompi_if_list);
        intf != (ompi_if_t*)ompi_list_get_end(&ompi_if_list);
        intf =  (ompi_if_t*)ompi_list_get_next(intf)) {
        if(intf->if_index == if_index) {
            do {
                ompi_if_t* if_next = (ompi_if_t*)ompi_list_get_next(intf);
                ompi_if_t* if_end =  (ompi_if_t*)ompi_list_get_end(&ompi_if_list);
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

int ompi_ifindextoaddr(int if_index, struct sockaddr* if_addr, int length)
{
    ompi_if_t* intf;
    int rc = ompi_ifinit();
    if(rc != OMPI_SUCCESS)
        return rc;

    for(intf =  (ompi_if_t*)ompi_list_get_first(&ompi_if_list);
        intf != (ompi_if_t*)ompi_list_get_end(&ompi_if_list);
        intf =  (ompi_if_t*)ompi_list_get_next(intf)) {
        if(intf->if_index == if_index) {
            memcpy(if_addr, &intf->if_addr, length);
            return OMPI_SUCCESS;
        }
    }
    return OMPI_ERROR;
}


/* 
 *  Lookup the interface by kernel index and return the 
 *  network mask assigned to the interface.
 */

int ompi_ifindextomask(int if_index, struct sockaddr* if_mask, int length)
{
    ompi_if_t* intf;
    int rc = ompi_ifinit();
    if(rc != OMPI_SUCCESS)
        return rc;

    for(intf =  (ompi_if_t*)ompi_list_get_first(&ompi_if_list);
        intf != (ompi_if_t*)ompi_list_get_end(&ompi_if_list);
        intf =  (ompi_if_t*)ompi_list_get_next(intf)) {
        if(intf->if_index == if_index) {
            memcpy(if_mask, &intf->if_mask, length);
            return OMPI_SUCCESS;
        }
    }
    return OMPI_ERROR;
}



/* 
 *  Lookup the interface by kernel index and return
 *  the associated name.
 */

int ompi_ifindextoname(int if_index, char* if_name, int length)
{
    ompi_if_t *intf;
    int rc = ompi_ifinit();
    if(rc != OMPI_SUCCESS)
        return rc;

    for(intf =  (ompi_if_t*)ompi_list_get_first(&ompi_if_list);
        intf != (ompi_if_t*)ompi_list_get_end(&ompi_if_list);
        intf =  (ompi_if_t*)ompi_list_get_next(intf)) {
        if(intf->if_index == if_index) {
            strncpy(if_name, intf->if_name, length);
            return OMPI_SUCCESS;
        }
    }
    return OMPI_ERROR;
}

#define ADDRLEN 100
bool
ompi_ifislocal(char *hostname)
{
    char addrname[ADDRLEN - 1];
    int ret;

    ret = ompi_ifaddrtoname(hostname, addrname, ADDRLEN);
    if (OMPI_SUCCESS == ret) return true;

    return false;
}
