/*
 * $HEADER$
 */

#include <string.h>
#include <unistd.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <net/if.h>
#include <netdb.h>

#include "include/constants.h"
#include "lfc/lam_list.h"
#include "util/if.h"
#include "util/output.h"
#include "util/strncpy.h"

#ifndef IF_NAMESIZE
#define IF_NAMESIZE 32
#endif

struct lam_if_t {
    lam_list_item_t     super;
    char                if_name[IF_NAMESIZE];
    int                 if_index;
    int                 if_flags;
    int                 if_speed;
    struct sockaddr_in  if_addr;
    struct sockaddr_in  if_mask;
    uint32_t            if_bandwidth;
};
typedef struct lam_if_t lam_if_t;

static lam_list_t lam_if_list;


/*
 *  Discover the list of configured interfaces. Don't care about any
 *  interfaces that are not up or are local loopbacks.
 */

static int lam_ifinit(void) 
{
    char buff[1024];
    char *ptr;
    struct ifconf ifconf;
    int sd;
    ifconf.ifc_len = sizeof(buff);
    ifconf.ifc_buf = buff;
                                                                                
    if (lam_list_get_size(&lam_if_list) > 0)
        return LAM_SUCCESS;

    if((sd = socket(AF_INET, SOCK_DGRAM, 0)) < 0) {
        lam_output(0, "lam_ifinit: socket() failed with errno=%d\n", errno);
        return LAM_ERROR;
    }
                                                                                
    if(ioctl(sd, SIOCGIFCONF, &ifconf) < 0) {
        lam_output(0, "lam_ifinit: ioctl(SIOCGIFCONF) failed with errno=%d", errno);
        close(sd);
        return LAM_ERROR;
    }
    OBJ_CONSTRUCT(&lam_if_list, lam_list_t);

    for(ptr = buff; ptr < buff + ifconf.ifc_len; ) {
        struct ifreq* ifr = (struct ifreq*)ptr;
        lam_if_t intf;
        lam_if_t *intf_ptr;

        OBJ_CONSTRUCT(&intf, lam_list_item_t);

#if defined(__APPLE__)
        ptr += (sizeof(ifr->ifr_name) + 
               MAX(sizeof(struct sockaddr),ifr->ifr_addr.sa_len));
#else
        switch(ifr->ifr_addr.sa_family) {
        case AF_INET6:
            ptr += sizeof(ifr->ifr_name) + sizeof(struct sockaddr_in6);
            break;
        case AF_INET:
        default:
            ptr += sizeof(ifr->ifr_name) + sizeof(struct sockaddr);
            break;
        }
#endif
        if(ifr->ifr_addr.sa_family != AF_INET)
            continue;

        if(ioctl(sd, SIOCGIFFLAGS, ifr) < 0) {
            lam_output(0, "lam_ifinit: ioctl(SIOCGIFFLAGS) failed with errno=%d", errno);
            continue;
        }
        if ((ifr->ifr_flags & IFF_UP) == 0) 
            continue;
        if ((ifr->ifr_flags & IFF_LOOPBACK) != 0)
            continue;
                                                                                
        strcpy(intf.if_name, ifr->ifr_name);
        intf.if_flags = ifr->ifr_flags;

#if defined(__APPLE__)
        intf.if_index = lam_list_get_size(&lam_if_list)+1;
#else
        if(ioctl(sd, SIOCGIFINDEX, ifr) < 0) {
            lam_output(0,"lam_ifinit: ioctl(SIOCGIFINDEX) failed with errno=%d", errno);
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
            lam_output(0, "lam_ifinit: ioctl(SIOCGIFADDR) failed with errno=%d", errno);
            break;
        }
        if(ifr->ifr_addr.sa_family != AF_INET) 
            continue;

        memcpy(&intf.if_addr, &ifr->ifr_addr, sizeof(intf.if_addr));
        if(ioctl(sd, SIOCGIFNETMASK, ifr) < 0) {
            lam_output(0, "lam_ifinit: ioctl(SIOCGIFNETMASK) failed with errno=%d", errno);
            continue;
        }
        memcpy(&intf.if_mask, &ifr->ifr_addr, sizeof(intf.if_mask));

        intf_ptr = malloc(sizeof(lam_if_t));
        if(intf_ptr == 0) {
            lam_output(0, "lam_ifinit: unable to allocated %d bytes\n", sizeof(lam_if_t));
            return LAM_ERR_OUT_OF_RESOURCE;
        }
        memcpy(intf_ptr, &intf, sizeof(intf));
        lam_list_append(&lam_if_list, (lam_list_item_t*)intf_ptr);
    }
    close(sd);
    return LAM_SUCCESS;
}


/*
 *  Look for interface by name and returns its address 
 *  as a dotted decimal formatted string.
 */

int lam_ifnametoaddr(const char* if_name, struct sockaddr* addr, int length)
{
    lam_if_t* intf;
    int rc = lam_ifinit();
    if(rc != LAM_SUCCESS)
        return rc;

    for(intf =  (lam_if_t*)lam_list_get_first(&lam_if_list);
        intf != (lam_if_t*)lam_list_get_end(&lam_if_list);
        intf =  (lam_if_t*)lam_list_get_next(intf)) {
        if(strcmp(intf->if_name, if_name) == 0) {
            memcpy(addr, &intf->if_addr, length);
            return LAM_SUCCESS;
        }
    }
    return LAM_ERROR;
}


/*
 *  Look for interface by name and returns its 
 *  corresponding kernel index.
 */

int lam_ifnametoindex(const char* if_name)
{
    lam_if_t* intf;
    int rc = lam_ifinit();
    if(rc != LAM_SUCCESS)
        return rc;

    for(intf =  (lam_if_t*)lam_list_get_first(&lam_if_list);
        intf != (lam_if_t*)lam_list_get_end(&lam_if_list);
        intf =  (lam_if_t*)lam_list_get_next(intf)) {
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

int lam_ifaddrtoname(const char* if_addr, char* if_name, int length)
{
    lam_if_t* intf;
    in_addr_t inaddr = inet_addr(if_addr);
    int rc = lam_ifinit();
    if(rc != LAM_SUCCESS)
        return rc;

    if(inaddr == INADDR_ANY) {
        struct hostent *h = gethostbyname(if_addr);
        if(h == 0) {
            lam_output(0,"lam_ifaddrtoname: unable to resolve %s\n", if_addr);
            return LAM_ERROR;
        }
        memcpy(&inaddr, h->h_addr, sizeof(inaddr));
    }

    for(intf =  (lam_if_t*)lam_list_get_first(&lam_if_list);
        intf != (lam_if_t*)lam_list_get_end(&lam_if_list);
        intf =  (lam_if_t*)lam_list_get_next(intf)) {
        if(intf->if_addr.sin_addr.s_addr == inaddr) {
            strncpy(if_name, intf->if_name, length);
            return LAM_ERROR;
        }
    }
    return LAM_SUCCESS;
}

/*
 *  Return the number of discovered interface.
 */

int lam_ifcount()
{
    if(lam_ifinit() != LAM_SUCCESS)
        return (-1);
    return lam_list_get_size(&lam_if_list);
}


/*
 *  Return the kernels interface index for the first
 *  interface in our list.
 */

int lam_ifbegin()
{
    lam_if_t *intf;
    if(lam_ifinit() != LAM_SUCCESS)
        return (-1);
    intf = (lam_if_t*)lam_list_get_first(&lam_if_list);
    if(intf != 0)
        return intf->if_index;
    return (-1);
}


/*
 *  Located the current position in the list by if_index and
 *  return the interface index of the next element in our list
 *  (if it exists).
 */

int lam_ifnext(int if_index)
{
    lam_if_t *intf;
    if(lam_ifinit() != LAM_SUCCESS)
        return (-1);

    for(intf =  (lam_if_t*)lam_list_get_first(&lam_if_list);
        intf != (lam_if_t*)lam_list_get_end(&lam_if_list);
        intf =  (lam_if_t*)lam_list_get_next(intf)) {
        if(intf->if_index == if_index) {
            lam_if_t* if_next = (lam_if_t*)lam_list_get_next(intf);
            return (if_next == (lam_if_t*)lam_list_get_end(&lam_if_list)) ? -1 : if_next->if_index;
        }
    }
    return (-1);
}


/* 
 *  Lookup the interface by kernel index and return the 
 *  primary address assigned to the interface.
 */

int lam_ifindextoaddr(int if_index, struct sockaddr* if_addr, int length)
{
    lam_if_t* intf;
    int rc = lam_ifinit();
    if(rc != LAM_SUCCESS)
        return rc;

    for(intf =  (lam_if_t*)lam_list_get_first(&lam_if_list);
        intf != (lam_if_t*)lam_list_get_end(&lam_if_list);
        intf =  (lam_if_t*)lam_list_get_next(intf)) {
        if(intf->if_index == if_index) {
            memcpy(if_addr, &intf->if_addr, length);
            return LAM_SUCCESS;
        }
    }
    return LAM_ERROR;
}


/* 
 *  Lookup the interface by kernel index and return the 
 *  network mask assigned to the interface.
 */

int lam_ifindextomask(int if_index, struct sockaddr* if_mask, int length)
{
    lam_if_t* intf;
    int rc = lam_ifinit();
    if(rc != LAM_SUCCESS)
        return rc;

    for(intf =  (lam_if_t*)lam_list_get_first(&lam_if_list);
        intf != (lam_if_t*)lam_list_get_end(&lam_if_list);
        intf =  (lam_if_t*)lam_list_get_next(intf)) {
        if(intf->if_index == if_index) {
            memcpy(if_mask, &intf->if_mask, length);
            return LAM_SUCCESS;
        }
    }
    return LAM_ERROR;
}



/* 
 *  Lookup the interface by kernel index and return
 *  the associated name.
 */

int lam_ifindextoname(int if_index, char* if_name, int length)
{
    lam_if_t *intf;
    int rc = lam_ifinit();
    if(rc != LAM_SUCCESS)
        return rc;

    for(intf =  (lam_if_t*)lam_list_get_first(&lam_if_list);
        intf != (lam_if_t*)lam_list_get_end(&lam_if_list);
        intf =  (lam_if_t*)lam_list_get_next(intf)) {
        if(intf->if_index == if_index) {
            strncpy(if_name, intf->if_name, length);
            return LAM_SUCCESS;
        }
    }
    return LAM_ERROR;
}

