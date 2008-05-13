#include "ompi_config.h"
#include <infiniband/verbs.h>
#if OMPI_HAVE_RDMACM

#include <rdma/rdma_cma.h>
#include <malloc.h>

#include "opal/util/argv.h"
#include "opal/util/if.h"

#include "connect/connect.h"
#include "btl_openib_endpoint.h"
#include "btl_openib_iwarp.h"

/* 
 * The cruft below maintains the linked list of rdma ipv4 addresses and their
 * associated rdma device names and device port numbers.  
 */
struct rdma_addr_list {
    opal_list_item_t      super;
    uint32_t              addr;
    uint32_t              subnet;
    char                  addr_str[16];
    char                  dev_name[IBV_SYSFS_NAME_MAX];
    uint8_t               dev_port;
};
typedef struct rdma_addr_list rdma_addr_list_t;

static OBJ_CLASS_INSTANCE(rdma_addr_list_t, opal_list_item_t, 
                          NULL, NULL);
static opal_list_t myaddrs;

uint64_t get_iwarp_subnet_id(struct ibv_device *ib_dev)
{
    opal_list_item_t *item;

    for (item = opal_list_get_first(&myaddrs);
         item != opal_list_get_end(&myaddrs);
         item = opal_list_get_next(item)) {
        struct rdma_addr_list *addr = (struct rdma_addr_list *)item;
        if (!strcmp(addr->dev_name, ib_dev->name)) {
            return addr->subnet;
        }
    }

    return 0;
}

uint32_t rdma_get_ipv4addr(struct ibv_context *verbs, uint8_t port)
{
    opal_list_item_t *item;

    for (item = opal_list_get_first(&myaddrs);
         item != opal_list_get_end(&myaddrs);
         item = opal_list_get_next(item)) {
        struct rdma_addr_list *addr = (struct rdma_addr_list *)item;
        if (!strcmp(addr->dev_name, verbs->device->name) && 
            port == addr->dev_port) {
            return addr->addr;
        }
    }
    return 0;
}

static int dev_specified(char *name, int port)
{
    char **list;

    if (NULL != mca_btl_openib_component.if_include) {
        int i;

        list = opal_argv_split(mca_btl_openib_component.if_include, ',');
        for (i = 0; NULL != list[i]; i++) {
            char **temp = opal_argv_split(list[i], ':');
            if (0 == strcmp(name, temp[0]) &&
                (NULL == temp[1] || port == atoi(temp[1]))) {
                return 0;
            }
        }

        return 1;
    }

    if (NULL != mca_btl_openib_component.if_exclude) {
        int i;

        list = opal_argv_split(mca_btl_openib_component.if_exclude, ',');
        for (i = 0; NULL != list[i]; i++) {
            char **temp = opal_argv_split(list[i], ':');
            if (0 == strcmp(name, temp[0]) &&
                (NULL == temp[1] || port == atoi(temp[1]))) {
                return 1;
            }
        }
    }

    return 0;
}

static int add_rdma_addr(struct sockaddr *ipaddr, uint32_t netmask)
{
    struct sockaddr_in *sinp;
    struct rdma_cm_id *cm_id;
    struct rdma_event_channel *ch;
    int rc = OMPI_SUCCESS;
    struct rdma_addr_list *myaddr;

    ch = rdma_create_event_channel();
    if (NULL == ch) {
        BTL_ERROR(("failed creating event channel"));
        rc = OMPI_ERROR;
        goto out1;
    }

    rc = rdma_create_id(ch, &cm_id, NULL, RDMA_PS_TCP);
    if (rc) {
        BTL_ERROR(("rdma_create_id returned %d", rc));
        rc = OMPI_ERROR;
        goto out2;
    }

    rc = rdma_bind_addr(cm_id, ipaddr);
    if (rc) {
        rc = OMPI_SUCCESS;
        goto out3;
    }

    if (!cm_id->verbs ||
        0 == ((struct sockaddr_in *)ipaddr)->sin_addr.s_addr ||
        dev_specified(cm_id->verbs->device->name, cm_id->port_num)) {
        goto out3;
    }

    myaddr = OBJ_NEW(rdma_addr_list_t);
    if (NULL == myaddr) {
        BTL_ERROR(("malloc failed!"));
        rc = OMPI_ERROR;
        goto out3;
    }

    sinp = (struct sockaddr_in *)ipaddr;
    myaddr->addr = sinp->sin_addr.s_addr;
    myaddr->subnet = myaddr->addr & netmask;
    inet_ntop(sinp->sin_family, &sinp->sin_addr, 
            myaddr->addr_str, sizeof myaddr->addr_str);
    memcpy(myaddr->dev_name, cm_id->verbs->device->name, IBV_SYSFS_NAME_MAX);
    myaddr->dev_port = cm_id->port_num;
    BTL_VERBOSE(("adding addr %s dev %s port %d to rdma_addr_list", 
                 myaddr->addr_str, myaddr->dev_name, myaddr->dev_port));

    opal_list_append(&myaddrs, &(myaddr->super));

out3:
    rdma_destroy_id(cm_id);
out2:
    rdma_destroy_event_channel(ch);
out1:
    return rc;
}

int build_rdma_addr_list(void)
{
    int rc, i;

    OBJ_CONSTRUCT(&myaddrs, opal_list_t);

    for (i = opal_ifbegin(); i < opal_ifcount(); i = opal_ifnext(i)) {
        struct sockaddr ipaddr;
        uint32_t netmask;

        opal_ifindextoaddr(i, &ipaddr, sizeof(struct sockaddr));
        opal_ifindextomask(i, &netmask, sizeof(uint32_t));

        if (ipaddr.sa_family == AF_INET) {
            rc = add_rdma_addr(&ipaddr, netmask);
            if (OMPI_SUCCESS != rc) {
                break;
            }
        }
    }
    return rc;
}
  
void free_rdma_addr_list(void)
{
    opal_list_item_t *item;

    if (0 != opal_list_get_size(&myaddrs)) {
        for (item = opal_list_get_first(&myaddrs);
             item != opal_list_get_end(&myaddrs);
             item = opal_list_get_next(item)) {
            opal_list_remove_item(&myaddrs, item);
        }
    }
}
#else
static inline uint64_t get_iwarp_subnet_id(struct ibv_device *ib_dev) {return 0;}
static inline uint32_t rdma_get_ipv4addr(struct ibv_context *verbs, uint8_t port) {return 0;}
static inline int build_rdma_addr_list(void) {return 0;}
static inline void free_rdma_addr_list(void) {}
#endif
