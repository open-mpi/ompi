#include "ompi_config.h"
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
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


void mca_oob_tcp_addr_pack(ompi_buffer_t buffer)
{
    uint32_t count = 0;
    int i;

    ompi_pack(buffer, &mca_oob_name_self, 1, OMPI_NAME);
    for(i=ompi_ifbegin(); i>0; i=ompi_ifnext(i)) {
        struct sockaddr_in inaddr;
        ompi_ifindextoaddr(i, (struct sockaddr*)&inaddr, sizeof(inaddr));
        if(inaddr.sin_addr.s_addr == inet_addr("127.0.0.1"))
            continue;
        count++;
    }
    ompi_pack(buffer, &count, 1, OMPI_INT32);

    for(i=ompi_ifbegin(); i>0; i=ompi_ifnext(i)) {
        struct sockaddr_in inaddr;
        ompi_ifindextoaddr(i, (struct sockaddr*)&inaddr, sizeof(inaddr));
        if(inaddr.sin_addr.s_addr == inet_addr("127.0.0.1"))
            continue;
        inaddr.sin_port = mca_oob_tcp_component.tcp_listen_port;
        ompi_pack(buffer,&inaddr,sizeof(inaddr),OMPI_BYTE);
    }
}


mca_oob_tcp_addr_t* mca_oob_tcp_addr_unpack(ompi_buffer_t buffer)
{
    mca_oob_tcp_addr_t* addr = OBJ_NEW(mca_oob_tcp_addr_t);
    if(NULL == addr) 
         return NULL;

    ompi_unpack(buffer, &addr->addr_name, 1, OMPI_NAME);
    ompi_unpack(buffer, &addr->addr_count, 1, OMPI_INT32);
    if(addr->addr_count != 0) {
        addr->addr_inet = malloc(sizeof(struct sockaddr_in) * addr->addr_count);
        if(NULL == addr->addr_inet) {
             OBJ_RELEASE(addr);
             return NULL;
        }
        addr->addr_alloc = addr->addr_count;
        ompi_unpack(buffer, addr->addr_inet, (addr->addr_count * sizeof(struct sockaddr_in)), OMPI_BYTE);
    }
    return addr;
}


int mca_oob_tcp_addr_get_next(mca_oob_tcp_addr_t* addr, struct sockaddr_in* inaddr)
{
    if(addr == NULL || addr->addr_count == 0)
        return OMPI_ERROR;
    *inaddr = addr->addr_inet[addr->addr_next];
    if(++addr->addr_next >= addr->addr_count)
        addr->addr_next = 0;
    return OMPI_SUCCESS;
}


int mca_oob_tcp_addr_insert(mca_oob_tcp_addr_t* addr, const struct sockaddr_in* inaddr)
{
    if(addr->addr_alloc == 0) {
        addr->addr_alloc = 2;
        addr->addr_inet = malloc(addr->addr_alloc * sizeof(struct sockaddr_in));
    } else if(addr->addr_count == addr->addr_alloc) {
        addr->addr_alloc <<= 1;
        addr->addr_inet = realloc(addr->addr_inet, addr->addr_alloc * sizeof(struct sockaddr_in));
    }
    if(NULL == addr->addr_inet)
        return OMPI_ERR_OUT_OF_RESOURCE;
    memcpy(addr->addr_inet+addr->addr_count, inaddr, sizeof(struct sockaddr_in));
    addr->addr_count++;
    return OMPI_SUCCESS;
}

