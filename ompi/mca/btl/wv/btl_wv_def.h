/*
 * Structs for infiniband 
 */
#ifndef BTL_WV_DEF_H
#define BTL_WV_DEF_H
#include <windows.h>
#include <rdma\winverbs.h>
#include <comp_channel.h>

typedef unsigned __int8     uint8_t;
typedef unsigned __int16    uint16_t;
typedef unsigned __int32    uint32_t;
typedef unsigned __int64    uint64_t;
    
union wv_gid
{
    uint8_t            raw[16];
    struct
    {
        uint64_t    subnet_prefix;
        uint64_t    interface_id;
    }global;
};

enum wv_node_type
{
    WV_NODE_UNKNOWN    = -1,
    WV_NODE_CA         = 1,
    WV_NODE_SWITCH,
    WV_NODE_ROUTER,
};

struct wv_device
{
    enum wv_node_type        node_type;
    WV_DEVICE_TYPE           transport_type;
    char                     name[64];
};

struct wv_context
{
    struct wv_device        *device;
    IWVDevice               *device_if;
    COMP_CHANNEL            channel;
};

enum wv_mtu
{
    WV_MTU_256  = 1,
    WV_MTU_512  = 2,
    WV_MTU_1024 = 3,
    WV_MTU_2048 = 4,
    WV_MTU_4096 = 5
};

struct wv_pd
{
    struct wv_context    *context;
    IWVProtectionDomain  *handle;
};

struct wv_mr 
{
    struct wv_context        *context;
    struct wv_pd             *pd;
    void                     *addr;
    size_t                   length;
    uint32_t                 lkey;
    uint32_t                 rkey;
};

struct wv_srq_attr
{
    uint32_t        max_wr;
    uint32_t        max_sge;
    uint32_t        srq_limit;
};

struct wv_srq_init_attr
{
    void                  *srq_context;
    struct wv_srq_attr    attr;
};

struct wv_recv_wr
{
    uint64_t                wr_id;
    struct wv_recv_wr       *next;
    WV_SGE                  *sg_list;
    int                     num_sge;
};

struct wv_srq
{
    struct wv_context       *context;
    void                    *srq_context;
    struct wv_pd            *pd;
    IWVSharedReceiveQueue   *handle;
};

struct wv_qp
{
    struct wv_context       *context;
    void                    *qp_context;
    struct wv_pd            *pd;
    struct wv_cq            *send_cq;
    struct wv_cq            *recv_cq;
    struct wv_srq           *srq;
    IWVQueuePair            *handle;
    union
    {
        IWVDatagramQueuePair    *ud_handle;
        IWVConnectQueuePair     *conn_handle;
    };
    uint32_t                qp_num;
    WV_QP_STATE             state;
    WV_QP_TYPE              qp_type;
};

struct wv_comp_channel
{
    struct wv_context       *context;
    COMP_CHANNEL            comp_channel;
};

struct wv_cq
{
    struct wv_context        *context;
    struct wv_comp_channel   *channel;
    void                     *cq_context;
    IWVCompletionQueue       *handle;
    int                      cqe;
    COMP_ENTRY               comp_entry;
    LONG volatile            notify_cnt;
    LONG volatile            ack_cnt;
};

struct wverbs_device
{
    struct wv_device    device;
    uint64_t            guid;
    uint8_t             phys_port_cnt;
};

struct wverbs_port
{
    COMP_ENTRY           comp_entry;
    DWORD                event_flag;
    uint8_t              port_num;
};

struct wverbs_context
{
    struct wv_context      context;
    struct wverbs_device   device;
    uint8_t                closing;
    struct wverbs_port     *port;
    wverbs_port            *event_port;
};

struct wv_ah
{
    IWVAddressHandle        *handle;
    ULONG_PTR                key;
};
#endif 