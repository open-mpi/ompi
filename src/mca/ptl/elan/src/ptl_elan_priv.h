/*
 * $HEADER$
 */
#ifndef MCA_PTL_ELAN_PRIV_H
#define MCA_PTL_ELAN_PRIV_H

#include <errno.h>
#include <unistd.h>
#include <string.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

#include "constants.h"
#include "event/event.h"
#include "util/if.h"
#include "util/argv.h"
#include "util/output.h"
#include "mca/pml/pml.h"
#include "mca/ptl/ptl.h"
#include "mca/pml/base/pml_base_sendreq.h"
#include "mca/pml/base/pml_base_recvreq.h"
#include "mca/base/mca_base_param.h"
#include "mca/base/mca_base_module_exchange.h"
#include "ptl_elan.h"
#include "ptl_elan_proc.h"
#include "ptl_elan_frag.h"

#define _TRACK_MALLOC 0

#include <elan/elan.h>
#include <elan/init.h>

#include <rms/rmscall.h>
#include "misc_sys.h"
#include "init_sys.h"
#include "elan4/events.h"

#define  PTL_ELAN_DEBUG_NONE    (0x000)
#define  PTL_ELAN_DEBUG_INIT    (0x001)
#define  PTL_ELAN_DEBUG_FIN     (0x002)
#define  PTL_ELAN_DEBUG_QDESC   (0x004)
#define  PTL_ELAN_DEBUG_RDESC   (0x008)
#define  PTL_ELAN_DEBUG_SEND    (0x010)
#define  PTL_ELAN_DEBUG_RECV    (0x020)
#define  PTL_ELAN_DEBUG_ACK     (0x040)
#define  PTL_ELAN_DEBUG_MAC     (0x080)
#define  PTL_ELAN_DEBUG_QDMA    (0x100)
#define  PTL_ELAN_DEBUG_PUT     (0x200)
#define  PTL_ELAN_DEBUG_GET     (0x400)
#define  PTL_ELAN_DEBUG_CHAIN   (0x800)
#define  PTL_ELAN_DEBUG_FLAG    PTL_ELAN_DEBUG_NONE

#define START_FUNC(flag)                                       \
do {                                                           \
    if (PTL_ELAN_DEBUG_FLAG & flag) {                          \
	char hostname[32]; gethostname(hostname, 32);          \
	fprintf(stdout, "[%s:%s:%d] Entering ...\n",           \
	    hostname, __FUNCTION__, __LINE__);                 \
    }                                                          \
} while (0) 

#define END_FUNC(flag)                                         \
do {                                                           \
    if (PTL_ELAN_DEBUG_FLAG & flag) {                          \
	char hostname[32]; gethostname(hostname, 32);          \
	fprintf(stdout, "[%s:%s:%d] Completes ...\n",          \
	    hostname, __FUNCTION__, __LINE__);                 \
    }                                                          \
} while (0)

#define  LOG_PRINT(flag, args...)                              \
do {                                                           \
    if (PTL_ELAN_DEBUG_FLAG & flag) {                          \
	char hostname[32]; gethostname(hostname, 32);          \
	fprintf(stdout, "[%s:%s:%d] ",                         \
		hostname, __FUNCTION__, __LINE__);             \
	fprintf(stdout, args);                                 \
    }                                                          \
} while (0)

/* PTL_ELAN related MACROS, expose some as configurable options if needed */
#define  OMPI_PTL_ELAN_ENABLE_GET    (1)
#define  OMPI_PTL_ELAN_COMP_QUEUE    (0) 
#define  OMPI_PTL_ELAN_THREADING     (OMPI_HAVE_POSIX_THREADS)

#define  OMPI_PTL_ELAN_MAX_QSIZE     (2048)
#define  OMPI_PTL_ELAN_MAX_QSLOTS    (128)
#define  OMPI_PTL_ELAN_LOST_QSLOTS   (1)

#define  OMPI_PTL_ELAN_MAX_QDESCS    (128)
#define  OMPI_PTL_ELAN_NUM_QDESCS    (4)
#define  OMPI_PTL_ELAN_QDMA_RETRY    (16)

#define  OMPI_PTL_ELAN_MAX_PUTGET    (32)
#define  OMPI_PTL_ELAN_NUM_PUTGET    (8)
#define  OMPI_PTL_ELAN_MAX_PGDESC    (8)

#define  OMPI_PTL_ELAN_FASTPATH      (0x1)
#define  OMPI_PTL_ELAN_SLOT_ALIGN    (128)
#define  OMPI_PTL_ELAN_GET_MAX(a,b)  ((a>b)? a:b)
#define  OMPI_PTL_ELAN_GET_MIN(a,b)  ((a<b)? a:b)
#define  OMPI_PTL_ELAN_ALIGNUP(x,a)  (((unsigned int)(x) + ((a)-1)) & (-(a)))

#if (QSNETLIBS_VERSION_CODE <= QSNETLIBS_VERSION(1,6,6))
#define OMPI_PTL_ELAN_ALLOC_CMDQ elan4_alloc_cmdq 
#define OMPI_PTL_ELAN_PROBE_CMDQ elan4_probe_cmdq
#else
#define OMPI_PTL_ELAN_ALLOC_CMDQ(ctx, alloc, size, bits, params) \
	elan4_alloc_cmdq (ctx, size, bits, params)
#define OMPI_PTL_ELAN_PROBE_CMDQ(ctx, alloc, maxcmds, flags) \
	elan4_probe_cmdq (ctx, maxcmds, flags)
#endif

#define OMPI_PTL_ELAN_CHECK_UNEX(value, unexp, errno, output)  \
do {                                                           \
    if (value == unexp) {                                      \
	ompi_output(output,                                    \
		"[%s:%d] allocate received unexpect value \n", \
		__FILE__, __LINE__);                           \
	return errno;                                          \
    }                                                          \
} while (0)

enum {
    /* the first four bits for type */
    MCA_PTL_ELAN_DESC_NULL   = 0x00,
    MCA_PTL_ELAN_DESC_QDMA   = 0x01,
    MCA_PTL_ELAN_DESC_PUT    = 0x02,
    MCA_PTL_ELAN_DESC_GET    = 0x04,
    /* next first four bits for status */
    MCA_PTL_ELAN_DESC_LOCAL  = 0x10,
    MCA_PTL_ELAN_DESC_CACHED = 0x20
};

struct ompi_ptl_elan_thread_t
{
    ompi_thread_t thread;
    mca_ptl_elan_module_t *ptl;
};
typedef struct ompi_ptl_elan_thread_t ompi_ptl_elan_thread_t;

/**
 * Structure used to publish elan information to peers.
 */
struct mca_ptl_elan_addr_t {
    int         elan_vp;      
    int         inuse;
    ompi_process_name_t gid; 
};
typedef struct mca_ptl_elan_addr_t mca_ptl_elan_addr_t;

struct ompi_ptl_elan_recv_queue_t {
    /* Events needs to be aligned */
    EVENT_WORD  qr_doneWord;
    ADDR_SDRAM  qr_qEvent;
    EVENT32    *qr_elanDone;

    /* The one don't care */
    E4_uint64   qr_efitem;
    E4_uint64   qr_efptr;
    E4_uint64   qr_elitem;
    void       *qr_base;
    void       *qr_fptr;
    void       *qr_top;

    E4_CmdQ    *qr_cmdq;
    ELAN_SLEEP *qr_es;
    RAIL       *qr_rail;
};
typedef struct ompi_ptl_elan_recv_queue_t ompi_ptl_elan_recv_queue_t;

struct ompi_ptl_elan_comp_queue_t {
    /** <Elan located INPUT_QUEUE_ALIGN'ed with INPUT_QUEUE_SIZE */
    E4_InputQueue    *input;
    ompi_mutex_t      rx_lock;
    int               rx_buffsize;
    int               rx_slotsize;
    int               rx_nslots;
    /* Recv Queue has to be well-aligned */
    ompi_ptl_elan_recv_queue_t *rxq;
};
typedef struct ompi_ptl_elan_comp_queue_t ompi_ptl_elan_comp_queue_t;

/**
 * ELAN descriptor for send
 */
#define ELAN_BASE_DESC_FIELDS                                \
    E4_DMA64           main_dma; /**< 8-byte aligned */      \
    /* 8 byte aligned */                                     \
    volatile E4_uint64 main_doneWord;                        \
    /* 8 byte aligned */                                     \
    E4_Event          *elan_event;                           \
    void              *desc_buff;                            \
    /* 8 byte aligned */                                     \
    mca_pml_base_request_t *req;                             \
    mca_ptl_elan_module_t  *ptl;                             \
    /* 8 byte aligned */                                     \
    int    desc_type;                                        \
    int    desc_status;                                      \
    /* 8 byte aligned */                                     \
    E4_DMA64           comp_dma;                             \
    /* 8 byte aligned */                                     \
    volatile E4_uint64 comp_doneWord;                        \
    /* 8 byte aligned */                                     \
    E4_Event32        *comp_event; /* E4_Event plus pad */   \
    /* 8 byte aligned */                                     \
    E4_Addr           *comp_buff;                            \
    E4_Addr           *comp_pad;                             \
    E4_Addr            comp_srcAddr;                         \
    E4_Addr            comp_dstAddr;                         \
    /* 8 byte aligned */

struct ompi_ptl_elan_base_desc_t {
    ELAN_BASE_DESC_FIELDS 
    /* 8 byte aligned */
};
typedef struct ompi_ptl_elan_base_desc_t ompi_ptl_elan_base_desc_t;

struct ompi_ptl_elan_qdma_desc_t {

    ELAN_BASE_DESC_FIELDS 
    /* 8 byte aligned */
    uint8_t         buff[INPUT_QUEUE_MAX];   /**< queue data */
    /* 8 byte aligned */
};
typedef struct ompi_ptl_elan_qdma_desc_t ompi_ptl_elan_qdma_desc_t;

struct ompi_ptl_elan_queue_ctrl_t {

    /** <Elan located INPUT_QUEUE_ALIGN'ed with INPUT_QUEUE_SIZE */
    E4_InputQueue    *input;

    /** <transmit queue structures */
    void             *tx_q;
    E4_CmdQ          *tx_cmdq;
    ELAN4_COOKIEPOOL *tx_cpool;
    ompi_free_list_t  tx_desc_free;

    /* User progression */
    ompi_mutex_t      rx_lock;
    int               rx_buffsize;
    int               rx_slotsize;
    int               rx_nslots;

    /* Recv Queue has to be well-aligned */
    ompi_ptl_elan_recv_queue_t *rxq;
};
typedef struct ompi_ptl_elan_queue_ctrl_t ompi_ptl_elan_queue_ctrl_t;

struct ompi_ptl_elan_putget_desc_t {
    ELAN_BASE_DESC_FIELDS 
    /* 8 byte aligned */

    E4_DMA64           chain_dma; /**< Must be 8-byte aligned */
    /* 8 byte aligned */
    volatile E4_uint64 chain_doneWord;
    /* 8 byte aligned */
    E4_Event32        *chain_event; /* E4_Event plus pad */
    E4_Addr           *chain_buff;
 
    E4_Addr            src_elan_addr;
    E4_Addr            dst_elan_addr;
    /* 8 byte aligned */
};
typedef struct ompi_ptl_elan_putget_desc_t ompi_ptl_elan_putget_desc_t;

struct ompi_ptl_elan_putget_ctrl_t {

    /** <transmit queue structures */
    u_int             pg_throttle;
    int               pg_retryCount;
    int               pg_evictCache;
    int32_t           pg_waitType;
    ELAN_FLAGS        pg_flags;
    ompi_mutex_t      pg_lock;

    E4_CmdQ          *put_cmdq;
    E4_CmdQ          *get_cmdq;

    E4_uint64        *pg_cmdStream;
    ELAN4_COOKIEPOOL *pg_cpool;
    E4_CmdQParams    *pg_cmdPar;

    u_int            *pg_pendingGetCount; 
    ompi_list_t       put_desc;
    ompi_list_t       get_desc;
                      
    ompi_free_list_t  put_desc_free;
    ompi_free_list_t  get_desc_free;
};
typedef struct ompi_ptl_elan_putget_ctrl_t ompi_ptl_elan_putget_ctrl_t;

struct mca_ptl_elan_state_t {

    /* User configurable parameters */
    int         initialized;
    char       *elan_version;     /**< Version of the elan library */
    uint64_t    elan_debug;       /**< elan debug tracing output */
    uint64_t    elan_traced;      /**< elan TRACE output */
    uint64_t    elan_flags;       
    FILE       *elan_debugfile;   /* Debug output file handle      */
    int         elan_signalnum;  
                                  
    long        elan_waittype;    /**< how to wait for events */
    size_t      main_size;        /**< size of Main memory allocator heap */
    size_t      elan_size;        /**< size of Elan memory allocator heap */
    void       *main_base;        /**< Main memory allocator heap base */
    void       *elan_base;        /**< Elan memory allocator heap base */

    /* other state parameters */
    unsigned int elan_vp;         /**< elan vpid, not ompi vpid */
    unsigned int elan_nvp;        /**< total # of elan vpid */
    int         *elan_localvps;   /**< mapping of localId to elan vp */
    int          elan_localid;    /**< # of local elan vpids */
    int          elan_numlocals;  /**< # of local elan vpids */
    int          elan_maxlocals;  /**< maximum # of local elan vpids */
    int          elan_nrails;     /**< # of rails */
    int          elan_rmsid;      /**< rms resource id */
    int          intcookie;
    long         elan_pagesize;
    pid_t        elan_pid;

    /* TODO:
     *   Even though the elan threads are not utilized for now. 
     *   We provide memory/state control structures for later extensions.
     *   A simple type casting of ELAN_ESTATE can bring
     *   the complete structure of the ELAN_EPRIVSATE.
     */
    ELAN_LOCATION elan_myloc;
    void       *elan_cap;        /**< job capability */
    ELAN_CTX   *elan_ctx;        /**< Elan ctx of the 0th rail */
    void       *elan_estate;     /**< Elan state of the 0th rail */
    ELAN_RAIL **elan_rail;       /**< pointers to Rail control struct for all rails */
    RAIL      **all_rails;       /**< all rails */
    int        *rail_intcookie;  /**< record the cookies for the rail */
    ADDR_SDRAM *all_estates;
    mca_ptl_elan_component_t *elan_component;
};
typedef struct mca_ptl_elan_state_t mca_ptl_elan_state_t;

/* Util functions, consider moving into a file ptl_elan_util.h */
ELAN_SLEEP *ompi_init_elan_sleepdesc (mca_ptl_elan_state_t * ems,
                                      RAIL * rail);

/* Initialization and finalization routines */
int         mca_ptl_elan_state_init(mca_ptl_elan_component_t * emp);
int         mca_ptl_elan_state_finalize(mca_ptl_elan_component_t * emp);
int         mca_ptl_elan_thread_init(mca_ptl_elan_component_t * emp);
int         mca_ptl_elan_thread_close(mca_ptl_elan_component_t * emp);

/* communication initialization prototypes */
int         ompi_init_elan_qdma (mca_ptl_elan_component_t * emp,
                                 int num_rails);
int         ompi_init_elan_putget (mca_ptl_elan_component_t * emp,
                                   int num_rails);
int         ompi_init_elan_stat (mca_ptl_elan_component_t * emp,
                                 int num_rails);

/* communication prototypes */
int         mca_ptl_elan_start_desc(mca_ptl_elan_send_frag_t *desc,
				    mca_ptl_elan_peer_t *ptl_peer,
				    mca_pml_base_send_request_t *sendreq,
				    size_t offset,
				    size_t *size,
				    int flags);

int         mca_ptl_elan_start_ack (mca_ptl_base_module_t * ptl, 
				    mca_ptl_elan_send_frag_t * desc,
				    mca_ptl_elan_recv_frag_t * recv_frag);

int         mca_ptl_elan_get_with_ack (mca_ptl_base_module_t * ptl, 
				       mca_ptl_elan_send_frag_t * frag,
				       mca_ptl_elan_recv_frag_t * recv_frag);

int         mca_ptl_elan_poll_desc(mca_ptl_elan_send_frag_t *desc);
int         mca_ptl_elan_wait_desc(mca_ptl_elan_send_frag_t *desc);

/* control, synchronization and state prototypes */
int         mca_ptl_elan_drain_recv(mca_ptl_elan_module_t  * ptl);
int         mca_ptl_elan_update_desc(mca_ptl_elan_module_t * ptl); 

int
mca_ptl_elan_start_get (mca_ptl_elan_send_frag_t * frag,
			struct mca_ptl_elan_peer_t *ptl_peer,
			struct mca_pml_base_recv_request_t *req,
			size_t offset,
			size_t *size,
			int flags);

/**
 * utility routines for parameter registration
 */
static inline char *
mca_ptl_elan_param_register_string (const char *param_name,
                                    const char *default_value)
{
    int         id;
    char       *param_value;

    id = mca_base_param_register_string ("ptl", "elan", param_name, NULL,
                                         default_value);
    mca_base_param_lookup_string (id, &param_value);
    return param_value;
}

static inline int
mca_ptl_elan_param_register_int (const char *param_name,
                                 int default_value)
{
    int         id;
    int         param_value;

    param_value = default_value;
    id = mca_base_param_register_int ("ptl", "elan", param_name, NULL,
                                      default_value);
    mca_base_param_lookup_int (id, &param_value);
    return param_value;
}

#endif
