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
#include "ptl_elan_req.h"

#define _TRACK_MALLOC 0

#include <elan/elan.h>
#include <elan/init.h>

#include <rms/rmscall.h>
#include "misc_sys.h"
#include "init_sys.h"
#include "elan4/events.h"

#define OMPI_PTL_ELAN_CHECK_UNEX(value, unexp, errno, output)          \
        do {                                                           \
            if (value == unexp) {                                      \
                ompi_output(output,                                    \
                        "[%s:%d] received unexpect allocated value \n",\
                        __FILE__, __LINE__);                           \
                return errno;                                          \
            }                                                          \
	} while (0)

#define CHECK_ELAN 0

#if CHECK_ELAN
#define START_FUNC()                                         \
    do {                                                     \
	char hostname[32]; gethostname(hostname, 32);        \
	fprintf(stderr, "[%s:%s:%d] Entering ...\n",         \
	    hostname, __FUNCTION__, __LINE__);               \
    } while (0);

#define END_FUNC()                                           \
    do {                                                     \
	char hostname[32]; gethostname(hostname, 32);        \
	fprintf(stderr, "[%s:%s:%d] Completes ...\n",        \
	    hostname, __FUNCTION__, __LINE__);               \
    } while (0);

#else

#define START_FUNC()
#define END_FUNC()

#endif


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

typedef struct {
    /* SHOULD BE 128-byte aligned */
    uint8_t     data[INPUT_QUEUE_MAX];  /* queue req data packet */
    /* SHOULD be 32-byte aligned */
    E4_Event32  event32;        /* Local elan completion event */
} ompi_elan_event_t;

/**
 * ELAN descriptor for send
 */
#define ELAN_BASE_DESC_FIELDS  \
    E4_DMA64           main_dma; /**< Must be 8-byte aligned */   \
    /* 8 byte aligned */                                          \
    volatile E4_uint64 main_doneWord;                             \
    /* 8 byte aligned */                                          \
    ompi_elan_event_t *elan_data_event;                           \
    mca_ptl_elan_send_request_t *req;                             \
    /* 8 byte aligned */                                          \
    int    desc_type;                                             \
    int    desc_status;                                           \
    /* 8 byte aligned */

struct ompi_ptl_elan_base_desc_t {
    ELAN_BASE_DESC_FIELDS 
    /* 8 byte aligned */
};
typedef struct ompi_ptl_elan_base_desc_t ompi_ptl_elan_base_desc_t;

struct ompi_ptl_elan_qdma_desc_t {

    ELAN_BASE_DESC_FIELDS 
    /* 8 byte aligned */

    mca_ptl_elan_t *ptl;
    RAIL           *rail;
    /* 8 byte aligned */

    uint8_t         buff[INPUT_QUEUE_MAX];        /**< queue data */
    /* 8 byte aligned */
};
typedef struct ompi_ptl_elan_qdma_desc_t ompi_ptl_elan_qdma_desc_t;

struct ompi_ptl_elan_queue_ctrl_t {
    /* Transmit Queues */
    /** < elan located INPUT_QUEUE_ALIGN'ed with INPUT_QUEUE_SIZE */
    E4_InputQueue *input;

    /** <transmit queue structures */
    void       *tx_q;
    E4_CmdQ    *tx_cmdq;
    ELAN4_COOKIEPOOL *tx_cpool;
    ompi_event_t *tx_events;

    ompi_list_t tx_desc;
    ompi_free_list_t tx_desc_free;

    /* User progression */
    ompi_mutex_t rx_lock;
    int         rx_buffsize;
    int         rx_slotsize;
    int         rx_nslots;

    /*Automatic progression */
    void        (*rx_fn) (void);
    void       *rx_handle;

    /* Recv Queue has to be well-aligned */
    ompi_ptl_elan_recv_queue_t *rxq;
};
typedef struct ompi_ptl_elan_queue_ctrl_t ompi_ptl_elan_queue_ctrl_t;


struct mca_ptl_elan_state_t {

    /* User configurable parameters */
    int         initialized;
    char       *elan_version;    /**< Version of the elan library */
    uint64_t    elan_debug;      /**< elan debug tracing output */
    uint64_t    elan_traced;     /**< elan TRACE output */
    uint64_t    elan_flags;
    FILE       *elan_debugfile; /* Debug output file handle      */
    int         elan_signalnum;

    long        elan_waittype;  /**< how to wait for events */
    size_t      main_size;    /**< size of Main memory allocator heap */
    size_t      elan_size;    /**< size of Elan memory allocator heap */
    void       *main_base;    /**< Main memory allocator heap base */
    void       *elan_base;    /**< Elan memory allocator heap base */

    /* other state parameters */

    unsigned int elan_vp;          /**< elan vpid, not ompi vpid */
    unsigned int elan_nvp;         /**< total # of elan vpid */
    int        *elan_localvps;     /**< mapping of localId to elan vp */
    int         elan_localid;    /**< # of local elan vpids */
    int         elan_numlocals;    /**< # of local elan vpids */
    int         elan_maxlocals;    /**< maximum # of local elan vpids */
    int         elan_nrails;       /**< # of rails */
    int         elan_rmsid;        /**< rms resource id */
    int         intcookie;
    long        elan_pagesize;
    pid_t       elan_pid;

    /* TODO:
     *   Even though the elan threads are not utilized for now. 
     *   We provide memory/state control structures for later extensions.
     *   A simple type casting of ELAN_ESTATE can bring
     *   the complete structure of the ELAN_EPRIVSATE.
     */
    ELAN_LOCATION elan_myloc;
    void       *elan_cap;         /**< job capability */
    ELAN_CTX   *elan_ctx;         /**< Elan ctx of the 0th rail */
    void       *elan_estate;      /**< Elan state of the 0th rail */
    ELAN_RAIL **elan_rail;        /**< pointers to Rail control struct for all rails */
    RAIL      **all_rails;        /**< all rails */
    int        *rail_intcookie;      /**< record the cookies for the rail */
    ADDR_SDRAM *all_estates;
    mca_ptl_elan_module_1_0_0_t *elan_module;
};
typedef struct mca_ptl_elan_state_t mca_ptl_elan_state_t;

/* Util functions, consider moving into a file ptl_elan_util.h */
ELAN_SLEEP *ompi_init_elan_sleepdesc (mca_ptl_elan_state_t * ems,
                                      RAIL * rail);

/* Initialization and finalization routines */
int         ompi_mca_ptl_elan_init (mca_ptl_elan_module_1_0_0_t * emp);
int         ompi_mca_ptl_elan_finalize (mca_ptl_elan_module_1_0_0_t * emp);

/* communication initialization prototypes */
int         ompi_init_elan_qdma (mca_ptl_elan_module_1_0_0_t * emp,
                                 int num_rails);
int         ompi_init_elan_rdma (mca_ptl_elan_module_1_0_0_t * emp,
                                 int num_rails);
int         ompi_init_elan_stat (mca_ptl_elan_module_1_0_0_t * emp,
                                 int num_rails);

/* communication prototypes */
int         mca_ptl_elan_start_desc(mca_ptl_elan_desc_item_t *desc,
		  struct mca_ptl_elan_peer_t *ptl_peer,
                  struct mca_pml_base_send_request_t *sendreq,
                  size_t offset,
                  size_t size,
                  int flags);

int         mca_ptl_elan_poll_desc(mca_ptl_elan_desc_item_t *desc);
int         mca_ptl_elan_wait_desc(mca_ptl_elan_desc_item_t *desc);

/* control, synchronization and state prototypes */
int         mca_ptl_elan_drain_recv(mca_ptl_elan_module_1_0_0_t *emp);
int         mca_ptl_elan_update_send(mca_ptl_elan_module_1_0_0_t *emp);

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
