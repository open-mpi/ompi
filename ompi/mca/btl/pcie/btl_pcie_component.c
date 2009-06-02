/*
 * Copyright (c) 2007      Los Alamos National Security, LLC.
 *                         All righs reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"
#include "ompi/constants.h"

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <sched.h>
#include <ctype.h>

#include "opal/event/event.h"
#include "opal/util/argv.h"
#include "opal/util/if.h"
#include "opal/util/output.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/mca/paffinity/paffinity.h"
#include "opal/mca/paffinity/base/base.h"

#include "orte/util/proc_info.h"
#include "orte/util/show_help.h"
#include "orte/mca/errmgr/errmgr.h"

#include "ompi/constants.h"
#include "ompi/datatype/convertor.h" 
#include "ompi/mca/btl/btl.h"
#include "ompi/mca/btl/base/base.h" 
#include "ompi/mca/btl/base/btl_base_error.h"
#include "ompi/mca/mpool/base/base.h" 
#include "ompi/mca/mpool/rdma/mpool_rdma.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/runtime/ompi_module_exchange.h"

#include "btl_pcie.h"
#include "btl_pcie_frag.h"
#include "btl_pcie_endpoint.h" 
#include "btl_pcie_ddriver.h"

static int
dd_register_memory_region (DD_adapter_handle *a_handle,
			   AXON_memory_region_handle *mr_handle,
			   void *starting_addr,
			   int size,
			   int flags)
{
  int rc;
  struct AXON_MR_registration regInfo;
  
  memset (&regInfo, 0, sizeof(regInfo));
  
  regInfo.local_dma_memory_size = size;
  regInfo.local_dma_memory = (__u64) starting_addr;
/* codeme Q-should we check for valid permission flags here  or just
 * let the ioctl code handle it?
 */
  regInfo.permissions = flags|1;    /* always turn on local access */

  rc = ioctl (a_handle->fd, AXONIO_DMA_REGISTER, &regInfo);
  if(-1 == rc) {
    return -1;
  }
  *mr_handle = (AXON_memory_region_handle)regInfo.memory_region_handle;
  return 0;
}

static int
dd_deregister_memory_region (DD_adapter_handle *a_handle,
			     AXON_memory_region_handle *mr_handle)
{
  return (ioctl (a_handle->fd, AXONIO_DMA_DEREGISTER, mr_handle));
}

static int pcie_reg_mr(void *reg_data, void *base, size_t size, 
                       mca_mpool_base_registration_t *reg);
static int pcie_dereg_mr(void* reg_data, mca_mpool_base_registration_t *reg);


mca_btl_pcie_component_t mca_btl_pcie_component = {
    {
        /* First, the mca_base_component_t struct containing meta information
           about the component itself */

        {
            /* Indicate that we are a pml v2.0.0 component (which also implies a
               specific MCA version) */

            MCA_BTL_BASE_VERSION_2_0_0,

            "pcie", /* MCA component name */
            OMPI_MAJOR_VERSION,  /* MCA component major version */
            OMPI_MINOR_VERSION,  /* MCA component minor version */
            OMPI_RELEASE_VERSION,  /* MCA component release version */
            mca_btl_pcie_component_open,  /* component open */
            mca_btl_pcie_component_close  /* component close */
        },

        /* Next the MCA v2.0.0 component meta data */

        {
            false
        },

        mca_btl_pcie_component_init,  
        mca_btl_pcie_component_progress,
    }
};


/*
 * utility routines for parameter registration
 */
static char*
mca_btl_pcie_param_register_string(const char* param_name, 
                                   const char* param_desc,
				   const char* default_value)
{
    char *value;

    mca_base_param_reg_string(&mca_btl_pcie_component.super.btl_version,
                              param_name, param_desc, false, false,
                              default_value, &value);
    return value;
}


static int
mca_btl_pcie_param_register_int(const char* param_name, 
                                const char* param_desc,
				int default_value)
{
    int value;

    mca_base_param_reg_int(&mca_btl_pcie_component.super.btl_version,
                           param_name, param_desc, false, false,
                           default_value, &value);
    return value;
}


/*
 *  Register PCIE device found in local config file. The MCA framework
 *  will make this available to all peers.
 */
static int
btl_pcie_modex_send(void)
{
    size_t size;
    unsigned int i;
    mca_btl_pcie_modex_info_t *info;

    size = mca_btl_pcie_component.pcie_num_btls * 
        sizeof(mca_btl_pcie_modex_info_t);
    info = malloc(size);
    if (NULL == info) return OMPI_ERR_TEMP_OUT_OF_RESOURCE;

    for (i = 0 ; i < mca_btl_pcie_component.pcie_num_btls ; ++i) {
        strncpy(info[i].hostname,
                orte_process_info.nodename,
                ORTE_MAX_HOSTNAME_SIZE - 1);
        info[i].hostname[ORTE_MAX_HOSTNAME_SIZE - 1] = '\0';
        strncpy(info[i].devicename,
                mca_btl_pcie_component.pcie_btls[i].lcl_dev_name, PATH_MAX - 1);
        info[i].devicename[PATH_MAX - 1] = '\0';
        MCA_BTL_PCIE_MODEX_INFO_HTON(info[i]);
    }

#if (OMPI_MAJOR_VERSION <= 1) && (OMPI_MINOR_VERSION <= 2)
    return mca_pml_base_modex_send(&mca_btl_pcie_component.super.btl_version, info, size);
#else
    return ompi_modex_send(&mca_btl_pcie_component.super.btl_version, info, size);
#endif
}


/*
 *  Called by MCA framework to open the component, registers
 *  component parameters.
 */
int
mca_btl_pcie_component_open(void)
{
    /* initialize state */
    mca_btl_pcie_component.pcie_num_btls = 0;
    mca_btl_pcie_component.pcie_btls = NULL;
    
    /* initialize objects */ 
    OBJ_CONSTRUCT(&mca_btl_pcie_component.pcie_procs, opal_list_t);

    /* component parameters */
    mca_btl_pcie_component.pcie_free_list_num =
        mca_btl_pcie_param_register_int ("free_list_num", 
                                         "Initial size of free lists (must be >= 1)",
                                         16);
    /* BWB - FIX ME - The need to limit the free list max size is an
       artifact of the lack of flow control in the BTL.  Since we're
       already using bounce fragments, it should be possible to make
       this unlimited, and then properly handle the case where an SMA
       region isn't available when send is called on a given frag.
       Something similar to what Open IB does when we don't have send
       credits would work really well here.  See comment in
       btl_pcie_send() for more information. */
    mca_btl_pcie_component.pcie_free_list_max =
        mca_btl_pcie_param_register_int ("free_list_max", 
                                         "Max size of free lists.  "
                                         "free_list_max * (first_frag_size + max_send_size) "
                                         "must be less than (SMA memory size - (recv_queue_len * 4) - 8)",
                                         32);
    mca_btl_pcie_component.pcie_free_list_inc =
        mca_btl_pcie_param_register_int ("free_list_inc", 
                                         "Increment size of free lists (must be >= 1)",
                                         8);

    mca_btl_pcie_component.pcie_send_mpool_name =
        mca_btl_pcie_param_register_string("send_mpool", 
                                           "Name of the memory pool to be used for send messages. "
                                           "(it is unlikely that you will ever want to change this)", 
                                           "pcie"); 

    mca_btl_pcie_component.pcie_dma_mpool_name = 
        mca_btl_pcie_param_register_string("dma_mpool", 
                                           "Name of the memory pool to be used for rdma messages. "
                                           "(it is unlikely that you will ever want to change this)", 
                                           "rdma"); 

    mca_btl_pcie_component.pcie_recv_queue_len = 
        mca_btl_pcie_param_register_int("recv_queue_len", 
                                        "Length of receive fifo.  Must be 4 * free_list_max",
                                        256);

    mca_btl_pcie_module.super.btl_exclusivity =
        mca_btl_pcie_param_register_int ("exclusivity", 
                                         "Priority of PCIe BTL.  (must be > 0)",
                                         MCA_BTL_EXCLUSIVITY_DEFAULT + 1);

    mca_btl_pcie_module.super.btl_eager_limit = 
        mca_btl_pcie_param_register_int ("first_frag_size", 
                                         "Size (in bytes) of the first fragment sent of any "
                                         "message.  It is the maximum size of \"short\" messages "
                                         "and the maximum size of the \"phase 1\" fragment sent "
                                         "for all large messages (must be >= 1).",
                                         1*1024) - sizeof(mca_btl_pcie_header_t);
    mca_btl_pcie_module.super.btl_rndv_eager_limit =
        mca_btl_pcie_param_register_int ("btl_rndv_eager_limit",
                                         "Minimum message size (in bytes) that will be striped "
                                         "across multiple network devices when using "
                                         "send/receive semantics.  Messages shorter than this "
                                         "size will be sent across a single network (must be >= "
                                         "1)",
                                         2*1024) - sizeof(mca_btl_pcie_header_t);
    mca_btl_pcie_module.super.btl_max_send_size =
        mca_btl_pcie_param_register_int ("max_send_size", 
                                         "Maximum size (in bytes) of a single \"phase 2\" fragment "
                                         "of a long message when using the pipeline protocol "
                                         "(must be >= 1)",
                                         4*1024) - sizeof(mca_btl_pcie_header_t);
    mca_btl_pcie_module.super.btl_rdma_pipeline_send_length =
        mca_btl_pcie_param_register_int("rdma_pipeline_send_length",
                                        "Length of the \"phase 2\" portion of a large message (in "
                                        "bytes) when using the pipeline protocol.  This part of "
                                        "the message will be split into fragments of size "
                                        "max_send_size and sent using send/receive semantics "
                                        "(must be >= 0; only relevant when the PUT flag is "
                                        "set)",
                                        12*1024);
    mca_btl_pcie_module.super.btl_rdma_pipeline_frag_size =
        mca_btl_pcie_param_register_int("rdma_pipeline_frag_size",
                                        "Maximum size (in bytes) of a single \"phase 3\" fragment "
                                        "from a long message when using the pipeline protocol.  "
                                        "These fragments will be sent using RDMA semantics "
                                        "(must be >= 1; only relevant when the PUT flag is "
                                        "set)",
                                        2*1024*1024);
    mca_btl_pcie_module.super.btl_min_rdma_pipeline_size =
        mca_btl_pcie_param_register_int("min_rdma_pipeline_size",
                                        "Messages smaller than this size (in bytes) will not "
                                        "use the RDMA pipeline protocol.  Instead, they will be "
                                        "split into fragments of max_send_size and sent using "
                                        "send/receive semantics (must be >=0, and is "
                                        "automatically adjusted up to at least "
                                        "(eager_limit+btl_rdma_pipeline_send_length); only "
                                        "relevant when the PUT flag is set)",
                                        16 * 1024);

    mca_btl_pcie_module.super.btl_flags  = 
        mca_btl_pcie_param_register_int("flags", 
                                        "BTL control flags.  Defaults to (SEND|PUT|HETEROGENEOUS_RDMA)",
#ifdef MCA_BTL_FLAGS_HETEROGENEOUS_RDMA
					MCA_BTL_FLAGS_HETEROGENEOUS_RDMA |
#endif
                                        MCA_BTL_FLAGS_SEND | 
					MCA_BTL_FLAGS_PUT);

    return OMPI_SUCCESS;
}


int
mca_btl_pcie_component_close(void)
{
    return OMPI_SUCCESS;
}


mca_btl_base_module_t**
mca_btl_pcie_component_init(int *num_btl_modules, 
			    bool enable_progress_threads,
			    bool enable_mpi_threads)
{
    cpu_set_t cpu_set;
    unsigned int i;
    int num_cpus, *cpus;
    struct stat stat_buf;
    struct mca_mpool_base_resources_t mpool_resources;
    mca_btl_base_module_t **btl_array;

    *num_btl_modules = 0;
    
    /* find all cpus we're bound to */
    cpus = malloc(CPU_SETSIZE * sizeof(int));
    memset(cpus, 0, CPU_SETSIZE * sizeof(int));
    num_cpus = 0;
    CPU_ZERO(&cpu_set);

    sched_getaffinity(0, sizeof(cpu_set), &cpu_set);
    for (i = 0 ; i < CPU_SETSIZE ; ++i) {
      if (CPU_ISSET(i, &cpu_set)) cpus[num_cpus++] = i;
    }
#if defined(__PPC__)
    if (num_cpus > 1) {
        orte_show_help("help-mpi-btl-pcie.txt", "initialization:more-than-one-cpu",
                       true, num_cpus);
        return NULL;
    }
#endif /* #ifdef __PPC__ */
    if (0 == num_cpus) {
        orte_show_help("help-mpi-btl-pcie.txt", "initialization:no-cpus",
                       true);
        return NULL;
    }

    /* Create the module storage space */
    mca_btl_pcie_component.pcie_num_btls = num_cpus;
    mca_btl_pcie_component.pcie_btls = malloc(mca_btl_pcie_component.pcie_num_btls *
                                              sizeof(struct mca_btl_pcie_module_t));
    btl_array = malloc(mca_btl_pcie_component.pcie_num_btls * 
                       sizeof(mca_btl_base_module_t*));

    /* initialize the modules */
    for (i = 0 ; i < mca_btl_pcie_component.pcie_num_btls ; ++i) {
        mca_btl_pcie_module_t *btl = &(mca_btl_pcie_component.pcie_btls[i]);

        btl_array[i] = (mca_btl_base_module_t*) btl;

        memcpy(btl, &mca_btl_pcie_module, sizeof(mca_btl_pcie_module_t));

        /* check if we have a device listed in our local config file */
        btl->lcl_dev_name = 
            ompi_btl_pcie_cfg_get_local_device(orte_process_info.nodename, cpus[i]);
        BTL_VERBOSE(("Local device for %s:%d = %s", orte_process_info.nodename, cpus[i], 
                     btl->lcl_dev_name));
    
        /* make sure said device is sane */
        if(stat(btl->lcl_dev_name, &stat_buf)) { 
            BTL_ERROR(("Error %s opening device %s\n", strerror(errno), 
                       btl->lcl_dev_name));
            return NULL;
        }

        OBJ_CONSTRUCT(&btl->pcie_sma_buf_eager, ompi_free_list_t);
        OBJ_CONSTRUCT(&btl->pcie_sma_buf_max, ompi_free_list_t);
    
        OBJ_CONSTRUCT(&btl->pcie_frag_eager, ompi_free_list_t);
        OBJ_CONSTRUCT(&btl->pcie_frag_max, ompi_free_list_t);

        OBJ_CONSTRUCT(&btl->pcie_frag_dma, ompi_free_list_t);
    
        OBJ_CONSTRUCT(&btl->pcie_lock, opal_mutex_t);

        /* time to setup DMA mpool */
        mpool_resources.reg_data = (void*) btl;
        mpool_resources.sizeof_reg = sizeof(mca_btl_pcie_reg_t);
        mpool_resources.register_mem = pcie_reg_mr;
        mpool_resources.deregister_mem = pcie_dereg_mr;
        btl->rdma_mpool = 
            mca_mpool_base_module_create("rdma",
                                         &btl->super,
                                         &mpool_resources);
        btl->super.btl_mpool = btl->rdma_mpool;

        btl->active = false;
    }
    
    /* push our address info to everyone */
    btl_pcie_modex_send();

    *num_btl_modules = mca_btl_pcie_component.pcie_num_btls;
    return btl_array;;
}


int
mca_btl_pcie_component_progress()
{
    unsigned int i;
    btl_pcie_fifo_entry_t msg_idx;
    int count = 0;

    for (i = 0 ; i < mca_btl_pcie_component.pcie_num_btls ; ++i) {
        mca_btl_pcie_module_t *pcie_btl = 
            &(mca_btl_pcie_component.pcie_btls[i]);
        mca_btl_base_endpoint_t *endpoint = pcie_btl->endpoint;

        if (!pcie_btl->active) continue;

        msg_idx = ompi_btl_pcie_fifo_get_msg(&endpoint->recv_fifo);

        /* Potential optimization is to drain every time we enter progress */
        if (msg_idx) {
            int rc;
            int ack = ((msg_idx & BTL_PCIE_FIFO_TYPE_MASK) == BTL_PCIE_FIFO_TYPE_ACK) ? 1 : 0;
            msg_idx &= BTL_PCIE_FIFO_DATA_MASK;

            if (ack) {
                /* we have a send frag ack */
                mca_btl_pcie_frag_t *frag = (mca_btl_pcie_frag_t*) msg_idx;
                mca_btl_pcie_sma_buf_t *buf = frag->sma_buf;

                BTL_VERBOSE(("received ack for frag %lx (0x%lx)", (long)msg_idx, (long)frag));

                /* Done with buffer, can return now */
                MCA_BTL_PCIE_SMA_BUF_RETURN(pcie_btl, buf, rc);

                frag->base.des_cbfunc(&pcie_btl->super, endpoint, 
                                      &(frag->base),
                                      OMPI_SUCCESS);
            
                /* return the send credit */
                ompi_btl_pcie_fifo_complete_msg(&endpoint->send_fifo, 1);
                count++;
            } else { 
                /* we have a send frag (incoming data) */
                mca_btl_pcie_frag_t *recv_frag = &pcie_btl->pcie_recv_frag;
                mca_btl_pcie_header_t *hdr = (mca_btl_pcie_header_t*) (endpoint->lcl_frag_base + msg_idx);
                recv_frag->hdr = hdr;
                OMPI_BTL_PCIE_HEADER_NTOH((*recv_frag->hdr));
                recv_frag->segment.seg_addr.pval = ((unsigned char*) recv_frag->hdr) + sizeof(mca_btl_pcie_header_t);
                recv_frag->segment.seg_len = recv_frag->hdr->length;
                BTL_VERBOSE(("received tag %d, base 0x%lx", recv_frag->hdr->tag, (long)&recv_frag->base));
                pcie_btl->pcie_reg[recv_frag->hdr->tag].cbfunc(&pcie_btl->super,
                                                               recv_frag->hdr->tag, &recv_frag->base, 
                                                               pcie_btl->pcie_reg[recv_frag->hdr->tag].cbdata);
            
                rc = ompi_btl_pcie_fifo_set_msg(&endpoint->send_fifo, hdr->send_frag.lval);
                /* BWB - FIX ME - this is only safe if the number of
                   queue entries is twice the free list size */
                ompi_btl_pcie_fifo_complete_msg(&endpoint->send_fifo, 1);
                count++;
            }
        }
    }

    return count;
}


static int
pcie_reg_mr(void *reg_data, void *base, size_t size, 
            mca_mpool_base_registration_t *reg) 
{
    mca_btl_pcie_module_t * pcie_btl = (mca_btl_pcie_module_t*) reg_data;
    mca_btl_pcie_endpoint_t * endpoint = pcie_btl->endpoint;
    mca_btl_pcie_reg_t * pcie_reg = (mca_btl_pcie_reg_t*) reg;

    if(dd_register_memory_region(&endpoint->pcie_adapter, 
                                 &pcie_reg->handle, 
                                 base,
                                 size,
                                 AXON_MR_LOCAL_READ | 
                                 AXON_MR_LOCAL_WRITE |
                                 AXON_MR_REMOTE_ACCESS |
                                 AXON_MR_REMOTE_READ  |
                                 AXON_MR_REMOTE_WRITE )) { 
        BTL_ERROR(("error deregistering memory!\n"));
        return OMPI_ERROR;
    } 

    return OMPI_SUCCESS;
}


static int
pcie_dereg_mr(void* reg_data, mca_mpool_base_registration_t *reg)
{ 
    mca_btl_pcie_module_t * pcie_btl = (mca_btl_pcie_module_t*) reg_data;
    mca_btl_pcie_endpoint_t * endpoint = pcie_btl->endpoint;
    mca_btl_pcie_reg_t * pcie_reg = (mca_btl_pcie_reg_t*) reg;
    
    if(pcie_reg->handle >= 0) { 
        if(dd_deregister_memory_region(&endpoint->pcie_adapter,
                                       &pcie_reg->handle)) { 
            BTL_ERROR(("error deregistering memory!\n"));
            return OMPI_ERROR;
        }
    } else { 
        return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
}
