/*
 * Copyright (c) 2004-2009 The University of Tennessee and The Univer
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"
#include <string.h>
#include "orte/util/show_help.h"
#include "opal/util/if.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/btl/btl.h"

#include "btl_sicortex.h"
#include "btl_sicortex_frag.h" 
#include "btl_sicortex_proc.h"
#include "btl_sicortex_endpoint.h"
#include "ompi/datatype/convertor.h" 
#include "ompi/mca/mpool/base/base.h" 
#include "ompi/mca/mpool/mpool.h" 

#include <unistd.h>
#include <linux/sicortex/scdma_hw.h>
#include <linux/sicortex/scdma.h>
#include <assert.h> 


static int mca_btl_sicortex_add_procs( struct mca_btl_base_module_t* btl, 
                                       size_t nprocs, 
                                       struct ompi_proc_t **ompi_procs, 
                                       struct mca_btl_base_endpoint_t** peers, 
                                       opal_bitmap_t* reachable )
{
    mca_btl_sicortex_module_t* sicortex_btl = (mca_btl_sicortex_module_t*)btl;
    int i,rc,index = 0;

    sicortex_btl->peers = (mca_btl_base_endpoint_t**)malloc(sizeof(mca_btl_base_endpoint_t*)*(nprocs-1));
    for(i = 0; i < (int) nprocs; i++) {

        struct ompi_proc_t* ompi_proc = ompi_procs[i];
        mca_btl_sicortex_proc_t* sicortex_proc;
        mca_btl_base_endpoint_t* sicortex_endpoint;

	if(ompi_proc_local_proc == ompi_proc)
            continue;

        if(NULL == (sicortex_proc = mca_btl_sicortex_proc_create(ompi_proc))) {
            return OMPI_ERR_OUT_OF_RESOURCE;
        }

        /*
         * Check to make sure that the peer has at least as many interface 
         * addresses exported as we are trying to use. If not, then 
         * don't bind this PTL instance to the proc.
         */

        OPAL_THREAD_LOCK(&sicortex_proc->proc_lock);

        /* The btl_proc datastructure is shared by all SICORTEX PTL
         * instances that are trying to reach this destination. 
         * Cache the peer instance on the btl_proc.
         */
        sicortex_endpoint = OBJ_NEW(mca_btl_sicortex_endpoint_t);
        if(NULL == sicortex_endpoint) {
            OPAL_THREAD_UNLOCK(&sicortex_proc->proc_lock);
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
        sicortex_endpoint->route = (peer_struct_t*)malloc(sizeof(peer_struct_t));
        sicortex_endpoint->endpoint_btl = sicortex_btl;
        rc = mca_btl_sicortex_proc_insert(sicortex_proc, sicortex_endpoint);
        if(rc != OMPI_SUCCESS) {
            OBJ_RELEASE(sicortex_endpoint);
            OPAL_THREAD_UNLOCK(&sicortex_proc->proc_lock);
            continue;
        }
        rc = scdma_set_route(sicortex_btl->ctx,
			     sicortex_endpoint->route->route_handles[0],
			     sicortex_endpoint->route->route_handles[1],
			     sicortex_endpoint->route->route_handles[2],
			     sicortex_endpoint->route->context_id,
			     &(sicortex_endpoint->route->ports[0]),
			     &(sicortex_endpoint->route->ports[1]),
			     &(sicortex_endpoint->route->ports[2]));
	if(rc != 0) {
            OBJ_RELEASE(sicortex_endpoint);
            OPAL_THREAD_UNLOCK(&sicortex_proc->proc_lock);
            continue;
	}
        opal_bitmap_set_bit(reachable, i);
        OPAL_THREAD_UNLOCK(&sicortex_proc->proc_lock);
        sicortex_btl->peers[index++] = peers[i] = sicortex_endpoint;
    }

    sicortex_btl->bds_size  = sicortex_btl->ctx->bdt_size;
    sicortex_btl->bds_limit = sicortex_btl->ctx->num_bdt_entries;
    sicortex_btl->bds_head  = 0;

    return OMPI_SUCCESS;
}

static int
mca_btl_sicortex_del_procs(struct mca_btl_base_module_t* btl, 
                           size_t nprocs, 
                           struct ompi_proc_t **procs, 
                           struct mca_btl_base_endpoint_t ** peers)
{
    /* TODO */
    return OMPI_SUCCESS;
}

/**
 * Allocate a segment.
 *
 * @param btl (IN)      BTL module
 * @param size (IN)     Request segment size.
 */

static mca_btl_base_descriptor_t*
mca_btl_sicortex_alloc( struct mca_btl_base_module_t* btl,
			struct mca_btl_base_endpoint_t* endpoint,
			uint8_t order,
			size_t size,
			uint32_t flags )
{
    mca_btl_sicortex_frag_t* frag = NULL;
    int rc;

    if(size <= btl->btl_eager_limit) {
        MCA_BTL_SICORTEX_FRAG_ALLOC_EAGER(sicortex_btl, frag, rc);
    } else if( size <=  btl->btl_max_send_size ) {
        MCA_BTL_SICORTEX_FRAG_ALLOC_MAX(sicortex_btl, frag, rc);
    }
    if(OPAL_UNLIKELY(NULL == frag)) {
      return NULL;
    }
    frag->segment.seg_len       = size;
    frag->segment.seg_addr.pval = (void*)((char*)(frag + 1));
    frag->segment.seg_len       = size;
    frag->base.des_src          = &frag->segment;
    frag->base.des_src_cnt      = 1;
    frag->base.des_flags        = flags;
    frag->base.order            = MCA_BTL_NO_ORDER;
    return (mca_btl_base_descriptor_t*)frag;
}


/**
 * Return a segment
 */

static int
mca_btl_sicortex_free( struct mca_btl_base_module_t* btl, 
                       mca_btl_base_descriptor_t* des )
{
    mca_btl_sicortex_frag_t* frag = (mca_btl_sicortex_frag_t*)des; 
    MCA_BTL_SICORTEX_FRAG_RETURN(btl, frag);
    return OMPI_SUCCESS;
}

/**
 * Initiate an immediate blocking send. 
 * Completion Semantics: the BTL will make a best effort 
 *  to send the header and "size" bytes from the datatype using the convertor. 
 *  The header is guaranteed to be delivered entirely in the first segment. 
 *  Should the BTL be unable to deliver the data due to resource constraints 
 *  the BTL will return a descriptor (via the OUT param) 
 *  of size "payload_size + header_size".
 *
 * @param btl (IN)             BTL module
 * @param endpoint (IN)        BTL addressing information
 * @param convertor (IN)       Data type convertor
 * @param header (IN)          Pointer to header.
 * @param header_size (IN)     Size of header. * @param payload_size (IN)    Size of payload (from convertor).
 * @param order (IN)           The ordering tag (may be MCA_BTL_NO_ORDER)
 * @param flags (IN)           Flags.
 * @param tag (IN)             The tag value used to notify the peer.
 * @param descriptor (OUT)     The descriptor to be returned unable to be sent immediately
 */
static int mca_btl_sicortex_sendi( struct mca_btl_base_module_t* btl,
                                   struct mca_btl_base_endpoint_t* endpoint,
                                   struct ompi_convertor_t* convertor,
                                   void* header,
                                   size_t header_size,
                                   size_t payload_size,
                                   uint8_t order,
                                   uint32_t flags,
                                   mca_btl_base_tag_t tag,
                                   mca_btl_base_descriptor_t** descriptor )
{
    mca_btl_sicortex_module_t* sicortex_btl = (mca_btl_sicortex_module_t*) btl;
    size_t max_data, length = header_size + payload_size;
    struct iovec iov;
    uint32_t iov_count = 1, length_on_wire;
    scdma_hw_cmd_t* cmd;
    char* payload;

    if( length > SCDMA_HW_CMD_PAYLOAD_MAX_SIZE ) {
        *descriptor = mca_btl_sicortex_alloc( btl, endpoint, order, length, flags );
        return OMPI_ERR_RESOURCE_BUSY;
    }

    /* Adjust the length on the wire to allow the DMA engine to
     * know exactly how many cache lines have to be transfered.
     */
    length_on_wire = scdma_send_event_len(length + 2 * sizeof(uint64_t));
    cmd = (scdma_hw_cmd_t *)scdma_cq_head_spinwait(sicortex_btl->ctx);
    cmd->header = scdma_cmd_header(SCDMA_HW_CMD_TYPE_SEND_EVENT,
                                   length_on_wire,
                                   endpoint->route->route_handles[0/*sicortex_btl->handle_count*/],
                                   endpoint->route->ports[0/*sicortex_btl->handle_count*/]);
    /*sicortex_btl->handle_count = (sicortex_btl->handle_count + 1) % SICORTEX_PORTS_NUM;*/
    cmd->payload[0] = (((uint64_t)tag << 16) | (EVENT_SEND_IM << 8)
                       | (length + 2 * sizeof(uint64_t)) );
    payload = (char*)&cmd->payload[1];
    memcpy( payload, header, header_size );

    iov.iov_base = payload + header_size;
    iov.iov_len = payload_size;
    max_data = payload_size;

    if( 0 != payload_size ) {
        ompi_convertor_pack( convertor, &iov, &iov_count, &max_data );
        assert( max_data == payload_size );
    }

    if( length > (SCDMA_HW_CMD_PAYLOAD_MAX_SIZE-64) )
        cmd->payload[15] = 0;
    scdma_cq_post_fastpath(sicortex_btl->ctx,cmd->header);

    return OMPI_SUCCESS;
}

/**
 * Initiate an asynchronous send.
 *
 * @param btl (IN)         BTL module
 * @param endpoint (IN)    BTL addressing information
 * @param descriptor (IN)  Description of the data to be transfered
 * @param tag (IN)         The tag value used to notify the peer.
 */

static int
mca_btl_sicortex_send( struct mca_btl_base_module_t* btl,
		       struct mca_btl_base_endpoint_t* endpoint,
		       struct mca_btl_base_descriptor_t* descriptor, 
		       mca_btl_base_tag_t tag )
{
    mca_btl_sicortex_module_t* sicortex_btl = (mca_btl_sicortex_module_t*) btl; 
    mca_btl_sicortex_frag_t* frag = (mca_btl_sicortex_frag_t*)descriptor; 
    int btl_ownership = (frag->base.des_flags & MCA_BTL_DES_FLAGS_BTL_OWNERSHIP );
    size_t length = frag->segment.seg_len;
    int offset, len, length_on_wire;
    scdma_hw_cmd_t *cmd;
    scdma_hw_put_im_heap_cmd_t *pb;

    length_on_wire = length + 2 * sizeof(uint64_t);
    frag->endpoint = endpoint;
    
    if( SCDMA_HW_CMD_PAYLOAD_MAX_SIZE < length_on_wire ) {
	offset = 0;
	do {
            if( length_on_wire > 16*8 )
                len = 8*8;/*  SCDMA_HW_CMD_PAYLOAD_MAX_SIZE;*/
            else
                len = length_on_wire - 2 * 8;
            cmd = (scdma_hw_cmd_t *)scdma_cq_head_spinwait(sicortex_btl->ctx);
	    pb = (scdma_hw_put_im_heap_cmd_t *)cmd;
            pb->header = scdma_cmd_header(SCDMA_HW_CMD_TYPE_PUT_IM_HP,
                                          (len + 0x17) & ~0x7, /* 2 * 64 bits + len */
                                          endpoint->route->route_handles[0/*sicortex_btl->handle_count*/],
                                          endpoint->route->ports[0/*sicortex_btl->handle_count*/]);
            pb->count_offset = (SCDMA_USABLE_HEAP_OFFSET + offset);
            memcpy( scdma_cmd_payload(cmd), (char*)frag->base.des_src->seg_addr.pval + offset, len);
            scdma_cq_post_fastpath(sicortex_btl->ctx, pb->header);
            offset += len;
            length_on_wire -= len;
	} while(length_on_wire > 2*8);
	cmd = (scdma_hw_cmd_t *)scdma_cq_head_spinwait(sicortex_btl->ctx);
	cmd->header = scdma_cmd_header(SCDMA_HW_CMD_TYPE_SEND_EVENT,
				       0x10,
				       endpoint->route->route_handles[0/*sicortex_btl->handle_count*/],
				       endpoint->route->ports[0/*sicortex_btl->handle_count*/]);
	cmd->payload[0] = (((uint64_t)length << 32) | ((uint64_t)tag << 16) |
                           (EVENT_SEND_IM_PUT_POST << 8) | 0x10);
	cmd->payload[1] = 0;
        /* Next operation will take another path. */
        /*sicortex_btl->handle_count = (sicortex_btl->handle_count + 1) % SICORTEX_PORTS_NUM;*/
    } else {

    	/* Adjust the length on the wire to allow the DMA engine to
         * know exactly how many cache lines have to be transfered.
         */
    	length_on_wire = scdma_send_event_len(length + 2 * sizeof(uint64_t));
    	cmd = (scdma_hw_cmd_t *)scdma_cq_head_spinwait(sicortex_btl->ctx);
    	cmd->header = scdma_cmd_header(SCDMA_HW_CMD_TYPE_SEND_EVENT,
                                       length_on_wire,
                                       endpoint->route->route_handles[0/*sicortex_btl->handle_count*/],
                                       endpoint->route->ports[0/*sicortex_btl->handle_count*/]);
    	cmd->payload[0] = (((uint64_t)tag << 16) | (EVENT_SEND_IM << 8)
                           | (length + 2 * sizeof(uint64_t)) );
    	memcpy( &cmd->payload[1], frag->base.des_src->seg_addr.pval, length );

    	cmd->payload[15] = 0;
    }

    scdma_cq_post_fastpath(sicortex_btl->ctx,cmd->header);

    if( MCA_BTL_DES_SEND_ALWAYS_CALLBACK & frag->base.des_flags ) {
        frag->base.des_cbfunc( &(sicortex_btl->super), frag->endpoint,
			       &(frag->base), OMPI_SUCCESS);
    }
    if( btl_ownership ){
        MCA_BTL_SICORTEX_FRAG_RETURN(sicortex_btl, frag);
    }
    return 1;
}

#define MCA_BTL_SICORTEX_PACK_BD(v64bits, offset, overhead, bd_index, num_entries) \
    do {                                                                \
        v64bits = (((uint64_t)(offset) << 48) | ((uint64_t)(overhead) << 32) | \
                   (((uint64_t)(bd_index) << 16) | (uint64_t)(num_entries))); \
    } while (0)

#define MCA_BTL_SICORTEX_UNPACK_BD(v64bits, offset, overhead, bd_index, num_entries) \
    do {                                                                \
        (num_entries) = ((v64bits) & 0xFFFF);                           \
        (bd_index)    = (((v64bits) >> 16) & 0xFFFF);                   \
        (overhead)    = (((v64bits) >> 32) & 0xFFFF);                   \
        (offset)      = (((v64bits) >> 48) & 0xFFFF);                   \
    } while (0)

static inline void
mca_btl_sicortex_map_bds( mca_btl_sicortex_module_t* sicortex_btl,
                          void* base_addr,
                          uint16_t offset,
                          size_t* size,
                          uint16_t* bd_start,
                          uint16_t* num_entries )
{
    OPAL_THREAD_LOCK( &sicortex_btl->sicortex_lock);
    *bd_start = sicortex_btl->bds_head;
    *num_entries = 1 + (offset + *size) / sicortex_btl->bds_size;
    sicortex_btl->bds_head += *num_entries;
    if( sicortex_btl->bds_head >= sicortex_btl->bds_limit ) {
        sicortex_btl->bds_head = 0;  /* rollback at the begining */
        *num_entries = sicortex_btl->bds_limit - *bd_start;
        *size = (*size % sicortex_btl->bds_size) + (*num_entries - 1) * sicortex_btl->bds_size;
    }
    OPAL_THREAD_UNLOCK( &sicortex_btl->sicortex_lock);
    /*opal_output(0, "map base_addr %p size %ld [bd_start %d: num_entries %d]",
      base_addr, (unsigned long)*size, *bd_start, *num_entries );*/
}

/**
 * Pack data and return a descriptor that can be
 * used for send/put.
 *
 * @param btl (IN)      BTL module
 * @param peer (IN)     BTL peer addressing
 */
static mca_btl_base_descriptor_t*
mca_btl_sicortex_prepare_src( struct mca_btl_base_module_t* btl,
                              struct mca_btl_base_endpoint_t* endpoint,
                              struct mca_mpool_base_registration_t* registration,
                              struct ompi_convertor_t* convertor,
                              uint8_t order,
                              size_t reserve,
                              size_t* size,
                              uint32_t flags )
{ 
    mca_btl_sicortex_module_t *sicortex_btl = (mca_btl_sicortex_module_t *)btl;
    mca_btl_sicortex_frag_t* frag;
    struct iovec iov;
    uint32_t iov_count = 1;
    size_t max_data = *size;
    int rc;

    if( 0 != reserve ) {
        if( max_data + reserve <= btl->btl_eager_limit ) {
            MCA_BTL_SICORTEX_FRAG_ALLOC_EAGER(sicortex_btl,frag, rc);
        } else {
            MCA_BTL_SICORTEX_FRAG_ALLOC_MAX(sicortex_btl,frag, rc);
            if( (max_data + reserve) > btl->btl_max_send_size ) {
                max_data = btl->btl_max_send_size - reserve;
            }
        }
        if(OPAL_UNLIKELY(NULL == frag)) {
            return NULL;
        }
        frag->segment.seg_addr.pval = (void*)((unsigned char*)(frag + 1));
        iov.iov_len = max_data;
        iov.iov_base = (unsigned char*)frag->segment.seg_addr.pval + reserve;
        rc = ompi_convertor_pack(convertor, &iov, &iov_count, &max_data );                
        *size  = max_data;
        if( rc < 0 ) {
            MCA_BTL_SICORTEX_FRAG_RETURN(sicortex_btl, frag);
            return NULL;
        }
        frag->segment.seg_len = max_data + reserve;
    } else {  /* this is a real RDMA operation */
        uint16_t bd_index, num_entries, offset;
        void* base_addr;

        MCA_BTL_SICORTEX_FRAG_ALLOC_USER(sicortex_btl,frag, rc);
        if(NULL == frag) {
            return NULL;
        }
        /**
         * The best we can do is to decrease the number of bd we will have to map and
         * to decrease the number of over 64K segments we have to transfer. Therefore,
         * we will limit the amount of data to be sent to whatever is on the first 64K
         * fragment plus a multiple of 64K.
         * We know that the offset and the overhead will be 16 bits each. Similarly,
         * the bd_index and num_entries are smaller than 16 bits each, so we can
         * pack all required information in 64 bits.
         */
        ompi_convertor_get_current_pointer( convertor, (void**)&(frag->segment.seg_addr.pval) );
        base_addr = (void*)(frag->segment.seg_addr.lval & ~((uintptr_t)0xFFFF));
        offset = frag->segment.seg_addr.lval & (uintptr_t)0xffff;

        mca_btl_sicortex_map_bds( sicortex_btl, base_addr, offset,
                                  size, &bd_index, &num_entries);
        assert( *size != 0 );
        iov.iov_base = NULL;
        iov.iov_len = *size;
        ompi_convertor_pack(convertor, &iov, &iov_count, &max_data);
        *size = max_data;
        assert( 0 != iov_count );
        frag->segment.seg_addr.pval = iov.iov_base;
        frag->segment.seg_len = max_data;
        assert( frag->segment.seg_len != 0 );
        assert( frag->segment.seg_addr.pval != NULL );
	
	rc = scdma_map_bds(sicortex_btl->ctx, bd_index,
                           base_addr, num_entries);
	
	frag->base.des_src = &(frag->segment);
        MCA_BTL_SICORTEX_PACK_BD( frag->base.des_src->seg_key.key64,
                                  offset, 0, bd_index, num_entries );
        /*opal_output(0, "prepare_src index %d offset %d (num_entries %d) length %d [base %p addr %p]",
          bd_index, offset, num_entries, (int)max_data, base_addr, (void*)iov.iov_base );*/
    }

    frag->base.des_src = &(frag->segment);
    frag->base.des_src_cnt = 1;
    frag->base.order = MCA_BTL_NO_ORDER;
    frag->base.des_dst = NULL;
    frag->base.des_dst_cnt = 0;
    frag->base.des_flags = flags;
    return &frag->base;
}

/**
 * Prepare a descriptor for send/rdma using the supplied
 * convertor. If the convertor references data that is contigous,
 * the descriptor may simply point to the user buffer. Otherwise,
 * this routine is responsible for allocating buffer space and
 * packing if required.
 *
 * @param btl (IN)          BTL module
 * @param endpoint (IN)     BTL peer addressing
 * @param convertor (IN)    Data type convertor
 * @param reserve (IN)      Additional bytes requested by upper layer to precede user data
 * @param size (IN/OUT)     Number of bytes to prepare (IN), number of bytes actually prepared (OUT)
 */

static mca_btl_base_descriptor_t*
mca_btl_sicortex_prepare_dst( struct mca_btl_base_module_t* btl,
                              struct mca_btl_base_endpoint_t* endpoint,
                              struct mca_mpool_base_registration_t* registration,
                              struct ompi_convertor_t* convertor,
                              uint8_t order,
                              size_t reserve,
                              size_t* size,
                              uint32_t flags)
{
    mca_btl_sicortex_module_t* sicortex_btl = (mca_btl_sicortex_module_t*) btl;
    mca_btl_sicortex_frag_t* frag;
    uint16_t bd_index, offset, num_entries;
    int rc, length;
    void* base_addr;

    MCA_BTL_SICORTEX_FRAG_ALLOC_USER(btl, frag, rc);
    if(OPAL_UNLIKELY(NULL == frag)) {
        return NULL;
    }

    /* Ignore all about alignments. We will cope with these on the _put function */
    ompi_convertor_get_current_pointer( convertor, (void**)&(frag->segment.seg_addr.pval) );
    offset = (frag->segment.seg_addr.lval & (uintptr_t)0xffff);
    base_addr = (void*)(frag->segment.seg_addr.lval & ~((uintptr_t)0xffff));

    mca_btl_sicortex_map_bds( sicortex_btl, base_addr, offset,
                              size, &bd_index, &num_entries );

    frag->segment.seg_len = *size;
    length = *size;

    rc = scdma_map_bds(sicortex_btl->ctx, bd_index,
                       base_addr, num_entries);

    frag->base.des_src = NULL;
    frag->base.des_src_cnt = 0;
    frag->base.des_dst = &frag->segment;

    MCA_BTL_SICORTEX_PACK_BD(frag->base.des_dst->seg_key.key64,
                             offset, 0, bd_index, num_entries );
    /*opal_output(0, "prepare_dst index %d offset %d (num_entries %d) length %d [base %p addr %p]",
      bd_index, offset, num_entries, length, base_addr, (void*)frag->segment.seg_addr.pval);*/
    frag->base.des_dst_cnt = 1;
    frag->base.des_flags = flags;
    return &frag->base;
}

#define MCA_BTL_SICORTEX_NEXT_BDS(val)                  \
    do {                                                \
        if( ++(val) >= sicortex_btl->bds_limit )        \
            (val) = 0;                                  \
    } while(0)

/**
 * Initiate an asynchronous put.
 *
 * @param btl (IN)         BTL module
 * @param endpoint (IN)    BTL addressing information
 * @param descriptor (IN)  Description of the data to be transferred
 */
static int
mca_btl_sicortex_put( mca_btl_base_module_t* btl,
                      mca_btl_base_endpoint_t* endpoint,
                      mca_btl_base_descriptor_t* des )
{
    mca_btl_sicortex_module_t* sicortex_btl = (mca_btl_sicortex_module_t*) btl;
    mca_btl_sicortex_frag_t* frag = (mca_btl_sicortex_frag_t*) des; 
    scdma_hw_put_bf_bf_cmd_t *pb;
    uint32_t dst_offset, dst_overhead, src_offset, src_overhead;
    uint32_t dst_bd_index, dst_num_entries, src_bd_index, src_num_entries;
    uint32_t total_length = des->des_src->seg_len, seg_length, align;
    uintptr_t dst_ptr, src_ptr;

    frag->endpoint = endpoint;
    frag->count = 0;

    MCA_BTL_SICORTEX_UNPACK_BD( des->des_src->seg_key.key64, src_offset, src_overhead,
                                src_bd_index, src_num_entries );
    MCA_BTL_SICORTEX_UNPACK_BD( des->des_dst->seg_key.key64, dst_offset, dst_overhead,
                                dst_bd_index, dst_num_entries );
    src_ptr = des->des_src->seg_addr.lval;
    dst_ptr = des->des_dst->seg_addr.lval;
    /*opal_output(0, "src key %llx (src_ptr %llx) dst key %llx (dst_ptr %llx)",
      (unsigned long long)des->des_src->seg_key.key64, (unsigned long long)src_ptr,
      (unsigned long long)des->des_dst->seg_key.key64, (unsigned long long)dst_ptr );*/

    /* Let's check for the conditions imposed by the hardware:
     * - If the destination buffer does not start and end at cache block (32-byte)
     *   boundaries, software must use another mechanism (probably built upon
     *   Send Event) to deliver the data which belongs in partial blocks.
     * - If the destination buffer does not start at a 64-byte boundary, the
     *   transfer will make most efficient use of the memory bandwidth if the
     *   library uses a short segment to achieve 64-byte alignment for the
     *   bulk of the transfer.
     * - If the source and destination buffers  do not have the same alignment,
     *   the starting offset in the source buffer should be specified to align the
     *   first packet to a 64-byte boundary at the destination.
     * - If the source and destination buffer alignments differ by an amount which
     *   is not a multiple of 4, the alignment must be adjusted by a software copy
     *   before or after the transfer.
     */
    if( (align = (src_ptr ^ dst_ptr) & 0x3) ) {
        opal_output(0, "Impossible to do a direct PUT of %d bytes. Data will have to be moved around by %d bytes (src_ptr %p dst_ptr %p)",
                    total_length, (int)((src_ptr | dst_ptr) & 0x3), (void*)src_ptr, (void*)dst_ptr);
        align = dst_ptr & 0x3;
        src_ptr = ((src_ptr + 4) & ~0x3) | align;
        align = des->des_src->seg_addr.lval - src_ptr;
        total_length -= align;
        /*return OMPI_ERR_OUT_OF_RESOURCE;*/
    }
    /* Send a small amount of data in such a way that the destination PUT will
     * be aligned on the cache boundaries (64 bytes).
     */
    if( (align = (0x20 - (dst_ptr & 0x1F))) ) {
        uint32_t length_on_wire = scdma_send_event_len(align + 3 * sizeof(uint64_t));
        scdma_hw_cmd_t* cmd = (scdma_hw_cmd_t *)scdma_cq_head_spinwait(sicortex_btl->ctx);

        cmd->header = scdma_cmd_header(SCDMA_HW_CMD_TYPE_SEND_EVENT,
                                       length_on_wire,
                                       endpoint->route->route_handles[sicortex_btl->handle_count],
                                       endpoint->route->ports[sicortex_btl->handle_count]);
        cmd->payload[0] = (((uint64_t)0 << 16) | (EVENT_SEND_PUT_ALIGN << 8)
                           | (align + 3 * sizeof(uint64_t)) );
        cmd->payload[1] = dst_ptr;
        memcpy( &(cmd->payload[2]), (void*)src_ptr, align );
        cmd->payload[15] = 0;
        scdma_cq_post_fastpath(sicortex_btl->ctx, cmd->header);
        src_ptr += align;
        dst_ptr += align;
        total_length -= align;
        dst_offset += align;
        /*opal_output(0, "Align the begining of the destination buffer by %d (dst_ptr %llx dst_offset %d)\n",
          align, (unsigned long long)dst_ptr, dst_offset );*/
        if( dst_offset >= (64*1024) ) {
            assert( 0 == (dst_offset % (64*1024)) );
            dst_offset = 0;
            MCA_BTL_SICORTEX_NEXT_BDS(dst_bd_index);
            dst_num_entries--;
        }
        src_offset += align;
        if( src_offset >= (64*1024) ) {
            src_offset %= (64*1024);
            MCA_BTL_SICORTEX_NEXT_BDS(src_bd_index);
            src_num_entries--;
        }
    }
    /* Send a small amount of data to make sure that the last PUT will
     * stop on a cache boundary alignment (64 bytes).
     */
    if( (align = (total_length & 0x1F)) ) {
        uint32_t length_on_wire = scdma_send_event_len(align + 3 * sizeof(uint64_t));
        scdma_hw_cmd_t* cmd = (scdma_hw_cmd_t *)scdma_cq_head_spinwait(sicortex_btl->ctx);

        cmd->header = scdma_cmd_header(SCDMA_HW_CMD_TYPE_SEND_EVENT,
                                       length_on_wire,
                                       endpoint->route->route_handles[sicortex_btl->handle_count],
                                       endpoint->route->ports[sicortex_btl->handle_count]);
        cmd->payload[0] = (((uint64_t)0 << 16) | (EVENT_SEND_PUT_ALIGN << 8)
                           | (align + 3 * sizeof(uint64_t)) );
        cmd->payload[1] = dst_ptr + total_length - align;
        memcpy( &(cmd->payload[2]), (void*)(src_ptr + total_length - align), align );
        cmd->payload[15] = 0;
        scdma_cq_post_fastpath(sicortex_btl->ctx, cmd->header);
        total_length -= align;
        /*opal_output(0, "Align the end of the destination buffer by %d (dst_ptr %llx)\n",
          align, (unsigned long long)(dst_ptr + total_length) );*/
    }

    /* We can use overlapping on the remote side, but not locally. Therefore, we will
     * compute the size of the first chunck, which will allow us to align locally on
     * the 64K boundary. Then we will put 64K by 64K, except for the last put,
     * where we will have to recompute the overhead.
     */
    seg_length = (64 * 1024) - dst_offset;
    do {
        if( seg_length > total_length ) {
            seg_length = total_length;
        }

        frag->count++;  /* count how many puts per fragment */
        /* Prepare the PUT order */
        pb = (scdma_hw_put_bf_bf_cmd_t*)scdma_cq_head_spinwait(sicortex_btl->ctx);
        scdma_build_put_bf_bf_cmd( (void*)pb,
                                   endpoint->route->route_handles[sicortex_btl->handle_count],
                                   endpoint->route->ports[sicortex_btl->handle_count],
                                   src_bd_index, src_offset,
                                   dst_bd_index, dst_offset,
                                   seg_length, 0, (uintptr_t)frag );
        scdma_cq_post(sicortex_btl->ctx);

        /*opal_output(0, "put src(index %d:offset %d) dst(index %d:offset %d) length %d route %d",
          src_bd_index, src_offset, dst_bd_index, dst_offset, seg_length,
          sicortex_btl->handle_count);*/

        /* Update local informations: the route and the count */
        sicortex_btl->handle_count = (sicortex_btl->handle_count + 1) % SICORTEX_PORTS_NUM;

        /* Now let's update the values */
        src_offset = src_offset + seg_length;
        if( src_offset >= (64 * 1024) ) {
            src_offset = src_offset % (64 * 1024);
            MCA_BTL_SICORTEX_NEXT_BDS(src_bd_index);
        }
        dst_offset = 0;  /* We're now indexed by destination pages */
        MCA_BTL_SICORTEX_NEXT_BDS(dst_bd_index);
        total_length -= seg_length;  /* Update the remaining length */
        seg_length = 64 * 1024;  /* We're dealing now with full pages */
    } while (total_length != 0);

    return OMPI_SUCCESS; 
}


/**
 * Initiate an asynchronous get.
 *
 * @param btl (IN)         BTL module
 * @param endpoint (IN)    BTL addressing information
 * @param descriptor (IN)  Description of the data to be transferre 
 *
 */
#if 0
static int
mca_btl_sicortex_get( mca_btl_base_module_t* btl,
                      mca_btl_base_endpoint_t* endpoint,
                      mca_btl_base_descriptor_t* des)
{
    mca_btl_sicortex_module_t* sicortex_btl = (mca_btl_sicortex_module_t*) btl;
    mca_btl_sicortex_frag_t* frag = (mca_btl_sicortex_frag_t*) des;
    void * cmd;
    mca_btl_base_segment_t *src = des->des_src;
    mca_btl_base_segment_t *dst = des->des_dst;
    int seg_length = src->seg_len;
    uint32_t tx_bd_index =(uint32_t)((src->seg_key.key64 >> 48) &0xffff);/* src->seg_key.key32[0];*/
    uint32_t tx_bd_offset =(uint32_t)((src->seg_key.key64 >> 32) &0xffff); /*src->seg_key.key32[1];*/
    uint32_t remote_route_handle = (uint32_t)(src->seg_key.key64 &0xffffffff);
    uint32_t rx_begin_bd_index =(uint32_t)((dst->seg_key.key64 >> 48) &0xffff);
    uint32_t rx_begin_bd_offset=(uint32_t) ((dst->seg_key.key64 >> 32) & 0xffff);
    uint32_t rx_end_bd_index = (uint32_t)((dst->seg_key.key64 >>16) &0xffff);
    uint32_t rx_end_bd_offset = (uint32_t)(dst->seg_key.key64 & 0xffff);
    uint64_t sw_bucket = (uintptr_t)frag;
    int length = rx_begin_bd_offset + seg_length;
    uint32_t counter = rx_begin_bd_index;
    int rx_bd_offset;
    int tx_bd_index_now;
    int tx_bd_index_next = tx_bd_index;
    int tx_bd_offset_next = tx_bd_offset;
    int tx_bd_offset_now;
    sw_bucket = sw_bucket | 0x1;
   /* opal_output(0,"sender %llx tx_bd_index %d\t tx_bd_offset %d\trx_begin_bd_index %d\t rx_begin_bd_offset %d\trx_end_bd_index%d\trx_end_bd_offset%d\t ",sw_bucket, tx_bd_index,tx_bd_offset,rx_begin_bd_index,rx_begin_bd_offset,rx_end_bd_index,rx_end_bd_offset);*/
    frag->endpoint = endpoint;
    frag->count = 0;
    frag->bds_index = tx_bd_index;
    frag->remote_bds_start= rx_begin_bd_offset;
    do {
        frag->count++;
        rx_bd_offset = 0;
        tx_bd_offset_now = tx_bd_offset_next;
        tx_bd_index_now = tx_bd_index_next;
        if(counter == rx_begin_bd_index) {
            rx_bd_offset = rx_begin_bd_offset;
            if(length > BD_SIZE) {
                seg_length = BD_SIZE - rx_begin_bd_offset;
                length -= BD_SIZE;
            } else {
                seg_length = seg_length;
                length =0;
            }
        } else if(counter == rx_end_bd_index) {
            seg_length = rx_end_bd_offset;
            length = 0;
        } else if(counter> rx_begin_bd_index && counter< rx_end_bd_index) {
            seg_length = BD_SIZE;
            length -= BD_SIZE;
        } else {
            opal_output(0,"hahaha, u r wrong!!!");
        }
        if( tx_bd_offset_now + seg_length >= BD_SIZE )
            tx_bd_index_next = tx_bd_index_now+1;
        tx_bd_offset_next = (tx_bd_offset_now + seg_length) %(BD_SIZE);
    
        cmd = (void*)scdma_cq_head_spinwait(sicortex_btl->ctx);
        scdma_build_send_put_bf_bf_cmd(cmd,
                                       endpoint->route->route_handles[sicortex_btl->handle_count],
                                       endpoint->route->ports[sicortex_btl->handle_count],
                                       remote_route_handle,
                                       tx_bd_index_now,
                                       tx_bd_offset_now,
                                       counter,
                                       rx_bd_offset,
                                       seg_length,
                                       0,
                                       sw_bucket);
        sicortex_btl->handle_count = (sicortex_btl->handle_count + 1) % SICORTEX_PORTS_NUM;
        scdma_cq_post(sicortex_btl->ctx);
    } while( counter++ != rx_end_bd_index );

    return OMPI_SUCCESS;
}
#endif
/*
 * Cleanup/release module resources.
 */

static int mca_btl_sicortex_finalize(struct mca_btl_base_module_t* btl)
{
    mca_btl_sicortex_module_t* sicortex_btl = (mca_btl_sicortex_module_t*) btl; 

    OBJ_DESTRUCT(&sicortex_btl->sicortex_lock);
    free(sicortex_btl);

    return OMPI_SUCCESS;
}

static int mca_btl_sicortex_ft_event(int state)
{
    if(OPAL_CRS_CHECKPOINT == state) {
        ;
    }
    else if(OPAL_CRS_CONTINUE == state) {
        ;
    }
    else if(OPAL_CRS_RESTART == state) {
        ;
    }
    else if(OPAL_CRS_TERM == state ) {
        ;
    }
    else {
        ;
    }

    return OMPI_SUCCESS;
}

mca_btl_sicortex_module_t mca_btl_sicortex_module = {
    {
        &mca_btl_sicortex_component.super,
        0, /* max size of first fragment */
        0, /* min send fragment size*/
        0, /* max send fragment size */
        0, /* rdma pipeline offset */
        0, /* rdma pipeline frag size */
        0, /* min rdma pipeline size */
        0, /* exclusivity */
        0, /* latency */
        0, /* bandwidth */
        0, /* flags */
        mca_btl_sicortex_add_procs,
        mca_btl_sicortex_del_procs,
        NULL,
        mca_btl_sicortex_finalize,
        mca_btl_sicortex_alloc,
        mca_btl_sicortex_free,
        mca_btl_sicortex_prepare_src,
        mca_btl_sicortex_prepare_dst,
        mca_btl_sicortex_send,
        mca_btl_sicortex_sendi,/* send immediate */
        mca_btl_sicortex_put, /* put */
#if 0
        mca_btl_sicortex_get, /* get */
#else
        NULL, /* get */
#endif
        NULL, /*dump */
        NULL, /* mpool */
        NULL, /* register error cb */
        mca_btl_sicortex_ft_event
    }
};

