/*
 * $HEADER$
 */

#include <signal.h>
#include <unistd.h>
#include <stdio.h>

#include "ptl_elan.h"
#include "ptl_elan_priv.h"

#define OMPI_PTL_ELAN_CTRL_LIST(flist, init_num, inc_num, max_num) \
do {                                                               \
    OBJ_CONSTRUCT (flist, ompi_free_list_t);                       \
    OBJ_CONSTRUCT(&flist->fl_lock, ompi_mutex_t);                  \
    flist->fl_elem_size = flist->fl_max_to_alloc = max_num;        \
    flist->fl_num_allocated = init_num;                            \
    flist->fl_num_per_alloc = inc_num;                             \
    flist->fl_elem_class = NULL;     /* leave it null */           \
    flist->fl_mpool      = NULL;     /* leave it null */           \
} while (0)


static int
ompi_init_elan_queue_events (mca_ptl_elan_module_t * ptl,
                             ompi_ptl_elan_queue_ctrl_t * queue)
{
    int         i;
    int         count;
    int         main_align, main_size;
    int         elan_align, elan_size;

    mca_ptl_elan_send_frag_t *frag;

    RAIL       *rail;
    ELAN4_CTX  *ctx;

    ompi_free_list_t *flist;
    ompi_ptl_elan_qdma_desc_t *desc;
    E4_Event32 *elan_ptr;

    START_FUNC(PTL_ELAN_DEBUG_INIT);

    rail = (RAIL *) ptl->ptl_elan_rail;
    ctx = (ELAN4_CTX *) ptl->ptl_elan_ctx;

    /* initialize list */
    OBJ_CONSTRUCT (&queue->tx_desc_free, ompi_free_list_t);
    flist = &queue->tx_desc_free;

    main_align = OMPI_PTL_ELAN_GET_MAX (sizeof (void *), 8);
    elan_align = OMPI_PTL_ELAN_GET_MAX (sizeof (int *), ELAN_BLOCK_ALIGN);
    main_size  = OMPI_PTL_ELAN_ALIGNUP (sizeof (ompi_ptl_elan_qdma_desc_t), 
	    main_align);
#if OMPI_PTL_ELAN_COMP_QUEUE
    elan_size  = OMPI_PTL_ELAN_ALIGNUP ( 
	    (2*sizeof (E4_Event32) + ELAN_BLOCK_SIZE), elan_align);
#else
    elan_size  = OMPI_PTL_ELAN_ALIGNUP (sizeof (E4_Event32), elan_align);

#endif

    OBJ_CONSTRUCT(&flist->fl_lock, ompi_mutex_t);
    flist->fl_elem_size = flist->fl_max_to_alloc = OMPI_PTL_ELAN_MAX_QDESCS;
    flist->fl_num_allocated = 0;
    flist->fl_num_per_alloc = count = OMPI_PTL_ELAN_NUM_QDESCS;
    flist->fl_elem_class = NULL;     /* leave it null */
    flist->fl_mpool      = NULL;     /* leave it null */

    /* Allocate the elements */
    frag = (mca_ptl_elan_send_frag_t *) 
        malloc(sizeof(mca_ptl_elan_send_frag_t) * count);
    OMPI_PTL_ELAN_CHECK_UNEX (frag, NULL, OMPI_ERROR, 0);

    desc = (ompi_ptl_elan_qdma_desc_t *) elan4_allocMain (rail->r_alloc,
                                                          main_align,
                                                          main_size * count);
    OMPI_PTL_ELAN_CHECK_UNEX (desc, NULL, OMPI_ERROR, 0);

    /* Allocating elan related structures */
    elan_ptr = (E4_Event32 *) elan4_allocElan (rail->r_alloc,
	    elan_align, elan_size * count);
    OMPI_PTL_ELAN_CHECK_UNEX (elan_ptr, NULL, OMPI_ERROR, 0);

    for (i = 0; i < flist->fl_num_per_alloc; i++) {
        ompi_list_item_t *item;

        desc->ptl   = ptl;
        desc->elan_event = (E4_Event *) elan_ptr;
        frag->desc = (ompi_ptl_elan_base_desc_t *)desc;

#if OMPI_PTL_ELAN_COMP_QUEUE
	/* XXX: provide a DMA structure for each chained event */
	desc->comp_dma.dma_typeSize = E4_DMA_TYPE_SIZE (
		sizeof(mca_ptl_base_frag_header_t), 
		DMA_DataTypeByte, DMA_QueueWrite, 8);
	desc->comp_dma.dma_cookie   = elan4_local_cookie(ptl->queue->tx_cpool,
		E4_COOKIE_TYPE_LOCAL_DMA, ptl->elan_vp);
	desc->comp_dma.dma_vproc    = ptl->elan_vp;
	desc->comp_dma.dma_srcAddr  = 0x0ULL; /* To be filled in */
	desc->comp_dma.dma_dstAddr  = 0x0ULL; /* To be filled in */

	/* XXX: If completion is to be detected from the Queue
	 *      there is no need to trigger a local event */
	desc->comp_dma.dma_dstEvent = elan4_main2elan (ctx, 
		(void *) ptl->comp->input);
	desc->comp_dma.dma_srcEvent = 0x0ULL;
	desc->comp_dma.dma_typeSize |= RUN_DMA_CMD;
	desc->comp_dma.dma_pad       = NOP_CMD;
	desc->comp_event = (E4_Event *) (elan_ptr + 1);
	desc->comp_buff  = (E4_Addr *) (elan_ptr + 2);
	desc->comp_event->ev_CountAndType = E4_EVENT_INIT_VALUE(-32,
		E4_EVENT_COPY, E4_EVENT_DTYPE_LONG, 8);
	desc->comp_event->ev_Params[0]    = elan4_main2elan (ctx, 
		(void *)desc->comp_buff);
	desc->comp_event->ev_Params[1] = 0x0ULL; 

        /* Initialize some of the dma structures */
	desc->main_dma.dma_dstAddr = 0;
	desc->main_dma.dma_srcEvent= elan4_main2elan(ctx, 
		(E4_Event *)desc->comp_event);
	desc->main_dma.dma_dstEvent= SDRAM2ELAN (ctx, queue->input);
#else
        /* Initialize some of the dma structures */
	desc->main_dma.dma_dstAddr = 0;
	desc->main_dma.dma_srcEvent = SDRAM2ELAN (ctx, desc->elan_event);
	desc->main_dma.dma_dstEvent = SDRAM2ELAN (ctx, queue->input);
	INITEVENT_WORD (ctx, desc->elan_event, &desc->main_doneWord);
	RESETEVENT_WORD (&desc->main_doneWord);
	PRIMEEVENT_WORD (ctx, desc->elan_event, 1);
#endif

        item = (ompi_list_item_t *) frag;
        ompi_list_append (&flist->super, item);

        /* Progress to the next element */
        desc = (ompi_ptl_elan_qdma_desc_t *) ((char *) desc + main_size);
        elan_ptr = (E4_Event32 *) ((char *) elan_ptr + elan_size);
        frag ++;
    }
    flist->fl_num_allocated += flist->fl_num_per_alloc;

    END_FUNC(PTL_ELAN_DEBUG_INIT);
    return OMPI_SUCCESS;
}

static void
mca_ptl_elan_putget_desc_contruct (
	mca_ptl_elan_module_t * ptl,
       	ompi_ptl_elan_putget_desc_t *desc, 
	EVENT *elan_event,
	E4_Addr src_elan4_addr,
	E4_Addr dst_elan4_addr, 
	int local /* dma_src is local */ )
{
    ELAN4_CTX *ctx;

    ctx = (ELAN4_CTX *)ptl->ptl_elan_ctx;
    memset(desc, 0, sizeof(desc));
    desc->ptl = ptl;
    desc->req = NULL;

#if OMPI_PTL_ELAN_COMP_QUEUE
    /* Allocate elan memory for chained event and buff */
    desc->chain_buff = (E4_Addr *) ((char *)elan_event);
    desc->comp_buff  = (E4_Addr *) ((char *)elan_event + ELAN_BLOCK_SIZE );
    desc->elan_event = (E4_Event *) ((char *)elan_event + 2*ELAN_BLOCK_SIZE ); 
    desc->chain_event= (E4_Event *) ((char *)elan_event 
	    + 2 * ELAN_BLOCK_SIZE + sizeof (E4_Event32));
    desc->comp_event= (E4_Event *) ((char *)elan_event 
	    + 2 * ELAN_BLOCK_SIZE + 2 * sizeof (E4_Event32));

    /* XXX: provide a DMA structure for each chained event */
    desc->comp_dma.dma_typeSize = E4_DMA_TYPE_SIZE (
	    sizeof(mca_ptl_base_frag_header_t), 
	    DMA_DataTypeByte, DMA_QueueWrite, 8);
    desc->comp_dma.dma_vproc    = ptl->elan_vp;
    desc->comp_dma.dma_srcAddr  = 0x0ULL; /* To be filled in */
    desc->comp_dma.dma_dstAddr  = 0x0ULL; /* To be filled in */

    /* XXX: If completion is to be detected from the Queue
     *      there is no need to trigger a local event */
    desc->comp_dma.dma_dstEvent = elan4_main2elan (ctx, 
	    (void *) ptl->comp->input);
    desc->comp_dma.dma_srcEvent = 0x0ULL;
    desc->comp_dma.dma_typeSize |= RUN_DMA_CMD;
    desc->comp_dma.dma_pad       = NOP_CMD;

    desc->comp_event->ev_CountAndType = E4_EVENT_INIT_VALUE(-32,
	    E4_EVENT_COPY, E4_EVENT_DTYPE_LONG, 8);
    desc->comp_event->ev_Params[0]    = elan4_main2elan (ctx, 
	    (void *)desc->comp_buff);
    desc->comp_event->ev_Params[1] = 0x0ULL; 

    /* Initialize some of the dma structures */
    desc->main_dma.dma_srcEvent= elan4_main2elan(ctx, 
	    (E4_Event *)desc->chain_event);
    desc->main_dma.dma_dstEvent= 0x0ULL;
#else
    desc->elan_event = elan_event; 
    desc->chain_event= (E4_Event32 *) 
	((char *)elan_event + sizeof (E4_Event32));
    desc->chain_buff = (E4_Addr *) 
	((char *)elan_event + 2*sizeof (E4_Event32));

    if (local) {
	desc->main_dma.dma_srcEvent = elan4_main2elan(ctx, elan_event);
    } else {
	desc->main_dma.dma_dstEvent = elan4_main2elan(ctx, elan_event);
    }

    /* XXX: Remember to reset all event and doneWord */
    INITEVENT_WORD (ctx, elan_event, &desc->main_doneWord);
    RESETEVENT_WORD (&desc->main_doneWord);
    PRIMEEVENT_WORD (ctx, elan_event, 1);
#endif

    /* Make PCI write visable */
    mb();
}

#define OMPI_ELAN_PUTGET_GROW(ptl, flist, frag, dp, eptr, msize, esize, local)\
do {                                                                      \
    int i;                                                                \
    for (i = 0; i < flist->fl_num_per_alloc; i++) {                       \
        ompi_list_item_t *item;                                           \
                                                                          \
        frag->desc = (ompi_ptl_elan_base_desc_t *)dp;                     \
                                                                          \
        /* Initialize some of the dma structures */                       \
	mca_ptl_elan_putget_desc_contruct (ptl, dp,                       \
		eptr, 0, 0, local);                                       \
                                                                          \
        item = (ompi_list_item_t *) frag;                                 \
        ompi_list_append (&flist->super, item);                           \
                                                                          \
        /* Progress to the next element */                                \
        dp= (ompi_ptl_elan_putget_desc_t *) ((char *)dp + msize);         \
        eptr = (E4_Event *) ((char *) eptr + esize);                      \
        frag ++;                                                          \
    }                                                                     \
    flist->fl_num_allocated += flist->fl_num_per_alloc;                   \
} while (0)


static int
ompi_ptl_elan_init_putget_ctrl (mca_ptl_elan_module_t * ptl,
				RAIL *rail,
				ompi_ptl_elan_putget_ctrl_t * putget,
				int init_num, int inc_num, int max_num)
{
    int         main_size; 
    int         main_align;
    int         elan_size; 
    int         elan_align;

    ELAN4_CTX  *ctx;
    E4_Event   *elan_ptr;
    mca_ptl_elan_send_frag_t *frag;
    ompi_free_list_t *put_list, *get_list;
    ompi_ptl_elan_putget_desc_t *put_desc, *get_desc;

    START_FUNC(PTL_ELAN_DEBUG_INIT);

    main_align = OMPI_PTL_ELAN_GET_MAX (sizeof (void *), ELAN_ALIGN);
    elan_align = OMPI_PTL_ELAN_GET_MAX (sizeof (int *), ELAN_BLOCK_ALIGN);
    main_size  = OMPI_PTL_ELAN_ALIGNUP(sizeof(ompi_ptl_elan_putget_desc_t), 
	    main_align);

#if OMPI_PTL_ELAN_COMP_QUEUE
    elan_size  = OMPI_PTL_ELAN_ALIGNUP( 
	    (ELAN_BLOCK_SIZE + sizeof(E4_Event32)*2 ), elan_align);
#else
    elan_size  = OMPI_PTL_ELAN_ALIGNUP(
	    (ELAN_BLOCK_SIZE * 2 + sizeof(E4_Event32)*3 ), elan_align);
#endif

    rail = (RAIL *) ptl->ptl_elan_rail;
    ctx  = (ELAN4_CTX *) ptl->ptl_elan_ctx;

    /* initialize list */
    OBJ_CONSTRUCT (&putget->put_desc_free, ompi_free_list_t);
    put_list = &putget->put_desc_free;
    OMPI_PTL_ELAN_CTRL_LIST(put_list, 0, inc_num, max_num);

    /* Allocate the elements */
    frag = (mca_ptl_elan_send_frag_t *) 
        malloc(sizeof(mca_ptl_elan_send_frag_t) * inc_num);
    OMPI_PTL_ELAN_CHECK_UNEX (frag, NULL, OMPI_ERROR, 0);

    /* Allocating elan related structures */
    elan_ptr = (E4_Event *) elan4_allocElan (rail->r_alloc,
	    elan_align, elan_size * inc_num);
    OMPI_PTL_ELAN_CHECK_UNEX (elan_ptr, NULL, OMPI_ERROR, 0);

    put_desc = (ompi_ptl_elan_putget_desc_t *) elan4_allocMain (
	    rail->r_alloc, main_align, main_size * inc_num);
    OMPI_PTL_ELAN_CHECK_UNEX (put_desc, NULL, OMPI_ERROR, 0);
    OMPI_ELAN_PUTGET_GROW(ptl, put_list, frag, put_desc, elan_ptr, 
	    main_size, elan_size, 1);

    OBJ_CONSTRUCT (&putget->get_desc_free, ompi_free_list_t);
    get_list = &putget->get_desc_free;
    OMPI_PTL_ELAN_CTRL_LIST(get_list, 0, inc_num, max_num);

    /* Allocate the elements */
    frag = (mca_ptl_elan_send_frag_t *) 
        malloc(sizeof(mca_ptl_elan_send_frag_t) * inc_num);
    OMPI_PTL_ELAN_CHECK_UNEX (frag, NULL, OMPI_ERROR, 0);

    /* Allocating elan related structures */
    elan_ptr = (E4_Event *) elan4_allocElan (rail->r_alloc,
	    elan_align, elan_size * inc_num);
    OMPI_PTL_ELAN_CHECK_UNEX (elan_ptr, NULL, OMPI_ERROR, 0);

    get_desc = (ompi_ptl_elan_putget_desc_t *) elan4_allocMain (
	    rail->r_alloc, main_align, main_size * inc_num);
    OMPI_PTL_ELAN_CHECK_UNEX (get_desc, NULL, OMPI_ERROR, 0);
    OMPI_ELAN_PUTGET_GROW(ptl, get_list, frag, get_desc, elan_ptr, 
	    main_size, elan_size, 0);
    END_FUNC (PTL_ELAN_DEBUG_INIT);
    return OMPI_SUCCESS;
}


int
ompi_init_elan_stat (mca_ptl_elan_component_t * emp,
                     int num_rails)
{
    return (OMPI_SUCCESS);
}


int
ompi_init_elan_qdma (mca_ptl_elan_component_t * emp,
                     int num_rails)
{
    int         i;
    int         nslots;
    int         slotsize;
    RAIL       *rail;
    ELAN4_CTX  *ctx;
    struct mca_ptl_elan_module_t *ptl;

    START_FUNC(PTL_ELAN_DEBUG_INIT);
    nslots = OMPI_PTL_ELAN_MAX_QSLOTS;

#if OMPI_PTL_ELAN_COMP_QUEUE
    /* Create a complete queue here, later use the queue above directly */
    /* Init the Transmit Queue structure */
    for (i = 0; i < num_rails; i++) {

        ompi_ptl_elan_recv_queue_t *rxq;
        ompi_ptl_elan_comp_queue_t *comp;

        ptl = emp->modules[i];
        rail = (RAIL *) ptl->ptl_elan_rail;
        ctx = (ELAN4_CTX *) ptl->ptl_elan_ctx;

        comp = ptl->comp = (ompi_ptl_elan_comp_queue_t *)
            malloc (sizeof (ompi_ptl_elan_comp_queue_t));
        OMPI_PTL_ELAN_CHECK_UNEX (comp, NULL, OMPI_ERROR, 0);
        memset (comp, 0, sizeof (ompi_ptl_elan_comp_queue_t));

        /* Allocate input queue */
        comp->input = (E4_InputQueue *) elan4_allocElan (rail->r_alloc,
                                                         INPUT_QUEUE_ALIGN,
                                                         INPUT_QUEUE_SIZE);
        OMPI_PTL_ELAN_CHECK_UNEX (comp->input, NULL, OMPI_ERROR, 0);

        /* Init the Receive Queue structure */
        comp->rx_nslots = nslots;
        nslots += OMPI_PTL_ELAN_LOST_QSLOTS;
	slotsize = sizeof(mca_ptl_base_header_t);
        comp->rx_slotsize = ELAN_ALIGNUP (slotsize, OMPI_PTL_ELAN_SLOT_ALIGN);
        comp->rx_buffsize = (slotsize > INPUT_QUEUE_MAX) ?
            INPUT_QUEUE_MAX : slotsize;
        rxq = comp->rxq = (ompi_ptl_elan_recv_queue_t *)
            elan4_allocMain (rail->r_alloc, 64,
                             sizeof (ompi_ptl_elan_recv_queue_t));
        OMPI_PTL_ELAN_CHECK_UNEX (rxq, NULL, OMPI_ERROR, 0);
        memset (rxq, 0, sizeof (ompi_ptl_elan_recv_queue_t));

        rxq->qr_rail = rail;
        rxq->qr_fptr = elan4_allocMain (rail->r_alloc,
                                        128, nslots * comp->rx_slotsize);
        OMPI_PTL_ELAN_CHECK_UNEX (rxq->qr_fptr, NULL, OMPI_ERROR, 0);
        memset (rxq->qr_fptr, 0xeb, nslots * comp->rx_slotsize);

        rxq->qr_elanDone = ALLOC_ELAN (rail, 
		OMPI_PTL_ELAN_SLOT_ALIGN, sizeof (EVENT32));
        OMPI_PTL_ELAN_CHECK_UNEX (rxq->qr_elanDone, NULL, OMPI_ERROR, 0);

        /* Set the top et al */
        rxq->qr_efitem = (E4_uint64) elan4_main2elan (ctx, rxq->qr_fptr);
	assert(rxq->qr_efitem != ELAN_BAD_ADDR); 
        rxq->qr_base = rxq->qr_fptr;
        rxq->qr_top = (void *) ((uintptr_t) rxq->qr_base + 
		(comp->rx_slotsize * (nslots - OMPI_PTL_ELAN_LOST_QSLOTS)));
        rxq->qr_efptr = rxq->qr_efitem;
        rxq->qr_elitem = rxq->qr_efitem + 
	    (comp->rx_slotsize * (nslots - OMPI_PTL_ELAN_LOST_QSLOTS));

        /* Event to wait/block on, Bug here for the event */
        rxq->qr_qEvent = rxq->qr_elanDone;
        comp->input->q_event= SDRAM2ELAN (ctx, (void *) rxq->qr_elanDone);
        comp->input->q_fptr = rxq->qr_efitem;
        comp->input->q_bptr = rxq->qr_efitem;
        comp->input->q_control =
            E4_InputQueueControl (rxq->qr_efitem, rxq->qr_elitem,
                                  comp->rx_slotsize);

        /* The event */
        INITEVENT_WORD (ctx, (EVENT *) rxq->qr_elanDone,
                        &rxq->qr_doneWord);
        RESETEVENT_WORD (&rxq->qr_doneWord);
        PRIMEEVENT_WORD (ctx, (EVENT *) rxq->qr_elanDone, 1);

        rxq->qr_cmdq = OMPI_PTL_ELAN_ALLOC_CMDQ (ctx, 
					 rail->r_alloc,
                                         CQ_Size1K,
                                         CQ_WriteEnableBit |
                                         CQ_WaitEventEnableBit, NULL);
        OMPI_PTL_ELAN_CHECK_UNEX (rxq->qr_cmdq, NULL, OMPI_ERROR, 0);

        /* Allocate a sleepDesc for threads to block on */
        rxq->qr_es = ompi_init_elan_sleepdesc (&mca_ptl_elan_global_state,
                                               rxq->qr_rail);
        OMPI_PTL_ELAN_CHECK_UNEX (rxq->qr_es, NULL, OMPI_ERROR, 0);
        OBJ_CONSTRUCT (&comp->rx_lock, ompi_mutex_t);
    }
#endif

    /* Init the Transmit Queue structure */
    for (i = 0; i < num_rails; i++) {

        ompi_ptl_elan_recv_queue_t *rxq;
        ompi_ptl_elan_queue_ctrl_t *queue;

        ptl = emp->modules[i];
        rail = (RAIL *) ptl->ptl_elan_rail;
        ctx = (ELAN4_CTX *) ptl->ptl_elan_ctx;

        queue = ptl->queue = (ompi_ptl_elan_queue_ctrl_t *)
            malloc (sizeof (ompi_ptl_elan_queue_ctrl_t));
        OMPI_PTL_ELAN_CHECK_UNEX (queue, NULL, OMPI_ERROR, 0);
        memset (queue, 0, sizeof (ompi_ptl_elan_queue_ctrl_t));

        /* TODO: move the input queue into ptl->comp */
        queue->input = (E4_InputQueue *) elan4_allocElan (rail->r_alloc,
                                                          INPUT_QUEUE_ALIGN,
                                                          INPUT_QUEUE_SIZE);
        OMPI_PTL_ELAN_CHECK_UNEX (queue->input, NULL, OMPI_ERROR, 0);

        queue->tx_cmdq = OMPI_PTL_ELAN_ALLOC_CMDQ (ctx,
                                           rail->r_alloc,
                                           CQ_Size8K,
                                           CQ_WriteEnableBit |
                                           CQ_DmaStartEnableBit |
                                           CQ_STENEnableBit, NULL);
        OMPI_PTL_ELAN_CHECK_UNEX (queue->tx_cmdq, NULL, OMPI_ERROR, 0);

        /* Allocate a cookie pool */
        queue->tx_cpool = elan4_allocCookiePool (ctx, ptl->elan_vp);
        ompi_init_elan_queue_events (ptl, queue);

        /* Init the Receive Queue structure */
        queue->rx_nslots = nslots;
        nslots += OMPI_PTL_ELAN_LOST_QSLOTS;
	slotsize = OMPI_PTL_ELAN_MAX_QSIZE;
        queue->rx_slotsize = ELAN_ALIGNUP (slotsize, OMPI_PTL_ELAN_SLOT_ALIGN);
        queue->rx_buffsize = (slotsize > INPUT_QUEUE_MAX) ?
            INPUT_QUEUE_MAX : slotsize;

        rxq = queue->rxq = (ompi_ptl_elan_recv_queue_t *)
            elan4_allocMain (rail->r_alloc, 64,
                             sizeof (ompi_ptl_elan_recv_queue_t));
        OMPI_PTL_ELAN_CHECK_UNEX (rxq, NULL, OMPI_ERROR, 0);
        memset (rxq, 0, sizeof (ompi_ptl_elan_recv_queue_t));

        rxq->qr_rail = rail;
        rxq->qr_fptr = elan4_allocMain (rail->r_alloc,
                                        128, nslots * queue->rx_slotsize);
        OMPI_PTL_ELAN_CHECK_UNEX (rxq->qr_fptr, NULL, OMPI_ERROR, 0);
        memset (rxq->qr_fptr, 0xeb, nslots * queue->rx_slotsize);

        rxq->qr_elanDone = ALLOC_ELAN (rail, 
		OMPI_PTL_ELAN_SLOT_ALIGN, sizeof (EVENT32));
        OMPI_PTL_ELAN_CHECK_UNEX (rxq->qr_elanDone, NULL, OMPI_ERROR, 0);

        /* Set the top etc */
        rxq->qr_efitem = (E4_uint64) elan4_main2elan (ctx, rxq->qr_fptr);
	assert(rxq->qr_efitem != ELAN_BAD_ADDR); 
        rxq->qr_base = rxq->qr_fptr;
        rxq->qr_top = (void *) ((uintptr_t) rxq->qr_base + 
		(queue->rx_slotsize * (nslots - OMPI_PTL_ELAN_LOST_QSLOTS)));
        rxq->qr_efptr = rxq->qr_efitem;
        rxq->qr_elitem = rxq->qr_efitem + 
	    (queue->rx_slotsize * (nslots - OMPI_PTL_ELAN_LOST_QSLOTS));

        /* XXX: Event to wait/block on, was buggy here for the event */
        rxq->qr_qEvent = rxq->qr_elanDone;
        queue->input->q_event= SDRAM2ELAN (ctx, (void *) rxq->qr_elanDone);
        queue->input->q_fptr = rxq->qr_efitem;
        queue->input->q_bptr = rxq->qr_efitem;
        queue->input->q_control =
            E4_InputQueueControl (rxq->qr_efitem, rxq->qr_elitem,
                                  queue->rx_slotsize);

        /* The event */
        INITEVENT_WORD (ctx, (EVENT *) rxq->qr_elanDone,
                        &rxq->qr_doneWord);
        RESETEVENT_WORD (&rxq->qr_doneWord);
        PRIMEEVENT_WORD (ctx, (EVENT *) rxq->qr_elanDone, 1);

        rxq->qr_cmdq = OMPI_PTL_ELAN_ALLOC_CMDQ (ctx, 
					 rail->r_alloc,
                                         CQ_Size1K,
                                         CQ_WriteEnableBit |
                                         CQ_WaitEventEnableBit, NULL);
        OMPI_PTL_ELAN_CHECK_UNEX (rxq->qr_cmdq, NULL, OMPI_ERROR, 0);

        /* Allocate a sleepDesc for threads to block on */
        rxq->qr_es = ompi_init_elan_sleepdesc (&mca_ptl_elan_global_state,
                                               rxq->qr_rail);
        OMPI_PTL_ELAN_CHECK_UNEX (rxq->qr_es, NULL, OMPI_ERROR, 0);
        OBJ_CONSTRUCT (&queue->rx_lock, ompi_mutex_t);
    }

    END_FUNC(PTL_ELAN_DEBUG_INIT);
    return (OMPI_SUCCESS);
}


int
ompi_init_elan_putget (mca_ptl_elan_component_t * emp,
                       int num_rails)
{
    int         i;
    RAIL       *rail;
    ELAN4_CTX  *ctx;
    struct mca_ptl_elan_module_t *ptl;

    START_FUNC(PTL_ELAN_DEBUG_INIT);

    /* Init the Transmit Queue structure */
    for (i = 0; i < num_rails; i++) {

	E4_CmdQParams *cqp;
	ompi_ptl_elan_putget_ctrl_t *putget;

        ptl = emp->modules[i];
        rail = (RAIL *) ptl->ptl_elan_rail;
        ctx = (ELAN4_CTX *) ptl->ptl_elan_ctx;

        putget = ptl->putget = (ompi_ptl_elan_putget_ctrl_t *)
            malloc (sizeof (ompi_ptl_elan_putget_ctrl_t));
        OMPI_PTL_ELAN_CHECK_UNEX (putget, NULL, OMPI_ERROR, 0);
        memset (putget, 0, sizeof (ompi_ptl_elan_putget_ctrl_t));

	putget->pg_throttle = OMPI_PTL_ELAN_MAX_PGDESC; 
	putget->pg_flags    = OMPI_PTL_ELAN_FASTPATH;
	putget->pg_retryCount = 16;
	putget->pg_evictCache = TRUE;
	putget->pg_waitType   = ELAN_POLL_EVENT;
	
	/* construct the lock variable */
        OBJ_CONSTRUCT (&putget->pg_lock, ompi_mutex_t);

	cqp = OMPI_PTL_ELAN_PROBE_CMDQ (ctx, rail->r_alloc, 
		0x10, CQ_AutoCtrlFlowOn);

	putget->put_cmdq = OMPI_PTL_ELAN_ALLOC_CMDQ(ctx,
			rail->r_alloc, 
			CQ_Size8K, 
			CQ_WriteEnableBit | 
			CQ_DmaStartEnableBit | 
			CQ_SetEventEnableBit | 
			CQ_STENEnableBit, cqp);
        OMPI_PTL_ELAN_CHECK_UNEX (putget->put_cmdq, NULL, OMPI_ERROR, 0);

	putget->get_cmdq = OMPI_PTL_ELAN_ALLOC_CMDQ(ctx,
		        rail->r_alloc,
			CQ_Size8K, 
			CQ_WriteEnableBit | 
			CQ_STENEnableBit | 
			CQ_SetEventEnableBit, cqp);
        OMPI_PTL_ELAN_CHECK_UNEX (putget->get_cmdq, NULL, OMPI_ERROR, 0);

	putget->pg_cmdStream = malloc(PAGESIZE);
        OMPI_PTL_ELAN_CHECK_UNEX (putget->pg_cmdStream, NULL, OMPI_ERROR, 0);

	/* Allocate a per vp counter to throttle outstanding get DMAs */
	putget->pg_pendingGetCount = malloc(sizeof(u_int)*ptl->elan_nvp);
        OMPI_PTL_ELAN_CHECK_UNEX (putget->pg_pendingGetCount, 
		NULL, OMPI_ERROR, 0);
	memset(putget->pg_pendingGetCount, 0, sizeof(u_int)*ptl->elan_nvp);
	putget->pg_cpool = elan4_allocCookiePool(ctx, ptl->elan_vp);
       	ompi_ptl_elan_init_putget_ctrl (ptl, rail, putget, 
		0, OMPI_PTL_ELAN_NUM_PUTGET, OMPI_PTL_ELAN_MAX_PUTGET);
    }

    END_FUNC(PTL_ELAN_DEBUG_INIT);
    return (OMPI_SUCCESS);
}

