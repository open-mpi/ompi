
#include <signal.h>
#include <unistd.h>
#include <stdio.h>

#define _ELAN4
#define __elan4__

#include "ptl_elan.h"
#include "ptl_elan_priv.h"

#define ELAN_QUEUE_MAX             INPUT_QUEUE_MAX
#define ELAN_QUEUE_LOST_SLOTS      1
#define SLOT_ALIGN                 128
#define MAX(a,b)                   ((a>b)? a:b)
#define ALIGNUP(x,a)	           (((unsigned int)(x) + ((a)-1)) & (-(a)))

static int
ompi_init_elan_queue_events (mca_ptl_elan_t * ptl,
                             ompi_ptl_elan_queue_ctrl_t * queue)
{
    int         i;
    int         count;
    int         main_align, main_size;
    int         elan_align, elan_size;

    mca_ptl_elan_desc_item_t *desc;

    RAIL       *rail;
    ELAN4_CTX  *ctx;

    ompi_free_list_t *flist;
    ompi_ptl_elan_qdma_desc_t *ptr;
    ompi_elan_event_t *elan_ptr;

    rail = (RAIL *) ptl->ptl_elan_rail;
    ctx = (ELAN4_CTX *) ptl->ptl_elan_ctx;

    flist = &queue->tx_desc_free;

    /* initialize list */
    OBJ_CONSTRUCT (&queue->tx_desc, ompi_list_t);
    OBJ_CONSTRUCT (&queue->tx_desc_free, ompi_free_list_t);

    main_align = MAX (sizeof (void *), 8);
    elan_align = MAX (sizeof (int *), 128);
    main_size = ALIGNUP (sizeof (ompi_ptl_elan_qdma_desc_t), main_align);
    elan_size = ALIGNUP (sizeof (ompi_elan_event_t), elan_align);

    flist->fl_elem_size = flist->fl_max_to_alloc = 128;
    flist->fl_num_allocated = 0;
    flist->fl_num_per_alloc = count = 16;
    flist->fl_elem_class = NULL;     /* leave it null */
    flist->fl_mpool      = NULL;     /* leave it null */

    /* Allocate the elements */

    desc = (mca_ptl_elan_desc_item_t *) 
        malloc(sizeof(mca_ptl_elan_desc_item_t) * (count + 1));
    OMPI_PTL_ELAN_CHECK_UNEX (desc, NULL, OMPI_ERROR, 0);

    ptr = (ompi_ptl_elan_qdma_desc_t *) elan4_allocMain (rail->r_alloc,
                                                          main_align,
                                                          main_size *
                                                          (count + 1));
    OMPI_PTL_ELAN_CHECK_UNEX (ptr, NULL, OMPI_ERROR, 0);

    /* Allocating elan related structures */
    elan_ptr = (ompi_elan_event_t *) elan4_allocElan (rail->r_alloc,
                                                      elan_align,
                                                      elan_size * (count +
                                                                   1));
    OMPI_PTL_ELAN_CHECK_UNEX (elan_ptr, NULL, OMPI_ERROR, 0);

    for (i = 0; i < flist->fl_num_per_alloc; i++) {
        ompi_list_item_t *item;

        ptr->rail  = rail;
        ptr->elan_data_event = elan_ptr;
        desc->item = (mca_ptl_elan_send_desc_t)ptr;

        /* Initialize some of the dma structures */
        {
            ptr->main_dma.dma_dstAddr = 0;
            ptr->main_dma.dma_srcEvent =
                SDRAM2ELAN (ctx, &elan_ptr->event32);
            ptr->main_dma.dma_dstEvent = SDRAM2ELAN (ctx, queue->input);
            INITEVENT_WORD (ctx, (EVENT *) & elan_ptr->event32,
                            &ptr->main_doneWord);
            RESETEVENT_WORD (&ptr->main_doneWord);
            PRIMEEVENT_WORD (ctx, (EVENT *) & elan_ptr->event32, 1);
        }

        item = (ompi_list_item_t *) desc;
        ompi_list_append (&flist->super, item);

        /* Progress to the next element */
        ptr = (ompi_ptl_elan_qdma_desc_t *) ((char *) ptr + main_size);
        elan_ptr = (ompi_elan_event_t *) ((char *) elan_ptr + elan_size);
        desc ++;
    }
    flist->fl_num_allocated += flist->fl_num_per_alloc;

    return OMPI_SUCCESS;
}

int
ompi_init_elan_stat (mca_ptl_elan_module_1_0_0_t * emp,
                     int num_rails)
{
    return (OMPI_SUCCESS);
}


int
ompi_init_elan_qdma (mca_ptl_elan_module_1_0_0_t * emp,
                     int num_rails)
{
    int         i;
    int         nslots = 128;
    int         slotsize = 2048;
    RAIL       *rail;
    ELAN4_CTX  *ctx;
    struct mca_ptl_elan_t *ptl;

    /* Init the Transmit Queue structure */
    for (i = 0; i < num_rails; i++) {

        ompi_ptl_elan_recv_queue_t *rxq;
        ompi_ptl_elan_queue_ctrl_t *queue;

        ptl = emp->elan_ptls[i];
        rail = (RAIL *) ptl->ptl_elan_rail;
        ctx = (ELAN4_CTX *) ptl->ptl_elan_ctx;

        queue = ptl->queue = (ompi_ptl_elan_queue_ctrl_t *)
            malloc (sizeof (ompi_ptl_elan_queue_ctrl_t));
        OMPI_PTL_ELAN_CHECK_UNEX (queue, NULL, OMPI_ERROR, 0);
        memset (queue, 0, sizeof (ompi_ptl_elan_queue_ctrl_t));

        /* Allocate input queue */
        queue->input = (E4_InputQueue *) elan4_allocElan (rail->r_alloc,
                                                          INPUT_QUEUE_ALIGN,
                                                          INPUT_QUEUE_SIZE);
        OMPI_PTL_ELAN_CHECK_UNEX (queue->input, NULL, OMPI_ERROR, 0);

        queue->tx_cmdq = elan4_alloc_cmdq (ctx,
                                           rail->r_alloc,
                                           CQ_Size8K,
                                           CQ_WriteEnableBit |
                                           CQ_DmaStartEnableBit |
                                           CQ_STENEnableBit, NULL);
        OMPI_PTL_ELAN_CHECK_UNEX (queue->tx_cmdq, NULL, OMPI_ERROR, 0);

        /* 
         * Elan4 has a rather complicated hierarchical event mechanism.
         * It is easy to use but nontrivial to manipulate
         * We implement a simpler event control mechanism, which
         * should also provide us the capability to chain event,
         * dma and IRQ etc but more open to update.
         *
         * Initialize a new event list managing this queue */

        ompi_init_elan_queue_events (ptl, queue);

        /* Allocate a cookie pool */
        queue->tx_cpool = elan4_allocCookiePool (ctx, ptl->elan_vp);

        /* Init the Receive Queue structure */
        queue->rx_nslots = 128;
        nslots += ELAN_QUEUE_LOST_SLOTS;

        queue->rx_buffsize = (slotsize > INPUT_QUEUE_MAX) ?
            INPUT_QUEUE_MAX : slotsize;
        queue->rx_slotsize = ELAN_ALIGNUP (slotsize, SLOT_ALIGN);

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

        rxq->qr_elanDone = ALLOC_ELAN (rail, SLOT_ALIGN, sizeof (EVENT32));
        OMPI_PTL_ELAN_CHECK_UNEX (rxq->qr_elanDone, NULL, OMPI_ERROR, 0);

        /* Set the top et al */
        rxq->qr_efitem = (E4_uint64) elan4_main2elan (ctx, rxq->qr_fptr);
        rxq->qr_base = rxq->qr_fptr;
        rxq->qr_top = (void *) ((uintptr_t) rxq->qr_base
                                + (queue->rx_slotsize * (nslots - 1)));
        rxq->qr_efptr = rxq->qr_efitem;
        rxq->qr_elitem =
            rxq->qr_efitem + (queue->rx_slotsize * (nslots - 1));

        /* Event to wait/block on */
        rxq->qr_qEvent = &rxq->qr_elanDone;

        queue->input->q_event =
            SDRAM2ELAN (ctx, (void *) &rxq->qr_elanDone);
        queue->input->q_fptr = rxq->qr_efitem;
        queue->input->q_bptr = rxq->qr_efitem;
        queue->input->q_control =
            E4_InputQueueControl (rxq->qr_efitem, rxq->qr_elitem,
                                  queue->rx_slotsize);

        /* The event */
        INITEVENT_WORD (ctx, (EVENT *) & rxq->qr_elanDone,
                        &rxq->qr_doneWord);
        RESETEVENT_WORD (&rxq->qr_doneWord);
        PRIMEEVENT_WORD (ctx, (EVENT *) & rxq->qr_elanDone, 1);

        rxq->qr_cmdq = elan4_alloc_cmdq (ctx, rail->r_alloc,
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

    return (OMPI_SUCCESS);
}

int
ompi_init_elan_rdma (mca_ptl_elan_module_1_0_0_t * emp,
                     int num_rails)
{
    return (OMPI_SUCCESS);
}

