
#include "qsnet/fence.h"
#include "ptl_elan.h"
#include "ptl_elan_priv.h"

extern mca_ptl_elan_module_1_0_0_t mca_ptl_elan_module;

int test_qdma(mca_ptl_elan_module_1_0_0_t *emp);

int ptl_elan_init_event();
int ptl_elan_free_event();
int ptl_elan_poll_event();
int ptl_elan_wait_event();

int ptl_elan_queue_send();  /* Initialize a Tx event */
int ptl_elan_queue_recv(); /* Wait for a recv event */

int main (int argc, char ** argv)
{
    /* Initialization test */
    ompi_mca_ptl_elan_init (&mca_ptl_elan_module);

    /* Please add a barrier at the beginning */
#if 0
    test_qdma(&mca_ptl_elan_module);
#endif

    /* Please add a barrier at the end */

    /* Finalize the device */
    ompi_mca_ptl_elan_finalize (&mca_ptl_elan_module);

    /* Tell alive */
    fprintf(stdout, "I am still alive\n");
    fflush(stdout);
    return 0;
}

int test_qdma(mca_ptl_elan_module_1_0_0_t *emp, int reps)
{
    uint64_t start0, end0;
    double   t;
    int      r;

    r = reps;
    ELAN4_CTX *ctx;
    RAIL      *rail;
    mca_ptl_elan_t *ptl;

    ptl = emp->elan_ptls[0];
    ctx = emp->ems->elan_ctx;
    rail = (RAIL *) emp->ems->elan_rail[0];
    
    start0 = elan4_clock(ctx);

    if (0 != ptl->elan_vp) {
        r--;
        /*elan_queueRxWait(qr,rbuf,waitType);*/
        ptl_elan_queue_recv();
    }

    while (--r >= 0) {
        /* Trigger a send event */
        ptl_elan_queue_send();

	if ( (&qm->qm_event)->e_state == EVENT_LIVE_LIVE )
	    (*(&qm->qm_event)->waitFn) (&qm->qm_event, ELAN_POLL_EVENT);
	(*(&qm->qm_event)->freeFn)(&qm->qm_event);

        /* Wait for a receive event to come by */
        /*elan_queueRxWait(qr,rbuf,waitType);*/
        ptl_elan_queue_recv();
    }

    if (0 != ptl->elan_vp) {

        /* Trigger a send event */
        ptl_elan_queue_send();

	if ( (&qm->qm_event)->e_state == EVENT_LIVE_LIVE )
	    (*(&qm->qm_event)->waitFn) (&qm->qm_event, ELAN_POLL_EVENT);
	(*(&qm->qm_event)->freeFn)(&qm->qm_event);
    }

    end0 = elan4_clock(ctx);
    nsec = ((end0 - start0) / reps);
    t = ((double) nsec)/(2*1000.0);

    return(0);
}

ptl_elan_queue_send() {

        QUEUE_TX_CTRL *qc = (QUEUE_TX_CTRL *)qt;
        QUEUE_TX_RAIL *qr;
        QUEUE_TX_MAIN *qm;
        
        /* XXX: Cache eviction */
        PRINTF5(qc->qc_state, DBG_QUEUE,
                "elan_queueTx(%p): destvp %d req=%p size=%d rail=%d\n",
                qc, destvp, req, size, rail);
        
        if ( rail == ELAN_RAIL_ALL )
            rail = 0;
        
        qr = &qc->qc_qr[rail];
        if ((qm = (QUEUE_TX_MAIN *)_elan_caAcquire(&qr->qr_ca))==NULL)
            elan_exception(qc->qc_state, ELAN_ENOMEM,
                           "elan_queueTx(): Failed to allocate descriptor");

        /*qm = list_entry(qm,QUEUE_TX_MAIN, qm_event);*/

        qm = ptl_elan_init_event();

        if ( size ) {
            
            if ( testFlag (qc->qc_flags, LIBELAN_QUEUEREUSEBUF) )
            {
                /* The current API allows the req buffer to be re-used on
                 * return if this flag is set.
                 */
                memcpy(&qm[1], req, size);
                
                qm->qm_dma.dma_srcAddr = MAIN2ELAN(qr->qr_rail->r_ctx, &qm[1]);
                
            } else {
                qm->qm_dma.dma_srcAddr = MAIN2ELAN(qr->qr_rail->r_ctx, req);
            }
        }

        /* XXXX Hardwired DMA retry count */

        qm->qm_dma.dma_typeSize = E4_DMA_TYPE_SIZE(size, 
                DMA_DataTypeByte, DMA_QueueWrite, 16);
        qm->qm_dma.dma_cookie = elan4_local_cookie(qr->qr_cpool, 
                E4_COOKIE_TYPE_LOCAL_DMA, destvp);
        qm->qm_dma.dma_vproc = destvp;

        ompi_output(0,
                "elan_queueTx(%p): DMA: typeSize %Lx vproc %lx srcAddr %Lx "
                "dstAddr %Lx srcEvent %Lx dstEvent %Lx\n",
                qc, 
                qm->qm_dma.dma_typeSize,
                qm->qm_dma.dma_vproc,
                qm->qm_dma.dma_srcAddr,
                qm->qm_dma.dma_dstAddr,
                qm->qm_dma.dma_srcEvent,
                qm->qm_dma.dma_dstEvent);

        /* Make main memory coherent with IO domain (IA64) */
        MEMBAR_VISIBLE();

        elan4_run_dma_cmd(qr->qr_cmdq, (DMA *)&qm->qm_dma);
        qr->qr_cmdq->cmdq_flush (qr->qr_cmdq);

        ELAN_EVENT_PRIME(&qm->qm_event);
        ompi_output(0, "elan_queueTx(%p) returning %p\n", qc, qm);

	/* Poll whole chain starting at head */
        /* See its initiation in _elan_queueTxDescInit();
         * Look for _elan_queueTxPoll(): _elan_queueTxWait():
         * and _elan_queueTxFree(); */

        return Event;
}

/* This function needs no event related knowledge */
void ptl_elan_queue_recv () 
{
    QUEUE_RX_CTRL *qc = (QUEUE_RX_CTRL *)qrx;
    int i;
    
    MUTEX_LOCK(&qc->qc_mutex);
    
    if ( qc->qc_lastUsed != NULL )
	_elan_queueRxComplete(qrx);
    
    if ( qc->qc_nrails == 1 ) {
	QUEUE_RX_RAIL *qr = qc->qc_qr[0];

	/* Call the libelan4 wait code directly passing the qr_cmdq 
         * not the es_cq as there is a pending wait(-32) in the qr_cq 
         * and the elan4 wait code does a wait(0) which must not 
         * overtake the wait(-32) or else we don't block when we 
         * should causing premature wakeups
	 */
	elan4_waitevent_word(qr->qr_rail->r_ctx, qr->qr_cmdq, 
                qr->qr_es->es_ecmdq, qr->qr_es->es_cmdBlk, 
                qr->qr_es->es_cookie, qr->qr_qEvent, 
                &qr->qr_doneWord, waitType);

	qc->qc_lastUsed = qr;

	if ( buf != NULL ) {

            QUEUE_RX_CTRL *qc = (QUEUE_RX_CTRL *)qrx;
            QUEUE_RX_RAIL *qr = qc->qc_lastUsed;
            RAIL        *rail;

 	    memcpy(buf, qr->qr_fptr, qc->qc_bufSize);

	    /*_elan_queueRxComplete(qrx);*/
            if ( ! qr )
                return;
            
            rail = qr->qr_rail;
            
#ifdef DEBUG
            memset(qr->qr_fptr, 0xfe, qc->qc_slotSize);
            //MEMBAR_STORESTORE();
#endif

            /* Work out the new front pointer */
            if (qr->qr_fptr == qr->qr_top) {
                qr->qr_fptr = qr->qr_base;
                qr->qr_efptr = qr->qr_efitem;
            } else {
                qr->qr_fptr = (void *)((uintptr_t)qr->qr_fptr 
                        + qc->qc_slotSize);
                qr->qr_efptr += qc->qc_slotSize;
            }
            
            /* PCI Write */
            qc->qc_q->q_fptr = qr->qr_efptr;
            MEMBAR_STORESTORE();
            
            /* Reset the event */
            RESETEVENT_WORD(&qr->qr_doneWord);

            /* Order RESETEVENT wrt to wait_event_cmd */
            MEMBAR_STORESTORE();
            
            /* Re-prime queue event by issuing a waitevent(1) on it */
            elan4_wait_event_cmd(qr->qr_cmdq, 
                    MAIN2ELAN(rail->r_ctx, &qr->qr_elan->qe_done), 
                    E4_EVENT_INIT_VALUE(-32, E4_EVENT_WRITE, 
                        E4_EVENT_DTYPE_LONG, 0), 
                    MAIN2ELAN(rail->r_ctx, (void *) &qr->qr_doneWord),
                    0xfeedfacedeadbeef);

            qr->qr_cmdq->cmdq_flush (qr->qr_cmdq);
            qc->qc_lastUsed = NULL;
	    MUTEX_UNLOCK(&qc->qc_mutex);
	    return buf;
	} else {
	    MUTEX_UNLOCK(&qc->qc_mutex);
	    return qr->qr_fptr;
	}
    } 
    return ;
}

int ptl_elan_init_event() {

    QUEUE_TX_RAIL *qr = (QUEUE_TX_RAIL *)handle;
    QUEUE_TX_MAIN *qm = list_entry(main,QUEUE_TX_MAIN,qm_event);
                                                                                
    if (ELAN_MISALIGNED (offsetof(QUEUE_TX_ELAN, qe_done), EVENT_ALIGN)) {
        elan_exception (state, ELAN_EINTERNAL, 
                "Bad QUEUE_TX_ELAN done event alignment (%d)",
                offsetof(QUEUE_TX_ELAN, qe_done));
    }
                                                                                
    /* Zero this descriptor */
    memset(qm,0,sizeof(QUEUE_TX_MAIN));
                                                                                
    qm->qm_elan = elanAddr;
    qm->qm_event.handle = handle;
                                                                                
    /* Initialise event desc */
    qm->qm_event.pollFn = _elan_queueTxPoll;
    qm->qm_event.waitFn = _elan_queueTxWait;
    qm->qm_event.freeFn = _elan_queueTxFree;
                                                                                
    qm->qm_dma.dma_dstAddr = 0;
    qm->qm_dma.dma_srcEvent = SDRAM2ELAN(rail->r_ctx,
            &qm->qm_elan->qe_done);
    qm->qm_dma.dma_dstEvent = SDRAM2ELAN(rail->r_ctx, qr->qr_q);
                                                                                
    INITEVENT_WORD(qr->qr_rail->r_ctx, 
            (EVENT *)&qm->qm_elan->qe_done, &qm->qm_doneWord);
                                                                                
    RESETEVENT_WORD(&qm->qm_doneWord);
    PRIMEEVENT_WORD(qr->qr_rail->r_ctx, 
            (EVENT *)&qm->qm_elan->qe_done, 1);
    return true;
}
