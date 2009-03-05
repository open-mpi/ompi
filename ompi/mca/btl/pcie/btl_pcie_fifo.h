/*
 * Copyright (c) 2007      Los Alamos National Security, LLC.
 *                         All righs reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef BTL_PCIE_FIFO_H
#define BTL_PCIE_FIFO_H

#include "ompi_config.h"
#include "ompi/constants.h"

#include "opal/threads/mutex.h"
#include "opal/types.h"
#include "ompi/mca/btl/base/btl_base_error.h"

BEGIN_C_DECLS

typedef uint64_t btl_pcie_fifo_entry_t;
#define BTL_PCIE_FIFO_TYPE_MASK 0x8000000000000000
#define BTL_PCIE_FIFO_DATA_MASK 0x7FFFFFFFFFFFFFFF
#define BTL_PCIE_FIFO_TYPE_ACK  0x0000000000000000
#define BTL_PCIE_FIFO_TYPE_SEND 0x8000000000000000

struct btl_pcie_fifo_t {
  /* number of entries in queue */
  uint32_t fifo_len;
  /* for sender: next place to write
   * for receiver: next place to read */
  uint32_t current_index;
  /* for sender: number of entries "in flight".  Must always be less
     than or equal to fifo_len */
  uint32_t num_outstanding;
  uint32_t mask;
  /* the actual buffer */
  btl_pcie_fifo_entry_t* queue;
};
typedef struct btl_pcie_fifo_t btl_pcie_fifo_t;


/**
 * Initialize fifo structure
 *
 * Initialize send/recv fifo structure.  The fifo structure does
 * double duty of maintaining both the sender and receiver.  This
 * function initializes the send view of the fifo structure, for
 * use to receive messages.  fifo_get_msg() should not be called on
 * this fifo.
 *
 * @note fifo_len must match the value given to the matching
 * fifo_init_recv(), although there are no checks to verify this.
 *
 * @param[in] fifo        A pointer to a fifo structure to be 
 *                        initialized
 * @param[in] fifo_len    Requested length of the fifo queue
 * @param[in] queue_space Space for the receive queue (remote pointer)
 *
 * @retval OMPI_SUCCESS   Everything worked
 * @retval OMPI_ERROR     Good luck!
 */
int ompi_btl_pcie_fifo_init_send(btl_pcie_fifo_t *fifo, 
				 unsigned int fifo_len,
				 void *queue_space);


/**
 * Initialize fifo structure
 *
 * Initialize send/recv fifo structure.  The fifo structure does
 * double duty of maintaining both the sender and receiver.  This
 * function initializes the receive view of the fifo structure, for
 * use to receive messages.  fifo_set_msg() should not be called on
 * this fifo.
 *
 * @note fifo_len must match the value given to the matching
 * fifo_init_send(), although there are no checks to verify this.
 *
 * @param[in] fifo        A pointer to a fifo structure to be 
 *                        initialized
 * @param[in] fifo_len    Requested length of the fifo queue
 * @param[in] queue_space Space for the receive queue (local pointer)
 * @param[in] queue_space_len Length of queue_space
 *
 * @retval OMPI_SUCCESS   Everything worked
 * @retval OMPI_ERROR     Good luck!
 */
int ompi_btl_pcie_fifo_init_recv(btl_pcie_fifo_t *fifo, 
				 unsigned int fifo_len,
				 void *queue_space,
				 size_t queue_space_len);

int ompi_btl_pcie_fifo_finalize(btl_pcie_fifo_t *fifo);


/**
 * Read a message from the queue 
 *
 * Read a message from the queue
 *
 * @param[in] fifo     The receive view of the fifo
 *
 * @return A non-zero message or 0 if no new messages are
 * available.
 */
static inline btl_pcie_fifo_entry_t
ompi_btl_pcie_fifo_get_msg(btl_pcie_fifo_t *fifo)
{
  /* BWB - TODO - if we ever want to be multi-threaded, we'll
     need to fix this */
  btl_pcie_fifo_entry_t ret = 0;
  if (0 != (ret = fifo->queue[fifo->current_index])) {
    fifo->queue[fifo->current_index] = 0;
    fifo->current_index++;
    fifo->current_index &= fifo->mask;
  }

  return ret;
}


/**
 * Write a message pointer into the queue 
 *
 * Write a message pointer into the send queue view of the fifo.
 *
 * @param[in] fifo     The send view of the fifo
 * @param[in] msg      The index to the payload to deliver
 *
 * @retval OMPI_SUCCESS Fifo successfully updated
 * @retval OMPI_ERR_RESOURCE_BUSY There was no space in the fifo
 */
static inline int
ompi_btl_pcie_fifo_set_msg(btl_pcie_fifo_t *fifo, btl_pcie_fifo_entry_t msg)
{
  uint32_t outstanding;

  /* see if we have a slot */
  outstanding = OPAL_THREAD_ADD32(&fifo->num_outstanding, 1);
  if (outstanding > fifo->fifo_len) {
    OPAL_THREAD_ADD32(&fifo->num_outstanding, -1);
    return OMPI_ERR_RESOURCE_BUSY;
  }

  /* now that we have a slot, figure out where it is.  Allow the
     outstanding to wrap around forever - just mask out the bits we
     don't care about. */
  outstanding = OPAL_THREAD_ADD32(&fifo->current_index, 1);
  outstanding &= fifo->mask;

  fifo->queue[outstanding] = msg;

  return OMPI_SUCCESS;
}


static inline int
ompi_btl_pcie_fifo_complete_msg(btl_pcie_fifo_t *fifo, 
				unsigned int num_msgs)
{
  OPAL_THREAD_ADD32(&fifo->num_outstanding, -num_msgs);
  return OMPI_SUCCESS;
}


END_C_DECLS

#endif /* BTL_PCIE_FIFO_H */
