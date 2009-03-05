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

#include <string.h>

#include "opal/threads/mutex.h"
#include "opal/types.h"
#include "ompi/constants.h"

#include "btl_pcie_fifo.h"

static uint32_t
get_mask(unsigned int len)
{
  int pop_count, highest_used_bit, tmp_input_integer;
  unsigned int pow;

  /* init counters */
  pop_count=0;
  highest_used_bit=1;

  /* get population count and highest non-zero bit */
  tmp_input_integer = len;
  while (tmp_input_integer > 0) {
    pop_count += (tmp_input_integer & 1);
    highest_used_bit++;
    tmp_input_integer >> 1;
  }
  if (1 < pop_count) {
    /* round up */
    highest_used_bit++;
  }

  /* generate power value */
  pow = 1 << highest_used_bit;

  if (pow != len) return 0;
  return pow - 1;
}


int
ompi_btl_pcie_fifo_init_send(btl_pcie_fifo_t *fifo, 
			     unsigned int fifo_len,
			     void *queue_space)
{
  fifo->fifo_len = fifo_len;
  fifo->current_index = 0;
  fifo->num_outstanding = 0;
  fifo->mask = get_mask(fifo_len);
  fifo->queue = queue_space;

  if (fifo->mask == 0) return OMPI_ERROR;

  return OMPI_SUCCESS;
}


int
ompi_btl_pcie_fifo_init_recv(btl_pcie_fifo_t *fifo, 
			     unsigned int fifo_len,
			     void *queue_space,
			     size_t queue_space_len)
{
  fifo->fifo_len = fifo_len;
  fifo->current_index = 1;
  fifo->num_outstanding = 0;
  fifo->mask = get_mask(fifo_len);
  fifo->queue = queue_space;

  if (fifo->mask == 0) return OMPI_ERROR;

  if (fifo_len * sizeof(btl_pcie_fifo_entry_t) > queue_space_len) {
    return OMPI_ERROR;
  }

  /* initialize the queue to empty */
  memset(fifo->queue, 0, fifo_len * sizeof(btl_pcie_fifo_entry_t));

  return OMPI_SUCCESS;
}


int
ompi_btl_pcie_fifo_finalize(btl_pcie_fifo_t *fifo)
{
  return OMPI_SUCCESS;
}
