/**
 * VampirTrace
 * http://www.tu-dresden.de/zih/vampirtrace
 *
 * Copyright (c) 2005-2008, ZIH, TU Dresden, Federal Republic of Germany
 *
 * Copyright (c) 1998-2005, Forschungszentrum Juelich, Juelich Supercomputing
 *                          Centre, Federal Republic of Germany
 *
 * See the file COPYING in the package base directory for details
 **/

#include <stdlib.h>
#include "vt_omplock.h"

struct VTLock {
  void *lock;
  long lkid;
};

#define VT_LOCKBLK_SIZE 100

struct VTLockBlock {
  struct VTLock lock[VT_LOCKBLK_SIZE];
  struct VTLockBlock *next;
  struct VTLockBlock *prev;
};

static struct VTLockBlock *head_block = 0;
static struct VTLockBlock *last_block = 0;
static struct VTLock *lastlock = 0;
static int lastidx = VT_LOCKBLK_SIZE;

static uint32_t curlkid = 0;

void vt_lock_close()
{
  struct VTLockBlock *block;

  /* free lock blocks */

  while (head_block) {
    block = head_block;
    head_block = head_block->next;
    free(block);
  }
}

uint32_t vt_lock_init(void* lock)
{
  struct VTLockBlock *new_block;

  lastidx++;
  if (lastidx >= VT_LOCKBLK_SIZE) 
    {
      if (head_block == 0 ) 
	{
	  /* first time: allocate and initialize first block */
	  new_block = (struct VTLockBlock*)malloc(sizeof(struct VTLockBlock));
	  new_block->next = 0;
	  new_block->prev = 0;
	  head_block = last_block = new_block;
	} 
      else if (last_block == 0 ) 
	{
	  /* lock list empty: re-initialize */
	  last_block = head_block;
	} 
      else 
	{
	  if (last_block->next == 0 ) 
	    {
	      /* lock list full: expand */
	      new_block = (struct VTLockBlock*)malloc(sizeof(struct VTLockBlock));
	      new_block->next = 0;
	      new_block->prev = last_block;
	      last_block->next = new_block;
	    }
	  /* use next available block */
	  last_block = last_block->next;
	}
      lastlock = &(last_block->lock[0]);
      lastidx  = 0;
    } 
  else 
    {
      lastlock++;
    }
  /* store lock information */
  lastlock->lock = lock;
  lastlock->lkid = curlkid++;
  return lastlock->lkid;
}

static struct VTLock* vt_lock_get(void* lock)
{
  int i;
  struct VTLockBlock *block;
  struct VTLock *curr;

  /* search all locks in all blocks */
  block = head_block;
  while (block) {
    curr = &(block->lock[0]);
    for (i = 0; i < VT_LOCKBLK_SIZE; ++i) 
      {
	if (curr->lock == lock) 
	  return curr;

	curr++;
      }
    block = block->next;
  }
  return 0;
}

uint32_t vt_lock_id(void* lock)
{
  return vt_lock_get(lock)->lkid;
}

void vt_lock_destroy(void* lock)
{
  /* delete lock by copying last lock in place of lock */ 

  *vt_lock_get(lock) = *lastlock;
  
  /* adjust pointer to last lock  */
  lastidx--;
  if (lastidx < 0) 
    {
      /* reached low end of block */
      if (last_block->prev) 
	{
	  /* goto previous block if existing */
	  lastidx = VT_LOCKBLK_SIZE-1;
	  lastlock = &(last_block->prev->lock[lastidx]);
	} 
      else 
	{
	  /* no previous block: re-initialize */
	  lastidx = VT_LOCKBLK_SIZE;
	  lastlock = 0;
	}
      last_block = last_block->prev;
    } 
  else 
    {
      lastlock--;
    }  
}










