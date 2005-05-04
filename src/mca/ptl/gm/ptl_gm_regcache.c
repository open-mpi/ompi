/*************************************************************************
 * Myricom MPICH-GM ch_gm backend                                        *
 * Copyright (c) 2001 by Myricom, Inc.                                   *
 * All rights reserved.                                                  *
 *************************************************************************/
#include "ompi_config.h"
#include "ptl_gm.h"
#include "ptl_gm_priv.h"

#if OMPI_MCA_PTL_GM_CACHE_ENABLE

#include <stdio.h>
#include <assert.h>
#include <stdlib.h>

#define GM_DISABLE_REGISTRATION 0
#define GMPI_ENABLE_REG_CACHE 1

#define GMPI_DEBUG_REG_CACHE_PRINT0(A)
#define GMPI_DEBUG_REG_CACHE_PRINT1(A,B,C)
#define GMPI_DEBUG_REG_CACHE_PRINT2(A,B,C,D,E)
#define GMPI_DEBUG_REGISTRATION_USE_SEGMENT(ADDR, LENGTH)
#define GMPI_DEBUG_DMA_MEMORY_USE(SIZE)
#define GMPI_DEBUG_DMA_MEMORY_ACQUIRE(ADDR, LENGTH)
#define GMPI_DEBUG_DMA_MEMORY_RELEASE(ADDR, LENGTH)
#define GMPI_DEBUG_DMA_MEMORY_UNUSE(SIZE)
#define GMPI_DEBUG_REGISTRATION_CLEAR_SEGMENT(ADDR, LENGTH)
#define GMPI_DEBUG_REGISTRATION_UNUSE_SEGMENT(ADDR, LENGTH)
#define GMPI_DEBUG_REGISTRATION_CLEAR_ALL_SEGMENTS()

#define gmpi_debug_assert            assert
#define gmpi_malloc_assert(A, B, C)  assert( NULL != (A) )
#define gmpi_abort(ID)               abort()
#if GM_DISABLE_REGISTRATION


void 
gmpi_clear_interval(gm_up_t start, unsigned int length)
{
  GMPI_DEBUG_REG_CACHE_PRINT2("Clear_interval", "start",
			      start, "length", length);
}

void 
gmpi_clear_all_intervals(void)
{
  GMPI_DEBUG_REG_CACHE_PRINT0("Clear_all_intervals");
}

#else

#if GMPI_ENABLE_REG_CACHE

typedef struct _entry
{
  gm_up_t addr;
  struct _entry * prev;
  struct _entry * next;
  unsigned int refcount;
} regcache_entry;


static struct gm_hash * regcache_hash = NULL;
static struct gm_lookaside * regcache_lookaside = NULL;
static regcache_entry * regcache_head = NULL;
static regcache_entry * regcache_tail = NULL;


void 
mca_ptl_gm_regcache_init(void)
{
  gmpi_debug_assert(GM_PAGE_LEN != 0);
  if (regcache_hash == NULL)
    {
      regcache_hash = gm_create_hash(gm_hash_compare_ptrs,
    				     gm_hash_hash_ptr, 0, 0,
				     4096, 0);
      regcache_lookaside = gm_create_lookaside(sizeof(regcache_entry),
					       4096);
    }

  gmpi_malloc_assert(regcache_hash,
		     "mca_ptl_gm_regcache_init",
		     "gm_create_hash: regcache page hash");
  gmpi_malloc_assert(regcache_lookaside,
		     "mca_ptl_gm_regcache_init",
		     "gm_create_lookaside: regcache entries list");
}


static void 
gmpi_regcache_deregister(struct gm_port *gmpi_gm_port, void * addr, unsigned int pages)
{
  if (pages > 0)
    {
      gm_deregister_memory(gmpi_gm_port, addr, GM_PAGE_LEN*pages);
      GMPI_DEBUG_DMA_MEMORY_RELEASE((gm_up_t)addr,GM_PAGE_LEN*pages);
    }
}


static void 
gmpi_regcache_garbage_collector(struct gm_port *gmpi_gm_port, unsigned int required)
{
  regcache_entry * entry_ptr, * next_entry;
  unsigned int count = 0;
  gm_up_t batch_addr = 0;
  unsigned int batch_pages = 0;
  
  GMPI_DEBUG_REG_CACHE_PRINT1("Garbage_collector start", "required", required);
  entry_ptr = regcache_head;
  while ((count < required) && (entry_ptr != NULL))
    {
      if (entry_ptr->refcount == 0)
	{
	  gm_hash_remove(regcache_hash, (void *)entry_ptr->addr);
	  if (batch_addr == 0)	  
	    {
	      batch_addr = entry_ptr->addr;
	      batch_pages++;
	    }
	  else
	    {
	      if (entry_ptr->addr == batch_addr+batch_pages*GM_PAGE_LEN)
		{
		  batch_pages++;
		}
	      else
		{
		  gmpi_regcache_deregister(gmpi_gm_port, (void *)batch_addr, batch_pages);
		  batch_addr = entry_ptr->addr;
		  batch_pages = 1;
		}
	    }
	  
	  count++;
	  next_entry = entry_ptr->next;
	  
	  if (regcache_head == entry_ptr)
	    regcache_head = next_entry;
	  else
	    entry_ptr->prev->next = next_entry;
	  
	  if (regcache_tail == entry_ptr)
	    regcache_tail = entry_ptr->prev;
	  else
	    entry_ptr->next->prev = entry_ptr->prev;
	  
	  gm_lookaside_free(entry_ptr);
          entry_ptr = next_entry;
        }
      else
        {
          entry_ptr = entry_ptr->next;
        }
    }
  
  if (batch_addr)
    {
      gmpi_regcache_deregister(gmpi_gm_port, (void *)batch_addr, batch_pages);
    }
  GMPI_DEBUG_REG_CACHE_PRINT2("Garbage_collector stop", "required", 
			      required, "count", count);
}


static unsigned int 
gmpi_regcache_register(struct gm_port *gmpi_gm_port, void * addr, unsigned int pages)
{
  unsigned int i;
  regcache_entry * entry_ptr;
  gm_status_t status;
  
  GMPI_DEBUG_REG_CACHE_PRINT2("Regcache_register", "addr", 
			      (gm_up_t)addr, "len", pages*GM_PAGE_LEN);
  
  if (gm_register_memory(gmpi_gm_port, addr, GM_PAGE_LEN*pages) != GM_SUCCESS)
    {
      GMPI_DEBUG_REG_CACHE_PRINT0("Regcache_register - using GC");
      gmpi_regcache_garbage_collector(gmpi_gm_port, 4096);
      if (gm_register_memory(gmpi_gm_port, addr, GM_PAGE_LEN*pages)
	  != GM_SUCCESS)
	{
	  GMPI_DEBUG_REG_CACHE_PRINT2("Register_memory failed", "start",
				      addr, "length", GM_PAGE_LEN*pages);
	  return 0;
	}
    }

  GMPI_DEBUG_DMA_MEMORY_ACQUIRE((gm_up_t)addr,GM_PAGE_LEN*pages);
  
  for (i=0; i<pages; i++)
    {
      entry_ptr = (regcache_entry *)gm_lookaside_alloc(regcache_lookaside); 
      gmpi_malloc_assert(entry_ptr,
			 "gmpi_regcache_register",
			 "gm_lookaside_alloc: regcache entry");
			       
      if (regcache_head == NULL)
	{
	  regcache_head = entry_ptr;
	}
      else
        {     
          regcache_tail->next = entry_ptr;
        }
 
      entry_ptr->prev = regcache_tail;
      entry_ptr->next = NULL;
      regcache_tail = entry_ptr;
      entry_ptr->refcount = 1;
      GMPI_DEBUG_DMA_MEMORY_USE(GM_PAGE_LEN);
      entry_ptr->addr = (gm_up_t)addr + i*GM_PAGE_LEN;
      
      status = gm_hash_insert(regcache_hash,
                              (void *)(entry_ptr->addr),
			      (void *)(entry_ptr));
      if (status != GM_SUCCESS)
	{
	  fprintf(stderr, "[%d]: gm_hash_insert failure in "
		  "gmpi_regcache_register: out of memory\n", 0 );
	  gmpi_abort (0);
	}
    }
  
  return 1;
}


unsigned int 
gmpi_use_interval(struct gm_port *gmpi_gm_port, gm_up_t start, unsigned int length)
{
  gm_up_t addr, end, batch_addr;
  unsigned int batch_pages;
  regcache_entry * entry_ptr;
 
  GMPI_DEBUG_REG_CACHE_PRINT2("Use_interval", "start",
			      start, "len", length);
  if (length == 0)
    {
      return 0;
    }
  GMPI_DEBUG_REGISTRATION_USE_SEGMENT(start, length);
  addr = start & ~(GM_PAGE_LEN-1);
  end = start + length;
  batch_addr = 0;
  batch_pages = 0;

  while (addr < end)
    {
      entry_ptr = (regcache_entry *)gm_hash_find(regcache_hash, (void *)addr);
      if (entry_ptr == NULL)
	{
	  if (batch_addr == 0)
	    {
	      batch_addr = addr;
	    }
	  batch_pages++;
	}
      else
	{	  
	  if (entry_ptr->refcount == 0)
	    {
	      GMPI_DEBUG_DMA_MEMORY_USE(GM_PAGE_LEN);
	    }
	  
	  entry_ptr->refcount++;
          if (batch_addr != 0)
	    {
	      GMPI_DEBUG_REG_CACHE_PRINT2("Use_interval batch", "batch_addr",
					  batch_addr, "batch_pages",
					  batch_pages);
	      if (gmpi_regcache_register(gmpi_gm_port, (void *)batch_addr, batch_pages) == 0)
		{
		  entry_ptr->refcount--;
		  
		  if (entry_ptr->refcount == 0)
		    {
		      GMPI_DEBUG_DMA_MEMORY_UNUSE(GM_PAGE_LEN);
		    }
		  
		  if (batch_addr > start)
		    {
		      return (batch_addr-start);
		    }
		  else
		    {
		      return 0;
		    }
		}
	      
	      batch_addr = 0;
	      batch_pages = 0; 

	      /* move the entry to the end of the list (LRU policy) */
	      if (entry_ptr != regcache_tail)
		{
		  if (entry_ptr == regcache_head)
		    {
		      gmpi_debug_assert(entry_ptr->next != NULL);
		      entry_ptr->next->prev = NULL;
		      regcache_head = entry_ptr->next;
		    }
		  else
		    {
		      gmpi_debug_assert(entry_ptr->prev != NULL);
		      gmpi_debug_assert(entry_ptr->next != NULL);
		      entry_ptr->prev->next = entry_ptr->next;
		      entry_ptr->next->prev = entry_ptr->prev;
		    }
		  
		  entry_ptr->next = NULL;
		  entry_ptr->prev = regcache_tail;
		  regcache_tail->next = entry_ptr;
                  regcache_tail = entry_ptr;
		}
	    }
	}
      addr += GM_PAGE_LEN;
    }
  
  if (batch_addr != 0)
    {
      if (gmpi_regcache_register(gmpi_gm_port, (void *)batch_addr, batch_pages) == 0)
	{
	  if (batch_addr > start)
	    {
	      return (batch_addr-start);
	    }
	  else
	    {
	      return 0;
	    }
	}
    }

  return length;
}


gm_status_t 
gmpi_unuse_interval(struct gm_port *gmpi_gm_port, gm_up_t start, unsigned int length)
{
  gm_up_t addr, end;
  regcache_entry * entry_ptr;
  
  GMPI_DEBUG_REG_CACHE_PRINT2("Unuse_interval", "start",
			      start, "length", length);
  if (length == 0)
    {
      return GM_SUCCESS;
    }
  GMPI_DEBUG_REGISTRATION_UNUSE_SEGMENT(start, length);

  addr = start & ~(GM_PAGE_LEN-1);
  end = start + length;
  
  while (addr < end)
    {
      entry_ptr = (regcache_entry *)gm_hash_find(regcache_hash, (void *)addr);
      
      gmpi_debug_assert(entry_ptr != NULL);
      gmpi_debug_assert(entry_ptr->refcount > 0);
      
      entry_ptr->refcount--;
      if (entry_ptr->refcount == 0)
	{
	  GMPI_DEBUG_DMA_MEMORY_UNUSE(GM_PAGE_LEN); 
	}
      addr += GM_PAGE_LEN;
    }
  return GM_SUCCESS;
}


void 
gmpi_clear_interval(struct gm_port *gmpi_gm_port, gm_up_t start, unsigned int length)
{
  gm_up_t addr, end, batch_addr;
  unsigned int batch_pages;
  regcache_entry * entry_ptr;
  
  GMPI_DEBUG_REG_CACHE_PRINT2("Clear_interval", "start",
			      start, "length", length);
  GMPI_DEBUG_REGISTRATION_CLEAR_SEGMENT(start, length);
				    
  if (regcache_hash != NULL)
    {
      addr = start & ~(GM_PAGE_LEN-1);
      end = start + length;
      batch_addr = 0;
      batch_pages = 0;
      
      while (addr < end)
	{
	  entry_ptr = (regcache_entry *)gm_hash_find(regcache_hash, 
						     (void *)addr);
	  if (entry_ptr != NULL)
	    {
	      gmpi_debug_assert(entry_ptr->refcount == 0);
	      if (entry_ptr->refcount > 0)
		{
		  GMPI_DEBUG_DMA_MEMORY_UNUSE(GM_PAGE_LEN);
		}
              gm_hash_remove(regcache_hash, (void *)addr);
	      
	      if (batch_addr == 0)
		batch_addr = addr;
	      batch_pages++;
	      
	      if (regcache_head == entry_ptr)
		regcache_head = entry_ptr->next;
	      else
		entry_ptr->prev->next = entry_ptr->next;
	  
	      if (regcache_tail == entry_ptr)
		regcache_tail = entry_ptr->prev;
	      else
		entry_ptr->next->prev = entry_ptr->prev;
	  
              gm_lookaside_free(entry_ptr);
	    }
	  else
	    {
	      if (batch_addr != 0)
		{
		  gmpi_regcache_deregister(gmpi_gm_port, (void *)batch_addr, batch_pages);
		  batch_addr = 0;
		  batch_pages = 0;
		}
	    }
	  addr += GM_PAGE_LEN;
	}

      if (batch_addr != 0)
	gmpi_regcache_deregister(gmpi_gm_port, (void *)batch_addr, batch_pages);
    }
}


void 
gmpi_clear_all_intervals(void)
{
  struct gm_hash *old_regcache_hash;

  GMPI_DEBUG_REG_CACHE_PRINT0("Clear_all_intervals");
  GMPI_DEBUG_REGISTRATION_CLEAR_ALL_SEGMENTS();
  
  if (regcache_hash != NULL)
    {
      old_regcache_hash = regcache_hash;
      regcache_hash = NULL;
      gm_destroy_hash (old_regcache_hash);
      gm_destroy_lookaside (regcache_lookaside);
    }
}


#else /* NO_REG_CACHE */


unsigned int
gmpi_use_interval(struct gm_port *gmpi_gm_port, gm_up_t start, unsigned int length)
{
  gm_status_c cc;

  GMPI_DEBUG_REG_CACHE_PRINT2("Use_interval", "start",
			      start, "len", length);
  if (length == 0)
    {
      return 0;
    }
  GMPI_DEBUG_REGISTRATION_USE_SEGMENT(start, length);
  
  if ((cc = gm_register_memory(gmpi_gm_port, (void*)start, length)) != GM_SUCCESS)
    {
      GMPI_DEBUG_REG_CACHE_PRINT2("Use_interval no_regcache: register failed",
				  "start", start, "len", length);
      return 0;
    }
  
  GMPI_DEBUG_DMA_MEMORY_USE(length);
  GMPI_DEBUG_DMA_MEMORY_ACQUIRE(start,length);
  return length;
}

gm_status_t 
gmpi_unuse_interval(struct gm_port *gmpi_gm_port, gm_up_t start, unsigned int length)
{
  gm_status_t cc;

  GMPI_DEBUG_REG_CACHE_PRINT2("Unuse_interval", "start",
			      start, "length", length);
  if (length == 0)
    {
      return;
    }
  GMPI_DEBUG_REGISTRATION_UNUSE_SEGMENT(start, length);

  cc = gm_deregister_memory(gmpi_gm_port, (void*)start, length);
  gmpi_debug_assert(cc == GM_SUCCESS);

  GMPI_DEBUG_DMA_MEMORY_UNUSE(length);
  GMPI_DEBUG_DMA_MEMORY_RELEASE(start,length);
  return cc;
}


void 
mca_ptl_gm_regcache_init(void)
{
  ;
}

void 
gmpi_clear_interval(gm_up_t start, unsigned int length)
{
  GMPI_DEBUG_REG_CACHE_PRINT2("Clear_interval", "start",
			      start, "length", length);
  GMPI_DEBUG_REGISTRATION_CLEAR_SEGMENT(start, length);
}

void 
gmpi_clear_all_intervals(void)
{
  GMPI_DEBUG_REG_CACHE_PRINT0("Clear_all_intervals");
  GMPI_DEBUG_REGISTRATION_CLEAR_ALL_SEGMENTS();
}

#endif

#endif

#endif  /* OMPI_MCA_PTL_GM_CACHE_ENABLE */
