/*
 * Copyright (c) 2006-2007 Los Alamos National Security, LLC.  All rights 
 *                         reserved. 
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include <string.h>

#include "opal/util/output.h"

#include "mpool_pcie.h"

void
mca_mpool_pcie_module_init(mca_mpool_pcie_module_t* mpool)
{
  
}


void* mca_mpool_pcie_base(mca_mpool_base_module_t* mpool)
{
    return ((mca_mpool_pcie_module_t*) mpool)->base;
}


void*
mca_mpool_pcie_alloc(mca_mpool_base_module_t* mpool, 
                     size_t size, 
                     size_t align, 
                     uint32_t flags,
                     mca_mpool_base_registration_t** registration)
{
  mca_mpool_pcie_module_t* mpool_pcie = 
      (mca_mpool_pcie_module_t*) mpool;
  void *addr;
  
  if(mpool_pcie->offset + size > mpool_pcie->len) { 
      addr = NULL;
  } else { 
      addr = (char*)mpool_pcie->base + mpool_pcie->offset;
      mpool_pcie->offset += size;
  }
  
  return addr;
}


void*
mca_mpool_pcie_realloc(mca_mpool_base_module_t* mpool, 
                       void* addr, 
                       size_t size, 
                       mca_mpool_base_registration_t** registration)
{
    /* we don't need no realloc */
    return NULL;
}


void
mca_mpool_pcie_free(mca_mpool_base_module_t* mpool, void * addr, 
                    mca_mpool_base_registration_t* registration)
{
    /* we don't need no free */
}
