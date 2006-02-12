/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "opal_config.h"

#include "opal/util/output.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_msgbuf_internal.h"
#include "opal/constants.h"

/* 
 * local prototypes 
 */
int  mca_base_msgbuf_init (void);





/*
 * local variables
 */
static bool initialized = false;


/* blank prototypes for now that return unimplemented */


/* get/create a free buffer */
/* if reqsize = MCA_BASE_MSGBUF_GETBUF, then the system gives an unlimited buffer. */
/* Giving a req size just makes it more memory efficient. */
mca_base_msgbuf_t mca_base_msgbuf_new (size_t reqsize)
{
  if (!initialized) mca_base_msgbuf_init ();
  return ((mca_base_msgbuf_t)OPAL_ERR_NOT_SUPPORTED);
}

/* make a copy of an existing buffer */
/* this is usefull for the registry and is needed as unpack is */
/* destructive */
int  mca_base_msgbuf_copy (mca_base_msgbuf_t* copybufid, 
                                         mca_base_msgbuf_t orgbufid)
{
  if (!initialized) mca_base_msgbuf_init ();
  return (OPAL_ERR_NOT_SUPPORTED);
}

/* set a buffer. As base_pack send/recv handles buffer you might not want */
/* to pack a buffer but do a send from memory directly */
/* a free on this special buffer just frees its structure not the memory */
mca_base_msgbuf_t mca_base_msgbuf_construct (void* ptr, size_t datasize)
{
  if (!initialized) mca_base_msgbuf_init ();
  return ((mca_base_msgbuf_t)OPAL_ERR_NOT_SUPPORTED);
}

/* explicit free of a buffer when not auto freeing them */
int mca_base_msgbuf_free (mca_base_msgbuf_t* bufid)
{
  if (!initialized) mca_base_msgbuf_init ();
  return OPAL_ERR_NOT_SUPPORTED;
}

/* pack and unpack non-string typed data */
int  mca_base_msgbuf_pack (mca_base_msgbuf_t bufid, void* ptr, size_t num_items,
                            mca_base_msgbuf_data_t datatype)
{
  if (!initialized) mca_base_msgbuf_init ();
  return OPAL_ERR_NOT_SUPPORTED;
}

int  mca_base_msgbuf_unpack (mca_base_msgbuf_t bufid, void* ptr, 
                             size_t num_items, mca_base_msgbuf_data_t datatype)
{
  if (!initialized) mca_base_msgbuf_init ();
  return OPAL_ERR_NOT_SUPPORTED;
}

/* handles strings */
/* note this takes NULL terminated strings and returns null terminated strings */
int  mca_base_msgbuf_pack_string (mca_base_msgbuf_t bufid, char* strptr)
{
  if (!initialized) mca_base_msgbuf_init ();
  return OPAL_ERR_NOT_SUPPORTED;
}

int  mca_base_msgbuf_unpack_string (mca_base_msgbuf_t bufid, char* strptr, 
                                    size_t maxlen)
{
  if (!initialized) mca_base_msgbuf_init ();
  return OPAL_ERR_NOT_SUPPORTED;
}


/* private functions just inside this code */

int  mca_base_msgbuf_init ()
{
  if (initialized) return (0);
  return OPAL_ERR_NOT_SUPPORTED;
}
