/*
 * $HEADER$
 */

#include "ompi_config.h"

#include "util/output.h"
#include "mca/mca.h"
#include "mca/base/base.h"

#include "mca/base/mca_base_msgbuf_internal.h"


/* blank prototypes for now that return unimplemented */


/* get/create a free buffer */
/* if reqsize = MCA_BASE_MSGBUF_GETBUF, then the system gives an unlimited buffer. */
/* Giving a req size just makes it more memory efficient. */
mca_base_msgbuf_t mca_base_msgbuf_new (size_t reqsize)
{
  return ((mca_base_msgbuf_t)OMPI_ERR_NOT_SUPPORTED);
}

/* make a copy of an existing buffer */
/* this is usefull for the registry and is needed as unpack is */
/* destructive */
mca_base_msgbuf_t mca_base_msgbuf_copy (mca_base_msgbuf_t* copybufid, 
                                         mca_base_msgbuf_t orgbufid)
{
  return ((mca_base_msgbuf_t)OMPI_ERR_NOT_SUPPORTED);
}

/* set a buffer. As base_pack send/recv handles buffer you might not want */
/* to pack a buffer but do a send from memory directly */
/* a free on this special buffer just frees its structure not the memory */
mca_base_msgbuf_t mca_base_msgbuf_construct (void* ptr, size_t datasize)
{
  return ((mca_base_msgbuf_t)OMPI_ERR_NOT_SUPPORTED);
}

/* explicit free of a buffer when not auto freeing them */
int mca_base_msgbuf_free (mca_base_msgbuf_t bufid)
{
  return OMPI_ERR_NOT_SUPPORTED;
}

/* pack and unpack non-string typed data */
int  mca_base_msgbuf_pack (mca_base_msgbuf_t bufid, void* ptr, size_t num_items,
                            mca_base_msgbuf_data_t datatype)
{
  return OMPI_ERR_NOT_SUPPORTED;
}

int  mca_base_msgbuf_unpack (mca_base_msgbuf_t bufid, void* ptr, 
                             size_t num_items, mca_base_msgbuf_data_t datatype)
{
  return OMPI_ERR_NOT_SUPPORTED;
}

/* handles strings */
/* note this takes NULL terminated strings and returns null terminated strings */
int  mca_base_msgbuf_pack_string (mca_base_msgbuf_t bufid, char* strptr)
{
  return OMPI_ERR_NOT_SUPPORTED;
}

int  mca_base_msgbuf_unpack_string (mca_base_msgbuf_t bufid, char* strptr, 
                                    size_t maxlen)
{
  return OMPI_ERR_NOT_SUPPORTED;
}

