/*
 * $HEADER$
 */

/* lam_dataype_t pack function(s) */

#include <stdlib.h>

#include "datatype.h"

/**
 * Incrementally generate an iovec referencing a datatype (or fragment
 * of a dataype).
 *
 * @param state         current state of the incremental pack/unpack
 * @param vec           iovec buffer
 * @param vec_count     maximum length of iovec buffer
 * @param max_bytes     maximum bytes addressed by iovec
 * @param typebuf       array of types
 * @param ntype         size of type array
 * @param type          type descriptor
 * @return              0 if complete, non-zero otherwise
 *
 * Incrementally traverse an array of datatypes and generate an iovec
 * of at most length vec_count and addressing at most max_bytes.  This
 * can be used to do a (partial) RDMA gather of the datatype array.
 *
 * The state (all members) should be initialized to 0 before the first
 * call.
 */
int lam_datatype_get_iovec(lam_pack_state_t *state,
                           struct iovec *vec,
                           size_t vec_count,
                           size_t max_bytes,
                           const void *typebuf,
                           size_t ntype,
                           lam_datatype_t *datatype)
{
    return 0;
}




/**** OLD STUFF BELOW ****/


/**
 * Incrementally generate an iovec for gathering from an array of
 * datatypes
 *
 * @param state         current state of the incremental pack/unpack
 * @param base_addr     base address for iovec offsets
 * @param vec           iovec buffer
 * @param vec_count     maximum length of iovec buffer
 * @param max_bytes     maximum bytes addressed by iovec
 * @param buf           buffer to pack into/unpack from
 * @param bufsize       size of buffer
 * @param typebuf       array of types
 * @param ntype         size of type array
 * @param type          type descriptor
 * @return              0 if complete, non-zero otherwise
 *
 * Incrementally traverse an array of datatypes and generate an iovec
 * of at most length vec_count and addressing at most max_bytes.  This
 * can be used to do a (partial) RDMA gather of the datatype array.
 *
 * The state (all members) should be initialized to 0 before the first
 * call.
 */
int lam_datatype_gather_iovec(lam_pack_state_t *state,
                              void *base_addr,
                              struct iovec *vec,
                              size_t vec_count,
                              size_t max_bytes,
                              const void *typebuf,
                              size_t ntype,
                              lam_datatype_t *datatype,
                              lam_checksum_t *checksum);

/**
 * Incrementally generate an iovec for scattering from a packed array
 * of datatypes
 *
 * @param state         current state of the incremental pack/unpack
 * @param base_addr     base address for iovec offsets
 * @param vec           iovec buffer
 * @param vec_count     maximum length of iovec buffer
 * @param max_bytes     maximum bytes addressed by iovec
 * @param buf           packed buffer
 * @param bufsize       size of buffer
 * @param typebuf       array of types
 * @param ntype         size of type array
 * @param type          type descriptor
 * @return              0 if complete, non-zero otherwise
 *
 * Incrementally copy data type arrays to/from a packed buffer.  by
 * iterating over the type and type_map until we finish or run out of
 * room.
 *
 * Incrementally traverse a packed array of datatypes and generate an
 * iovec of at most length vec_count and addressing at most max_bytes.
 * This can be used to do a (partial) RDMA scatter of the datatype
 * array.
 *
 * The state (all members) should be initialized to 0 before the first
 * call.
 */
int lam_datatype_scatter_iovec(lam_pack_state_t *state,
                               void *base_addr,
                               struct iovec *vec,
                               size_t vec_count,
                               size_t max_bytes,
                               const void *buf,
                               size_t bufsize,
                               lam_datatype_t *datatype,
                               lam_memcpy_fn_t *memcpy_fn,
                               lam_memcpy_state_t *check);


