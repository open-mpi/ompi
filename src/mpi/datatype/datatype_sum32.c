/*
 * $HEADER$
 */

/** @file 32-bit checksum support */

#include <stdlib.h>

#include "lam_config.h"
#include "lam/stdint.h"
#include "datatype.h"

#define IS_32BIT_ALIGNED(X) \
    (((uint32_t)(X) & (uint32_t) 3) == ((uint32_t) 0 ? 1 : 0))


/**
 * Copy data from one buffer to another and calculate a 32-bit checksum
 *
 * @param dst      pointer to the destination buffer
 * @param src      pointer to the source buffer
 * @param size     size of the buffer
 * @param state    pointer to a memcpy with checksum/CRC state structure
 * @return         the original value of dst
 *
 * This handles cumulative checksumming for arbitrary lengths and
 * address alignments as best as it can; the contents of
 * lastPartialLong and lastPartialLength are updated to reflected the
 * last partial word's value and length (in bytes) -- this should
 * allow proper handling of checksumming contiguous or noncontiguous
 * buffers via multiple calls of bcopy_csum() - Mitch
 */
void *lam_memcpy_sum32(void *restrict dst,
		       const void *restrict src,
		       size_t size, lam_memcpy_state_t * state)
{
    uint32_t *restrict p = (uint32_t *) dst;
    uint32_t *restrict q = (uint32_t *) src;
    size_t csumlen = state->size;
    size_t i;
    ssize_t csumlenresidue;
    uint32_t csum = 0;
    uint32_t temp;

    if (state->first_call) {
	state->first_call = false;
	state->partial_int = 0;
	state->partial_size = 0;
    }

    csumlenresidue = (csumlen > size) ? (csumlen - size) : 0;
    temp = state->partial_int;

    if (IS_32BIT_ALIGNED(p) && IS_32BIT_ALIGNED(q)) {
	if (state->partial_size) {
	    /* do we have enough data to fill out the partial word? */
	    if (size >= (sizeof(uint32_t) - state->partial_size)) {
		/* YES, we do... */
		memcpy(((char *) &temp + state->partial_size), q,
		       (sizeof(uint32_t) - state->partial_size));
		memcpy(p, ((char *) &temp + state->partial_size),
		       (sizeof(uint32_t) - state->partial_size));
		q = (uint32_t *) ((char *) q + sizeof(uint32_t) -
				  state->partial_size);
		p = (uint32_t *) ((char *) p + sizeof(uint32_t) -
				  state->partial_size);
		csum += (temp - state->partial_int);
		size -= sizeof(uint32_t) - state->partial_size;
		/*
                 * now we have an unaligned source and an unaligned
                 * destination
                 */
		for (; size >= sizeof(*q); size -= sizeof(*q)) {
		    memcpy(&temp, q, sizeof(temp));
		    q++;
		    csum += temp;
		    memcpy(p, &temp, sizeof(temp));
		    p++;
		}
		state->partial_size = 0;
		state->partial_int = 0;
	    } else {
		/* NO, we don't... */
		memcpy(((char *) &temp + state->partial_size), q, size);
		memcpy(p, ((char *) &temp + state->partial_size), size);
		q = (uint32_t *) ((char *) q + size);
		p = (uint32_t *) ((char *) p + size);
		csum += (temp - state->partial_int);
		state->partial_int = temp;
		state->partial_size += size;
		size = 0;
	    }
	} else {		/* fast path... */
	    size_t numLongs = size / sizeof(uint32_t);
	    for (i = 0; i < numLongs; i++) {
		csum += *q;
		*p++ = *q++;
	    }
	    state->partial_int = 0;
	    state->partial_size = 0;
	    if (IS_32BIT_ALIGNED(size) && (csumlenresidue == 0)) {
		state->sum = csum;
		return dst;
	    } else {
		size -= i * sizeof(uint32_t);
	    }
	}
    } else if (IS_32BIT_ALIGNED(q)) {
	if (state->partial_size) {
	    /* do we have enough data to fill out the partial word? */
	    if (size >= (sizeof(uint32_t) - state->partial_size)) {
		/* YES, we do... */
		memcpy(((char *) &temp + state->partial_size), q,
		       (sizeof(uint32_t) - state->partial_size));
		memcpy(p, ((char *) &temp + state->partial_size),
		       (sizeof(uint32_t) - state->partial_size));
		q = (uint32_t *) ((char *) q + sizeof(uint32_t) -
				  state->partial_size);
		p = (uint32_t *) ((char *) p + sizeof(uint32_t) -
				  state->partial_size);
		csum += (temp - state->partial_int);
		size -= sizeof(uint32_t) - state->partial_size;
		/*
                 * now we have an unaligned source and an unknown
                 * alignment for our destination
                 */
		if (IS_32BIT_ALIGNED(p)) {
		    size_t numLongs = size / sizeof(uint32_t);
		    for (i = 0; i < numLongs; i++) {
			memcpy(&temp, q, sizeof(temp));
			q++;
			csum += temp;
			*p++ = temp;
		    }
		    size -= i * sizeof(uint32_t);
		} else {
		    for (; size >= sizeof(*q); size -= sizeof(*q)) {
			memcpy(&temp, q, sizeof(temp));
			q++;
			csum += temp;
			memcpy(p, &temp, sizeof(temp));
			p++;
		    }
		}
		state->partial_int = 0;
		state->partial_size = 0;
	    } else {
		/* NO, we don't... */
		memcpy(((char *) &temp + state->partial_size), q, size);
		memcpy(p, ((char *) &temp + state->partial_size), size);
		q = (uint32_t *) ((char *) q + size);
		p = (uint32_t *) ((char *) p + size);
		csum += (temp - state->partial_int);
		state->partial_int = temp;
		state->partial_size += size;
		size = 0;
	    }
	} else {
	    for (; size >= sizeof(*q); size -= sizeof(*q)) {
		temp = *q++;
		csum += temp;
		memcpy(p, &temp, sizeof(temp));
		p++;
	    }
	    state->partial_int = 0;
	    state->partial_size = 0;
	}
    } else if (IS_32BIT_ALIGNED(p)) {
	if (state->partial_size) {
	    /* do we have enough data to fill out the partial word? */
	    if (size >= (sizeof(uint32_t) - state->partial_size)) {
		/* YES, we do... */
		memcpy(((char *) &temp + state->partial_size), q,
		       (sizeof(uint32_t) - state->partial_size));
		memcpy(p, ((char *) &temp + state->partial_size),
		       (sizeof(uint32_t) - state->partial_size));
		q = (uint32_t *) ((char *) q + sizeof(uint32_t) -
				  state->partial_size);
		p = (uint32_t *) ((char *) p + sizeof(uint32_t) -
				  state->partial_size);
		csum += (temp - state->partial_int);
		size -= sizeof(uint32_t) - state->partial_size;
		/*
                 * now we have a source of unknown alignment and a
                 * unaligned destination
                 */
		if (IS_32BIT_ALIGNED(q)) {
		    for (; size >= sizeof(*q); size -= sizeof(*q)) {
			temp = *q++;
			csum += temp;
			memcpy(p, &temp, sizeof(temp));
			p++;
		    }
		    state->partial_int = 0;
		    state->partial_size = 0;
		} else {
		    for (; size >= sizeof(*q); size -= sizeof(*q)) {
			memcpy(&temp, q, sizeof(temp));
			q++;
			csum += temp;
			memcpy(p, &temp, sizeof(temp));
			p++;
		    }
		    state->partial_size = 0;
		    state->partial_int = 0;
		}
	    } else {
		/* NO, we don't... */
		memcpy(((char *) &temp + state->partial_size), q, size);
		memcpy(p, ((char *) &temp + state->partial_size), size);
		q = (uint32_t *) ((char *) q + size);
		p = (uint32_t *) ((char *) p + size);
		csum += (temp - state->partial_int);
		state->partial_int = temp;
		state->partial_size += size;
		size = 0;
	    }
	} else {
	    for (; size >= sizeof(*q); size -= sizeof(*q)) {
		memcpy(&temp, q, sizeof(temp));
		q++;
		csum += temp;
		*p++ = temp;
	    }
	    state->partial_size = 0;
	    state->partial_int = 0;
	}
    } else {
	if (state->partial_size) {
	    /* do we have enough data to fill out the partial word? */
	    if (size >= (sizeof(uint32_t) - state->partial_size)) {
		/* YES, we do... */
		memcpy(((char *) &temp + state->partial_size), q,
		       (sizeof(uint32_t) - state->partial_size));
		memcpy(p, ((char *) &temp + state->partial_size),
		       (sizeof(uint32_t) - state->partial_size));
		q = (uint32_t *) ((char *) q + sizeof(uint32_t) -
				  state->partial_size);
		p = (uint32_t *) ((char *) p + sizeof(uint32_t) -
				  state->partial_size);
		csum += (temp - state->partial_int);
		size -= sizeof(uint32_t) - state->partial_size;
		/*
                 * now we have an unknown alignment for our source and
                 * destination
                 */
		if (IS_32BIT_ALIGNED(q) && IS_32BIT_ALIGNED(p)) {
		    size_t numLongs = size / sizeof(uint32_t);
		    for (i = 0; i < numLongs; i++) {
			csum += *q;
			*p++ = *q++;
		    }
		    size -= i * sizeof(uint32_t);
		} else {	/* safe but slower for all other alignments */
		    for (; size >= sizeof(*q); size -= sizeof(*q)) {
			memcpy(&temp, q, sizeof(temp));
			q++;
			csum += temp;
			memcpy(p, &temp, sizeof(temp));
			p++;
		    }
		}
		state->partial_int = 0;
		state->partial_size = 0;
	    } else {
		/* NO, we don't... */
		memcpy(((char *) &temp + state->partial_size), q, size);
		memcpy(p, ((char *) &temp + state->partial_size), size);
		q = (uint32_t *) ((char *) q + size);
		p = (uint32_t *) ((char *) p + size);
		csum += (temp - state->partial_int);
		state->partial_int = temp;
		state->partial_size += size;
		size = 0;
	    }
	} else {
	    for (; size >= sizeof(*q); size -= sizeof(*q)) {
		memcpy(&temp, q, sizeof(temp));
		q++;
		csum += temp;
		memcpy(p, &temp, sizeof(temp));
		p++;
	    }
	    state->partial_size = 0;
	    state->partial_int = 0;
	}
    }

    /*
     * if size is non-zero there was a bit left, less than an
     * uint32_t's worth
     */

    if ((size != 0) && (csumlenresidue == 0)) {
	temp = state->partial_int;
	if (state->partial_size) {
	    if (size >= (sizeof(uint32_t) - state->partial_size)) {
		/* copy all remaining bytes from q to p */
		uint32_t copytemp = 0;
		memcpy(&copytemp, q, size);
		memcpy(p, &copytemp, size);
		/* fill out rest of partial word and add to checksum */
		memcpy(((char *) &temp + state->partial_size), q,
		       (sizeof(uint32_t) - state->partial_size));
		/*
		 * avoid unsigned arithmetic overflow by subtracting
		 * the old partial word from the new one before adding
		 * to the checksum...
		 */
		csum += (temp - state->partial_int);
		size -= sizeof(uint32_t) - state->partial_size;
		q = (uint32_t *) ((char *) q + sizeof(uint32_t) -
				  state->partial_size);
		state->partial_size = size;
		/* reset temp, and calculate next partial word */
		temp = 0;
		if (size) {
		    memcpy(&temp, q, size);
		}
		/* add it to the the checksum */
		csum += temp;
		state->partial_int = temp;
	    } else {
		/* copy all remaining bytes from q to p */
		uint32_t copytemp = 0;
		memcpy(&copytemp, q, size);
		memcpy(p, &copytemp, size);
		/* fill out rest of partial word and add to checksum */
		memcpy(((char *) &temp + state->partial_size), q, size);
		/*
		 * avoid unsigned arithmetic overflow by subtracting
		 * the old partial word from the new one before adding
		 * to the checksum...
		 */
		csum += temp - state->partial_int;
		state->partial_int = temp;
		state->partial_size += size;
	    }
	} else {		/* fast path... */
	    /*
             * temp and state->partial_int are 0 if
             * state->partial_size is 0...
             */
	    memcpy(&temp, q, size);
	    csum += temp;
	    memcpy(p, &temp, size);
	    state->partial_int = temp;
	    state->partial_size = size;
	    /* done...return the checksum */
	}
    } else if (csumlenresidue != 0) {
	if (size != 0) {
	    temp = 0;
	    memcpy(&temp, q, size);
	    memcpy(p, &temp, size);
	}
	if (csumlenresidue <
	    (ssize_t) (sizeof(uint32_t) - size - state->partial_size)) {
	    temp = state->partial_int;
	    memcpy(((char *) &temp + state->partial_size), q,
		   (size + csumlenresidue));
	    /*
	     * avoid unsigned arithmetic overflow by subtracting the
	     * old partial word from the new one before adding to the
	     * checksum...
	     */
	    csum += temp - state->partial_int;
	    q++;
	    state->partial_int = temp;
	    state->partial_size += size + csumlenresidue;
	    csumlenresidue = 0;
	} else {
	    /*
	     * we have enough chksum data to fill out our last partial
	     * word
	     */
	    temp = state->partial_int;
	    memcpy(((char *) &temp + state->partial_size), q,
		   (sizeof(uint32_t) - state->partial_size));
	    /*
	     * avoid unsigned arithmetic overflow by subtracting the
	     * old partial word from the new one before adding to the
	     * checksum...
	     */
	    csum += temp - state->partial_int;
	    q = (uint32_t *) ((char *) q + sizeof(uint32_t) -
			      state->partial_size);
	    csumlenresidue -=
		sizeof(uint32_t) - state->partial_size - size;
	    state->partial_size = 0;
	    state->partial_int = 0;
	}
	if (IS_32BIT_ALIGNED(q)) {
	    for (i = 0; i < csumlenresidue / sizeof(uint32_t); i++) {
		csum += *q++;
	    }
	} else {
	    for (i = 0; i < csumlenresidue / sizeof(uint32_t); i++) {
		memcpy(&temp, q, sizeof(temp));
		csum += temp;
		q++;
	    }
	}
	csumlenresidue -= i * sizeof(uint32_t);
	if (csumlenresidue) {
	    temp = 0;
	    memcpy(&temp, q, csumlenresidue);
	    csum += temp;
	    state->partial_int = temp;
	    state->partial_size = csumlenresidue;
	}
    }
    /* end else if (csumlenresidue != 0) */

    state->sum = csum;

    return dst;
}
