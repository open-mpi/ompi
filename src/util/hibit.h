/*
 * $HEADER$
 */

#ifndef LAM_HIBIT_H
#define LAM_HIBIT_H

/**
 * Calculates the highest bit in an integer
 *
 * @param value The integer value to examine
 * @param start Position to start looking
 *
 * @returns pos Position of highest-set integer or -1 if none are set.
 *
 * Look at the integer "value" starting at position "start", and move
 * to the right.  Return the index of the highest bit that is set to
 * 1.
 *
 * WARNING: *NO* error checking is performed.  This is meant to be a
 * fast inline function.
 */
static inline int lam_hibit(int value, int start)
{
  unsigned int mask;

  --start;
  mask = 1 << start;

  for (; start >= 0; --start, mask >>= 1) {
    if (value & mask) 
      break;
  }
  
  return start;
}

#endif /* LAM_HIBIT_H */
