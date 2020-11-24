/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil ; -*- */
/*
 *  (C) 2001 by Argonne National Laboratory.
 *      See COPYRIGHT in top-level directory.
 */

#if !defined(MPL_MATH_H_INCLUDED)
#define MPL_MATH_H_INCLUDED

/* *INDENT-ON* */
#if defined(__cplusplus)
extern "C" {
#endif
/* *INDENT-OFF* */

/* Returns the nearest (smaller than or equal to) power of two of a number*/
static inline int MPL_pof2(int number)
{
    int pof2 = 1;

    while (pof2 <= number)
        pof2 <<= 1;
    pof2 >>= 1;

    return pof2;
}

/* Returns non-zero if val is a power of two.  If ceil_pof2 is non-NULL, it sets
   *ceil_pof2 to the power of two that is just larger than or equal to val.
   That is, it rounds up to the nearest power of two. */
static inline int MPL_is_pof2(int val, int *ceil_pof2)
{
    int pof2 = 1;

    while (pof2 < val)
        pof2 *= 2;
    if (ceil_pof2)
        *ceil_pof2 = pof2;

    if (pof2 == val)
        return 1;
    else
        return 0;
}

/* Routine to calculate log_k of an integer */
static inline int MPL_ilog(int k, int number)
{
    int i = 1, p = k - 1;

    for (; p - 1 < number; i++)
        p *= k;

    return i;
}

/* Routing to calculate base^exp for integers */
static inline int MPL_ipow(int base, int exp)
{
    int result = 1;

    while (exp) {
        if (exp & 1)
            result *= base;

        exp >>= 1;
        base *= base;
    }

    return result;
}

/* get the number at 'digit'th location in base k representation of 'number' */
static inline int MPL_getdigit(int k, int number, int digit)
{
    return (number / MPL_ipow(k, digit)) % k;
}

/* set the number at 'digit'the location in base k representation of 'number' to newdigit */
static inline int MPL_setdigit(int k, int number, int digit, int newdigit)
{
    int res = number;
    int lshift = MPL_ipow(k, digit);
    res -= MPL_getdigit(k, number, digit) * lshift;
    res += newdigit * lshift;
    return res;
}

/* Implements the "mirror permutation" of "bits" low-order bits of an integer "x".
 *
 * positions 76543210, bits==3 yields 76543012.
 */
ATTRIBUTE((const)) /* tells the compiler that this func only depends on its args
                      and may be optimized much more aggressively, similar to "pure" */
static inline int MPL_mirror_permutation(unsigned int x, int bits)
{
    /* a mask for the high order bits that should be copied as-is */
    int high_mask = ~((0x1 << bits) - 1);
    int retval = x & high_mask;
    int i;

    for (i = 0; i < bits; ++i) {
        unsigned int bitval = (x & (0x1 << i)) >> i; /* 0x1 or 0x0 */
        retval |= bitval << ((bits - i) - 1);
    }

    return retval;
}

/* *INDENT-ON* */
#if defined(__cplusplus)
}
#endif
/* *INDENT-OFF* */

#endif /* MPL_MATH_H_INCLUDED */
