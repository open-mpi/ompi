/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */

#if !defined(MPL_MATH_H_INCLUDED)
#define MPL_MATH_H_INCLUDED

/* *INDENT-ON* */
#if defined(__cplusplus)
extern "C" {
#endif
/* *INDENT-OFF* */

#define MPL_DIV_ROUNDUP(total, chunk)  ((total + (chunk - 1)) / chunk)

/* Returns int(log2(number)) */
static inline int MPL_log2(int number)
{
#ifndef MPL_HAVE_BUILTIN_CLZ
    int p = 0;

    while (number > 0) {
        number >>= 1;
        p++;
    }
    return p - 1;
#else
    /* NOTE: if number < 0, the result is undefined. Add assertion if necessary. */
    return sizeof(unsigned int) * 8 - __builtin_clz((unsigned int) number) - 1;
#endif
}

/* Returns the nearest (smaller than or equal to) power of two of a number*/
static inline int MPL_pof2(int number)
{
    if (number > 0) {
        return 1 << MPL_log2(number);
    } else {
        return 0;
    }
}

/* Returns non-zero if val is a power of two. */
static inline int MPL_is_pof2(int val)
{
#ifndef MPL_HAVE_BUILTIN_POPCOUNT
    int pof2 = 1;

    while (pof2 < val)
        pof2 *= 2;

    if (pof2 == val)
        return 1;
    else
        return 0;
#else
    return __builtin_popcount((unsigned int) val) == 1;
#endif
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

/* Round denominator to the closest integer to divide numerator completely. Rounded value lies
 * within +/- range of denominator*/
static inline int MPL_round_closest_multiple(int numerator, int denominator, int range)
{
    /* increase and decrease denominator until it divides numerator completely. Break if it takes
     * more than range iterations and return numerator */
    int iter = 1;
    int increased_val = denominator, decreased_val = denominator;

    if (numerator % denominator == 0)
        return denominator;

    while (true) {
        increased_val += 1;
        if (decreased_val > 1)
            decreased_val -= 1;

        if (numerator % decreased_val == 0)
            return decreased_val;
        else if (numerator % increased_val == 0)
            return increased_val;
        else if (iter >= range)
            return numerator;

        iter += 1;
    }
}


/* *INDENT-ON* */
#if defined(__cplusplus)
}
#endif
/* *INDENT-OFF* */

#endif /* MPL_MATH_H_INCLUDED */
