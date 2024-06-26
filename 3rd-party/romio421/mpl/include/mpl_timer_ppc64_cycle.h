/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */

#ifndef MPL_TIMER_PPC64_CYCLE_H_INCLUDED
#define MPL_TIMER_PPC64_CYCLE_H_INCLUDED

#define SPRN_TBRU 0x10D
#define SPRN_TBRL 0x10C

static inline uint64_t tb()
{
    unsigned temp;
    union {
        struct {
            unsigned hi, lo;
        } w;
        uint64_t d;
    } result;

    do {
        asm volatile ("mfspr %0,%1":"=r" (temp):"i"(SPRN_TBRU));
        asm volatile ("mfspr %0,%1":"=r" (result.w.lo):"i"(SPRN_TBRL));
        asm volatile ("mfspr %0,%1":"=r" (result.w.hi):"i"(SPRN_TBRU));
    }
    while (temp != result.w.hi);

    return result.d;
}

static inline int MPL_wtime(MPL_time_t * timeval)
{
    *timeval = tb();
    return MPL_SUCCESS;
}

#endif /* MPL_TIMER_PPC64_CYCLE_H_INCLUDED */
