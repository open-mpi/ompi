/*
 * Copyright © 2009 CNRS, INRIA, Université Bordeaux 1
 * Copyright © 2009-2010 Cisco Systems, Inc.  All rights reserved.
 * See COPYING in top-level directory.
 */

/** \file
 * \brief The Cpuset API, for use in hwloc itself.
 */

#ifndef HWLOC_CPUSET_H
#define HWLOC_CPUSET_H

#include <hwloc/config.h>


/** \defgroup hwlocality_cpuset The Cpuset API
 *
 * For use in hwloc itself, a hwloc_cpuset_t represents a set of logical
 * processors.
 *
 * \note cpusets are indexed by OS logical processor number.
 * @{
 */


/** \brief
 * Set of CPUs represented as an opaque pointer to an internal bitmask.
 */
typedef struct hwloc_cpuset_s * hwloc_cpuset_t;
typedef const struct hwloc_cpuset_s * hwloc_const_cpuset_t;


/*
 * CPU set allocation, freeing and copying.
 */

/** \brief Allocate a new empty CPU set.  
    
    \returns A valid CPU set or NULL.

    The CPU set should be freed by a corresponding call to
    hwloc_cpuset_free(). */
HWLOC_DECLSPEC hwloc_cpuset_t hwloc_cpuset_alloc(void) __hwloc_attribute_malloc;

/** \brief Free CPU set \p set */
HWLOC_DECLSPEC void hwloc_cpuset_free(hwloc_cpuset_t set);

/** \brief Duplicate CPU set \p set by allocating a new CPU set and copying \p set's contents */
HWLOC_DECLSPEC hwloc_cpuset_t hwloc_cpuset_dup(hwloc_const_cpuset_t set) __hwloc_attribute_malloc;

/** \brief Copy the contents of CPU set \p src into the already allocated CPU set \p dst */
HWLOC_DECLSPEC void hwloc_cpuset_copy(hwloc_cpuset_t dst, hwloc_const_cpuset_t src);


/*
 * Cpuset/String Conversion
 */

/** \brief Stringify a cpuset.
 *
 * Up to \p buflen characters may be written in buffer \p buf.
 *
 * \return the number of character that were actually written if not truncating,
 * or that would have been written  (not including the ending \\0).
 */
HWLOC_DECLSPEC int hwloc_cpuset_snprintf(char * __hwloc_restrict buf, size_t buflen, hwloc_const_cpuset_t set);

/** \brief Stringify a cpuset into a newly allocated string.
 *
 * \return the number of character that were actually written
 * (not including the ending \\0).
 */
HWLOC_DECLSPEC int hwloc_cpuset_asprintf(char ** strp, hwloc_const_cpuset_t set);

/** \brief Parse a cpuset string and stores it in CPU set \p set.
 *
 * Must start and end with a digit.
 */
HWLOC_DECLSPEC int hwloc_cpuset_from_string(hwloc_cpuset_t set, const char * __hwloc_restrict string);


/*
 *  Primitives & macros for building, modifying and consulting "sets" of cpus.
 */

/** \brief Empty the CPU set \p set */
HWLOC_DECLSPEC void hwloc_cpuset_zero(hwloc_cpuset_t set);

/** \brief Fill CPU set \p set with all possible CPUs (even if those CPUs don't exist or are otherwise unavailable) */
HWLOC_DECLSPEC void hwloc_cpuset_fill(hwloc_cpuset_t set);

/** \brief Setup CPU set \p set from unsigned long \p mask */
HWLOC_DECLSPEC void hwloc_cpuset_from_ulong(hwloc_cpuset_t set, unsigned long mask);

/** \brief Setup CPU set \p set from unsigned long \p mask used as \p i -th subset */
HWLOC_DECLSPEC void hwloc_cpuset_from_ith_ulong(hwloc_cpuset_t set, unsigned i, unsigned long mask);

/** \brief Convert the beginning part of CPU set \p set into unsigned long \p mask */
HWLOC_DECLSPEC unsigned long hwloc_cpuset_to_ulong(hwloc_const_cpuset_t set) __hwloc_attribute_pure;

/** \brief Convert the \p i -th subset of CPU set \p set into unsigned long mask */
HWLOC_DECLSPEC unsigned long hwloc_cpuset_to_ith_ulong(hwloc_const_cpuset_t set, unsigned i) __hwloc_attribute_pure;

/** \brief Empty the CPU set \p set and add CPU \p cpu */
HWLOC_DECLSPEC void hwloc_cpuset_cpu(hwloc_cpuset_t set, unsigned cpu);

/** \brief Empty the CPU set \p set and add all but the CPU \p cpu */
HWLOC_DECLSPEC void hwloc_cpuset_all_but_cpu(hwloc_cpuset_t set, unsigned cpu);

/** \brief Add CPU \p cpu in CPU set \p set */
HWLOC_DECLSPEC void hwloc_cpuset_set(hwloc_cpuset_t set, unsigned cpu);

/** \brief Add CPUs from \p begincpu to \p endcpu in CPU set \p set */
HWLOC_DECLSPEC void hwloc_cpuset_set_range(hwloc_cpuset_t set, unsigned begincpu, unsigned endcpu);

/** \brief Remove CPU \p cpu from CPU set \p set */
HWLOC_DECLSPEC void hwloc_cpuset_clr(hwloc_cpuset_t set, unsigned cpu);

/** \brief Remove CPUs from \p begincpu to \p endcpu in CPU set \p set */
HWLOC_DECLSPEC void hwloc_cpuset_clr_range(hwloc_cpuset_t set, unsigned begincpu, unsigned endcpu);

/** \brief Test whether CPU \p cpu is part of set \p set */
HWLOC_DECLSPEC int hwloc_cpuset_isset(hwloc_const_cpuset_t set, unsigned cpu) __hwloc_attribute_pure;

/** \brief Test whether set \p set is empty */
HWLOC_DECLSPEC int hwloc_cpuset_iszero(hwloc_const_cpuset_t set) __hwloc_attribute_pure;

/** \brief Test whether set \p set is completely full */
HWLOC_DECLSPEC int hwloc_cpuset_isfull(hwloc_const_cpuset_t set) __hwloc_attribute_pure;

/** \brief Test whether set \p set1 is equal to set \p set2 */
HWLOC_DECLSPEC int hwloc_cpuset_isequal (hwloc_const_cpuset_t set1, hwloc_const_cpuset_t set2) __hwloc_attribute_pure;

/** \brief Test whether sets \p set1 and \p set2 intersects */
HWLOC_DECLSPEC int hwloc_cpuset_intersects (hwloc_const_cpuset_t set1, hwloc_const_cpuset_t set2) __hwloc_attribute_pure;

/** \brief Test whether set \p sub_set is part of set \p super_set */
HWLOC_DECLSPEC int hwloc_cpuset_isincluded (hwloc_const_cpuset_t sub_set, hwloc_const_cpuset_t super_set) __hwloc_attribute_pure;

/** \brief Or sets \p set1 and \p set2 and store the result in set \p res */
HWLOC_DECLSPEC void hwloc_cpuset_or (hwloc_cpuset_t res, hwloc_const_cpuset_t set1, hwloc_const_cpuset_t set2);

/** \brief And sets \p set1 and \p set2 and store the result in set \p res */
HWLOC_DECLSPEC void hwloc_cpuset_and (hwloc_cpuset_t res, hwloc_const_cpuset_t set1, hwloc_const_cpuset_t set2);

/** \brief And set \p set1 and the negation of \p set2 and store the result in set \p res */
HWLOC_DECLSPEC void hwloc_cpuset_andnot (hwloc_cpuset_t res, hwloc_const_cpuset_t set1, hwloc_const_cpuset_t set2);

/** \brief Xor sets \p set1 and \p set2 and store the result in set \p res */
HWLOC_DECLSPEC void hwloc_cpuset_xor (hwloc_cpuset_t res, hwloc_const_cpuset_t set1, hwloc_const_cpuset_t set2);

/** \brief Negate set \p set and store the result in set \p res */
HWLOC_DECLSPEC void hwloc_cpuset_not (hwloc_cpuset_t res, hwloc_const_cpuset_t set);

/** \brief Compute the first CPU (least significant bit) in CPU set \p set
 *
 * \return -1 if no CPU is set.
 */
HWLOC_DECLSPEC int hwloc_cpuset_first(hwloc_const_cpuset_t set) __hwloc_attribute_pure;

/** \brief Compute the last CPU (most significant bit) in CPU set \p set
 *
 * \return -1 if no CPU is set.
 */
HWLOC_DECLSPEC int hwloc_cpuset_last(hwloc_const_cpuset_t set) __hwloc_attribute_pure;

/** \brief Compute the next CPU in CPU set \p set which is after CPU \p prev_cpu
 *
 * \return -1 if no CPU with higher index is set.
 */
HWLOC_DECLSPEC int hwloc_cpuset_next(hwloc_const_cpuset_t set, unsigned prev_cpu) __hwloc_attribute_pure;

/** \brief Keep a single CPU among those set in CPU set \p set
 *
 * May be useful before binding so that the process does not
 * have a chance of migrating between multiple logical CPUs
 * in the original mask.
 */
HWLOC_DECLSPEC void hwloc_cpuset_singlify(hwloc_cpuset_t set);

/** \brief Compare CPU sets \p set1 and \p set2 using their lowest index CPU.
 *
 * Smaller least significant bit is smaller.
 * The empty CPU set is considered higher than anything.
 */
HWLOC_DECLSPEC int hwloc_cpuset_compare_first(hwloc_const_cpuset_t set1, hwloc_const_cpuset_t set2) __hwloc_attribute_pure;

/** \brief Compare CPU sets \p set1 and \p set2 using their highest index CPU.
 *
 * Higher most significant bit is higher.
 * The empty CPU set is considered lower than anything.
 */
HWLOC_DECLSPEC int hwloc_cpuset_compare(hwloc_const_cpuset_t set1, hwloc_const_cpuset_t set2) __hwloc_attribute_pure;

/** \brief Compute the "weight" of CPU set \p set (i.e., number of
 * CPUs that are in the set).
 *
 * \return the number of CPUs that are in the set.
 */
HWLOC_DECLSPEC int hwloc_cpuset_weight(hwloc_const_cpuset_t set) __hwloc_attribute_pure;

/** \brief Loop macro iterating on CPU set \p set
 * \hideinitializer
 *
 * \p cpu is the loop variable; it should be an unsigned int.  The
 * first iteration will set \p cpu to the lowest index CPU in the set.
 * Successive iterations will iterate through, in order, all remaining
 * CPUs that in the set.  To be specific: each iteration will return a
 * value for \p cpu such that hwloc_cpuset_isset(set, cpu) is true.
 */
#define hwloc_cpuset_foreach_begin(cpu, set) \
do { \
        for (cpu = hwloc_cpuset_first(set); \
             cpu != (__typeof__(cpu)) -1; \
             cpu = hwloc_cpuset_next(set, cpu)) { \
/** \brief End of loop. Needs a terminating ';'.
 * \hideinitializer
 *
 * \sa hwloc_cpuset_foreach_begin */
#define hwloc_cpuset_foreach_end() \
        } \
} while (0)

/** @} */

#endif /* HWLOC_CPUSET_H */
