/*
 * $HEADER$
 */

/** @file
 *
 * Atomic operations.
 *
 * This API is patterned after the FreeBSD kernel atomic interface,
 * but using C99 integer types.  The FreeBSD interface is documented
 * at
 *
 * http://www.freebsd.org/cgi/man.cgi?query=atomic&sektion=9
 */

#ifndef LAM_ATOMIC_H
#define LAM_ATOMIC_H 1

#include "lam_config.h"

/*
 * prototypes (we may not implement all of this interface)
 */

int lam_atomic_cmpset_acq_int(volatile int *p, int old, int fresh);
int lam_atomic_cmpset_rel_int(volatile int *p, int old, int fresh);
int lam_atomic_load_acq_int(volatile int *p);
int lam_atomic_readandclear_int(volatile int *p);
void lam_atomic_add_acq_int(volatile int *p, int v);
void lam_atomic_add_rel_int(volatile int *p, int v);
void lam_atomic_clear_acq_int(volatile int *p, int v);
void lam_atomic_clear_rel_int(volatile int *p, int v);
void lam_atomic_set_acq_int(volatile int *p, int v);
void lam_atomic_set_rel_int(volatile int *p, int v);
void lam_atomic_store_rel_int(volatile int *p, int v);
void lam_atomic_subtract_acq_int(volatile int *p, int v);
void lam_atomic_subtract_rel_int(volatile int *p, int v);

int lam_atomic_cmpset_acq_long(volatile long *p, long old, long fresh);
int lam_atomic_cmpset_rel_long(volatile long *p, long old, long fresh);
long lam_atomic_load_acq_long(volatile long *p);
long lam_atomic_readandclear_long(volatile long *p);
void lam_atomic_add_acq_long(volatile long *p, long v);
void lam_atomic_add_rel_long(volatile long *p, long v);
void lam_atomic_clear_acq_long(volatile long *p, long v);
void lam_atomic_clear_rel_long(volatile long *p, long v);
void lam_atomic_set_acq_long(volatile long *p, long v);
void lam_atomic_set_rel_long(volatile long *p, long v);
void lam_atomic_store_rel_long(volatile long *p, long v);
void lam_atomic_subtract_acq_long(volatile long *p, long v);
void lam_atomic_subtract_rel_long(volatile long *p, long v);

int lam_atomic_cmpset_acq_ptr(volatile uintptr_t *p, uintptr_t old, uintptr_t fresh);
int lam_atomic_cmpset_rel_ptr(volatile uintptr_t *p, uintptr_t old, uintptr_t fresh);
uintptr_t lam_atomic_load_acq_ptr(volatile uintptr_t *p);
uintptr_t lam_atomic_readandclear_ptr(volatile uintptr_t *p);
void lam_atomic_add_acq_ptr(volatile uintptr_t *p, uintptr_t v);
void lam_atomic_add_rel_ptr(volatile uintptr_t *p, uintptr_t v);
void lam_atomic_clear_acq_ptr(volatile uintptr_t *p, uintptr_t v);
void lam_atomic_clear_rel_ptr(volatile uintptr_t *p, uintptr_t v);
void lam_atomic_set_acq_ptr(volatile uintptr_t *p, uintptr_t v);
void lam_atomic_set_rel_ptr(volatile uintptr_t *p, uintptr_t v);
void lam_atomic_store_rel_ptr(volatile uintptr_t *p, uintptr_t v);
void lam_atomic_subtract_acq_ptr(volatile uintptr_t *p, uintptr_t v);
void lam_atomic_subtract_rel_ptr(volatile uintptr_t *p, uintptr_t v);

int lam_atomic_cmpset_acq_uint32_t(volatile uint32_t *p, uint32_t old, uint32_t fresh);
int lam_atomic_cmpset_rel_uint32_t(volatile uint32_t *p, uint32_t old, uint32_t fresh);
uint32_t lam_atomic_load_acq_uint32_t(volatile uint32_t *p);
uint32_t lam_atomic_readandclear_uint32_t(volatile uint32_t *p);
void lam_atomic_add_acq_uint32_t(volatile uint32_t *p, uint32_t v);
void lam_atomic_add_rel_uint32_t(volatile uint32_t *p, uint32_t v);
void lam_atomic_clear_acq_uint32_t(volatile uint32_t *p, uint32_t v);
void lam_atomic_clear_rel_uint32_t(volatile uint32_t *p, uint32_t v);
void lam_atomic_set_acq_uint32_t(volatile uint32_t *p, uint32_t v);
void lam_atomic_set_rel_uint32_t(volatile uint32_t *p, uint32_t v);
void lam_atomic_store_rel_uint32_t(volatile uint32_t *p, uint32_t v);
void lam_atomic_subtract_acq_uint32_t(volatile uint32_t *p, uint32_t v);
void lam_atomic_subtract_rel_uint32_t(volatile uint32_t *p, uint32_t v);

int lam_atomic_cmpset_acq_uint64_t(volatile uint64_t *p, uint64_t old, uint64_t fresh);
int lam_atomic_cmpset_rel_uint64_t(volatile uint64_t *p, uint64_t old, uint64_t fresh);
uint64_t lam_atomic_load_acq_uint64_t(volatile uint64_t *p);
uint64_t lam_atomic_readandclear_uint64_t(volatile uint64_t *p);
void lam_atomic_add_acq_uint64_t(volatile uint64_t *p, uint64_t v);
void lam_atomic_add_rel_uint64_t(volatile uint64_t *p, uint64_t v);
void lam_atomic_clear_acq_uint64_t(volatile uint64_t *p, uint64_t v);
void lam_atomic_clear_rel_uint64_t(volatile uint64_t *p, uint64_t v);
void lam_atomic_set_acq_uint64_t(volatile uint64_t *p, uint64_t v);
void lam_atomic_set_rel_uint64_t(volatile uint64_t *p, uint64_t v);
void lam_atomic_store_rel_uint64_t(volatile uint64_t *p, uint64_t v);
void lam_atomic_subtract_acq_uint64_t(volatile uint64_t *p, uint64_t v);
void lam_atomic_subtract_rel_uint64_t(volatile uint64_t *p, uint64_t v);

/*
 * implementation (system specific)
 */

#include "os/atomic.h"

#endif /* LAM_ATOMIC_H */
