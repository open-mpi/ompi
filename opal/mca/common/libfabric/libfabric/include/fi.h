/*
 * Copyright (c) 2013-2014 Intel Corporation. All rights reserved.
 *
 * This software is available to you under a choice of one of two
 * licenses.  You may choose to be licensed under the terms of the GNU
 * General Public License (GPL) Version 2, available from the file
 * COPYING in the main directory of this source tree, or the
 * BSD license below:
 *
 *     Redistribution and use in source and binary forms, with or
 *     without modification, are permitted provided that the following
 *     conditions are met:
 *
 *      - Redistributions of source code must retain the above
 *        copyright notice, this list of conditions and the following
 *        disclaimer.
 *
 *      - Redistributions in binary form must reproduce the above
 *        copyright notice, this list of conditions and the following
 *        disclaimer in the documentation and/or other materials
 *        provided with the distribution.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
 * BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
 * ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

#ifndef _FI_H_
#define _FI_H_

#if HAVE_CONFIG_H
#  include <config.h>
#endif /* HAVE_CONFIG_H */

#include <string.h>
#include <pthread.h>

#include <rdma/fabric.h>
#include <rdma/fi_prov.h>
#include <rdma/fi_atomic.h>
#include <rdma/fi_log.h>

#ifdef __APPLE__
#include <osx/osd.h>
#else
#include <linux/osd.h>
#endif

#ifdef HAVE_ATOMICS
#  include <stdatomic.h>
#endif

#ifdef __cplusplus
extern "C" {
#endif

#ifdef INCLUDE_VALGRIND
#   include <valgrind/memcheck.h>
#   ifndef VALGRIND_MAKE_MEM_DEFINED
#      warning "Valgrind requested, but VALGRIND_MAKE_MEM_DEFINED undefined"
#   endif
#endif

#ifndef VALGRIND_MAKE_MEM_DEFINED
#   define VALGRIND_MAKE_MEM_DEFINED(addr, len)
#endif

#if __BYTE_ORDER == __LITTLE_ENDIAN
#ifndef htonll
static inline uint64_t htonll(uint64_t x) { return bswap_64(x); }
#endif
#ifndef ntohll
static inline uint64_t ntohll(uint64_t x) { return bswap_64(x); }
#endif
#else
#ifndef htonll
static inline uint64_t htonll(uint64_t x) { return x; }
#endif
#ifndef ntohll
static inline uint64_t ntohll(uint64_t x) { return x; }
#endif
#endif

#define sizeof_field(type, field) sizeof(((type *)0)->field)

#define MIN(a, b) ((a) < (b) ? a : b)
#define MAX(a, b) ((a) > (b) ? a : b)

/* Restrict to size of struct fi_context */
struct fi_prov_context {
	int disable_logging;
};

struct fi_filter {
	char **names;
	int negated;
};

extern struct fi_filter prov_log_filter;

void fi_create_filter(struct fi_filter *filter, const char *env_name);
void fi_free_filter(struct fi_filter *filter);
int fi_apply_filter(struct fi_filter *filter, const char *name);

void fi_log_init(void);
void fi_log_fini(void);


/* flsll is defined on BSD systems, but is different. */
static inline int fi_flsll(long long int i)
{
	return i ? 65 - ffsll(htonll(i)) : 0;
}

static inline uint64_t roundup_power_of_two(uint64_t n)
{
	return 1ULL << fi_flsll(n - 1);
}

#define FI_TAG_GENERIC	0xAAAAAAAAAAAAAAAAULL


#if PT_LOCK_SPIN == 1

#define fastlock_t pthread_spinlock_t
#define fastlock_init(lock) pthread_spin_init(lock, PTHREAD_PROCESS_PRIVATE)
#define fastlock_destroy(lock) pthread_spin_destroy(lock)
#define fastlock_acquire(lock) pthread_spin_lock(lock)
#define fastlock_release(lock) pthread_spin_unlock(lock)

#else

#define fastlock_t pthread_mutex_t
#define fastlock_init(lock) pthread_mutex_init(lock, NULL)
#define fastlock_destroy(lock) pthread_mutex_destroy(lock)
#define fastlock_acquire(lock) pthread_mutex_lock(lock)
#define fastlock_release(lock) pthread_mutex_unlock(lock)

#endif /* PT_LOCK_SPIN */


#ifdef HAVE_ATOMICS
typedef atomic_int atomic_t;

static inline int atomic_inc(atomic_t *atomic)
{
	return atomic_fetch_add_explicit(atomic, 1, memory_order_acq_rel) + 1;
}

static inline int atomic_dec(atomic_t *atomic)
{
	return atomic_fetch_sub_explicit(atomic, 1, memory_order_acq_rel) - 1;
}

static inline int atomic_set(atomic_t *atomic, int value)
{
	atomic_store(atomic, value);
	return value;
}

static inline int atomic_get(atomic_t *atomic)
{
	return atomic_load(atomic);
}

#else

typedef struct { fastlock_t lock; int val; } atomic_t;

static inline int atomic_inc(atomic_t *atomic)
{
	int v;

	fastlock_acquire(&atomic->lock);
	v = ++(atomic->val);
	fastlock_release(&atomic->lock);
	return v;
}

static inline int atomic_dec(atomic_t *atomic)
{
	int v;

	fastlock_acquire(&atomic->lock);
	v = --(atomic->val);
	fastlock_release(&atomic->lock);
	return v;
}

static inline int atomic_set(atomic_t *atomic, int value)
{
	fastlock_acquire(&atomic->lock);
	atomic->val = value;
	fastlock_release(&atomic->lock);
	return value;
}

static inline void atomic_init(atomic_t *atomic, int value)
{
	fastlock_init(&atomic->lock);
	atomic->val = value;
}

static inline int atomic_get(atomic_t *atomic)
{
	return atomic->val;
}

#endif // HAVE_ATOMICS

/* non exported symbols */
int fi_read_file(const char *dir, const char *file, char *buf, size_t size);
int fi_poll_fd(int fd, int timeout);
int fi_wait_cond(pthread_cond_t *cond, pthread_mutex_t *mut, int timeout);

size_t fi_datatype_size(enum fi_datatype datatype);
uint64_t fi_tag_bits(uint64_t mem_tag_format);
uint64_t fi_tag_format(uint64_t tag_bits);

int fi_send_allowed(uint64_t caps);
int fi_recv_allowed(uint64_t caps);
int fi_rma_initiate_allowed(uint64_t caps);
int fi_rma_target_allowed(uint64_t caps);

uint64_t fi_gettime_ms(void);

#define RDMA_CONF_DIR  SYSCONFDIR "/" RDMADIR
#define FI_CONF_DIR RDMA_CONF_DIR "/fabric"

#define DEFAULT_ABI "FABRIC_1.0"

#if  HAVE_ALIAS_ATTRIBUTE == 1
#define DEFAULT_SYMVER_PRE(a) a##_
#else
#define DEFAULT_SYMVER_PRE(a) a
#endif

/* symbol -> external symbol mappings */
#ifdef HAVE_SYMVER_SUPPORT

#  define SYMVER(name, api, ver) \
        asm(".symver " #name "," #api "@" #ver)
#  define DEFAULT_SYMVER(name, api) \
        asm(".symver " #name "," #api "@@" DEFAULT_ABI)
#else
#  define SYMVER(Name, api, ver)
#if  HAVE_ALIAS_ATTRIBUTE == 1
#  define DEFAULT_SYMVER(name, api) \
        extern typeof (name) api __attribute__((alias(#name)));
#else
#  define DEFAULT_SYMVER(name, api)
#endif  /* HAVE_ALIAS_ATTRIBUTE == 1*/

#endif /* HAVE_SYMVER_SUPPORT */

#ifdef __cplusplus
}
#endif

#endif /* _FI_H_ */
