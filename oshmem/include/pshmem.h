/*
 * Copyright (c) 2014-2016 Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2014      Intel, Inc. All rights reserved
 * Copyright (c) 2016      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef PSHMEM_SHMEM_H
#define PSHMEM_SHMEM_H


#include <stddef.h>     /* include for ptrdiff_t */
#include <stdint.h>     /* include for fixed width types */
#if defined(c_plusplus) || defined(__cplusplus)
#    include <complex>
#    define OSHMEM_COMPLEX_TYPE(type)    std::complex<type>
#else
#    include <complex.h>
#    define OSHMEM_COMPLEX_TYPE(type)    type complex
#endif


#ifndef OSHMEM_DECLSPEC
#  if defined(OPAL_C_HAVE_VISIBILITY) && (OPAL_C_HAVE_VISIBILITY == 1)
#     define OSHMEM_DECLSPEC __attribute__((visibility("default")))
#  else
#     define OSHMEM_DECLSPEC
#  endif
#endif

#if defined(__STDC_VERSION__) && (__STDC_VERSION__ >= 201112L)
#define OSHMEMP_HAVE_C11 1
#else
#define OSHMEMP_HAVE_C11 0
#endif

#include <pshmemx.h>

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/*
 * Profiling API
 */

/*
 * Initialization routines
 */
OSHMEM_DECLSPEC  void pshmem_init(void);
OSHMEM_DECLSPEC  void pshmem_global_exit(int status);

/*
 * Finalization routines
 */
OSHMEM_DECLSPEC  void pshmem_finalize(void);

/*
 * Query routines
 */
OSHMEM_DECLSPEC  int pshmem_n_pes(void);
OSHMEM_DECLSPEC  int pshmem_my_pe(void);


/*
 * Accessability routines
 */
OSHMEM_DECLSPEC int pshmem_pe_accessible(int pe);
OSHMEM_DECLSPEC int pshmem_addr_accessible(const void *addr, int pe);

/*
 * Symmetric heap routines
 */
OSHMEM_DECLSPEC  void* pshmem_malloc(size_t size);
OSHMEM_DECLSPEC  void* pshmem_align(size_t align, size_t size);
OSHMEM_DECLSPEC  void* pshmem_realloc(void *ptr, size_t size);
OSHMEM_DECLSPEC  void pshmem_free(void* ptr);

/*
 * Remote pointer operations
 */
OSHMEM_DECLSPEC  void *pshmem_ptr(const void *ptr, int pe);

/*
 * Elemental put routines
 */
OSHMEM_DECLSPEC  void pshmem_char_p(char* addr, char value, int pe);
OSHMEM_DECLSPEC  void pshmem_short_p(short* addr, short value, int pe);
OSHMEM_DECLSPEC  void pshmem_int_p(int* addr, int value, int pe);
OSHMEM_DECLSPEC  void pshmem_long_p(long* addr, long value, int pe);
OSHMEM_DECLSPEC  void pshmem_float_p(float* addr, float value, int pe);
OSHMEM_DECLSPEC  void pshmem_double_p(double* addr, double value, int pe);
OSHMEM_DECLSPEC  void pshmem_longlong_p(long long* addr, long long value, int pe);
OSHMEM_DECLSPEC  void pshmem_longdouble_p(long double* addr, long double value, int pe);
#if OSHMEMP_HAVE_C11
#define pshmem_p(dst, val, pe)                                \
    _Generic(&*(dst),                                         \
            char*:        pshmem_char_p,                      \
            short*:       pshmem_short_p,                     \
            int*:         pshmem_int_p,                       \
            long*:        pshmem_long_p,                      \
            long long*:   pshmem_longlong_p,                  \
            float*:       pshmem_float_p,                     \
            double*:      pshmem_double_p,                    \
            long double*: pshmem_longdouble_p)(dst, val, pe)
#endif

/*
 * Block data put routines
 */
OSHMEM_DECLSPEC  void pshmem_char_put(char *target, const char *source, size_t len, int pe);
OSHMEM_DECLSPEC  void pshmem_short_put(short *target, const short *source, size_t len, int pe);
OSHMEM_DECLSPEC  void pshmem_int_put(int* target, const int* source, size_t len, int pe);
OSHMEM_DECLSPEC  void pshmem_long_put(long *target, const long *source, size_t len, int pe);
OSHMEM_DECLSPEC  void pshmem_float_put(float *target, const float *source, size_t len, int pe);
OSHMEM_DECLSPEC  void pshmem_double_put(double *target, const double *source, size_t len, int pe);
OSHMEM_DECLSPEC  void pshmem_longlong_put(long long *target, const long long *source, size_t len, int pe);
OSHMEM_DECLSPEC  void pshmem_longdouble_put(long double *target, const long double *source, size_t len, int pe);
#if OSHMEMP_HAVE_C11
#define pshmem_put(dst, src, len, pe)               \
    _Generic(&*(dst),                               \
            char*:        pshmem_char_put,          \
            short*:       pshmem_short_put,         \
            int*:         pshmem_int_put,           \
            long*:        pshmem_long_put,          \
            long long*:   pshmem_longlong_put,      \
            float*:       pshmem_float_put,         \
            double*:      pshmem_double_put,        \
            long double*: pshmem_longdouble_put)(dst, src, len, pe)
#endif

OSHMEM_DECLSPEC  void pshmem_put8(void *target, const void *source, size_t len, int pe);
OSHMEM_DECLSPEC  void pshmem_put16(void *target, const void *source, size_t len, int pe);
OSHMEM_DECLSPEC  void pshmem_put32(void *target, const void *source, size_t len, int pe);
OSHMEM_DECLSPEC  void pshmem_put64(void *target, const void *source, size_t len, int pe);
OSHMEM_DECLSPEC  void pshmem_put128(void *target, const void *source, size_t len, int pe);
OSHMEM_DECLSPEC  void pshmem_putmem(void *target, const void *source, size_t len, int pe);

/*
 * Strided put routines
 */
OSHMEM_DECLSPEC void pshmem_char_iput(char* target, const char* source, ptrdiff_t tst, ptrdiff_t sst,size_t len, int pe);
OSHMEM_DECLSPEC void pshmem_short_iput(short* target, const short* source, ptrdiff_t tst, ptrdiff_t sst,size_t len, int pe);
OSHMEM_DECLSPEC void pshmem_int_iput(int* target, const int* source, ptrdiff_t tst, ptrdiff_t sst,size_t len, int pe);
OSHMEM_DECLSPEC void pshmem_long_iput(long* target, const long* source, ptrdiff_t tst, ptrdiff_t sst,size_t len, int pe);
OSHMEM_DECLSPEC void pshmem_float_iput(float* target, const float* source, ptrdiff_t tst, ptrdiff_t sst,size_t len, int pe);
OSHMEM_DECLSPEC void pshmem_double_iput(double* target, const double* source, ptrdiff_t tst, ptrdiff_t sst,size_t len, int pe);
OSHMEM_DECLSPEC void pshmem_longlong_iput(long long* target, const long long* source, ptrdiff_t tst, ptrdiff_t sst,size_t len, int pe);
OSHMEM_DECLSPEC void pshmem_longdouble_iput(long double* target, const long double* source, ptrdiff_t tst, ptrdiff_t sst,size_t len, int pe);
#if OSHMEMP_HAVE_C11
#define pshmem_iput(dst, src, tst, sst, len, pe)     \
    _Generic(&*(dst),                                \
            char*:        pshmem_char_iput,          \
            short*:       pshmem_short_iput,         \
            int*:         pshmem_int_iput,           \
            long*:        pshmem_long_iput,          \
            long long*:   pshmem_longlong_iput,      \
            float*:       pshmem_float_iput,         \
            double*:      pshmem_double_iput,        \
            long double*: pshmem_longdouble_iput)(dst, src, tst, sst, len, pe)
#endif

OSHMEM_DECLSPEC void pshmem_iput8(void* target, const void* source, ptrdiff_t tst, ptrdiff_t sst,size_t len, int pe);
OSHMEM_DECLSPEC void pshmem_iput16(void* target, const void* source, ptrdiff_t tst, ptrdiff_t sst,size_t len, int pe);
OSHMEM_DECLSPEC void pshmem_iput32(void* target, const void* source, ptrdiff_t tst, ptrdiff_t sst,size_t len, int pe);
OSHMEM_DECLSPEC void pshmem_iput64(void* target, const void* source, ptrdiff_t tst, ptrdiff_t sst,size_t len, int pe);
OSHMEM_DECLSPEC void pshmem_iput128(void* target, const void* source, ptrdiff_t tst, ptrdiff_t sst,size_t len, int pe);

/*
 * Nonblocking put routines
 */
OSHMEM_DECLSPEC  void pshmem_putmem_nbi(void *target, const void *source, size_t len, int pe);
OSHMEM_DECLSPEC  void pshmem_char_put_nbi(char *target, const char *source, size_t len, int pe);
OSHMEM_DECLSPEC  void pshmem_short_put_nbi(short *target, const short *source, size_t len, int pe);
OSHMEM_DECLSPEC  void pshmem_int_put_nbi(int* target, const int* source, size_t len, int pe);
OSHMEM_DECLSPEC  void pshmem_long_put_nbi(long *target, const long *source, size_t len, int pe);
OSHMEM_DECLSPEC  void pshmem_longlong_put_nbi(long long *target, const long long *source, size_t len, int pe);
OSHMEM_DECLSPEC  void pshmem_float_put_nbi(float *target, const float *source, size_t len, int pe);
OSHMEM_DECLSPEC  void pshmem_double_put_nbi(double *target, const double *source, size_t len, int pe);
OSHMEM_DECLSPEC  void pshmem_longdouble_put_nbi(long double *target, const long double *source, size_t len, int pe);
#if OSHMEMP_HAVE_C11
#define pshmem_put_nbi(dst, src, len, pe)               \
    _Generic(&*(dst),                                   \
            char*:        pshmem_char_put_nbi,          \
            short*:       pshmem_short_put_nbi,         \
            int*:         pshmem_int_put_nbi,           \
            long*:        pshmem_long_put_nbi,          \
            long long*:   pshmem_longlong_put_nbi,      \
            float*:       pshmem_float_put_nbi,         \
            double*:      pshmem_double_put_nbi,        \
            long double*: pshmem_longdouble_put_nbi)(dst, src, len, pe)
#endif

OSHMEM_DECLSPEC  void pshmem_put8_nbi(void *target, const void *source, size_t len, int pe);
OSHMEM_DECLSPEC  void pshmem_put16_nbi(void *target, const void *source, size_t len, int pe);
OSHMEM_DECLSPEC  void pshmem_put32_nbi(void *target, const void *source, size_t len, int pe);
OSHMEM_DECLSPEC  void pshmem_put64_nbi(void *target, const void *source, size_t len, int pe);
OSHMEM_DECLSPEC  void pshmem_put128_nbi(void *target, const void *source, size_t len, int pe);

/*
 * Elemental get routines
 */
OSHMEM_DECLSPEC  char pshmem_char_g(const char* addr, int pe);
OSHMEM_DECLSPEC  short pshmem_short_g(const short* addr, int pe);
OSHMEM_DECLSPEC  int pshmem_int_g(const int* addr, int pe);
OSHMEM_DECLSPEC  long pshmem_long_g(const long* addr, int pe);
OSHMEM_DECLSPEC  float pshmem_float_g(const float* addr, int pe);
OSHMEM_DECLSPEC  double pshmem_double_g(const double* addr, int pe);
OSHMEM_DECLSPEC  long long pshmem_longlong_g(const long long* addr, int pe);
OSHMEM_DECLSPEC  long double pshmem_longdouble_g(const long double* addr, int pe);
#if OSHMEMP_HAVE_C11
#define pshmem_g(addr, pe)                                    \
    _Generic(&*(dst),                                         \
            char*:        pshmem_char_g,                      \
            short*:       pshmem_short_g,                     \
            int*:         pshmem_int_g,                       \
            long*:        pshmem_long_g,                      \
            long long*:   pshmem_longlong_g,                  \
            float*:       pshmem_float_g,                     \
            double*:      pshmem_double_g,                    \
            long double*: pshmem_longdouble_g)(addr, pe)
#endif

/*
 * Block data get routines
 */
OSHMEM_DECLSPEC  void pshmem_char_get(char *target, const char *source, size_t len, int pe);
OSHMEM_DECLSPEC  void pshmem_short_get(short *target, const short *source, size_t len, int pe);
OSHMEM_DECLSPEC  void pshmem_int_get(int *target, const int *source, size_t len, int pe);
OSHMEM_DECLSPEC  void pshmem_long_get(long *target, const long *source, size_t len, int pe);
OSHMEM_DECLSPEC  void pshmem_float_get(float *target, const float *source, size_t len, int pe);
OSHMEM_DECLSPEC  void pshmem_double_get(double *target, const double *source, size_t len, int pe);
OSHMEM_DECLSPEC  void pshmem_longlong_get(long long *target, const long long *source, size_t len, int pe);
OSHMEM_DECLSPEC  void pshmem_longdouble_get(long double *target, const long double *source, size_t len, int pe);
#if OSHMEMP_HAVE_C11
#define pshmem_get(dst, src, len, pe)               \
    _Generic(&*(dst),                              \
            char*:        pshmem_char_get,          \
            short*:       pshmem_short_get,         \
            int*:         pshmem_int_get,           \
            long*:        pshmem_long_get,          \
            long long*:   pshmem_longlong_get,      \
            float*:       pshmem_float_get,         \
            double*:      pshmem_double_get,        \
            long double*: pshmem_longdouble_get)(dst, src, len, pe)
#endif

OSHMEM_DECLSPEC  void pshmem_get8(void *target, const void *source, size_t len, int pe);
OSHMEM_DECLSPEC  void pshmem_get16(void *target, const void *source, size_t len, int pe);
OSHMEM_DECLSPEC  void pshmem_get32(void *target, const void *source, size_t len, int pe);
OSHMEM_DECLSPEC  void pshmem_get64(void *target, const void *source, size_t len, int pe);
OSHMEM_DECLSPEC  void pshmem_get128(void *target, const void *source, size_t len, int pe);
OSHMEM_DECLSPEC  void pshmem_getmem(void *target, const void *source, size_t len, int pe);

/*
 * Strided get routines
 */
OSHMEM_DECLSPEC void pshmem_char_iget(char* target, const char* source, ptrdiff_t tst, ptrdiff_t sst,size_t len, int pe);
OSHMEM_DECLSPEC void pshmem_short_iget(short* target, const short* source, ptrdiff_t tst, ptrdiff_t sst,size_t len, int pe);
OSHMEM_DECLSPEC void pshmem_int_iget(int* target, const int* source, ptrdiff_t tst, ptrdiff_t sst,size_t len, int pe);
OSHMEM_DECLSPEC void pshmem_float_iget(float* target, const float* source, ptrdiff_t tst, ptrdiff_t sst,size_t len, int pe);
OSHMEM_DECLSPEC void pshmem_double_iget(double* target, const double* source, ptrdiff_t tst, ptrdiff_t sst,size_t len, int pe);
OSHMEM_DECLSPEC void pshmem_longlong_iget(long long* target, const long long* source, ptrdiff_t tst, ptrdiff_t sst,size_t len, int pe);
OSHMEM_DECLSPEC void pshmem_longdouble_iget(long double* target, const long double* source, ptrdiff_t tst, ptrdiff_t sst,size_t len, int pe);
OSHMEM_DECLSPEC void pshmem_long_iget(long* target, const long* source, ptrdiff_t tst, ptrdiff_t sst,size_t len, int pe);
#if OSHMEMP_HAVE_C11
#define pshmem_iget(dst, src, tst, sst, len, pe)     \
    _Generic(&*(dst),                               \
            char*:        pshmem_char_iget,          \
            short*:       pshmem_short_iget,         \
            int*:         pshmem_int_iget,           \
            long*:        pshmem_long_iget,          \
            long long*:   pshmem_longlong_iget,      \
            float*:       pshmem_float_iget,         \
            double*:      pshmem_double_iget,        \
            long double*: pshmem_longdouble_iget)(dst, src, tst, sst, len, pe)
#endif

OSHMEM_DECLSPEC void pshmem_iget8(void* target, const void* source, ptrdiff_t tst, ptrdiff_t sst,size_t len, int pe);
OSHMEM_DECLSPEC void pshmem_iget16(void* target, const void* source, ptrdiff_t tst, ptrdiff_t sst,size_t len, int pe);
OSHMEM_DECLSPEC void pshmem_iget32(void* target, const void* source, ptrdiff_t tst, ptrdiff_t sst,size_t len, int pe);
OSHMEM_DECLSPEC void pshmem_iget64(void* target, const void* source, ptrdiff_t tst, ptrdiff_t sst,size_t len, int pe);
OSHMEM_DECLSPEC void pshmem_iget128(void* target, const void* source, ptrdiff_t tst, ptrdiff_t sst,size_t len, int pe);

/*
 * Nonblocking data get routines
 */
OSHMEM_DECLSPEC  void pshmem_getmem_nbi(void *target, const void *source, size_t len, int pe);
OSHMEM_DECLSPEC  void pshmem_char_get_nbi(char *target, const char *source, size_t len, int pe);
OSHMEM_DECLSPEC  void pshmem_short_get_nbi(short *target, const short *source, size_t len, int pe);
OSHMEM_DECLSPEC  void pshmem_int_get_nbi(int *target, const int *source, size_t len, int pe);
OSHMEM_DECLSPEC  void pshmem_long_get_nbi(long *target, const long *source, size_t len, int pe);
OSHMEM_DECLSPEC  void pshmem_longlong_get_nbi(long long *target, const long long *source, size_t len, int pe);
OSHMEM_DECLSPEC  void pshmem_float_get_nbi(float *target, const float *source, size_t len, int pe);
OSHMEM_DECLSPEC  void pshmem_double_get_nbi(double *target, const double *source, size_t len, int pe);
OSHMEM_DECLSPEC  void pshmem_longdouble_get_nbi(long double *target, const long double *source, size_t len, int pe);
#if OSHMEMP_HAVE_C11
#define pshmem_get_nbi(dst, src, len, pe)               \
    _Generic(&*(dst),                                  \
            char*:        pshmem_char_get_nbi,          \
            short*:       pshmem_short_get_nbi,         \
            int*:         pshmem_int_get_nbi,           \
            long*:        pshmem_long_get_nbi,          \
            long long*:   pshmem_longlong_get_nbi,      \
            float*:       pshmem_float_get_nbi,         \
            double*:      pshmem_double_get_nbi,        \
            long double*: pshmem_longdouble_get_nbi)(dst, src, len, pe)
#endif

OSHMEM_DECLSPEC  void pshmem_get8_nbi(void *target, const void *source, size_t len, int pe);
OSHMEM_DECLSPEC  void pshmem_get16_nbi(void *target, const void *source, size_t len, int pe);
OSHMEM_DECLSPEC  void pshmem_get32_nbi(void *target, const void *source, size_t len, int pe);
OSHMEM_DECLSPEC  void pshmem_get64_nbi(void *target, const void *source, size_t len, int pe);
OSHMEM_DECLSPEC  void pshmem_get128_nbi(void *target, const void *source, size_t len, int pe);

/*
 * Atomic operations
 */
/* Atomic swap */
OSHMEM_DECLSPEC int pshmem_int_swap(int *target, int value, int pe);
OSHMEM_DECLSPEC long pshmem_long_swap(long *target, long value, int pe);
OSHMEM_DECLSPEC long long pshmem_longlong_swap(long long*target, long long value, int pe);
OSHMEM_DECLSPEC float pshmem_float_swap(float *target, float value, int pe);
OSHMEM_DECLSPEC double pshmem_double_swap(double *target, double value, int pe);
#if OSHMEMP_HAVE_C11
#define pshmem_swap(dst, val, pe)                             \
    _Generic(&*(dst),                                        \
            int*:         pshmem_int_swap,                    \
            long*:        pshmem_long_swap,                   \
            long long*:   pshmem_longlong_swap,               \
            float*:       pshmem_float_swap,               \
            double*:      pshmem_double_swap)(dst, val, pe)
#endif

/* Atomic set */
OSHMEM_DECLSPEC void pshmem_int_set(int *target, int value, int pe);
OSHMEM_DECLSPEC void pshmem_long_set(long *target, long value, int pe);
OSHMEM_DECLSPEC void pshmem_longlong_set(long long*target, long long value, int pe);
OSHMEM_DECLSPEC void pshmem_float_set(float *target, float value, int pe);
OSHMEM_DECLSPEC void pshmem_double_set(double *target, double value, int pe);
#if OSHMEMP_HAVE_C11
#define pshmem_set(dst, val, pe)                             \
    _Generic(&*(dst),                                        \
            int*:         pshmem_int_set,                    \
            long*:        pshmem_long_set,                   \
            long long*:   pshmem_longlong_set,               \
            float*:       pshmem_float_set,                  \
            double*:      pshmem_double_set)(dst, val, pe)
#endif

/* Atomic conditional swap */
OSHMEM_DECLSPEC int pshmem_int_cswap(int *target, int cond, int value, int pe);
OSHMEM_DECLSPEC long pshmem_long_cswap(long *target, long cond, long value, int pe);
OSHMEM_DECLSPEC long long pshmem_longlong_cswap(long long *target, long long cond, long long value, int pe);
#if OSHMEMP_HAVE_C11
#define pshmem_cswap(dst, cond, val, pe)                       \
    _Generic(&*(dst),                                          \
            int*:         pshmem_int_cswap,                    \
            long*:        pshmem_long_cswap,                   \
            long long*:   pshmem_longlong_cswap)(dst, cond, val, pe)
#endif

/* Atomic Fetch&Add */
OSHMEM_DECLSPEC int pshmem_int_fadd(int *target, int value, int pe);
OSHMEM_DECLSPEC long pshmem_long_fadd(long *target, long value, int pe);
OSHMEM_DECLSPEC long long pshmem_longlong_fadd(long long *target, long long value, int pe);
#if OSHMEMP_HAVE_C11
#define pshmem_fadd(dst, val, pe)                             \
    _Generic(&*(dst),                                         \
            int*:         pshmem_int_fadd,                    \
            long*:        pshmem_long_fadd,                   \
            long long*:   pshmem_longlong_fadd)(dst, val, pe)
#endif

/* Atomic Fetch */
OSHMEM_DECLSPEC int pshmem_int_fetch(const int *target, int pe);
OSHMEM_DECLSPEC long pshmem_long_fetch(const long *target, int pe);
OSHMEM_DECLSPEC long long pshmem_longlong_fetch(const long long *target, int pe);
OSHMEM_DECLSPEC float pshmem_float_fetch(const float *target, int pe);
OSHMEM_DECLSPEC double pshmem_double_fetch(const double *target, int pe);
#if OSHMEM_HAVE_C11
#define pshmem_fetch(dst, pe)                             \
    _Generic(&*(dst),                                     \
            int*:         pshmem_int_fetch,               \
            long*:        pshmem_long_fetch,              \
            long long*:   pshmem_longlong_fetch,          \
            float*:       pshmem_float_fetch,             \
            double*:      pshmem_double_fetch)(dst, pe)
#endif

/* Atomic Fetch&Inc */
OSHMEM_DECLSPEC int pshmem_int_finc(int *target, int pe);
OSHMEM_DECLSPEC long pshmem_long_finc(long *target, int pe);
OSHMEM_DECLSPEC long long pshmem_longlong_finc(long long *target, int pe);
#if OSHMEMP_HAVE_C11
#define pshmem_finc(dst, val, pe)                             \
    _Generic(&*(dst),                                         \
            int*:         pshmem_int_finc,                    \
            long*:        pshmem_long_finc,                   \
            long long*:   pshmem_longlong_finc)(dst, val, pe)
#endif

/* Atomic Add*/
OSHMEM_DECLSPEC void pshmem_int_add(int *target, int value, int pe);
OSHMEM_DECLSPEC void pshmem_long_add(long *target, long value, int pe);
OSHMEM_DECLSPEC void pshmem_longlong_add(long long *target, long long value, int pe);
#if OSHMEMP_HAVE_C11
#define pshmem_add(dst, val, pe)                             \
    _Generic(&*(dst),                                        \
            int*:         pshmem_int_add,                    \
            long*:        pshmem_long_add,                   \
            long long*:   pshmem_longlong_add)(dst, val, pe)
#endif

/* Atomic Inc */
OSHMEM_DECLSPEC void pshmem_int_inc(int *target, int pe);
OSHMEM_DECLSPEC void pshmem_long_inc(long *target, int pe);
OSHMEM_DECLSPEC void pshmem_longlong_inc(long long *target, int pe);
#if OSHMEMP_HAVE_C11
#define pshmem_inc(dst, pe)                            \
    _Generic(&*(dst),                                  \
            int*:         pshmem_int_inc,              \
            long*:        pshmem_long_inc,             \
            long long*:   pshmem_longlong_inc)(dst, pe)
#endif
/*
 * Lock functions
 */
OSHMEM_DECLSPEC void pshmem_set_lock(volatile long *lock);
OSHMEM_DECLSPEC void pshmem_clear_lock(volatile long *lock);
OSHMEM_DECLSPEC int pshmem_test_lock(volatile long *lock);

/*
 * P2P sync routines
 */
OSHMEM_DECLSPEC  void pshmem_short_wait(volatile short *addr, short value);
OSHMEM_DECLSPEC  void pshmem_int_wait(volatile int *addr, int value);
OSHMEM_DECLSPEC  void pshmem_long_wait(volatile long *addr, long value);
OSHMEM_DECLSPEC  void pshmem_longlong_wait(volatile long long *addr, long long value);
OSHMEM_DECLSPEC  void pshmem_wait(volatile long *addr, long value);

OSHMEM_DECLSPEC  void pshmem_short_wait_until(volatile short *addr, int cmp, short value);
OSHMEM_DECLSPEC  void pshmem_int_wait_until(volatile int *addr, int cmp, int value);
OSHMEM_DECLSPEC  void pshmem_long_wait_until(volatile long *addr, int cmp, long value);
OSHMEM_DECLSPEC  void pshmem_longlong_wait_until(volatile long long *addr, int cmp, long long value);
OSHMEM_DECLSPEC  void pshmem_wait_until(volatile long *addr, int cmp, long value);

/*
 * Barrier sync routines
 */
OSHMEM_DECLSPEC  void pshmem_barrier(int PE_start, int logPE_stride, int PE_size, long *pSync);
OSHMEM_DECLSPEC  void pshmem_barrier_all(void);
OSHMEM_DECLSPEC  void pshmem_fence(void);
OSHMEM_DECLSPEC  void pshmem_quiet(void);

/*
 * Collective routines
 */
OSHMEM_DECLSPEC void pshmem_broadcast32(void *target, const void *source, size_t nlong, int PE_root, int PE_start, int logPE_stride, int PE_size, long *pSync);
OSHMEM_DECLSPEC void pshmem_broadcast64(void *target, const void *source, size_t nlong, int PE_root, int PE_start, int logPE_stride, int PE_size, long *pSync);
OSHMEM_DECLSPEC void pshmem_broadcast(void *target, const void *source, size_t nlong, int PE_root, int PE_start, int logPE_stride, int PE_size, long *pSync);
OSHMEM_DECLSPEC void pshmem_collect32(void *target, const void *source, size_t nlong, int PE_start, int logPE_stride, int PE_size, long *pSync);
OSHMEM_DECLSPEC void pshmem_collect64(void *target, const void *source, size_t nlong, int PE_start, int logPE_stride, int PE_size, long *pSync);
OSHMEM_DECLSPEC void pshmem_fcollect32(void *target, const void *source, size_t nlong, int PE_start, int logPE_stride, int PE_size, long *pSync);
OSHMEM_DECLSPEC void pshmem_fcollect64(void *target, const void *source, size_t nlong, int PE_start, int logPE_stride, int PE_size, long *pSync);
OSHMEM_DECLSPEC void pshmem_alltoall32(void *target, const void *source, size_t nelems, int PE_start, int logPE_stride, int PE_size, long *pSync);
OSHMEM_DECLSPEC void pshmem_alltoall64(void *target, const void *source, size_t nelems, int PE_start, int logPE_stride, int PE_size, long *pSync);
OSHMEM_DECLSPEC void pshmem_alltoalls32(void *target, const void *source, ptrdiff_t dst, ptrdiff_t sst, size_t nelems, int PE_start, int logPE_stride, int PE_size, long *pSync);
OSHMEM_DECLSPEC void pshmem_alltoalls64(void *target, const void *source, ptrdiff_t dst, ptrdiff_t sst, size_t nelems, int PE_start, int logPE_stride, int PE_size, long *pSync);

/*
 * Reduction routines
 */
OSHMEM_DECLSPEC void pshmem_short_and_to_all(short *target, const short *source, int nreduce, int PE_start, int logPE_stride, int PE_size, short *pWrk, long *pSync);
OSHMEM_DECLSPEC void pshmem_int_and_to_all(int *target, const int *source, int nreduce, int PE_start, int logPE_stride, int PE_size, int *pWrk, long *pSync);
OSHMEM_DECLSPEC void pshmem_long_and_to_all(long *target, const long *source, int nreduce, int PE_start, int logPE_stride, int PE_size, long *pWrk, long *pSync);
OSHMEM_DECLSPEC void pshmem_longlong_and_to_all(long long *target, const long long *source, int nreduce, int PE_start, int logPE_stride, int PE_size, long long *pWrk, long *pSync);

OSHMEM_DECLSPEC void pshmem_short_or_to_all(short *target, const short *source, int nreduce, int PE_start, int logPE_stride, int PE_size, short *pWrk, long *pSync);
OSHMEM_DECLSPEC void pshmem_int_or_to_all(int *target, const int *source, int nreduce, int PE_start, int logPE_stride, int PE_size, int *pWrk, long *pSync);
OSHMEM_DECLSPEC void pshmem_long_or_to_all(long *target, const long *source, int nreduce, int PE_start, int logPE_stride, int PE_size, long *pWrk, long *pSync);
OSHMEM_DECLSPEC void pshmem_longlong_or_to_all(long long *target, const long long *source, int nreduce, int PE_start, int logPE_stride, int PE_size, long long *pWrk, long *pSync);

OSHMEM_DECLSPEC void pshmem_short_xor_to_all(short *target, const short *source, int nreduce, int PE_start, int logPE_stride, int PE_size, short *pWrk, long *pSync);
OSHMEM_DECLSPEC void pshmem_int_xor_to_all(int *target, const int *source, int nreduce, int PE_start, int logPE_stride, int PE_size, int *pWrk, long *pSync);
OSHMEM_DECLSPEC void pshmem_long_xor_to_all(long *target, const long *source, int nreduce, int PE_start, int logPE_stride, int PE_size, long *pWrk, long *pSync);
OSHMEM_DECLSPEC void pshmem_longlong_xor_to_all(long long *target, const long long *source, int nreduce, int PE_start, int logPE_stride, int PE_size, long long *pWrk, long *pSync);

OSHMEM_DECLSPEC void pshmem_short_max_to_all(short *target, const short *source, int nreduce, int PE_start, int logPE_stride, int PE_size, short *pWrk, long *pSync);
OSHMEM_DECLSPEC void pshmem_int_max_to_all(int *target, const int *source, int nreduce, int PE_start, int logPE_stride, int PE_size, int *pWrk, long *pSync);
OSHMEM_DECLSPEC void pshmem_long_max_to_all(long *target, const long *source, int nreduce, int PE_start, int logPE_stride, int PE_size, long *pWrk, long *pSync);
OSHMEM_DECLSPEC void pshmem_longlong_max_to_all(long long *target, const long long *source, int nreduce, int PE_start, int logPE_stride, int PE_size, long long *pWrk, long *pSync);
OSHMEM_DECLSPEC void pshmem_float_max_to_all(float *target, const float *source, int nreduce, int PE_start, int logPE_stride, int PE_size, float *pWrk, long *pSync);
OSHMEM_DECLSPEC void pshmem_double_max_to_all(double *target, const double *source, int nreduce, int PE_start, int logPE_stride, int PE_size, double *pWrk, long *pSync);
OSHMEM_DECLSPEC void pshmem_longdouble_max_to_all(long double *target, const long double *source, int nreduce, int PE_start, int logPE_stride, int PE_size, long double *pWrk, long *pSync);

OSHMEM_DECLSPEC void pshmem_short_min_to_all(short *target, const short *source, int nreduce, int PE_start, int logPE_stride, int PE_size, short *pWrk, long *pSync);
OSHMEM_DECLSPEC void pshmem_int_min_to_all(int *target, const int *source, int nreduce, int PE_start, int logPE_stride, int PE_size, int *pWrk, long *pSync);
OSHMEM_DECLSPEC void pshmem_long_min_to_all(long *target, const long *source, int nreduce, int PE_start, int logPE_stride, int PE_size, long *pWrk, long *pSync);
OSHMEM_DECLSPEC void pshmem_longlong_min_to_all(long long *target, const long long *source, int nreduce, int PE_start, int logPE_stride, int PE_size, long long *pWrk, long *pSync);
OSHMEM_DECLSPEC void pshmem_float_min_to_all(float *target, const float *source, int nreduce, int PE_start, int logPE_stride, int PE_size, float *pWrk, long *pSync);
OSHMEM_DECLSPEC void pshmem_double_min_to_all(double *target, const double *source, int nreduce, int PE_start, int logPE_stride, int PE_size, double *pWrk, long *pSync);
OSHMEM_DECLSPEC void pshmem_longdouble_min_to_all(long double *target, const long double *source, int nreduce, int PE_start, int logPE_stride, int PE_size, long double *pWrk, long *pSync);

OSHMEM_DECLSPEC void pshmem_short_sum_to_all(short *target, const short *source, int nreduce, int PE_start, int logPE_stride, int PE_size, short *pWrk, long *pSync);
OSHMEM_DECLSPEC void pshmem_int_sum_to_all(int *target, const int *source, int nreduce, int PE_start, int logPE_stride, int PE_size, int *pWrk, long *pSync);
OSHMEM_DECLSPEC void pshmem_long_sum_to_all(long *target, const long *source, int nreduce, int PE_start, int logPE_stride, int PE_size, long *pWrk, long *pSync);
OSHMEM_DECLSPEC void pshmem_longlong_sum_to_all(long long *target, const long long *source, int nreduce, int PE_start, int logPE_stride, int PE_size, long long *pWrk, long *pSync);
OSHMEM_DECLSPEC void pshmem_float_sum_to_all(float *target, const float *source, int nreduce, int PE_start, int logPE_stride, int PE_size, float *pWrk, long *pSync);
OSHMEM_DECLSPEC void pshmem_double_sum_to_all(double *target, const double *source, int nreduce, int PE_start, int logPE_stride, int PE_size, double *pWrk, long *pSync);
OSHMEM_DECLSPEC void pshmem_longdouble_sum_to_all(long double *target, const long double *source, int nreduce, int PE_start, int logPE_stride, int PE_size, long double *pWrk, long *pSync);
OSHMEM_DECLSPEC void pshmem_complexf_sum_to_all(OSHMEM_COMPLEX_TYPE(float) *target, const OSHMEM_COMPLEX_TYPE(float) *source, int nreduce, int PE_start, int logPE_stride, int PE_size, OSHMEM_COMPLEX_TYPE(float) *pWrk, long *pSync);
OSHMEM_DECLSPEC void pshmem_complexd_sum_to_all(OSHMEM_COMPLEX_TYPE(double) *target, const OSHMEM_COMPLEX_TYPE(double) *source, int nreduce, int PE_start, int logPE_stride, int PE_size, OSHMEM_COMPLEX_TYPE(double) *pWrk, long *pSync);

OSHMEM_DECLSPEC void pshmem_short_prod_to_all(short *target, const short *source, int nreduce, int PE_start, int logPE_stride, int PE_size, short *pWrk, long *pSync);
OSHMEM_DECLSPEC void pshmem_int_prod_to_all(int *target, const int *source, int nreduce, int PE_start, int logPE_stride, int PE_size, int *pWrk, long *pSync);
OSHMEM_DECLSPEC void pshmem_long_prod_to_all(long *target, const long *source, int nreduce, int PE_start, int logPE_stride, int PE_size, long *pWrk, long *pSync);
OSHMEM_DECLSPEC void pshmem_longlong_prod_to_all(long long *target, const long long *source, int nreduce, int PE_start, int logPE_stride, int PE_size, long long *pWrk, long *pSync);
OSHMEM_DECLSPEC void pshmem_float_prod_to_all(float *target, const float *source, int nreduce, int PE_start, int logPE_stride, int PE_size, float *pWrk, long *pSync);
OSHMEM_DECLSPEC void pshmem_double_prod_to_all(double *target, const double *source, int nreduce, int PE_start, int logPE_stride, int PE_size, double *pWrk, long *pSync);
OSHMEM_DECLSPEC void pshmem_longdouble_prod_to_all(long double *target, const long double *source, int nreduce, int PE_start, int logPE_stride, int PE_size, long double *pWrk, long *pSync);
OSHMEM_DECLSPEC void pshmem_complexf_prod_to_all(OSHMEM_COMPLEX_TYPE(float) *target, const OSHMEM_COMPLEX_TYPE(float) *source, int nreduce, int PE_start, int logPE_stride, int PE_size, OSHMEM_COMPLEX_TYPE(float) *pWrk, long *pSync);
OSHMEM_DECLSPEC void pshmem_complexd_prod_to_all(OSHMEM_COMPLEX_TYPE(double) *target, const OSHMEM_COMPLEX_TYPE(double) *source, int nreduce, int PE_start, int logPE_stride, int PE_size, OSHMEM_COMPLEX_TYPE(double) *pWrk, long *pSync);

/*
 * Platform specific cache management routines
 */
OSHMEM_DECLSPEC void pshmem_udcflush(void);
OSHMEM_DECLSPEC void pshmem_udcflush_line(void* target);
OSHMEM_DECLSPEC void pshmem_set_cache_inv(void);
OSHMEM_DECLSPEC void pshmem_set_cache_line_inv(void* target);
OSHMEM_DECLSPEC void pshmem_clear_cache_inv(void);
OSHMEM_DECLSPEC void pshmem_clear_cache_line_inv(void* target);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif


#endif /* PSHMEM_SHMEM_H */
