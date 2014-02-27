/*
 * Copyright (c) 2014      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef BTL_USNIC_TEST_H
#define BTL_USNIC_TEST_H

#include "ompi_config.h"

typedef int (*ompi_btl_usnic_test_fn_t)(void *ctx);

#if OMPI_BTL_USNIC_UNIT_TESTS
#  define test_out(...) fprintf(stderr, __VA_ARGS__)
#  define check(a)                                                           \
    do {                                                                     \
        if (!(a)) {                                                          \
            test_out("%s:%d: check failed, '%s'\n", __func__, __LINE__, #a); \
            return TEST_FAILED;                                              \
        }                                                                    \
    } while (0)
#  define check_str_eq(a,b)                                     \
    do {                                                        \
        const char *a_ = (a);                                   \
        const char *b_ = (b);                                   \
        if (0 != strcmp(a_,b_)) {                               \
            test_out("%s:%d: check failed, \"%s\" != \"%s\"\n", \
                     __func__, __LINE__, a_, b_);               \
            return TEST_FAILED;                                 \
        }                                                       \
    } while (0)
#  define check_int_eq(got, expected)                                   \
    do {                                                                \
        if ((got) != (expected)) {                                      \
            test_out("%s:%d: check failed, \"%s\" != \"%s\", got %d\n", \
                     __func__, __LINE__, #got, #expected, (got));       \
            return TEST_FAILED;                                         \
        }                                                               \
    } while (0)
/* just use check_int_eq for now, no public error code to string routine
 * exists (opal_err2str is static) */
#  define check_err_code(got, expected)                                 \
    check_int_eq(got, expected)
#  define check_msg(a, msg)                                \
    do {                                                   \
        if (!(a)) {                                        \
            test_out("%s:%d: check failed, \"%s\" (%s)\n", \
                     __func__, __LINE__, #a, (msg));       \
            return TEST_FAILED;                            \
        }                                                  \
    } while (0)

extern int ompi_btl_usnic_num_tests_run;
extern int ompi_btl_usnic_num_tests_passed;
extern int ompi_btl_usnic_num_tests_failed;
extern int ompi_btl_usnic_num_tests_skipped;

enum test_result {
    TEST_PASSED = 0,
    TEST_FAILED,
    TEST_SKIPPED
};

/* let us actually paste __LINE__ with other tokens */
#  define USNIC_PASTE(a,b) USNIC_PASTE2(a,b)
#  define USNIC_PASTE2(a,b) a ## b
/* A helper macro to de-clutter test registration. */
#  define USNIC_REGISTER_TEST(name, test_fn, ctx)       \
__attribute__((__constructor__))                        \
static void USNIC_PASTE(usnic_reg_ctor_,__LINE__)(void) \
{                                                       \
    ompi_btl_usnic_register_test(name, test_fn, ctx);   \
}                                                       \

#else /* !OMPI_BTL_USNIC_UNIT_TESTS */
#  define test_out(...) do {} while(0)
#  define USNIC_REGISTER_TEST(name, test_fn, ctx)
#endif

/* Run all registered tests.  Typically called by an external utility that
 * dlopens the usnic BTL shared object.  See run_usnic_tests.c. */
void ompi_btl_usnic_run_tests(void);

void ompi_btl_usnic_register_test(const char *name,
                                  ompi_btl_usnic_test_fn_t test_fn,
                                  void *ctx);

/* should be called once, at component close time */
void ompi_btl_usnic_cleanup_tests(void);

#endif /* BTL_USNIC_TEST_H */
