/*
 * $HEADER$
 */

#ifndef OMPI_SUPPORT_H
#define OMPI_SUPPORT_H

#include <stdio.h>
#include <stdlib.h>

#define TEST_AND_REPORT(res, exp_res, str)  \
    if( res == exp_res ) test_success(); \
    else test_failure(str);

void test_init(char *a);
void test_success(void);
void test_failure(char *a);
int test_verify_str(const char *expected_result, const char *test_result);
int test_verify_int(int expected_result, int test_result);
int test_finalize(void);


/*
 * test_verify: Non-fatal assertion macro.
 */

#define test_verify(MESSAGE, EXPR)                               \
    do {                                                         \
        if (!(EXPR)) {                                           \
            char s[256];                                         \
            sprintf(s, "%s:%d: %s: %s\n",                        \
                    __FILE__, __LINE__, MESSAGE, # EXPR);        \
            test_failure(s);                                     \
        } else {                                                 \
            test_success();                                      \
        }                                                        \
    } while (0)

#endif /* OMPI_SUPPORT_H */

