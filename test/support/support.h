/*
 * $HEADER$
 */

#ifndef LAM_SUPPORT_H
#define LAM_SUPPORT_H

void test_init(char *a);
void test_success(void);
void test_failure(char *a);
int test_verify(const char *expected_result, const char *test_result);
int test_verify_int(int expected_result, int test_result);
int test_finalize(void);

#endif /* LAM_SUPPORT_H */

