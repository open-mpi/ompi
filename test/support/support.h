/*
 * $HEADER$
 */

#ifndef LAM_SUPPORT_H
#define LAM_SUPPORT_H

void test_init(char *a);
void test_success(void);
void test_failure(char *a);
int test_verify(char *expected_result, char *test_result);
void test_finalize(void);

#endif /* LAM_SUPPORT_H */

