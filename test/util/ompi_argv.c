/*
 * $HEADER$
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "support.h"
#include "ompi_config.h"
#include "include/constants.h"
#include "util/argv.h"

static bool test1(void);
static bool test2(void);
static bool test3(void);
static bool test4(void);
static bool test5(void);
static bool test6(void);
static bool test7(void);
static bool test8(void);


int main(int argc, char* argv[])
{

  test_init("ompi_argv_t");

  if( test1() ) test_success();
  else test_failure("test1 argv test failed");

  if( test2() ) test_success();
  else test_failure("test2 argv test failed");

  if( test3() ) test_success();
  else test_failure("test3 argv test failed");

  if( test4() ) test_success();
  else test_failure("test4 argv test failed");

  if( test5() ) test_success();
  else test_failure("test5 argv test failed");

  if( test6() ) test_success();
  else test_failure("test6 argv test failed");

  if( test7() ) test_success();
  else test_failure("test7 argv test failed");

  if( test8() ) test_success();
  else test_failure("test8 argv test failed");

  
  /* All done */
  test_finalize();

  return 0;
}


static bool test1(void)
{
  char *a[] = { "argv1", "argv2", "argv3", NULL };
  char **argv = NULL;
  int i, j, argc = 28;

  /* Test basic functionality.  Start with a NULL argv and add the
     contents of the a array.  

     Set argc to be an initiall bogus number -- ompi_argv_add() should
     reset it back to zero after the first iteration.

     After adding the a[i], ensure that argv[0 - (i-1)] are the same
     as a[0 - (i-1)].

     Also ensure that a[i + 1] == NULL and that argc is the proper
     value. */

  for (i = 0; a[i] != NULL; ++i) {
    if (ompi_argv_append(&argc, &argv, a[i]) != OMPI_SUCCESS) {
      return false;
    }
    for (j = 0; j <= i; ++j) {
      if (strcmp(argv[j], a[j]) != 0) {
        return false;
      }
    }
    if (NULL != argv[i + 1]) {
      return false;
    }
    if (argc != i + 1) {
      return false;
    }
  }

  return true;
}


static bool test2(void)
{
  int i, j;
  char **argv = NULL;
  int argc;
  char *a[] = { "aaa", "bbb", "ccc", NULL };
  char *b[4];

  /* Similar test to above, but ensure that ompi_argv_add is actually
     *copying* the string by value, not by reference.  So copy the a
     array into b, and then ompi_argv_add() from b.  After that,
     scribble in the first character of the b[] string that we just
     added, and compare all entries in a to argv -- they should be
     identical (even though b is now corrupted). */

  for (i = 0; a[i] != NULL; ++i) {
    b[i] = strdup(a[i]);
  }
  b[i] = NULL;

  for (i = 0; b[i] != NULL; ++i) {
    if (ompi_argv_append(&argc, &argv, b[i]) != OMPI_SUCCESS) {
      return false;
    }
    ++b[i][0];
    for (j = 0; j <= i; ++j) {
      if (strcmp(argv[j], a[j]) != 0) {
        return false;
      }
    }
    if (NULL != argv[i + 1]) {
      return false;
    }
    if (argc != i + 1) {
      return false;
    }
  }

  return true;
}


static bool test3(void)
{
  int i;
  int argc;
  char **argv = NULL;
  char *a[] = { "aaa", "bbb", "ccc", NULL };
  char *b[4];

  /* Try to free a null argv -- should be harmless (we'll seg fault if
     it's not!) */

  ompi_argv_free(argv);

  /* Now add some stuff and try to free it.  We'll seg fault if
     anything goes wrong.  a is on the stack, so if it mistakenly
     tries to free it, we should get a seg fault. */

  for (i = 0; a[i] != NULL; ++i) {
    if (ompi_argv_append(&argc, &argv, a[i]) != OMPI_SUCCESS) {
      return false;
    }
  }
  ompi_argv_free(argv);

  /* Do the same thing but guarantee that the copied array was from
     the heap and was freed before we call ompi_argv_free(). */

  for (i = 0; a[i] != NULL; ++i) {
    b[i] = strdup(a[i]);
  }
  for (i = 0; b[i] != NULL; ++i) {
    if (ompi_argv_append(&argc, &argv, b[i]) != OMPI_SUCCESS) {
      return false;
    }
  }
  for (i = 0; b[i] != NULL; ++i) {
    free(b[i]);
  }
  ompi_argv_free(argv);

  /* All done */

  return true;
}


static bool test4(void)
{
  int i, count;
  char *a = strdup("the quick  brown fox jumped over  the lazy  dog a_really_long_argument_to_force_a_long_copy_zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz");
  char **b;
  char *start;

  /* split a string into an argv, and compare it against the original
     string.  Add double spaces into the string; ompi_argv_split()
     should skip them. */

  b = ompi_argv_split(a, ' ');

  for (count = i = 1; i < strlen(a); ++i) {
    if (a[i] != ' ' && a[i - 1] == ' ') {
      ++count;
    }
  }
  for (i = 0; b[i] != NULL; ++i) {
    continue;
  }
  if (i != count) {
    return false;
  }

  /* now do the same thing and compare each token in b */

  for (start = a, count = i = 0; i < strlen(a); ++i) {
    if (a[i] == ' ' && a[i - 1] != ' ') {
      a[i] = '\0';
      if (strcmp(start, b[count]) != 0) {
        return false;
      }
      ++count;
      a[i] = ' ';
    }
    if (a[i] == ' ' && a[i + 1] != ' ') {
      start = a + i + 1;
    }
  }
  if (strcmp(start, b[count]) != 0) {
    return false;
  }

  /* all done */

  ompi_argv_free(b);
  free(a);
  return true;
}


static bool test5(void)
{
  char *a[] = { "aaa", "bbb", "ccc", NULL };

  return (ompi_argv_count(NULL) == 0 && ompi_argv_count(a) == 3);
}


static bool test6(void)
{
  char *a = "the quick brown fox jumped over the lazy dog a_really_long_argument_to_force_a_long_copy_zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz";
  char **b;
  char *c;

  /* split the string above and then join it -- the joined version
     should be just like the original */

  b = ompi_argv_split(a, ' ');
  c = ompi_argv_join(b, ' ');

  if (strcmp(a, c) != 0) {
    return false;
  }

  /* All done */

  free(c);
  ompi_argv_free(b);
  return true;
}


static bool test7(void)
{
  char *a[] = { "a", "b", "c", NULL };
  size_t a_len = (1 + 1 + sizeof(char *)) * 3 + sizeof(char **);

  /* check a NULL pointer first -- should return 0 */

  if (ompi_argv_len(NULL) != (size_t) 0) {
    return false;
  }

  /* now check a real string */
  /* size should be (sizeof(char **) + (sizeof(char) + sizeof('\0') +
     sizeof(char*)) * 3) */

  if (ompi_argv_len(a) != a_len) {
    return false;
  }

  /* All done */

  return true;
}


static bool test8(void)
{
  char *a[] = { "aaa", "bbbbbbbb", "cccccccccc", NULL };
  int i;
  char **b;

  /* bozo case */

  if (NULL != ompi_argv_copy(NULL)) {
    return false;
  }

  /* dup the a array and compare it (array length, contents, etc.) */

  b = ompi_argv_copy(a);

  if (ompi_argv_count(a) != ompi_argv_count(b)) {
    return false;
  }
  for (i = 0; a[i] != NULL; ++i) {
    if (0 != strcmp(a[i], b[i])) {
      return false;
    }
  }

  /* All done */

  ompi_argv_free(b);
  return true;
}
