/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "support.h"
#include "opal/constants.h"
#include "opal/util/argv.h"

static bool test1(void);
static bool test2(void);
static bool test3(void);
static bool test4(void);
static bool test5(void);
static bool test6(void);
static bool test7(void);
static bool test8(void);
static bool test9(void);
static bool test10(void);


int main(int argc, char* argv[])
{

  test_init("opal_argv_t");

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

  if (test9()) {
      test_success();
  } else {
      test_failure("test9 argv test failed");
  }
  
  if (test10()) {
      test_success();
  } else {
      test_failure("test10 argv test failed");
  }
  
  /* All done */
  return test_finalize();
}


static bool test1(void)
{
  char *a[] = { "argv1", "argv2", "argv3", NULL };
  char **argv = NULL;
  int i, j, argc = 28;

  /* Test basic functionality.  Start with a NULL argv and add the
     contents of the a array.  

     Set argc to be an initiall bogus number -- opal_argv_add() should
     reset it back to zero after the first iteration.

     After adding the a[i], ensure that argv[0 - (i-1)] are the same
     as a[0 - (i-1)].

     Also ensure that a[i + 1] == NULL and that argc is the proper
     value. */

  for (i = 0; a[i] != NULL; ++i) {
    if (opal_argv_append(&argc, &argv, a[i]) != OPAL_SUCCESS) {
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
  opal_argv_free(argv);

  return true;
}


static bool test2(void)
{
  int i, j;
  char **argv = NULL;
  int argc;
  char *a[] = { "aaa", "bbb", "ccc", NULL };
  char *b[4];

  /* Similar test to above, but ensure that opal_argv_add is actually
     *copying* the string by value, not by reference.  So copy the a
     array into b, and then opal_argv_add() from b.  After that,
     scribble in the first character of the b[] string that we just
     added, and compare all entries in a to argv -- they should be
     identical (even though b is now corrupted). */

  for (i = 0; a[i] != NULL; ++i) {
    b[i] = strdup(a[i]);
  }
  b[i] = NULL;

  for (i = 0; b[i] != NULL; ++i) {
    if (opal_argv_append(&argc, &argv, b[i]) != OPAL_SUCCESS) {
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

  opal_argv_free(argv);
  for (i = 0; b[i] != NULL; ++i) {
    free(b[i]);
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

  opal_argv_free(argv);

  /* Now add some stuff and try to free it.  We'll seg fault if
     anything goes wrong.  a is on the stack, so if it mistakenly
     tries to free it, we should get a seg fault. */

  for (i = 0; a[i] != NULL; ++i) {
    if (opal_argv_append(&argc, &argv, a[i]) != OPAL_SUCCESS) {
      return false;
    }
  }
  opal_argv_free(argv);

  /* Do the same thing but guarantee that the copied array was from
     the heap and was freed before we call opal_argv_free(). */

  argc = 0;
  argv = NULL;
  for (i = 0; a[i] != NULL; ++i) {
    b[i] = strdup(a[i]);
  }
  b[i] = NULL;
  for (i = 0; b[i] != NULL; ++i) {
    if (opal_argv_append(&argc, &argv, b[i]) != OPAL_SUCCESS) {
      return false;
    }
  }
  for (i = 0; b[i] != NULL; ++i) {
    free(b[i]);
  }
  opal_argv_free(argv);

  /* All done */

  return true;
}


static bool test4(void)
{
  size_t i, count;
  char *a = strdup("the quick  brown fox jumped over  the lazy  dog a_really_long_argument_to_force_a_long_copy_zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz");
  char **b;
  char *start;

  /* split a string into an argv, and compare it against the original
     string.  Add double spaces into the string; opal_argv_split()
     should skip them. */

  b = opal_argv_split(a, ' ');

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

  opal_argv_free(b);
  free(a);
  return true;
}


static bool test5(void)
{
  char *a[] = { "aaa", "bbb", "ccc", NULL };

  return (opal_argv_count(NULL) == 0 && opal_argv_count(a) == 3);
}


static bool test6(void)
{
  char *a = "the quick brown fox jumped over the lazy dog a_really_long_argument_to_force_a_long_copy_zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz";
  char **b;
  char *c;

  /* split the string above and then join it -- the joined version
     should be just like the original */

  b = opal_argv_split(a, ' ');
  c = opal_argv_join(b, ' ');

  if (strcmp(a, c) != 0) {
    return false;
  }

  /* All done */

  free(c);
  opal_argv_free(b);
  return true;
}


static bool test7(void)
{
  char *a[] = { "a", "b", "c", NULL };
  size_t a_len = (1 + 1 + sizeof(char *)) * 3 + sizeof(char **);

  /* check a NULL pointer first -- should return 0 */

  if (opal_argv_len(NULL) != (size_t) 0) {
    return false;
  }

  /* now check a real string */
  /* size should be (sizeof(char **) + (sizeof(char) + sizeof('\0') +
     sizeof(char*)) * 3) */

  if (opal_argv_len(a) != a_len) {
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

  if (NULL != opal_argv_copy(NULL)) {
    return false;
  }

  /* dup the a array and compare it (array length, contents, etc.) */

  b = opal_argv_copy(a);

  if (opal_argv_count(a) != opal_argv_count(b)) {
    return false;
  }
  for (i = 0; a[i] != NULL; ++i) {
    if (0 != strcmp(a[i], b[i])) {
      return false;
    }
  }

  /* All done */

  opal_argv_free(b);
  return true;
}


static bool test9(void)
{
    char **a = NULL;
    int argc;

    /* bozo cases */
    
    if (OPAL_SUCCESS != opal_argv_delete(NULL, NULL, 0, 0)) {
        return false;
    } 

    a = NULL;
    argc = 0;
    opal_argv_append(&argc, &a, "foo");
    if (OPAL_SUCCESS != opal_argv_delete(&argc, &a, 7, 1) ||
        1 != opal_argv_count(a)) {
        return false;
    } 
    opal_argv_free(a);

    a = NULL;
    argc = 0;
    opal_argv_append(&argc, &a, "foo");
    if (OPAL_SUCCESS != opal_argv_delete(&argc, &a, 0, 0) ||
        1 != opal_argv_count(a)) {
        return false;
    }
    opal_argv_free(a);

    /* now some real tests */
    /* delete 1 off the top */

    a = NULL;
    argc = 0;
    opal_argv_append(&argc, &a, "a");
    opal_argv_append(&argc, &a, "b");
    opal_argv_append(&argc, &a, "c");
    opal_argv_append(&argc, &a, "d");
    opal_argv_append(&argc, &a, "e");
    opal_argv_append(&argc, &a, "f");
    if (OPAL_SUCCESS != opal_argv_delete(&argc, &a, 0, 1) ||
        5 != opal_argv_count(a) ||
        0 != strcmp(a[0], "b") ||
        0 != strcmp(a[1], "c") ||
        0 != strcmp(a[2], "d") ||
        0 != strcmp(a[3], "e") ||
        0 != strcmp(a[4], "f")) {
        return false;
    }
    opal_argv_free(a);

    /* delete 2 off the top */

    a = NULL;
    argc = 0;
    opal_argv_append(&argc, &a, "a");
    opal_argv_append(&argc, &a, "b");
    opal_argv_append(&argc, &a, "c");
    opal_argv_append(&argc, &a, "d");
    opal_argv_append(&argc, &a, "e");
    opal_argv_append(&argc, &a, "f");
    if (OPAL_SUCCESS != opal_argv_delete(&argc, &a, 0, 2) ||
        4 != opal_argv_count(a) ||
        0 != strcmp(a[0], "c") ||
        0 != strcmp(a[1], "d") ||
        0 != strcmp(a[2], "e") ||
        0 != strcmp(a[3], "f")) {
        return false;
    }
    opal_argv_free(a);

    /* delete 1 in the middle */

    a = NULL;
    argc = 0;
    opal_argv_append(&argc, &a, "a");
    opal_argv_append(&argc, &a, "b");
    opal_argv_append(&argc, &a, "c");
    opal_argv_append(&argc, &a, "d");
    opal_argv_append(&argc, &a, "e");
    opal_argv_append(&argc, &a, "f");
    if (OPAL_SUCCESS != opal_argv_delete(&argc, &a, 1, 1) ||
        5 != opal_argv_count(a) ||
        0 != strcmp(a[0], "a") ||
        0 != strcmp(a[1], "c") ||
        0 != strcmp(a[2], "d") ||
        0 != strcmp(a[3], "e") ||
        0 != strcmp(a[4], "f")) {
        return false;
    }
    opal_argv_free(a);

    /* delete 2 in the middle */

    a = NULL;
    argc = 0;
    opal_argv_append(&argc, &a, "a");
    opal_argv_append(&argc, &a, "b");
    opal_argv_append(&argc, &a, "c");
    opal_argv_append(&argc, &a, "d");
    opal_argv_append(&argc, &a, "e");
    opal_argv_append(&argc, &a, "f");
    if (OPAL_SUCCESS != opal_argv_delete(&argc, &a, 1, 2) ||
        4 != opal_argv_count(a) ||
        0 != strcmp(a[0], "a") ||
        0 != strcmp(a[1], "d") ||
        0 != strcmp(a[2], "e") ||
        0 != strcmp(a[3], "f")) {
        return false;
    }
    opal_argv_free(a);

    /* delete everything from the top */

    a = NULL;
    argc = 0;
    opal_argv_append(&argc, &a, "a");
    opal_argv_append(&argc, &a, "b");
    if (OPAL_SUCCESS != opal_argv_delete(&argc, &a, 0, 99) ||
        0 != opal_argv_count(a)) {
        return false;
    }
    opal_argv_free(a);

    /* delete everything from the middle */

    a = NULL;
    argc = 0;
    opal_argv_append(&argc, &a, "a");
    opal_argv_append(&argc, &a, "b");
    if (OPAL_SUCCESS != opal_argv_delete(&argc, &a, 1, 99) ||
        1 != opal_argv_count(a) ||
        0 != strcmp(a[0], "a")) {
        return false;
    }
    opal_argv_free(a);

    /* All done */

    return true;
}


static bool test10(void)
{
    char **orig;
    char **insert;
    int o, i;

    /* bozo cases */
    
    orig = NULL;
    o = 0;
    insert = NULL;
    i = 0;
    opal_argv_append(&i, &insert, "insert a");
    if (OPAL_SUCCESS == opal_argv_insert(NULL, 0, insert)) {
        return false;
    } 
    opal_argv_append(&o, &orig, "orig a");
    if (OPAL_SUCCESS != opal_argv_insert(&orig, 0, NULL)) {
        return false;
    } 
    if (OPAL_SUCCESS == opal_argv_insert(&orig, -1, insert)) {
        return false;
    } 
    opal_argv_free(orig);
    opal_argv_free(insert);

    /* append to the end */

    orig = NULL;
    o = 0;
    insert = NULL;
    i = 0;
    opal_argv_append(&i, &insert, "insert a");
    opal_argv_append(&i, &insert, "insert b");
    opal_argv_append(&i, &insert, "insert c");
    opal_argv_append(&o, &orig, "orig a");
    opal_argv_append(&o, &orig, "orig b");
    opal_argv_append(&o, &orig, "orig c");
    if (OPAL_SUCCESS != opal_argv_insert(&orig, 99, insert) ||
        6 != opal_argv_count(orig) ||
        0 != strcmp(orig[0], "orig a") ||
        0 != strcmp(orig[1], "orig b") ||
        0 != strcmp(orig[2], "orig c") ||
        0 != strcmp(orig[3], "insert a") ||
        0 != strcmp(orig[4], "insert b") ||
        0 != strcmp(orig[5], "insert c")) {
        return false;
    }
    opal_argv_free(orig);
    opal_argv_free(insert);

    /* insert at the beginning */

    orig = NULL;
    o = 0;
    insert = NULL;
    i = 0;
    opal_argv_append(&i, &insert, "insert a");
    opal_argv_append(&i, &insert, "insert b");
    opal_argv_append(&i, &insert, "insert c");
    opal_argv_append(&o, &orig, "orig a");
    opal_argv_append(&o, &orig, "orig b");
    opal_argv_append(&o, &orig, "orig c");
    if (OPAL_SUCCESS != opal_argv_insert(&orig, 0, insert) ||
        6 != opal_argv_count(orig) ||
        0 != strcmp(orig[3], "orig a") ||
        0 != strcmp(orig[4], "orig b") ||
        0 != strcmp(orig[5], "orig c") ||
        0 != strcmp(orig[0], "insert a") ||
        0 != strcmp(orig[1], "insert b") ||
        0 != strcmp(orig[2], "insert c")) {
        return false;
    }
    opal_argv_free(orig);
    opal_argv_free(insert);

    /* insert in the middle */

    orig = NULL;
    o = 0;
    insert = NULL;
    i = 0;
    opal_argv_append(&i, &insert, "insert a");
    opal_argv_append(&i, &insert, "insert b");
    opal_argv_append(&i, &insert, "insert c");
    opal_argv_append(&o, &orig, "orig a");
    opal_argv_append(&o, &orig, "orig b");
    opal_argv_append(&o, &orig, "orig c");
    if (OPAL_SUCCESS != opal_argv_insert(&orig, 1, insert) ||
        6 != opal_argv_count(orig) ||
        0 != strcmp(orig[0], "orig a") ||
        0 != strcmp(orig[4], "orig b") ||
        0 != strcmp(orig[5], "orig c") ||
        0 != strcmp(orig[1], "insert a") ||
        0 != strcmp(orig[2], "insert b") ||
        0 != strcmp(orig[3], "insert c")) {
        return false;
    }
    opal_argv_free(orig);
    opal_argv_free(insert);

    /* insert in the middle */

    orig = NULL;
    o = 0;
    insert = NULL;
    i = 0;
    opal_argv_append(&i, &insert, "insert a");
    opal_argv_append(&i, &insert, "insert b");
    opal_argv_append(&i, &insert, "insert c");
    opal_argv_append(&o, &orig, "orig a");
    opal_argv_append(&o, &orig, "orig b");
    opal_argv_append(&o, &orig, "orig c");
    opal_argv_append(&o, &orig, "orig d");
    opal_argv_append(&o, &orig, "orig e");
    opal_argv_append(&o, &orig, "orig f");
    if (OPAL_SUCCESS != opal_argv_insert(&orig, 1, insert) ||
        9 != opal_argv_count(orig) ||
        0 != strcmp(orig[0], "orig a") ||
        0 != strcmp(orig[4], "orig b") ||
        0 != strcmp(orig[5], "orig c") ||
        0 != strcmp(orig[6], "orig d") ||
        0 != strcmp(orig[7], "orig e") ||
        0 != strcmp(orig[8], "orig f") ||
        0 != strcmp(orig[1], "insert a") ||
        0 != strcmp(orig[2], "insert b") ||
        0 != strcmp(orig[3], "insert c")) {
        return false;
    }
    opal_argv_free(orig);
    opal_argv_free(insert);

    /* All done */

    return true;
}
