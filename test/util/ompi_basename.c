/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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
#include "include/constants.h"
#include "util/basename.h"


static void test(const char* in, const char* out);


int main(int argc, char* argv[])
{
  test_init("ompi_basename()");

  test("foo.txt", "foo.txt");
  test("/foo/bar/baz", "baz");
  test("/yow.c", "yow.c");
  test("/", "/");

  test("foo.txt/", "foo.txt");
  test("/foo/bar/baz/", "baz");
  test("/yow.c/", "yow.c");
  test("//", "/");

#ifdef WIN32
  test("C:\\foo\\bar\\baz", "baz");
  test("D:foo.txt", "foo.txt");
  test("E:\\yow.c", "yow.c");
  test("F:", "F:");
  test("G:\\", "G:\\");
#endif
  
  /* All done */
  return test_finalize();
}


void test(const char* in, const char* out)
{
    char *msg;
    char *ret = ompi_basename(in);

    if (0 == strcmp(ret, out)) {
        test_success();
    } else {
        asprintf(&msg, "Mismatch: input \"%s\", expected \"%s\", got \"%s\"\n",
                 in, out, ret);
        test_failure(msg);
        free(msg);
    }
    if (NULL != ret) {
        free(ret);
    }
}


