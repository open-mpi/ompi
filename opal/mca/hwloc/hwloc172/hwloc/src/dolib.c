/*
 * Copyright © 2009 CNRS
 * Copyright © 2009 inria.  All rights reserved.
 * Copyright © 2009, 2012 Université Bordeaux 1
 * See COPYING in top-level directory.
 */

/* Wrapper to avoid msys' tendency to turn / into \ and : into ;  */

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char *argv[]) {
  char *prog, *arch, *def, *version, *lib;
  char s[1024];
  char name[16];
  int current, age, revision;

  if (argc != 6) {
    fprintf(stderr,"bad number of arguments");
    exit(EXIT_FAILURE);
  }

  prog = argv[1];
  arch = argv[2];
  def = argv[3];
  version = argv[4];
  lib = argv[5];

  if (sscanf(version, "%d:%d:%d", &current, &revision, &age) != 3)
    exit(EXIT_FAILURE);

  _snprintf(name, sizeof(name), "libhwloc-%d", current - age);
  printf("using soname %s\n", name);

  _snprintf(s, sizeof(s), "\"%s\" /machine:%s /def:%s /name:%s /out:%s",
      prog, arch, def, name, lib);
  if (system(s)) {
    fprintf(stderr, "%s failed\n", s);
    exit(EXIT_FAILURE);
  }

  exit(EXIT_SUCCESS);
}
