/*
 * Copyright © 2015 Intel, Inc.  All rights reserved.
 * Copyright © 2015-2018 Inria.  All rights reserved.
 * See COPYING in top-level directory.
 */

#include <private/autogen/config.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>
#include <errno.h>
#include <assert.h>

#define DEFAULT_DUMP_DIR RUNSTATEDIR "/hwloc"

extern int hwloc_dump_hwdata_knl_smbios(const char *input_fsroot, const char *filename);

static void usage(const char *name, FILE *where)
{
    fprintf (where, "Usage: %s [ options ] ...\n", name);
    fprintf (where, "Options:\n");
    fprintf (where, "  -o <dir>      Output files to directory <dir> instead of " DEFAULT_DUMP_DIR "\n");
}

int main(int argc, char *argv[])
{
    const char *callname = argv[0];
    char *dirname = (char *) DEFAULT_DUMP_DIR;
    char *input_fsroot;
    char *filename;
    int err;

    argv++; argc--;
    while (argc) {
      if (!strcmp(argv[0], "-h") || !strcmp(argv[0], "--help")) {
        usage(callname, stdout);
        exit(EXIT_SUCCESS);
      } else if (!strcmp(argv[0], "-o")) {
        if (argc == 1) {
          fprintf(stderr, "Missing output directory name.\n");
          usage(callname, stderr);
          exit(EXIT_FAILURE);
        }
        dirname = argv[1];
        argc -= 2;
        argv += 2;
      } else {
        fprintf(stderr, "Unknown option: %s\n", argv[0]);
        usage(callname, stderr);
        exit(EXIT_FAILURE);
      }
    }

    err = mkdir(dirname, S_IRWXU|S_IRGRP|S_IXGRP|S_IROTH|S_IXOTH);
    if (err < 0) {
      if (errno == EEXIST) {
        if (access(dirname, X_OK|W_OK)) {
          fprintf(stderr, "Cannot enter and modify output directory `%s'.\n", dirname);
          exit(EXIT_FAILURE);
        }
      } else {
        fprintf(stderr, "Failed to create output directory `%s' (%s).\n", dirname, strerror(errno));
        exit(EXIT_FAILURE);
      }
    }

    input_fsroot = getenv("HWLOC_FSROOT");
    if (!input_fsroot)
      input_fsroot = (char *) "/";

    err = asprintf(&filename, "%s/knl_memoryside_cache", dirname);
    assert(err >= 0);
    err = hwloc_dump_hwdata_knl_smbios(input_fsroot, filename);
    free(filename);
    printf("\n");

    return err ? EXIT_FAILURE : EXIT_SUCCESS;
}
