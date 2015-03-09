/*
 * Copyright (c) 2009-2015 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "opal/runtime/opal.h"
#include "opal/mca/dl/base/base.h"

#if !OPAL_HAVE_DL_SUPPORT
int main(int argc, char *argv[])
{
    /* If OPAL wasn't built with libltdl support, then skip this test */
    fprintf(stderr, "OPAL was not built with libltdl support; skipping\n");
    return 77;
}

#else /* OPAL_HAVE_DL_SUPPORT */

static int try_open(const char *filename)
{
    char *err_msg;
    opal_dl_handle_t *handle;
    int ret;

    ret = opal_dl_open(filename, true, true, &handle, &err_msg);
    if (OPAL_SUCCESS == ret) {
        opal_dl_close(handle);
        printf("File opened with private namespace, all passed\n");
        return 0;
    }

    printf("Failed to open with private namespace: %s\n", err_msg);
    printf("Retrying with global namespace\n");

    ret = opal_dl_open(filename, true, false, &handle, &err_msg);
    if (OPAL_SUCCESS == ret) {
        opal_dl_close(handle);
        printf("File opened with global namespace\n");
        return 0;
    }

    fprintf(stderr, "File failed to open with global namespace: %s\n",
            err_msg);

    return 2;
}

static int do_test(void)
{
    FILE *fp;
    char filename[] = "./libompi_dbg_msgq";
    char full_filename[] = "./libompi_dbg_msgq.la";
    char line[1024];
    int happy;

    /* Double check that the .la file is there that we expect; if it's
       not, skip this test. */
    fp = fopen(full_filename, "r");
    if (NULL == fp) {
        fprintf(stderr, 
                "File %s.la doesn't seem to exist; skipping this test\n",
                full_filename);
        exit(77);
    }
    /* We know the .la file is there, so read it, looking for the
       dlopen value.  If the dlopen value is '' (i.e., empty), then
       there's nothing to dlopen (i.e., OMPI was built with
       --enable-static --disable-shared, so return 77 to skip this
       test.  This is horrible, but I can't think of a better way to
       check it (since there is no good way to #define whether we have
       built statically or not...). */
    happy = 0;
    while (1) {
        if (0 == fgets(line, sizeof(line) - 1, fp)) {
            break;
        }
        if (0 == strncmp(line, "dlname=", 7)) {
            if (0 == strncmp(line + 7, "''", 2)) {
                happy = 0;
            } else {
                happy = 1;
            }
            break;
        }
    }
    fclose(fp);
    if (!happy) {
        fprintf(stderr, "No test file to dlopen (perhaps --enable-static?); skipping\n");
        exit(77);
    }

    char cwd[4096];
    getcwd(cwd, sizeof(cwd) - 1);
    cwd[sizeof(cwd) - 1] = '\0';
    printf("Running in CWD: %s\n", cwd);

    printf("Trying to open file with private namespace: %s\n", filename);

    /* If that works, great */
    if (0 == try_open(filename)) {
        return 0;
    }

    /* If we're using libltdl, it will find the .la file and may
       discover that it needs to open the actual file in the .libs
       directory.  If we're not using libltdl, then we won't know
       about the magic .la file / .libs directory.  Hueristic: if we
       get here, manually prefix the filename with .libs/ and try
       again. */
    char *rel_filename;
    asprintf(&rel_filename, ".libs/%s", filename);
    if (NULL == rel_filename) {
        return 1;
    }
    int rc = try_open(rel_filename);
    free(rel_filename);

    return rc;
}

int main(int argc, char *argv[])
{
    opal_init(&argc, &argv);
    int ret = do_test();
    opal_finalize();

    return ret;
}
#endif /* OPAL_HAVE_DL_SUPPORT */
