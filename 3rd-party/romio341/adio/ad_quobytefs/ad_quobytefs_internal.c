#include "ad_quobytefs_internal.h"

const char *ADIOI_QUOBYTEFSI_GetVolumeAndPath(const char *filename)
{
    const char *path;
    if (strlen(filename) > 1 && !strncmp(filename, "//", 2)) {
        path = strchr(filename + 2, '/');
    } else {
        path = filename;
    }
    return path;
}
