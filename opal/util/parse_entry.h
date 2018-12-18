/*
 * Copyright (c) 2019-2020 IBM Corporation. All rights reserved.
 * $COPYRIGHT$
 */

/*
 * Reads OMPI_MCA_tools_entry for
 *   lib,lib,lib
 *   v,vv
 *   preload,nopreload
 *   fort,fortran
 * Reads OMPI_MCA_tools_entry_base for
 *   lib,lib,lib
 */
void
ompi_entry_parse_mca(int *entry_is_active,
    int *verbose,
    char **preload_string,
    char ***libs, int *nlibs, /* leave "fortran" items in this list, remove v/preload */
    char ***baselibs, int *nbaselibs);
