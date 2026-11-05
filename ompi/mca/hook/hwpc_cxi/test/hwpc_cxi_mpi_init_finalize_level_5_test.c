/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * SPDX-FileCopyrightText:  Copyright Hewlett Packard Enterprise Development LP
 * SPDX-License-Identifier: BSD-3-Clause-Open-MPI
 *
 * Copyright (c) 2026       Hewlett Packard Enterprise Development LP. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include <dirent.h>
#include <glob.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "mpi.h"

#define REPORT_PREFIX "hwpc_cxi_report."

/* True when the file starts with the detail-report header followed by at least one data row. */
static bool host_report_has_data(const char *path)
{
    char line[512];
    bool ok = false;
    FILE *fp = fopen(path, "r");

    if (NULL == fp) {
        return false;
    }
    if (NULL != fgets(line, sizeof(line), fp)
        && 0 == strcmp(line, "CXI_COUNTER_DATA hostname device counter_name delta rate\n")) {
        ok = NULL != fgets(line, sizeof(line), fp) && 0 == strncmp(line, "CXI_COUNTER_DATA ", 17);
    }
    fclose(fp);
    return ok;
}

/* Checks the reports written with the default prefix, then deletes every file in the directory. */
static bool check_and_remove_reports(void)
{
    int host_reports = 0;
    bool host_report_ok = false;
    bool descriptions_found = false;
    struct dirent *entry;
    DIR *dir = opendir(".");

    if (NULL == dir) {
        return false;
    }
    while (NULL != (entry = readdir(dir))) {
        if (0 == strcmp(entry->d_name, ".") || 0 == strcmp(entry->d_name, "..")) {
            continue;
        }
        if (0 == strcmp(entry->d_name, REPORT_PREFIX "counter_descriptions")) {
            descriptions_found = true;
        } else if (0 == strncmp(entry->d_name, REPORT_PREFIX, strlen(REPORT_PREFIX))) {
            ++host_reports;
            host_report_ok = host_report_has_data(entry->d_name);
        }
        unlink(entry->d_name);
    }
    closedir(dir);

    if (1 != host_reports || !host_report_ok) {
        fprintf(stderr, "FAIL: expected one %s<hostname> report with data rows, found %d\n", REPORT_PREFIX, host_reports);
    }
    if (!descriptions_found) {
        fprintf(stderr, "FAIL: %scounter_descriptions was not written\n", REPORT_PREFIX);
    }
    return 1 == host_reports && host_report_ok && descriptions_found;
}

int main(int argc, char **argv)
{
    /* A nonzero report level aborts the job on a node without a Cassini device. */
    glob_t devices;
    if (0 != glob("/sys/class/cxi/cxi[0-9]*", 0, NULL, &devices)) {
        printf("SKIP: no Cassini (CXI) device available\n");
        return 77;
    }
    globfree(&devices);
    /* No report-file prefix: level 5 must write its detail files with the default one. */
    if (0 != setenv("OMPI_MCA_ompi_hook_hwpc_cxi_counter_report", "5", 1)
        || 0 != setenv("OMPI_MCA_ompi_hook_hwpc_cxi_counter_summary_filter_zeros", "false", 1)
        || 0 != unsetenv("OMPI_MCA_ompi_hook_hwpc_cxi_counter_report_file")
        || 0 != unsetenv("OMPI_MCA_hook_hwpc_cxi_counter_report_file")) {
        return EXIT_FAILURE;
    }

    int rc = MPI_Init(&argc, &argv);
    if (MPI_SUCCESS != rc) {
        return EXIT_FAILURE;
    }

    /* The default prefix is relative, so the reports land in this private directory. */
    char dir[] = "hwpc_cxi_level5_XXXXXX";
    if (NULL == mkdtemp(dir) || 0 != chdir(dir)) {
        MPI_Finalize();
        return EXIT_FAILURE;
    }

    rc = MPI_Finalize();

    bool reports_ok = check_and_remove_reports();
    if (0 != chdir("..") || 0 != rmdir(dir)) {
        fprintf(stderr, "FAIL: could not remove %s\n", dir);
        return EXIT_FAILURE;
    }
    if (MPI_SUCCESS != rc || !reports_ok) {
        return EXIT_FAILURE;
    }

    return EXIT_SUCCESS;
}
