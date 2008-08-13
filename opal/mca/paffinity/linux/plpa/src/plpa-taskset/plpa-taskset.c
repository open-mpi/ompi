/*
 * Copyright (c) 2007-2008 Cisco, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * And before you ask: no, none of this code is lifted from the GPL'ed
 * taskset source.  I looked at that source code just enough to read
 * the header and realize that it was GPL.  This is entirely new
 * source code unencumbered by the GPL.  I read the man page to make
 * this functionality similar to that of the taskset command.  The
 * goal here is to make an executable that behaves like the venerable
 * taskset command but has some more options, such as for socket and
 * core mapping.
 */

/* Needed for getopt_long() */
#define GNU_SOURCE

#include "plpa_config.h"

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <plpa.h>
#include <errno.h>
#include <unistd.h>
#include <getopt.h>
#include <ctype.h>

#include "plpa-taskset.h"

static void show_help(char *argv0, int ret) 
{
    printf("plpa_taskset version %s\n", PACKAGE_VERSION);
    printf("usage: %s [options] [mask | cpu-list] [pid | cmd [args...]]\n",
           argv0);
    printf("set or get the affinity of a process\n\n");
    printf("  -p, --pid                  operating on existing given pid\n");
    printf("  -c, --cpu-list             display and specify cpus in list format\n");
    printf("  -h, --help                 display this help\n");
    printf("  -v, --version              output version information\n");
    printf("\n");
    printf("[mask] is a bitmask of Linux processor IDs\n");
    printf("[cpu-list] is a list of Linux processor IDs, or a <core>@<socket>\n");
    printf("   specification list; <core> and <socket> are either keywords ('all',\n");
    printf("   'even', 'odd') or Linux core / socket IDs, respectively (vs.\n");
    printf("   the Nth socket / core)\n");
    printf("[pid] can be a PID or 'parent' or 'self'\n");
    printf("\n");
    printf("The default behavior is to run a new command:\n");
    printf("  %s 03 sshd -b 1024\n", argv0);
    printf("You can retrieve the mask of an existing task:\n");
    printf("  %s -p 700\n", argv0);
    printf("Or set it:\n");
    printf("  %s -p 03 700\n", argv0);
    printf("List format uses a comma-separated list instead of a mask:\n");
    printf("  %s -pc 0,3,7-11 700\n", argv0);
    printf("Ranges in list format can take a stride argument:\n");
    printf("  e.g. 0-31:2 is equivalent to mask 0x55555555\n");
    printf("\n");
    printf("Core/socket tuples can be specified with <core>@<socket> syntax:\n");
    printf("  %s -pc 1@3,0@2 700\n", argv0);
    printf("\"all\", \"even\", and \"odd\" are valid in the socket/core syntax:\n");
    printf("  %s -pc all@3,2@even 700\n", argv0);
    printf("\"all\" can be used to disable affinity:\n");
    printf("  %s -pc all@all 700\n", argv0);
    printf("Complex core/socket tuples can be specified with {}:\n");
    printf("  %s -pc {0-2}@{0-2,5} 700\n", argv0);
    printf("\"parent\" can be used as the PID:\n");
    printf("  %s -pc even@all parent\n", argv0);
    printf("\n");

    exit(ret);
}

static void show_version(void) 
{
    printf("plpa_taskset version %s\n", PACKAGE_VERSION);
    exit(0);
}

static void append(char *str, int val)
{
    char temp[8];

    if ('\0' != str[0]) {
        strcat(str, ",");
    }
    snprintf(temp, sizeof(temp) - 1, "%d", val);
    strcat(str, temp);
}

static char *cpu_set_to_list(const PLPA_NAME(cpu_set_t) *cpu_set)
{
    size_t i, j, last_bit, size = PLPA_BITMASK_CPU_MAX;
    unsigned long long mask_value = 0;
    /* Upper bound on string length: 4 digits per
       PLPA_BITMASK_CPU_MAX + 1 comma for each */
    static char str[PLPA_BITMASK_CPU_MAX * 5];
    char temp[8];

    if (sizeof(mask_value) * 8 < size) {
        size = sizeof(mask_value) * 8;
    }
    /* Only print ranges for 3 or more consecutive bits, otherwise
       print individual numbers. */
    str[0] = '\0';
    for (i = 0; i < size; ++i) {
        if (PLPA_CPU_ISSET(i, cpu_set)) {
            /* This bit is set -- is it part of a longer series? */
            /* Simple answer: if this is the last or next-to-last bit,
               just print it */
            if (i == size - 1 || i == size - 2) {
                append(str, i);
                continue;
            }
            /* Simple answer: if next bit is not set, then just print
               it */
            else if (!PLPA_CPU_ISSET(i + 1, cpu_set)) {
                append(str, i);
                continue;
            }

            /* Look for the next unset bit */
            last_bit = i;
            for (j = i + 1; j < size; ++j) {
                if (!PLPA_CPU_ISSET(j, cpu_set)) {
                    last_bit = j - 1;
                    break;
                }
            }
            /* If we fell off the end of the array without finding an
               unset bit, then they're all set. */
            if (j >= size) {
                last_bit = size - 1;
            }

            if (i != last_bit) {
                /* last_bit is now the last bit set after i (and it
                   might actually be i).  So if last_bit > i+2, print
                   the range. */
                if (last_bit >= i + 2) {
                    append(str, i);
                    strcat(str, "-");
                    snprintf(temp, sizeof(temp) - 1, "%d", (int) last_bit);
                    strcat(str, temp);
                } else {
                    /* It wasn't worth printing a range, so print
                       i, and possibly print last_bit */
                    append(str, i);
                    if (last_bit != i) {
                        append(str, last_bit);
                    }
                }
                i = last_bit + 1;
            }
        }
    }
    return str;
}

static unsigned long long cpu_set_to_ll(const PLPA_NAME(cpu_set_t) *cpu_set)
{
    size_t i, size = PLPA_BITMASK_CPU_MAX;
    unsigned long long mask_value = 0;

    if (sizeof(mask_value) * 8 < size) {
        size = sizeof(mask_value) * 8;
    }
    for (i = 0; i < size; ++i) {
        if (PLPA_CPU_ISSET(i, cpu_set)) {
            mask_value |= 1llu << i;
        }
    }
    return mask_value;
}

static int cpu_list_to_cpu_set(char *str, PLPA_NAME(cpu_set_t) *cpu_set)
{
    int ret;

    if (NULL == str) {
        return 1;
    }

    parser_setup_string(str);
    ret = token_parse(cpu_set);

    return ret;
}

static int mask_to_cpu_set(const char *mask_string, PLPA_NAME(cpu_set_t) *cpu_set)
{
    size_t i;
    unsigned int mask_value;

    PLPA_CPU_ZERO(cpu_set);
    sscanf(mask_string, "%x", &mask_value);
    for (i = 0; i < sizeof(mask_value) * 8; ++i) {
        if (0 != (mask_value & (1u << i))) {
            PLPA_CPU_SET(i, cpu_set);
        }
    }

    return 0;
}

static void run_test(int use_cpu_list, char *str)
{
    int ret = 0;
    PLPA_NAME(cpu_set_t) cpu_set;

    PLPA_CPU_ZERO(&cpu_set);
    if (use_cpu_list) {
        if (0 == cpu_list_to_cpu_set(str, &cpu_set)) {
            printf("in:%s\nout:%s\n", str, cpu_set_to_list(&cpu_set));
        } else {
            ret = 1;
        }
    } else {
        if (0 == mask_to_cpu_set(str, &cpu_set)) {
            printf("%s\n%llx\n", str, cpu_set_to_ll(&cpu_set));
        } else {
            ret = 1;
        }
    }

    exit(ret);
}

static int get_pid_affinity(int use_cpu_list, char *pid_string) 
{
    int ret, pid = atoi(pid_string);
    PLPA_NAME(cpu_set_t) cpu_set;

    /* Check for the special pid_strings of "self" and "parent" */
    if (0 == strcmp(pid_string, "self")) {
        pid = getpid();
    } else if (0 == strcmp(pid_string, "parent")) {
        pid = getppid();
    }

    /* Report the affinity */
    ret = PLPA_NAME(sched_getaffinity)((pid_t) pid, sizeof(cpu_set), &cpu_set);
    switch (ret) {
    case 0:
        if (use_cpu_list) {
            printf("pid %d's current affinity list: %s\n", pid, 
                   cpu_set_to_list(&cpu_set));
        } else {
            printf("pid %d's current affinity mask: %llx\n", pid, 
                   cpu_set_to_ll(&cpu_set));
        }
        break;

    case ENOSYS:
        printf("sched_getaffinity: processor affinity is not supported on this kernel\n");
        printf("failed to get pid %d's affinity.\n", pid);
        break;

    default:
        perror("sched_getaffinity");
        printf("failed to get pid %d's affinity.\n", pid);
        break;
    }

    return ret;
}

static int set_pid_affinity(int use_cpu_list, char *mask_string, 
                            char *pid_string) 
{
    int ret, pid;
    PLPA_NAME(cpu_set_t) cpu_set;

    /* Print the original affinity */
    ret = get_pid_affinity(use_cpu_list, pid_string);
    if (0 != ret) {
        return ret;
    }

    /* Convert the argv token mask string to a PLPA cpu set */
    PLPA_CPU_ZERO(&cpu_set);
    if (use_cpu_list) {
        if (0 != (ret = cpu_list_to_cpu_set(mask_string, &cpu_set))) {
            return ret;
        }
    } else {
        if (0 != (ret = mask_to_cpu_set(mask_string, &cpu_set))) {
            return ret;
        }
    }

    /* Set the affinity */

    /* Check for the special pid_strings of "self" and "parent" */
    if (0 == strcmp(pid_string, "self")) {
        pid = getpid();
    } else if (0 == strcmp(pid_string, "parent")) {
        pid = getppid();
    } else {
        pid = atoi(pid_string);
    }
    ret = PLPA_NAME(sched_setaffinity)((pid_t) pid, sizeof(cpu_set), &cpu_set);
    switch (ret) {
    case 0:
#if defined(PLPA_DEBUG) && PLPA_DEBUG
        /* JMS For debugging */
        if (use_cpu_list) {
            printf("pid %d's new affinity list: %s\n", 
                   pid, cpu_set_to_list(&cpu_set));
        } else {
            printf("pid %d's new affinity mask: %llx\n", 
                   pid, cpu_set_to_ll(&cpu_set));
        }
#endif
        break;

    case ENOSYS:
        printf("sched_setaffinity: processor affinity is not supported on this kernel\n");
        printf("failed to set pid %d's affinity.\n", pid);
        break;

    default:
        perror("sched_setaffinity");
        printf("failed to set pid %d's affinity.\n", pid);
        break;
    }

    return ret;
}

static int launch_task(int use_cpu_list, char **argv)
{
    int ret, cmd_begins;
    PLPA_NAME(cpu_set_t) cpu_set;

    /* The next argument is the list/mask, optionally followed by
       "--", then the argv to launch */

    PLPA_CPU_ZERO(&cpu_set);
    if (use_cpu_list) {
        if (0 != (ret = cpu_list_to_cpu_set(argv[0], &cpu_set))) {
            exit(ret);
        }
    } else {
        if (0 != (ret = mask_to_cpu_set(argv[0], &cpu_set))) {
            exit(ret);
        }
    }
    if (0 != PLPA_NAME(sched_setaffinity)(getpid(), sizeof(cpu_set), &cpu_set)) {
        perror("sched_setaffinity");
        fprintf(stderr, "failed to set pid %d's affinity.\n", getpid());
    }
#if defined(PLPA_DEBUG) && PLPA_DEBUG
    /* JMS For debugging */
    if (use_cpu_list) {
        printf("pid %d's new affinity list: %s\n", 
               getpid(), cpu_set_to_list(&cpu_set));
    } else {
        printf("pid %d's new affinity mask: %llx\n", 
               getpid(), cpu_set_to_ll(&cpu_set));
    }
#endif

    /* The next argument may be "--".  If so, ignore it */

    if (0 == strcmp(argv[1], "--")) {
        cmd_begins = 2;
    } else {
        cmd_begins = 1;
    }
    execvp(argv[cmd_begins], &argv[cmd_begins]);

    /* If we get here, Something Bad Happened(tm) */
    perror("execvp");
    fprintf(stderr, "failed to execute %s\n", argv[cmd_begins]);
    exit(1);
}

int main(int argc, char *argv[]) 
{
    int option;
    int getting_pid = 0;
    int use_cpu_list = 0;
    int testing = 0;
    struct option options[] = {
        { "pid", 0, NULL, 'p' },
        { "cpu-list", 0, NULL, 'c' },
        { "help", 0, NULL, 'h' },
        { "version", 0, NULL, 'V' },
        /* Undocumented testing function */
        { "testing", 0, NULL, 't' },
        { NULL, 0, NULL, 0 }
    };

    while (-1 != (option = getopt_long(argc, argv, "+pchVt", options, NULL))) {
        switch (option) {
        case 'p':
            getting_pid = 1;
            break;

        case 'c':
            use_cpu_list = 1;
            break;

        case 'h':
            show_help(argv[0], 0);
            break;

        case 'V':
            show_version();
            break;

        case 't':
            testing = 1;
            break;

        default:
            show_help(argv[0], 1);
            break;
        }
    }

    /* No argv?  Show the help. */
    if (1 == argc) {
        show_help(argv[0], 1);
    }

    /* Undocumented testing function */
    if (testing) {
        if (NULL == argv[optind]) {
            fprintf(stderr, "Nothing to test\n");
            exit(1);
        }
        run_test(use_cpu_list, argv[optind]);
    }

    /* If we're getting_pid:
       - If there's 1 argument, it's the PID to *get*
       - If there's 2 arguments, it's the mask/list and the PID to *set*
       - Otherwise, it's an error -- show the help message */
    if (getting_pid) {
        if (optind + 1 == argc) {
            return get_pid_affinity(use_cpu_list, argv[optind]);
        } else if (optind + 2 == argc) {
            return set_pid_affinity(use_cpu_list, 
                                    argv[optind], argv[optind + 1]);
        } else {
            show_help(argv[0], 1);
        }
    }

    /* Otherwise, it looks like we're launching a command */
    launch_task(use_cpu_list, argv + optind);

    /* We should never get here */
    return 1;
}
