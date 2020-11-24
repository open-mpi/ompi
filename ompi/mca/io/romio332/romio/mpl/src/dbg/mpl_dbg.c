/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil ; -*- */
/*
 *  (C) 2001 by Argonne National Laboratory.
 *      See COPYRIGHT in top-level directory.
 */

/*
 * This file provides a set of routines that can be used to record debug
 * messages in a ring so that the may be dumped at a later time.  For example,
 * this can be used to record debug messages without printing them.
 */

#include "mpl.h"

#ifdef MPL_HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef MPL_HAVE_ERRNO_H
#include <errno.h>
#endif

#if defined(MPL_HAVE_MKSTEMP) && defined(MPL_NEEDS_MKSTEMP_DECL)
extern int mkstemp(char *t);
#endif

#if defined(MPL_HAVE_FDOPEN) && defined(MPL_NEEDS_FDOPEN_DECL)
extern FILE *fdopen(int fd, const char *mode);
#endif

#ifdef MPL_USE_DBG_LOGGING

#ifndef MAXPATHLEN
#define MAXPATHLEN 1024
#endif

int MPL_dbg_active_classes = 0;
int MPL_dbg_max_level = MPL_DBG_TYPICAL;

static enum {
    DBG_UNINIT,
    DBG_PREINIT,
    DBG_INITIALIZED,
    DBG_ERROR
} dbg_initialized = DBG_UNINIT;

static char file_pattern_buf[MAXPATHLEN] = "";
static const char *file_pattern = "-stdout-";   /* "log%d.log"; */
static const char *default_file_pattern = "dbg@W%w-@%d@T-%t@.log";
static char temp_filename[MAXPATHLEN] = "";
static int world_num = 0;
static int world_rank = -1;
static int which_rank = -1;     /* all ranks */
static int reset_time_origin = 1;
static double time_origin = 0.0;

/* This variable is initialized to the appropriate threading level in
 * the DBG_Init call.  Before the debug init, the application cannot
 * be threaded, anyway.  So it is safe to statically set it to "0"
 * here. */
static int is_threaded = 0;

static int dbg_usage(const char *, const char *);
static int dbg_openfile(FILE ** dbg_fp);
static int dbg_set_class(const char *);
static int dbg_set_level(const char *, const char *(names[]));
static int dbg_get_filename(char *filename, int len);

#if (MPL_THREAD_PACKAGE_NAME != MPL_THREAD_PACKAGE_NONE)
static MPL_thread_tls_t dbg_tls_key;
#endif

static FILE *dbg_static_fp = 0;

/*
 * This function finds the basename in a path (ala "man 1 basename").
 * *basename will point to an element in path.
 * More formally: This function sets basename to the character just
 * after the last '/' in path.
*/
static void find_basename(char *path, char **basename) ATTRIBUTE((unused));
static void find_basename(char *path, char **basename)
{
    char *c;

    c = *basename = path;
    while (*c) {
        if (*c == '/')
            *basename = c + 1;
        ++c;
    }
}

static int dbg_init_tls(void)
{
    int err = 0;

#if (MPL_THREAD_PACKAGE_NAME != MPL_THREAD_PACKAGE_NONE)
    MPL_thread_tls_create(NULL, &dbg_tls_key, &err);
#endif

    return err;
}

static FILE *get_fp(void)
{
#if (MPL_THREAD_PACKAGE_NAME != MPL_THREAD_PACKAGE_NONE)
    int err;
    /* if we're not initialized, use the static fp, since there should
     * only be one thread in here until then */
    if (is_threaded) {
        if (dbg_initialized == DBG_INITIALIZED) {
            FILE *fp;
            MPL_thread_tls_get(&dbg_tls_key, (void **) &fp, &err);
            return fp;
        }
    }
#endif

    return dbg_static_fp;
}

static void set_fp(FILE * fp)
{
#if (MPL_THREAD_PACKAGE_NAME != MPL_THREAD_PACKAGE_NONE)
    int err;
    /* if we're not initialized, use the static fp, since there should
     * only be one thread in here until then */
    if (is_threaded) {
        if (dbg_initialized == DBG_INITIALIZED) {
            MPL_thread_tls_set(&dbg_tls_key, (void *) fp, &err);
            return;
        }
    }
#endif

    dbg_static_fp = fp;
}

int MPL_dbg_outevent(const char *file, int line, int class, int kind, const char *fmat, ...)
{
    int mpl_errno = MPL_DBG_SUCCESS;
    va_list list;
    char *str, stmp[MPL_DBG_MAXLINE];
    int i;
    void *p;
    MPL_time_t t;
    double curtime;
    unsigned long long int threadID = 0;
    int pid = -1;
    FILE *dbg_fp = NULL;

    if (dbg_initialized == DBG_UNINIT || dbg_initialized == DBG_ERROR)
        goto fn_exit;

    dbg_fp = get_fp();

#if (MPL_THREAD_PACKAGE_NAME != MPL_THREAD_PACKAGE_NONE)
    {
        /* the thread ID is not necessarily unique between processes, so a
         * (pid,tid) pair should be used to uniquely identify output from
         * particular threads on a system */
        MPL_thread_id_t tid;
        MPL_thread_self(&tid);
        threadID = (unsigned long long int) tid;
    }
#endif
#if defined(MPL_HAVE_GETPID)
    pid = (int) getpid();
#endif /* MPL_HAVE_GETPID */

    if (!dbg_fp) {
        mpl_errno = dbg_openfile(&dbg_fp);
        if (mpl_errno)
            goto fn_fail;
        set_fp(dbg_fp);
    }

    MPL_wtime(&t);
    MPL_wtime_todouble(&t, &curtime);
    curtime = curtime - time_origin;

    /* The kind values are used with the macros to simplify these cases */
    switch (kind) {
        case 0:
            va_start(list, fmat);
            str = va_arg(list, char *);
            fprintf(dbg_fp, "%d\t%d\t%llx[%d]\t%d\t%f\t%s\t%d\t%s\n",
                    world_num, world_rank, threadID, pid, class, curtime, file, line, str);
            va_end(list);
            break;
        case 1:
            va_start(list, fmat);
            str = va_arg(list, char *);
            MPL_snprintf(stmp, sizeof(stmp), fmat, str);
            va_end(list);
            fprintf(dbg_fp, "%d\t%d\t%llx[%d]\t%d\t%f\t%s\t%d\t%s\n",
                    world_num, world_rank, threadID, pid, class, curtime, file, line, stmp);
            break;
        case 2:
            va_start(list, fmat);
            i = va_arg(list, int);
            MPL_snprintf(stmp, sizeof(stmp), fmat, i);
            va_end(list);
            fprintf(dbg_fp, "%d\t%d\t%llx[%d]\t%d\t%f\t%s\t%d\t%s\n",
                    world_num, world_rank, threadID, pid, class, curtime, file, line, stmp);
            break;
        case 3:
            va_start(list, fmat);
            p = va_arg(list, void *);
            MPL_snprintf(stmp, sizeof(stmp), fmat, p);
            va_end(list);
            fprintf(dbg_fp, "%d\t%d\t%llx[%d]\t%d\t%f\t%s\t%d\t%s\n",
                    world_num, world_rank, threadID, pid, class, curtime, file, line, stmp);
            break;
        default:
            break;
    }
    fflush(dbg_fp);

  fn_exit:
  fn_fail:
    return 0;
}

/* These are used to simplify the handling of options.
   To add a new name, add an dbg_classname element to the array
   classnames.  The "classbits" values are defined by MPL_DBG_CLASS
   in mpl_dbg.h
 */

typedef struct dbg_classname {
    int classbits;
    const char *ucname, *lcname;
} dbg_classname;

#define MAX_DBG_CLASSNAMES (sizeof(unsigned int) * 8)

static dbg_classname classnames[MAX_DBG_CLASSNAMES];
static int num_classnames = 0;

static const char *unregistered_classes[MAX_DBG_CLASSNAMES];
static int num_unregistered_classes = 0;

/* Because the level values are simpler and are rarely changed, these
 * use a simple set of parallel arrays */
static const int level_values[] = {
    MPL_DBG_TERSE,
    MPL_DBG_TYPICAL,
    MPL_DBG_VERBOSE,
    100
};
static const char *level_name[] = { "TERSE", "TYPICAL", "VERBOSE", 0 };
static const char *lc_level_name[] = { "terse", "typical", "verbose", 0 };

void MPL_dbg_class_register(MPL_dbg_class class, const char *ucname, const char *lcname)
{
    int i, j;

    classnames[num_classnames].classbits = class;
    classnames[num_classnames].ucname = ucname;
    classnames[num_classnames].lcname = lcname;
    num_classnames++;

    if (num_unregistered_classes) {
        /* there are some unregistered classes.  look through to see
         * if any of them match this class. */
        size_t len = strlen(lcname);

        for (i = 0; i < num_unregistered_classes; i++) {
            size_t slen = strlen(unregistered_classes[i]);
            if (len == slen && (!strncmp(unregistered_classes[i], lcname, len) ||
                                !strncmp(unregistered_classes[i], ucname, len))) {
                /* got a match */
                MPL_dbg_active_classes |= class;
                for (j = i; j < num_unregistered_classes - 1; j++)
                    unregistered_classes[j] = unregistered_classes[j + 1];
                num_unregistered_classes--;
                break;
            }
        }
    }
}

MPL_dbg_class MPL_dbg_class_alloc(const char *ucname, const char *lcname)
{
    static unsigned int class = 1;

    /* create a user handle for this class */
    MPL_dbg_class_register(class, ucname, lcname);

    class <<= 1;

    return (class >> 1);
}

/*
 * Initialize the DBG_MSG system.  This is called during the job
 * initialization to process command-line arguments as well as
 * checking either the MPICH_DBG or MPL_DBG environment variables.
 * The initialization
 * is split into two steps: a preinit and an init. This makes it
 * possible to enable most of the features before the full
 * initialization, where a significant amount of the initialization
 * takes place.
 */

static int dbg_process_args(int *argc_p, char ***argv_p)
{
    int i, rc;

    /* Here's where we do the same thing with the command-line options */
    if (argc_p) {
        for (i = 1; i < *argc_p; i++) {
            if (strncmp((*argv_p)[i], "-mpich-dbg", 10) == 0) {
                char *s = (*argv_p)[i] + 10;
                /* Found a command */
                if (*s == 0) {
                    /* Just -mpich-dbg */
                    MPL_dbg_max_level = MPL_DBG_TYPICAL;
                    MPL_dbg_active_classes = MPL_DBG_ALL;
                } else if (*s == '=') {
                    /* look for file */
                    MPL_dbg_max_level = MPL_DBG_TYPICAL;
                    MPL_dbg_active_classes = MPL_DBG_ALL;
                    s++;
                    if (strncmp(s, "file", 4) == 0) {
                        file_pattern = default_file_pattern;
                    }
                } else if (strncmp(s, "-level", 6) == 0) {
                    char *p = s + 6;
                    if (*p == '=') {
                        p++;
                        rc = dbg_set_level(p, lc_level_name);
                        if (rc)
                            dbg_usage("-mpich-dbg-level", "terse, typical, verbose");
                    }
                } else if (strncmp(s, "-class", 6) == 0) {
                    char *p = s + 6;
                    if (*p == '=') {
                        p++;
                        rc = dbg_set_class(p);
                        if (rc)
                            dbg_usage("-mpich-dbg-class", 0);
                    }
                } else if (strncmp(s, "-filename", 9) == 0) {
                    char *p = s + 9;
                    if (*p == '=') {
                        p++;
                        /* A special case for a filepattern of "-default",
                         * use the predefined default pattern */
                        if (strcmp(p, "-default") == 0) {
                            file_pattern = default_file_pattern;
                        } else {
                            strncpy(file_pattern_buf, p, sizeof(file_pattern_buf) - 1);

                            /* Make sure the string is NULL-terminated */
                            file_pattern_buf[MAXPATHLEN - 1] = '\0';

                            file_pattern = file_pattern_buf;
                        }
                    }
                } else if (strncmp(s, "-rank", 5) == 0) {
                    char *p = s + 5;
                    if (*p == '=' && p[1] != 0) {
                        char *sOut;
                        p++;
                        which_rank = (int) strtol(p, &sOut, 10);
                        if (p == sOut) {
                            dbg_usage("-mpich-dbg-rank", 0);
                            which_rank = -1;
                        }
                    }
                } else {
                    dbg_usage((*argv_p)[i], 0);
                }

                /* Eventually, should null it out and reduce argc value */
            }
        }
    }
    return MPL_DBG_SUCCESS;
}

/* could two different environment variables control the same thing?  sure they
 * could! consider MPICH: we moved all our logging code into MPL, so it should
 * have an MPL_ prefix, but all the documentation assumes an "MPICH_" prefix.
 * So we'll look for both. */
static char *getenv_either(const char *env_a, const char *env_b)
{
    char *s;
    if ((s = getenv(env_a)) == NULL)
        s = getenv(env_b);

    return s;
}


static int dbg_process_env(void)
{
    char *s;
    int rc;

    s = getenv_either("MPICH_DBG", "MPL_DBG");
    if (s) {
        /* Set the defaults */
        MPL_dbg_max_level = MPL_DBG_TYPICAL;
        MPL_dbg_active_classes = MPL_DBG_ALL;
        if (strncmp(s, "FILE", 4) == 0) {
            file_pattern = default_file_pattern;
        }
    }
    s = getenv_either("MPICH_DBG_LEVEL", "MPL_DBG_LEVEL");
    if (s) {
        rc = dbg_set_level(s, level_name);
        if (rc)
            dbg_usage("MPL_DBG_LEVEL", "TERSE, TYPICAL, VERBOSE");
    }

    s = getenv_either("MPICH_DBG_CLASS", "MPL_DBG_CLASS");
    if (s) {
        rc = dbg_set_class(s);
        if (rc)
            dbg_usage("MPL_DBG_CLASS", 0);
    }

    s = getenv_either("MPICH_DBG_FILENAME", "MPL_DBG_FILENAME");
    if (s) {
        strncpy(file_pattern_buf, s, sizeof(file_pattern_buf) - 1);

        /* Make sure the string is NULL-terminated */
        file_pattern_buf[MAXPATHLEN - 1] = '\0';

        file_pattern = file_pattern_buf;
    }

    s = getenv_either("MPICH_DBG_RANK", "MPL_DBG_RANK");
    if (s) {
        char *sOut;
        which_rank = (int) strtol(s, &sOut, 10);
        if (s == sOut) {
            dbg_usage("MPL_DBG_RANK", 0);
            which_rank = -1;
        }
    }
    return MPL_DBG_SUCCESS;
}

MPL_dbg_class MPL_DBG_ROUTINE_ENTER;
MPL_dbg_class MPL_DBG_ROUTINE_EXIT;
MPL_dbg_class MPL_DBG_ROUTINE;
MPL_dbg_class MPL_DBG_ALL = ~(0);       /* pre-initialize the ALL class */

/*
 * Attempt to initialize the logging system.  This works only if the
 * full initialization is not required for updating the environment
 * and/or command-line arguments.
 */
int MPL_dbg_pre_init(int *argc_p, char ***argv_p, int wtimeNotReady)
{
    MPL_time_t t;

    /* if the DBG_MSG system was already initialized, say by the
     * device, then return immediately */
    if (dbg_initialized != DBG_UNINIT)
        return MPL_DBG_SUCCESS;

    if (dbg_init_tls())
        return MPL_DBG_ERR_OTHER;

    /* Check to see if any debugging was selected.  The order of these
     * tests is important, as they allow general defaults to be set,
     * followed by more specific modifications */
    /* First, the environment variables */
    dbg_process_env();

    dbg_process_args(argc_p, argv_p);

    if (wtimeNotReady == 0) {
        MPL_wtime(&t);
        MPL_wtime_todouble(&t, &time_origin);
        reset_time_origin = 0;
    }

    /* Allocate the predefined classes */
    MPL_DBG_ROUTINE_ENTER = MPL_dbg_class_alloc("ROUTINE_ENTER", "routine_enter");
    MPL_DBG_ROUTINE_EXIT = MPL_dbg_class_alloc("ROUTINE_EXIT", "routine_exit");

    MPL_DBG_CLASS_CLR(MPL_DBG_ROUTINE);
    MPL_DBG_CLASS_APPEND(MPL_DBG_ROUTINE, MPL_DBG_ROUTINE_ENTER);
    MPL_DBG_CLASS_APPEND(MPL_DBG_ROUTINE, MPL_DBG_ROUTINE_EXIT);
    MPL_dbg_class_register(MPL_DBG_ROUTINE, "ROUTINE", "routine");

    MPL_dbg_class_register(MPL_DBG_ALL, "ALL", "all");

    dbg_initialized = DBG_PREINIT;

    return MPL_DBG_SUCCESS;
}

int MPL_dbg_init(int *argc_p, char ***argv_p, int has_args, int has_env,
                 int wnum, int wrank, int threaded)
{
    int ret;
    FILE *dbg_fp = NULL;

    /* if the DBG_MSG system was already initialized, say by the
     * device, then return immediately.  Note that the device is then
     * responsible for handling the file mode (e.g., reopen when the
     * rank become available) */
    if (dbg_initialized == DBG_INITIALIZED || dbg_initialized == DBG_ERROR)
        return MPL_DBG_SUCCESS;

    if (dbg_initialized != DBG_PREINIT) {
        if (dbg_init_tls())
            return MPL_DBG_ERR_OTHER;
    }

    dbg_fp = get_fp();

    /* We may need to wait until the device is set up to initialize
     * the timer */
    if (reset_time_origin) {
        MPL_time_t t;
        MPL_wtime(&t);
        MPL_wtime_todouble(&t, &time_origin);
        reset_time_origin = 0;
    }
    /* Check to see if any debugging was selected.  The order of these
     * tests is important, as they allow general defaults to be set,
     * followed by more specific modifications. */
    /* Both of these may have already been set in the PreInit call; if
     * the command line and/or environment variables are set before
     * the full initialization, then don't call the routines to check
     * those values (as they were already handled in DBG_PreInit) */
    /* First, the environment variables */
    if (!has_env)
        dbg_process_env();
    /* Now the command-line arguments */
    if (!has_args)
        dbg_process_args(argc_p, argv_p);

    world_num = wnum;
    world_rank = wrank;
    is_threaded = threaded;

    if (which_rank >= 0 && which_rank != wrank) {
        /* Turn off logging on this process */
        MPL_dbg_active_classes = 0;
    }

    /* If the file has already been opened with a temp filename,
     * rename it. */
    if (dbg_fp && dbg_fp != stdout && dbg_fp != stderr) {
        char filename[MAXPATHLEN] = "";

        dbg_get_filename(filename, MAXPATHLEN);
        ret = rename(temp_filename, filename);
        if (ret) {
            /* Retry renaming file after closing it */
            fclose(dbg_fp);
            ret = rename(temp_filename, filename);
            if (ret) {
                MPL_error_printf("Could not rename temp log file to %s\n", filename);
                goto fn_fail;
            } else {
                dbg_fp = fopen(filename, "a+");
                set_fp(dbg_fp);
                if (dbg_fp == NULL) {
                    MPL_error_printf("Error re-opening log file, %s\n", filename);
                    goto fn_fail;
                }
            }
        }
    }

    dbg_initialized = DBG_INITIALIZED;

  fn_exit:
    return MPL_DBG_SUCCESS;
  fn_fail:
    dbg_initialized = DBG_ERROR;
    goto fn_exit;
}

/* Print the usage statement to stderr */
static int dbg_usage(const char *cmd, const char *vals)
{
    if (vals) {
        fprintf(stderr, "Incorrect value for %s, should be one of %s\n", cmd, vals);
    } else {
        fprintf(stderr, "Incorrect value for %s\n", cmd);
    }
    fprintf(stderr, "Command line for debug switches\n\
    -mpich-dbg-class=name[,name,...]\n\
    -mpich-dbg-level=name   (one of terse, typical, verbose)\n\
    -mpich-dbg-filename=pattern (includes %%d for world rank, %%t for thread id\n\
    -mpich-dbg-rank=val    (only this rank in COMM_WORLD will be logged)\n\
    -mpich-dbg   (shorthand for -mpich-dbg-class=all -mpich-dbg-level=typical)\n\
    -mpich-dbg=file (shorthand for -mpich-dbg -mpich-dbg-filename=%s)\n\
Environment variables\n\
    MPICH_DBG_CLASS=NAME[,NAME...]\n\
    MPICH_DBG_LEVEL=NAME\n\
    MPICH_DBG_FILENAME=pattern\n\
    MPICH_DBG_RANK=val\n\
    MPICH_DBG=YES or FILE\n", default_file_pattern);

    fflush(stderr);

    return 0;
}

#if defined (MPL_HAVE_MKSTEMP) && defined (MPL_HAVE_FDOPEN)

/* creates a temporary file in the same directory the user specified
 * for the log file */
#undef FUNCNAME
#define FUNCNAME dbg_open_tmpfile
#undef FCNAME
#define FCNAME MPL_QUOTE(FUNCNAME)
static int dbg_open_tmpfile(FILE ** dbg_fp)
{
    int mpl_errno = MPL_DBG_SUCCESS;
    const char temp_pattern[] = "templogXXXXXX";
    int fd;
    char *basename;
    int ret;

    ret = MPL_strncpy(temp_filename, file_pattern, MAXPATHLEN);
    if (ret)
        goto fn_fail;

    find_basename(temp_filename, &basename);

    /* make sure there's enough room in temp_filename to store temp_pattern */
    if (basename - temp_filename > MAXPATHLEN - sizeof(temp_pattern))
        goto fn_fail;

    MPL_strncpy(basename, temp_pattern, sizeof(temp_pattern));

    fd = mkstemp(temp_filename);
    if (fd == -1)
        goto fn_fail;

    *dbg_fp = fdopen(fd, "a+");
    if (*dbg_fp == NULL)
        goto fn_fail;

  fn_exit:
    return mpl_errno;
  fn_fail:
    MPL_error_printf("Could not open log file %s\n", temp_filename);
    dbg_initialized = DBG_ERROR;
    mpl_errno = MPL_DBG_ERR_INTERN;
    goto fn_exit;
}

#elif defined(MPL_HAVE__MKTEMP_S) && defined(MPL_HAVE_FOPEN_S)

/* creates a temporary file in the same directory the user specified
 * for the log file */
#undef FUNCNAME
#define FUNCNAME dbg_open_tmpfile
#undef FCNAME
#define FCNAME MPL_QUOTE(FUNCNAME)
static int dbg_open_tmpfile(FILE ** dbg_fp)
{
    int mpl_errno = MPL_DBG_SUCCESS;
    const char temp_pattern[] = "templogXXXXXX";
    int fd;
    char *basename;
    int ret;
    errno_t ret_errno;

    ret = MPL_strncpy(temp_filename, file_pattern, MAXPATHLEN);
    if (ret)
        goto fn_fail;

    find_basename(temp_filename, &basename);

    /* make sure there's enough room in temp_filename to store temp_pattern */
    if (basename - temp_filename > MAXPATHLEN - sizeof(temp_pattern))
        goto fn_fail;

    MPL_strncpy(basename, temp_pattern, sizeof(temp_pattern));

    ret_errno = _mktemp_s(temp_filename, MAXPATHLEN);
    if (ret_errno != 0)
        goto fn_fail;

    ret_errno = fopen_s(dbg_fp, temp_filename, "a+");
    if (ret_errno != 0)
        goto fn_fail;

  fn_exit:
    return mpl_errno;
  fn_fail:
    MPL_error_printf("Could not open log file %s\n", temp_filename);
    dbg_initialized = DBG_ERROR;
    mpl_errno = MPL_DBG_ERR_INTERN;
    goto fn_exit;
}

#else

/* creates a temporary file in some directory, which may not be where
 * the user wants the log file.  When the file is renamed later, it
 * may require a copy.
 *
 * Note that this is not safe: By the time we call fopen(), another
 * file with the same name may exist.  That file would get clobbered.
*/
#undef FUNCNAME
#define FUNCNAME dbg_open_tmpfile
#undef FCNAME
#define FCNAME MPL_QUOTE(FUNCNAME)
static int dbg_open_tmpfile(FILE ** dbg_fp)
{
    int mpl_errno = MPL_DBG_SUCCESS;
    char *cret;

    cret = tmpnam(temp_filename);
    if (cret == NULL)
        goto fn_fail;

    *dbg_fp = fopen(temp_filename, "w");
    if (*dbg_fp == NULL)
        goto fn_fail;

  fn_exit:
    return mpl_errno;
  fn_fail:
    MPL_error_printf("Could not open log file %s\n", temp_filename);
    dbg_initialized = DBG_ERROR;
    mpl_errno = MPL_DBG_ERR_INTERN;
    goto fn_exit;
}

#endif

/* This routine can make no MPI calls, since it may be logging those
 * calls. */
static int dbg_get_filename(char *filename, int len)
{
    int withinMworld = 0,       /* True if within an @W...@ */
        withinMthread = 0;      /* True if within an @T...@ */
    /* FIXME: Need to know how many process groups are known */
#if (MPL_THREAD_PACKAGE_NAME != MPL_THREAD_PACKAGE_NONE)
    unsigned long long int threadID = 0;
    int nThread = 2;
#else
    int nThread = 1;
#endif
    static char world_numAsChar[10] = "0";
    char *pDest;
    const char *p;

    if (world_num == 1) {
        world_numAsChar[0] = '1';
        world_numAsChar[1] = '\0';
    }

    p = file_pattern;
    pDest = filename;
    *filename = 0;
    while (*p && (pDest - filename) < len - 1) {
        /* There are two special cases that allow text to
         * be optionally included.  Those patterns are
         * @T...@ (only if multi-threaded) and
         * @W...@ (only if more than one process group)
         * UNIMPLEMENTED/UNTESTED */
        if (*p == '@') {
            /* Escaped @? */
            if (p[1] == '@') {
                *pDest++ = *++p;
                continue;
            }
            /* If within an @...@, terminate it */
            if (withinMworld) {
                withinMworld = 0;
                p++;
            } else if (withinMthread) {
                withinMthread = 0;
                p++;
            } else {
                /* Look for command */
                p++;
                if (*p == 'W') {
                    p++;
                    withinMworld = 1;
                } else if (*p == 'T') {
                    p++;
                    withinMthread = 1;
                } else {
                    /* Unrecognized char */
                    *pDest++ = *p++;
                }
            }
        } else if ((withinMworld && world_num == 0) || (withinMthread && nThread == 1)) {
            /* Simply skip this character since we're not showing
             * this string */
            p++;
        } else if (*p == '%') {
            p++;
            if (*p == 'd') {
                char rankAsChar[20];
                MPL_snprintf(rankAsChar, sizeof(rankAsChar), "%d", world_rank);
                *pDest = 0;
                MPL_strnapp(filename, rankAsChar, len);
                pDest += strlen(rankAsChar);
            } else if (*p == 't') {
#if (MPL_THREAD_PACKAGE_NAME != MPL_THREAD_PACKAGE_NONE)
                char threadIDAsChar[30];
                MPL_thread_id_t tid;
                MPL_thread_self(&tid);
                threadID = (unsigned long long int) tid;

                MPL_snprintf(threadIDAsChar, sizeof(threadIDAsChar), "%llx", threadID);
                *pDest = 0;
                MPL_strnapp(filename, threadIDAsChar, len);
                pDest += strlen(threadIDAsChar);
#else
                *pDest++ = '0';
#endif /* MPL_THREAD_PACKAGE_NAME != MPL_THREAD_PACKAGE_NONE */
            } else if (*p == 'w') {
                /* *pDest++ = '0'; */
                *pDest = 0;
                MPL_strnapp(filename, world_numAsChar, len);
                pDest += strlen(world_numAsChar);
            } else if (*p == 'p') {
                /* Appends the pid of the proceess to the file name. */
                char pidAsChar[20];
#if defined(MPL_HAVE_GETPID)
                pid_t pid = getpid();
#else
                int pid = -1;
#endif /* MPL_HAVE_GETPID */
                MPL_snprintf(pidAsChar, sizeof(pidAsChar), "%d", (int) pid);
                *pDest = 0;
                MPL_strnapp(filename, pidAsChar, len);
                pDest += strlen(pidAsChar);
            } else {
                *pDest++ = '%';
                *pDest++ = *p;
            }
            p++;
        } else {
            *pDest++ = *p++;
        }
    }
    *pDest = 0;

    return 0;
}

/* This routine can make no MPI calls, since it may be logging those
 * calls. */
static int dbg_openfile(FILE ** dbg_fp)
{
    int mpl_errno = MPL_DBG_SUCCESS;
    if (!file_pattern || *file_pattern == 0 || strcmp(file_pattern, "-stdout-") == 0) {
        *dbg_fp = stdout;
    } else if (strcmp(file_pattern, "-stderr-") == 0) {
        *dbg_fp = stderr;
    } else {
        char filename[MAXPATHLEN];

        /* if we're not at DBG_INITIALIZED, we don't know our
         * rank yet, so we create a temp file, to be renamed later */
        if (dbg_initialized != DBG_INITIALIZED) {
            mpl_errno = dbg_open_tmpfile(dbg_fp);
            if (mpl_errno)
                goto fn_fail;
        } else {
            mpl_errno = dbg_get_filename(filename, MAXPATHLEN);
            if (mpl_errno)
                goto fn_fail;

            *dbg_fp = fopen(filename, "w");
            if (!*dbg_fp) {
                MPL_error_printf("Could not open log file %s\n", filename);
                goto fn_fail;
            }
        }
    }
  fn_exit:
    return mpl_errno;
  fn_fail:
    dbg_initialized = DBG_ERROR;
    mpl_errno = MPL_DBG_ERR_INTERN;
    goto fn_exit;
}

/* Support routines for processing mpich-dbg values */
/* Update the GLOBAL variable MPL_dbg_active_classes with the bits
 * corresponding to this name */
static int dbg_set_class(const char *s)
{
    int i, found_match;
    size_t slen = 0;
    char *str;

    if (s && *s)
        slen = strlen(s);

    str = strtok((char *) s, ",");
    while (str) {
        found_match = 0;
        for (i = 0; i < num_classnames; i++) {
            size_t len = strlen(classnames[i].lcname);

            if (slen == len && (!strncmp(str, classnames[i].lcname, len) ||
                                !strncmp(str, classnames[i].ucname, len))) {
                /* we have a match */
                MPL_dbg_active_classes |= classnames[i].classbits;
                found_match = 1;
                break;
            }
        }

        if (!found_match) {
            /* no match was found.  the component might not have
             * registered yet.  store the user string for later
             * access. */
            unregistered_classes[num_unregistered_classes] = str;
            num_unregistered_classes++;
        }

        str = strtok(NULL, ",");
    }

    return 0;
}

/* Set the global MPL_dbg_max_level if there is a match with the known
 * level names */
static int dbg_set_level(const char *s, const char *(names[]))
{
    int i;

    for (i = 0; names[i]; i++) {
        if (strcmp(names[i], s) == 0) {
            MPL_dbg_max_level = level_values[i];
            return 0;
        }
    }
    return 1;
}
#endif /* MPL_USE_DBG_LOGGING */
