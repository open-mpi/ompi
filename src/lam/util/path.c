/*
 * $HEADER$
 */

#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "lam_config.h"
#include "lam/util/path.h"
#include "lam/util/argv.h"
#include "lam/util/malloc.h"

/** @file **/

/**
 * PATH environment variable separator
 */
#ifdef WIN32
#define PATHENVSEP  ';'
#else
#define PATHENVSEP  ':'
#endif

static void     path_env_load(char *path, int *pargc, char ***pargv);
static char     *path_access(char *fname, char *path, int mode);
static char     *list_env_get(char *var, char **list);

/**
 *  Locates a file with certain permissions
 *  
 *  @param fname File name
 *  @param pathv Array of search directories
 *  @param mode  Permissions which must be satisfied
 *  @param envv  Pointer to string containing environment
 *
 *  @retval Full pathname of located file Success
 *  @retval NULL Failure
 *
 *  Environment variable can
 *  appear in the form $variable at the start of a prefix path
 *  and will be replaced by the environment value if it is defined otherwise
 *  the whole prefix is ignored. Environment variable must be followed by a
 *  path delimiter or end-of-string
 */
char *
lam_path_findv(char *fname, char **pathv, int mode, char **envv)
{
    char    *fullpath;  
    char    *delimit;  
    char    *env;     
    char    *pfix;   
    int     i;
    /*
     * If absolute path is given, return it without searching.
     */
    if ('/' == *fname) {
        return(path_access(fname, "", mode));
    }
    /*
     * Initialize.
     */
    fullpath = NULL;
    i = 0;
    /*
     * Consider each directory until the file is found.
     * Thus, the order of directories is important.
     */
    while (pathv[i] && !fullpath) {
        /*
         * Replace environment variable at the head of the string.
         */
        if ('$' == *pathv[i]) {
            delimit = strchr(pathv[i], '/');
            if (delimit) {
                *delimit = '\0';
            }
            env = list_env_get(pathv[i]+1, envv);
            if (delimit) {
                *delimit = '/';
            }
            if (env) {
                if (!delimit) {
                    fullpath = path_access(fname, env, mode);
                } else {
                    pfix = LAM_MALLOC(strlen(env) + strlen(delimit) + 1);
                    if (NULL == pfix){
                        return(0);
                    }
                    strcpy(pfix, env);
                    strcat(pfix, delimit);
                    fullpath = path_access(fname, pfix, mode);
                    LAM_FREE(pfix);
                }
            }
        }
        else {
            fullpath = path_access(fname, pathv[i], mode);
        }
        i++;
    }
    return(fullpath);
}

/**
 *  
 *  Same as lam_path_findv
 *
 *  @param fname File name
 *  @param pathv Array of search directories
 *  @param mode  Permissions which must be satisfied
 *
 *  @retval Full pathname of located file Success
 *  @retval NULL Failure
 *
 *  Locates a file with certain permissions. Environment variable can 
 *  appear in the form $variable at the start of a prefix path
 *  and will be replaced by the environment value if it is defined otherwise
 *  the whole prefix is ignored. Environment variable must be followed by a
 *  path delimiter or end-of-string
 */
char *
lam_path_find(char *fname, char **pathv, int mode)
{
    return(lam_path_findv(fname, pathv, mode, 0));
}

/**
 *  Locates a file with certain permissions from a list of search paths
 *
 *  @param fname File name
 *  @param mode  Target permissions which must be satisfied
 *  @param envv  Pointer to environment list
 *  @param wrkdir Working directory
 *
 *  @retval Full pathname of located file Success
 *  @retval NULL Failure
 *
 *  Locates a file with certain permissions from the list of
 *  paths given by the $PATH environment variable. Replaces ./
 *  of found path with working dir
 */
char *
lam_path_env_findv(char *fname, int mode, char **envv, char *wrkdir)
{
    char    **dirv;     
    char    *fullpath; 
    char    *path;    
    int     dirc;    
    int     i;
    int     found_dot = 0;
    /*
     * Set the local search paths.
     */
    dirc = 0;
    dirv = NULL;

    if ((path = list_env_get("PATH", envv))) {
        path_env_load(path, &dirc, &dirv);
    }
    /*
     * Replace the "." path by the working directory.
     */
    for (i = 0; i < dirc; ++i) {
        if ((0 == strcmp(dirv[i], ".")) && wrkdir) {
            found_dot = 1;
            LAM_FREE(dirv[i]);
            dirv[i] = strdup(wrkdir);
            if (NULL == dirv[i]){
                 return(0);
            }
        }
    }
    /*
     * If we didn't find "." in the path and we have a wrkdir, append
     * the wrkdir to the end of the path
     */
    if (!found_dot && wrkdir) {
        lam_argv_add(&dirc, &dirv, wrkdir);
    }

    fullpath = lam_path_findv(fname, dirv, mode, envv);
    lam_argv_free(dirv);
    return(fullpath);
}

/**
 *  Locates a file with certain permissions. Replaces ./
 *  of found path with working dir
 *
 *  @param fname File name
 *  @param mode  Target permissions which must be satisfied
 *
 *  @retval Full pathname of located file Success
 *  @retval NULL Failure
 */
char *
lam_path_env_find(char *fname, int mode)
{
    char    cwd[LAM_PATH_MAX];
    char    *r;

    getcwd(cwd, LAM_PATH_MAX);
    r = lam_path_env_findv(fname, mode, 0, cwd);
    
    return(r);
}

/**
 *  Forms a complete pathname and checks it for existance and permissions
 *            
 *  Accepts:
 *      -fname File name
 *      -path  Path prefix 
 *      -mode  Target permissions which must be satisfied
 *
 *  Returns:
 *      -Full pathname of located file Success
 *      -NULL Failure
 */
static char *
path_access(char *fname, char *path, int mode)
{
    char    *fullpath;  /* full pathname of search file */
    /*
     * Allocate space for the full pathname.
     */
    fullpath = LAM_MALLOC(strlen(path) + strlen(fname) + 2);
    if (NULL == fullpath){
        return(0);
    }

    if (strlen(path) > 0) {
        strcpy(fullpath, path);
        strcat(fullpath, "/");
        strcat(fullpath, fname);
    } else {
        strcpy(fullpath, fname);
    }
    /*
     * Get status on the full path name to check for existance.
     * Then check the permissions.
     */
    if (access(fullpath, mode)) {
        LAM_FREE(fullpath);
        fullpath = 0;
    }

    return(fullpath);
}

/**
 *
 *  Loads argument array with $PATH env var.
 *
 *  Accepts
 *      -path String contiaing the $PATH
 *      -argc Pointer to argc
 *      -argv Pointer to list of argv
 *
 */
static void
path_env_load(char *path, int *pargc, char ***pargv)
{
    char    *p;     /* favourite pointer */
    char    saved;      /* saved character */

    if (NULL == path) {
        *pargc =  0;
    return;
    }
    /*
     * Loop through the paths (delimited by PATHENVSEP), adding each one to argv.
     */
    while (*path) {
       /*
        * Locate the delimiter.
        */
        for (p = path; *p && (*p != PATHENVSEP); ++p);
        /*
         * Add the path.
         */
        if (p != path) {
            saved = *p;
            *p = '\0';
            lam_argv_add(pargc, pargv, path);
            *p = saved;
            path = p;
        }
        /*
         * Skip past the delimiter, if present.
         */
        if (*path) {
            ++path;
        }
    }
}

/**
 *  Gets value of variable in list or environment. Looks in the list first
 *
 *  Accepts:
 *      -var  String variable
 *      -list Pointer to environment list
 *
 *  Returns:
 *      -List Pointer to environment list Success
 *      -NULL Failure
 */
static char *
list_env_get(char *var, char **list)
{
    int n;

    if (list) {
        n = strlen(var);

        while (*list) {
            if ((0 == strncmp(var, *list, n)) && ('=' == (*list)[n])) {
                return(*list + n+1);
            }
            list++;
        }
    }
    return(getenv(var));
}

