/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

/**
 * @file
 *
 * Generic helper routines for environment manipulation.
 */

#ifndef OMPI_ENVIRON_H
#define OMPI_ENVIRON_H

#include "ompi_config.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
    /**
     * Merge two environ-like arrays into a single, new array, ensuring
     * that there are no duplicate entries.
     *
     * @param minor Set 1 of the environ's to merge
     * @param major Set 2 of the environ's to merge
     * @retval New array of environ
     *
     * Merge two environ-like arrays into a single, new array,
     * ensuring that there are no duplicate entires.  If there are
     * duplicates, entries in the \em major array are favored over
     * those in the \em minor array.
     *
     * Both \em major and \em minor are expected to be argv-style
     * arrays (i.e., terminated with a NULL pointer).
     *
     * The array that is returned is an unencumbered array that should
     * later be freed with a call to ompi_argv_free().
     *
     * Either (or both) of \em major and \em minor can be NULL.  If
     * one of the two is NULL, the other list is simply copied to the
     * output.  If both are NULL, NULL is returned.
     */
    OMPI_DECLSPEC char **ompi_environ_merge(char **minor, char **major);

    /**
     * Portable version of setenv(3), allowing editing of any
     * environ-like array.
     *
     * @param name String name of the environment variable to look for
     * @param value String value to set
     * @param overwrite Whether to overwrite any existing value with
     * the same name
     * @param env The environment to use
     *
     * @retval OMPI_ERR_OUT_OF_RESOURCE If internal malloc() fails.
     * @retval OMPI_EXISTS If the name already exists in \em env and
     * \em overwrite is false (and therefore the \em value was not
     * saved in \em env)
     * @retval OMPI_SUCESS If the value replaced another value or is
     * appended to \em env.
     *
     * \em env is expected to be a NULL-terminated array of pointers
     * (argv-style).  Note that unlike some implementations of
     * putenv(3), if \em value is insertted in \em env, it is copied.
     * So the caller can modify/free both \em name and \em value after
     * ompi_setenv() returns.
     *
     * The \em env array will be grown if necessary.
     *
     * It is permissable to invoke this function with the
     * system-defined \em environ variable.  For example:
     *
     * \code
     *   extern char **environ;
     *   ompi_setenv("foo", "bar", true, &environ);
     * \endcode
     *
     * It is also permissable to call this function with an empty \em
     * env, as long as it is pre-initialized with NULL:
     *
     * \code
     *   char **my_env = NULL;
     *   ompi_setenv("foo", "bar", true, &my_env);
     * \endcode
     */
    OMPI_DECLSPEC int ompi_setenv(const char *name, const char *value,
                                  bool overwrite, char ***env);

    /**
     * Portable version of unsetenv(3), allowing editing of any
     * environ-like array.
     *
     * @param name String name of the environment variable to look for
     * @param env The environment to use
     *
     * @retval OMPI_ERR_OUT_OF_RESOURCE If an internal malloc fails.
     * @retval OMPI_ERR_NOT_FOUND If \em name is not found in \em env.
     * @retval OMPI_SUCCESS If \em name is found and successfully deleted.
     *
     * If \em name is found in \em env, the string corresponding to
     * that entry is freed and its entry is eliminated from the array.
     */
    OMPI_DECLSPEC int ompi_unsetenv(const char *name, char ***env);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif /* OMPI_ARGV_H */
