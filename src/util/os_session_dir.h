/*
 * $HEADER$
 */
/** @file:
 *
 * Find and/or create Open MPI session directory.
 *
 * The ompi_session_dir_init() function creates a temporary directory that is
 * used by the Open MPI system for storing system-critical information. For a given
 * system and user, the function attempts to create a directory that will be used
 * to independently house information for multiple universes, as the user creates
 * them. Thus, the function creates a directory tree of the form:
 *
 * \par \em <prefix-dir>
 * An absolute path that identifies a temporary directory that is
 * read-write-execute accessible to everyone. The function first checks to see if
 * the user has specified the <prefix> directory on the command line. If so, then the
 * function will use that <prefix> if the access permissions are correct, or will return
 * an error condition if not - the function will not search for alternative locations
 * if the user provides the <prefix> name.
 *
 * \par
 * If the <prefix> is not provided by the user, the function searches for a suitable 
 * directory in a specific order, taking the first option that meets the access
 * permission requirement, using: (a) the "OMPI_PREFIX_ENV" environment variable;
 * (b) the "TMPDIR" environment variable; (c) the "TMP" environment variable; and
 * (d) the "HOME" environment variable, appended with a "tmp" directory. If none of
 * those environmental variables have been defined and/or the function was unable
 * to create a suitable directory within any of them, then the function tries to use a default
 * location of "/tmp", where the "/" represents the top-level directory of the
 * local system. If none of these options are successful, the function returns an
 * error code.
 *
 * \par \em <openmpi-sessions>
 * This is a fixed name that serves as a concentrator for all Open MPI session
 * directories on the local system. If it doesn't already exist, this directory is
 * created with read-write-execute permissions for all. If it does exist, the access
 * permissions are checked to ensure they remain read-write-execute for all - if not,
 * an error condition is returned.
 *
 * \par \em <user-id>
 * The user's id on the local system. For security purposes, this directory is created
 * with read-write-execute permissions exclusively restricted to the user. This also
 * allows multiple users to specify identical universe names without conflict.
 *
 * \par
 * Note: The <prefix>/openmpi-sessions/<user-id> directory is left on the system
 * upon termination of an application and/or an Open MPI universe for future use
 * by the user. Thus, when checking a potential location for the directory, the
 * ompi_session_dir_init() function first checks to see if an appropriate directory
 * already exists, and uses it if it does.
 *
 * \par \em <universe-name>
 * Finally, a directory is created for the specified universe name. This is the directory
 * that will be used to house all information relating to the specific universe. If the
 * directory already exists (indicating that the user is joining an existing universe),
 * then the function ensures that the user has exclusive read-write-execute permissions
 * on the directory.
 */

/** 
 * @param prefix A pointer to a string identifying the root to be used for the session directory.
 * This value is provided by the user via the --tmpdir command-line option. If no value
 * is provided, prefix is passed by the calling program as a NULL value, and the
 * ompi_session_dir_init() function will attempt to find an appropriate root.
 *
 * @param universe A pointer to a string that specifies the name of the universe to be
 * established. This name is used to create the universe-specific sessions directory in
 * the user's openmpi-sessions directory. The universe-specific directory will be used
 * to store information required for universe operations. A NULL value will result in an
 * error condition being returned.
 *
 * @retval OMPI_ERROR If the directory could not be created, or if an existing directory
 * of the correct name is found, but cannot be set to the required access permissions.
 *
 * @retval OMPI_SUCCESS If the directory was successfully created with the required access
 * permissions
 *
 * In addition to the return values, the ompi_session_dir_init() function stores the
 * absolute path name of the session directory in the ompi_system_info.session_dir field
 * (see the ompi_sys_info() function for details on this structure).
 *
 */

int ompi_session_dir_init(char *prefix, char *universe);

/** The ompi_find_session_dir() function searches either a user-specified location, or a
 * set of standard locations that might contain the "openmpi-sessions" directory. Once
 * found, the function returns the pathname of that directory. The function calls the
 * ompi_check_dir() function.
 */

/** @param create A boolean variable that indicates whether or not to create the "openmpi-sessions"
 * directory. If set to "false", the function only checks to see if an existing directory
 * can be found. This is typically used to locate an already existing universe for reconnection
 * purposes. If set to "true", then the function creates the "openmpi-sessions" directory, if possible.
 * @param prefix A string variable indicating where the user stipulated the "openmpi-sessions" directory
 * should be placed. A value of "NULL" indicates that the user specified no location - hence, the
 * function explores a range of "standard" locations.
 *
 * @retval *path A pointer to a string containing the pathname of the "openmpi-sessions" directory.
 * A "NULL" value is returned if the directory cannot be found (if create is "false") or created (if
 * create is "true").
 */

char *ompi_find_session_dir(bool create, char *prefix);

