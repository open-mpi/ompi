/*
 * $HEADER$
 */

#ifndef OMPID_H
#define OMPID_H

#include <string.h>

#include "class/ompi_list.h"
#include "util/cmd_line.h"
#include "mca/mca.h"

/*
 * Definitions needed for communication
 */
#define OMPI_DAEMON_OOB_PACK_CMD        OMPI_INT16

#define OMPI_DAEMON_HOSTFILE_CMD        0x01
#define OMPI_DAEMON_SCRIPTFILE_CMD      0x02


/*
 * Globals
 */

typedef uint16_t ompi_daemon_cmd_flag_t;

typedef char *type_vector_t;

extern bool pretty;
extern ompi_cmd_line_t *cmd_line;

extern const char *type_all;
extern const char *type_ompi;
extern const char *type_base;
extern type_vector_t mca_types;

/*
 * Version-related strings and functions
 */

extern const char *ver_full;
extern const char *ver_major;
extern const char *ver_minor;
extern const char *ver_release;
extern const char *ver_alpha;
extern const char *ver_beta;
extern const char *ver_svn;

void do_version(bool want_all, ompi_cmd_line_t *cmd_line);
void show_ompi_version(const char *scope);

/*
 * Parameter/configuration-related functions
 */

extern char *param_all;

extern char *path_prefix;
extern char *path_bindir;
extern char *path_libdir;
extern char *path_incdir;
extern char *path_pkglibdir;
extern char *path_sysconfdir;


#endif /* OMPID_H */
