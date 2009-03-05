/*
 * Copyright (c) 2007      Los Alamos National Security, LLC.
 *                         All righs reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include <string.h>
#include <ctype.h>
#include <stdlib.h>

#include "opal/util/output.h"
#include "opal/util/os_path.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/mca/installdirs/installdirs.h"

#include "orte/util/proc_info.h"
#include "orte/util/show_help.h"

#include "btl_pcie.h"
#include "btl_pcie_lex.h"

static char *cfg_filename;
static char *key_buffer = NULL;
static size_t key_buffer_len = 0;

/*
 * Local functions
 */
static char* parse_file(char *filename, bool local, char *key);


/**************************************************************************/

char *
ompi_btl_pcie_cfg_get_local_device(char* hostname, int core)
{
    char *key, *ret, *file;

    file = opal_os_path(false, 
                        opal_install_dirs.sysconfdir,
                        "mca-btl-pcie-local-resources.cfg",
                        NULL);

    asprintf(&key, "%s:%d", hostname, core);
    ret = parse_file(file, true, key);
    free(key);
    free(file);

    return ret;
}

char *
ompi_btl_pcie_cfg_get_matching_device(char* remote_hostname,
				      char* remote_device)
{
    char *key, *ret, *pos, *file;

    file = opal_os_path(false, 
			opal_install_dirs.sysconfdir,
                        "mca-btl-pcie-remote-resources.cfg",
                        NULL);

    asprintf(&key, "%s:%s", remote_hostname, remote_device);
    ret = parse_file(file, false, key);
    free(file);
    free(key);

    if (ret == NULL) return NULL;

    pos = strchr(ret, ':');
    if (pos == NULL) {
        free(ret);
        return NULL;
    }

    /* make sure this is my hostname */
    *pos = '\0';
    if (0 != strcmp(orte_process_info.nodename, ret)) {
        free(ret);
        return NULL;
    }

    pos++;
    pos = strdup(pos);
    free(ret);

    return pos;
}


/*
 * Parse a single file
 */
static char* parse_file(char *filename, bool local, char* key)
{
    int val;
    bool me;
    char *tmp = NULL;

    /* Open the file */
    cfg_filename = filename;
    btl_pcie_cfg_yyin = fopen(filename, "r");
    if (NULL == btl_pcie_cfg_yyin) {
        orte_show_help("help-mpi-btl-pcie.txt", "ini file:file not found",
                       true, filename);
        goto cleanup;
    }

    /* Do the parsing */
    btl_pcie_cfg_parse_done = false;
    btl_pcie_cfg_yynewlines = 1;
    btl_pcie_cfg_init_buffer(btl_pcie_cfg_yyin);
    while (!btl_pcie_cfg_parse_done) {
        val = btl_pcie_cfg_yylex();
        switch (val) {
        case BTL_PCIE_CFG_PARSE_DONE:
            /* This will also set btl_pcie_cfg_parse_done to true, so just
               break here */
            break;

        case BTL_PCIE_CFG_PARSE_NEWLINE:
            /* blank line!  ignore it */
            break;

        case BTL_PCIE_CFG_PARSE_HOSTNAME_CORE:
            if (!local) {
                return NULL;
            }

            if (0 == strcmp(key, btl_pcie_cfg_yytext)) {
                me = true;
            } else {
                me = false;
            }

            val = btl_pcie_cfg_yylex();
            if (BTL_PCIE_CFG_PARSE_DEVICE != val) {
                abort();
            }

            if (me) return strdup(btl_pcie_cfg_yytext);

            break;

        case BTL_PCIE_CFG_PARSE_HOSTNAME_DEVICE:
            if (local) {
                return NULL;
            }

            if (0 == strcmp(key, btl_pcie_cfg_yytext)) {
                me = true;
            } else {
                tmp = strdup(btl_pcie_cfg_yytext);
                me = false;
            }

            val = btl_pcie_cfg_yylex();
            if (BTL_PCIE_CFG_PARSE_HOSTNAME_DEVICE != val) {
                abort();
            }

            if (me) {
                return strdup(btl_pcie_cfg_yytext);
            } else {
                if (0 == strcmp(key, btl_pcie_cfg_yytext)) {
                    return tmp;
                } else {
                    free(tmp);
                }
            }

            break;

        default:
            return NULL;
            break;
        }
    }
    fclose(btl_pcie_cfg_yyin);

cleanup:
    if (NULL != key_buffer) {
        free(key_buffer);
        key_buffer = NULL;
        key_buffer_len = 0;
    }

    return NULL;
}

