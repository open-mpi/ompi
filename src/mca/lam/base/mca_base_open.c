/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>
#include <string.h>
#include <syslog.h>

#include "lam/mem/malloc.h"
#include "lam/util/output.h"
#include "mca/mca.h"
#include "mca/lam/base/base.h"


/*
 * Public variables
 */
int mca_base_param_module_path = -1;
bool mca_base_opened = false;

/*
 * Private functions
 */
static void set_defaults(lam_output_stream_t *lds);
static void parse_verbose(char *e, lam_output_stream_t *lds);


/*
 * Main MCA initialization.  
 */
int mca_base_open(void)
{
  int param_index;
  char *value;
  lam_output_stream_t lds;

  if (!mca_base_opened) {
    mca_base_opened = true;
  } else {
    return LAM_SUCCESS;
  }

  /* Register some params */

  mca_base_param_module_path = 
    mca_base_param_register_string("base", NULL, "module_path",
                                   "module_path", LAM_PKGLIBDIR);
  param_index = mca_base_param_register_string("base", NULL, "verbose",
                                               "verbose", NULL);

  /* What verbosity level do we want? */

  mca_base_param_lookup_string(param_index, &value);
  memset(&lds, 0, sizeof(lds));
  if (NULL != value) {
    parse_verbose(value, &lds);
  } else {
    set_defaults(&lds);
  }
  lam_output_reopen(0, &lds);
  lam_output_verbose(0, 5, " Opening");

  /* Open up the module registry */

  return mca_base_module_registry_init();
}


/*
 * Set sane default values for the lds
 */
static void set_defaults(lam_output_stream_t *lds)
{
  /* Load up defaults */

  lds->lds_is_debugging = false;
  lds->lds_verbose_level = 0;
  lds->lds_want_syslog = false;
  lds->lds_syslog_priority = LOG_INFO;
  lds->lds_syslog_ident = "lam";
  lds->lds_want_stdout = false;
  lds->lds_want_stderr = true;
  lds->lds_want_file = false;
  lds->lds_want_file_append = false;
  lds->lds_file_suffix = NULL;
  lds->lds_file_suffix = "mca.txt";
}


/*
 * Parse the value of an environment variable describing verbosity
 */
static void parse_verbose(char *e, lam_output_stream_t *lds)
{
  char *edup;
  char *ptr, *next;

  if (NULL == e)
    return;

  edup = strdup(e);
  ptr = edup;

  /* Now parse the environment variable */

  while (NULL != ptr && strlen(ptr) > 0) {
    next = strchr(ptr, ',');
    if (NULL != next)
      *next = '\0';

    if (0 == strcasecmp(ptr, "syslog")) {
      lds->lds_want_syslog = true;
    }
    else if (strncasecmp(ptr, "syslogpri:", 10) == 0) {
      lds->lds_want_syslog = true;
      if (strcasecmp(ptr + 10, "notice") == 0)
	lds->lds_syslog_priority = LOG_NOTICE;
      else if (strcasecmp(ptr + 10, "INFO") == 0)
	lds->lds_syslog_priority = LOG_INFO;
      else if (strcasecmp(ptr + 10, "DEBUG") == 0)
	lds->lds_syslog_priority = LOG_DEBUG;
    } else if (strncasecmp(ptr, "syslogid:", 9) == 0) {
      lds->lds_want_syslog = true;
      lds->lds_syslog_ident = ptr + 9;
    }

    else if (strcasecmp(ptr, "stdout") == 0) {
      lds->lds_want_stdout = true;
    } else if (strcasecmp(ptr, "stderr") == 0) {
      lds->lds_want_stderr = true;
    }

    else if (strcasecmp(ptr, "file") == 0) {
      lds->lds_want_file = true;
    } else if (strncasecmp(ptr, "file:", 5) == 0) {
      lds->lds_want_file = true;
      lds->lds_file_suffix = ptr + 5;
    } else if (strcasecmp(ptr, "fileappend") == 0) {
      lds->lds_want_file = true;
      lds->lds_want_file_append = 1;
    } 

    else if (strncasecmp(ptr, "level", 5) == 0) {
      lds->lds_verbose_level = 0;
      if (ptr[5] == ':')
        lds->lds_verbose_level = atoi(ptr + 6);
    }

    if (NULL == next)
      break;
    ptr = next + 1;
  }

  /* All done */

  free(edup);
}
