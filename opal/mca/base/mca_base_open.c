/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */


#include "ompi_config.h"
#include <stdio.h>
#include <string.h>

#if HAVE_SYSLOG_H
#include <syslog.h>
#endif
#include "include/constants.h"
#include "util/output.h"
#include "util/printf.h"
#include "mca/mca.h"
#include "mca/base/base.h"

/*
 * Public variables
 */
int mca_base_param_component_path = -1;
bool mca_base_opened = false;

/*
 * Private functions
 */
static void set_defaults(ompi_output_stream_t *lds);
static void parse_verbose(char *e, ompi_output_stream_t *lds);


/*
 * Main MCA initialization.  
 */
int mca_base_open(void)
{
  int param_index;
  char *value;
  ompi_output_stream_t lds;

  if (!mca_base_opened) {
    mca_base_opened = true;
  } else {
    return OMPI_SUCCESS;
  }

  /* Setup the parameter system */

  mca_base_param_init();

  /* Register some params */

#ifdef WIN32
  asprintf(&value, "%s;~/.openmpi/components", OMPI_PKGLIBDIR);
#else
  asprintf(&value, "%s:~/.openmpi/components", OMPI_PKGLIBDIR);
#endif
  mca_base_param_component_path = 
    mca_base_param_register_string("base", NULL, "component_path",
                                   "component_path", value);
  free(value);
  param_index = mca_base_param_register_string("base", NULL, "verbose",
                                               "verbose", NULL);

  mca_base_param_register_int("base", NULL, "component_show_load_errors", 
                              "component_show_load_errors", 0);

  /* What verbosity level do we want? */

  mca_base_param_lookup_string(param_index, &value);
  memset(&lds, 0, sizeof(lds));
  if (NULL != value) {
    parse_verbose(value, &lds);
    free(value);
  } else {
    set_defaults(&lds);
  }
  ompi_output_reopen(0, &lds);
  ompi_output_verbose(5, 0, "mca: base: opening components");

  /* Open up the component repository */

  return mca_base_component_repository_initialize();
}


/*
 * Set sane default values for the lds
 */
static void set_defaults(ompi_output_stream_t *lds)
{
  /* Load up defaults */

  lds->lds_is_debugging = false;
  lds->lds_verbose_level = 0;
  lds->lds_want_syslog = false;
#ifndef WIN32
  lds->lds_syslog_priority = LOG_INFO;
#endif
  lds->lds_syslog_ident = "ompi";
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
static void parse_verbose(char *e, ompi_output_stream_t *lds)
{
  char *edup;
  char *ptr, *next;
  bool have_output = false;

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
#ifndef WIN32 /* there is no syslog */
		lds->lds_want_syslog = true;
      have_output = true;
    }
    else if (strncasecmp(ptr, "syslogpri:", 10) == 0) {
      lds->lds_want_syslog = true;
      have_output = true;
      if (strcasecmp(ptr + 10, "notice") == 0)
	lds->lds_syslog_priority = LOG_NOTICE;
      else if (strcasecmp(ptr + 10, "INFO") == 0)
	lds->lds_syslog_priority = LOG_INFO;
      else if (strcasecmp(ptr + 10, "DEBUG") == 0)
	lds->lds_syslog_priority = LOG_DEBUG;
    } else if (strncasecmp(ptr, "syslogid:", 9) == 0) {
      lds->lds_want_syslog = true;
      lds->lds_syslog_ident = ptr + 9;
#endif
	}

    else if (strcasecmp(ptr, "stdout") == 0) {
      lds->lds_want_stdout = true;
      have_output = true;
    } else if (strcasecmp(ptr, "stderr") == 0) {
      lds->lds_want_stderr = true;
      have_output = true;
    }

    else if (strcasecmp(ptr, "file") == 0) {
      lds->lds_want_file = true;
      have_output = true;
    } else if (strncasecmp(ptr, "file:", 5) == 0) {
      lds->lds_want_file = true;
      lds->lds_file_suffix = ptr + 5;
      have_output = true;
    } else if (strcasecmp(ptr, "fileappend") == 0) {
      lds->lds_want_file = true;
      lds->lds_want_file_append = 1;
      have_output = true;
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

  /* If we didn't get an output, default to stderr */

  if (!have_output) {
    lds->lds_want_stderr = true;
  }

  /* All done */

  free(edup);
}
