/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2024      Google, LLC. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include <regex.h>

#include "opal_config.h"

#include "opal/class/opal_include_list.h"
#include "opal/class/opal_object.h"
#include "opal/include/opal/constants.h"
#include "opal/mca/base/base.h"
#include "opal/util/argv.h"
#include "opal/util/output.h"
#include "opal/util/printf.h"

static void include_list_destructor(opal_include_list_t *p);

static int opal_include_list_deserialize(opal_include_list_t *object, const char *value)
{
    /* release any current value and set to defaults */
    include_list_destructor(object);

    if (NULL == value || 0 == strlen(value)) {
        return OPAL_SUCCESS;
    }

    if ('^' == value[0]) {
        object->is_exclude = true;
        ++value;
    }

    object->items = opal_argv_split(value, ',');
    return OPAL_SUCCESS;
}

static char *opal_include_list_serialize(const opal_include_list_t *object)
{
    if (NULL == object->items) {
        return strdup("");
    }

    char *tmp = opal_argv_join(object->items, ',');
    if (object->is_exclude) {
        char *value_str = NULL;
        (void) opal_asprintf(&value_str, "^%s", tmp);
        free(tmp);
        return value_str;
    }

    return tmp;
}

static bool opal_include_list_is_set(const opal_include_list_t *object)
{
    return (object->items != NULL);
}

static void include_list_contructor(opal_include_list_t *p)
{
    p->super.deserialize = (opal_serializable_deserialize_fn_t)opal_include_list_deserialize;
    p->super.serialize = (opal_serializable_serialize_fn_t)opal_include_list_serialize;
    p->super.is_set = (opal_serializable_is_set_fn_t)opal_include_list_is_set;
    p->items = NULL;
    p->is_exclude = false;
}

static void include_list_destructor(opal_include_list_t *p)
{
    opal_argv_free(p->items);
    include_list_contructor(p);
}

OBJ_CLASS_INSTANCE(opal_include_list_t, opal_object_t, include_list_contructor,
                   include_list_destructor);

static int include_list_match_regex(opal_include_list_t *include_list, const char *value,
				    bool case_sensitive)
{
  int regex_flags = REG_EXTENDED | REG_NOSUB;
  regex_t regex;

  if (!case_sensitive) {
    regex_flags |= REG_ICASE;
  }

  for (int i = 0 ; include_list->items[i] ; ++i) {
    int rc = regcomp(&regex, include_list->items[i], regex_flags);
    if (rc != 0) {
      /* incorrectly formatted regular expression */
      opal_output_verbose(MCA_BASE_VERBOSE_WARN, 0, "error compiling regular expression: %s, "
			  "ignoring", include_list->items[i]);
      continue;
    }

    rc = regexec(&regex, value, /*nmatch=*/0, /*pmatch=*/NULL, /*eflags=*/0);
    regfree(&regex);
    if (0 == rc) {
      return (include_list->is_exclude ? -1 : 1) * (i + 1);
    }
  }

  return include_list->is_exclude ? 1 : -1;
}

static int include_list_match(opal_include_list_t *include_list, const char *value,
			      bool case_sensitive)
{
   for (int i = 0 ; include_list->items[i] ; ++i) {
     bool match = false;
     if (case_sensitive) {
       if (0 == strcmp(include_list->items[i], value)) {
	 match = true;
       }
     } else if (0 == strcasecmp(include_list->items[i], value)) {
       match = true;
     }

     if (match) {
       return (include_list->is_exclude ? -1 : 1) * (i + 1);
     }
   }

  return include_list->is_exclude ? 1 : -1;
}

int opal_include_list_match(opal_include_list_t *include_list, const char *value,
                            bool regex_match, bool case_sensitive)
{
  if (include_list == NULL || value == NULL || include_list->items == NULL) {
    opal_output_verbose(MCA_BASE_VERBOSE_ERROR, 0, "error matching in include list");
    return -1;
  }

  if (regex_match) {
    return include_list_match_regex(include_list, value, case_sensitive);
  }

  return include_list_match(include_list, value, case_sensitive);
}
