/*
 * Copyright © 2009 CNRS
 * Copyright © 2009-2018 Inria.  All rights reserved.
 * Copyright © 2009-2011 Université Bordeaux
 * Copyright © 2009-2011 Cisco Systems, Inc.  All rights reserved.
 * See COPYING in top-level directory.
 */

#include <private/autogen/config.h>
#include <hwloc.h>
#include <hwloc/plugins.h>

/* private headers allowed because this plugin is built within hwloc */
#include <private/xml.h>
#include <private/debug.h>
#include <private/misc.h>

#include <libxml/parser.h>
#include <libxml/tree.h>

/*******************
 * Common routines *
 *******************/

static void hwloc_libxml2_error_callback(void * ctx __hwloc_attribute_unused, const char * msg __hwloc_attribute_unused, ...) { /* do nothing */ }

/* by default, do not cleanup to avoid issues with concurrent libxml users */
static int hwloc_libxml2_needs_cleanup = 0;

static void
hwloc_libxml2_init_once(void)
{
  static int checked = 0;
  if (!checked) {
    /* disable stderr warnings */
    xmlSetGenericErrorFunc(NULL, hwloc__xml_verbose() ? xmlGenericError : hwloc_libxml2_error_callback);
    /* enforce libxml2 cleanup ? */
    if (getenv("HWLOC_LIBXML_CLEANUP"))
      hwloc_libxml2_needs_cleanup = 1;
    checked = 1;
  }
}

static void
hwloc_libxml2_cleanup(void)
{
  if (hwloc_libxml2_needs_cleanup) {
    xmlCleanupParser();
  }
}

/*******************
 * Import routines *
 *******************/

typedef struct hwloc__libxml_import_state_data_s {
  xmlNode *node; /* current libxml node, always valid */
  xmlNode *child; /* next processed child, or NULL if none yet */
  xmlAttr *attr; /* last processed attribute, or NULL if none yet */
} __hwloc_attribute_may_alias * hwloc__libxml_import_state_data_t;

static int
hwloc__libxml_import_next_attr(hwloc__xml_import_state_t state, char **namep, char **valuep)
{
  hwloc__libxml_import_state_data_t lstate = (void*) state->data;

  xmlAttr *attr;
  if (lstate->attr)
    attr = lstate->attr->next;
  else
    attr = lstate->node->properties;
  for (; attr; attr = attr->next)
    if (attr->type == XML_ATTRIBUTE_NODE) {
      /* use the first valid attribute content */
      xmlNode *subnode;
      for (subnode = attr->children; subnode; subnode = subnode->next) {
        if (subnode->type == XML_TEXT_NODE) {
          if (subnode->content) {
            *namep = (char *) attr->name;
            *valuep = (char *) subnode->content;
            lstate->attr = attr;
            return 0;
          }
        } else {
          if (hwloc__xml_verbose())
            fprintf(stderr, "%s: ignoring unexpected xml attr node type %u\n",
                    state->global->msgprefix, subnode->type);
        }
      }
    } else {
      if (hwloc__xml_verbose())
        fprintf(stderr, "%s: ignoring unexpected xml attr type %u\n",
                state->global->msgprefix, attr->type);
    }
  return -1;
}

static int
hwloc__libxml_import_find_child(hwloc__xml_import_state_t state,
				hwloc__xml_import_state_t childstate,
				char **tagp)
{
  hwloc__libxml_import_state_data_t lstate = (void*) state->data;
  hwloc__libxml_import_state_data_t lchildstate = (void*) childstate->data;
  xmlNode *child;
  childstate->parent = state;
  childstate->global = state->global;
  if (!lstate->child)
    /* All children proceeded */
    return 0;

  child = lstate->child;
  if (child->type == XML_ELEMENT_NODE) {
    lstate->child = child->next;
    lchildstate->node = child;
    lchildstate->child = child->children;
    lchildstate->attr = NULL;
    *tagp = (char*) child->name;
    return 1;
  } else if (child->type == XML_TEXT_NODE) {
    if (child->content && child->content[0] != '\0' && child->content[0] != '\n')
      if (hwloc__xml_verbose())
        fprintf(stderr, "%s: ignoring object text content %s\n",
                state->global->msgprefix, (const char*) child->content);
  } else if (child->type != XML_COMMENT_NODE) {
      if (hwloc__xml_verbose())
        fprintf(stderr, "%s: ignoring unexpected xml node type %u\n",
                state->global->msgprefix, child->type);
  }

  return 0;
}

static int
hwloc__libxml_import_close_tag(hwloc__xml_import_state_t state __hwloc_attribute_unused)
{
  return 0;
}

static void
hwloc__libxml_import_close_child(hwloc__xml_import_state_t state __hwloc_attribute_unused)
{
  /* nothing to do */
}

static int
hwloc__libxml_import_get_content(hwloc__xml_import_state_t state,
				 char **beginp, size_t expected_length)
{
  hwloc__libxml_import_state_data_t lstate = (void*) state->data;
  xmlNode *child;
  size_t length;

  child = lstate->node->children;
  if (!child || child->type != XML_TEXT_NODE) {
    if (expected_length)
      return -1;
    *beginp = (char *) "";
    return 0;
  }

  length = strlen((char *) child->content);
  if (length != expected_length)
    return -1;
  *beginp = (char *) child->content;
  return 1;
}

static void
hwloc__libxml_import_close_content(hwloc__xml_import_state_t state __hwloc_attribute_unused)
{
  /* nothing to do */
}

static int
hwloc_libxml_look_init(struct hwloc_xml_backend_data_s *bdata,
		       struct hwloc__xml_import_state_s *state)
{
  hwloc__libxml_import_state_data_t lstate = (void*) state->data;
  xmlNodePtr root_node;
  xmlDtdPtr dtd;

  HWLOC_BUILD_ASSERT(sizeof(*lstate) <= sizeof(state->data));

  dtd = xmlGetIntSubset((xmlDocPtr) bdata->data);
  if (!dtd) {
    if (hwloc__xml_verbose())
      fprintf(stderr, "%s: Loading XML topology without DTD\n",
	      state->global->msgprefix);
  } else if (strcmp((char *) dtd->SystemID, "hwloc.dtd")
	     && strcmp((char *) dtd->SystemID, "hwloc2.dtd")) {
    if (hwloc__xml_verbose())
      fprintf(stderr, "%s: Loading XML topology with wrong DTD SystemID (%s instead of %s)\n",
	      state->global->msgprefix, (char *) dtd->SystemID, "hwloc.dtd or hwloc2.dtd");
  }

  root_node = xmlDocGetRootElement((xmlDocPtr) bdata->data);

  if (!strcmp((const char *) root_node->name, "root")) {
    bdata->version_major = 0;
    bdata->version_minor = 9;
  } else if (!strcmp((const char *) root_node->name, "topology")) {
    unsigned major, minor;
    xmlChar *version = xmlGetProp(root_node, (xmlChar*) "version");
    if (version && sscanf((const char *)version, "%u.%u", &major, &minor) == 2) {
      bdata->version_major = major;
      bdata->version_minor = minor;
    } else {
      bdata->version_major = 1;
      bdata->version_minor = 0;
    }
    xmlFree(version);
  } else {
    /* root node should be in "topology" class (or "root" if importing from < 1.0) */
    if (hwloc__xml_verbose())
      fprintf(stderr, "%s: ignoring object of class `%s' not at the top the xml hierarchy\n",
	      state->global->msgprefix, (const char *) root_node->name);
    goto failed;
  }

  state->global->next_attr = hwloc__libxml_import_next_attr;
  state->global->find_child = hwloc__libxml_import_find_child;
  state->global->close_tag = hwloc__libxml_import_close_tag;
  state->global->close_child = hwloc__libxml_import_close_child;
  state->global->get_content = hwloc__libxml_import_get_content;
  state->global->close_content = hwloc__libxml_import_close_content;
  state->parent = NULL;
  lstate->node = root_node;
  lstate->child = root_node->children;
  lstate->attr = NULL;
  return 0; /* success */

 failed:
  return -1; /* failed */
}

/* can be called at the end of the import (to cleanup things early),
 * or by backend_exit() if load failed for other reasons.
 */
static void
hwloc_libxml_free_buffers(struct hwloc_xml_backend_data_s *bdata)
{
  if (bdata->data) {
    xmlFreeDoc((xmlDoc*)bdata->data);
    bdata->data = NULL;
  }
}

static void
hwloc_libxml_look_done(struct hwloc_xml_backend_data_s *bdata, int result __hwloc_attribute_unused)
{
  hwloc_libxml_free_buffers(bdata);
}

static int
hwloc_libxml_import_diff(struct hwloc__xml_import_state_s *state, const char *xmlpath, const char *xmlbuffer, int xmlbuflen, hwloc_topology_diff_t *firstdiffp, char **refnamep)
{
  hwloc__libxml_import_state_data_t lstate = (void*) state->data;
  char *refname = NULL;
  xmlDoc *doc = NULL;
  xmlNode* root_node;
  xmlDtd *dtd;
  int ret;

  HWLOC_BUILD_ASSERT(sizeof(*lstate) <= sizeof(state->data));

  LIBXML_TEST_VERSION;
  hwloc_libxml2_init_once();

  errno = 0; /* set to 0 so that we know if libxml2 changed it */

  if (xmlpath)
    doc = xmlReadFile(xmlpath, NULL, XML_PARSE_NOBLANKS);
  else if (xmlbuffer)
    doc = xmlReadMemory(xmlbuffer, xmlbuflen, "", NULL, XML_PARSE_NOBLANKS);

  if (!doc) {
    if (!errno)
      /* libxml2 read the file fine, but it got an error during parsing */
      errno = EINVAL;
    hwloc_libxml2_cleanup();
    goto out;
  }

  dtd = xmlGetIntSubset(doc);
  if (!dtd) {
    if (hwloc__xml_verbose())
      fprintf(stderr, "%s: Loading XML topologydiff without DTD\n",
	      state->global->msgprefix);
  } else if (strcmp((char *) dtd->SystemID, "hwloc2-diff.dtd")) {
    if (hwloc__xml_verbose())
      fprintf(stderr, "%s: Loading XML topologydiff with wrong DTD SystemID (%s instead of %s)\n",
	      state->global->msgprefix, (char *) dtd->SystemID, "hwloc2-diff.dtd");
  }

  root_node = xmlDocGetRootElement(doc);

  if (strcmp((const char *) root_node->name, "topologydiff")) {
    /* root node should be in "topologydiff" class */
    if (hwloc__xml_verbose())
      fprintf(stderr, "%s: ignoring object of class `%s' not at the top the xml hierarchy\n",
	      state->global->msgprefix, (const char *) root_node->name);
    goto out_with_doc;
  }

  state->global->next_attr = hwloc__libxml_import_next_attr;
  state->global->find_child = hwloc__libxml_import_find_child;
  state->global->close_tag = hwloc__libxml_import_close_tag;
  state->global->close_child = hwloc__libxml_import_close_child;
  state->global->get_content = hwloc__libxml_import_get_content;
  state->global->close_content = hwloc__libxml_import_close_content;
  state->parent = NULL;
  lstate->node = root_node;
  lstate->child = root_node->children;
  lstate->attr = NULL;

  while (1) {
    char *attrname, *attrvalue;
    if (state->global->next_attr(state, &attrname, &attrvalue) < 0)
      break;
    if (!strcmp(attrname, "refname")) {
      free(refname);
      refname = strdup(attrvalue);
    } else
      goto out_with_doc;
  }

  ret = hwloc__xml_import_diff(state, firstdiffp);
  if (refnamep && !ret)
    *refnamep = refname;
  else
    free(refname);

  xmlFreeDoc(doc);
  hwloc_libxml2_cleanup();
  return ret;

out_with_doc:
  free(refname);
  xmlFreeDoc(doc);
  hwloc_libxml2_cleanup();
out:
  return -1; /* failed */
}

/********************
 * Backend routines *
 ********************/

static void
hwloc_libxml_backend_exit(struct hwloc_xml_backend_data_s *bdata)
{
  hwloc_libxml_free_buffers(bdata);
  hwloc_libxml2_cleanup();
}

static int
hwloc_libxml_backend_init(struct hwloc_xml_backend_data_s *bdata,
			  const char *xmlpath, const char *xmlbuffer, int xmlbuflen)
{
  xmlDoc *doc = NULL;

  LIBXML_TEST_VERSION;
  hwloc_libxml2_init_once();

  errno = 0; /* set to 0 so that we know if libxml2 changed it */

  if (xmlpath)
    doc = xmlReadFile(xmlpath, NULL, XML_PARSE_NOBLANKS);
  else if (xmlbuffer)
    doc = xmlReadMemory(xmlbuffer, xmlbuflen, "", NULL, XML_PARSE_NOBLANKS);

  if (!doc) {
    if (!errno)
      /* libxml2 read the file fine, but it got an error during parsing */
      errno = EINVAL;
    hwloc_libxml2_cleanup();
    return -1;
  }

  bdata->look_init = hwloc_libxml_look_init;
  bdata->look_done = hwloc_libxml_look_done;
  bdata->backend_exit = hwloc_libxml_backend_exit;
  bdata->data = doc;
  return 0;
}

/*******************
 * Export routines *
 *******************/

typedef struct hwloc__libxml_export_state_data_s {
  xmlNodePtr current_node; /* current node to output */
} __hwloc_attribute_may_alias * hwloc__libxml_export_state_data_t;

static void
hwloc__libxml_export_new_child(hwloc__xml_export_state_t parentstate,
			       hwloc__xml_export_state_t state,
			       const char *name)
{
  hwloc__libxml_export_state_data_t lpdata = (void *) parentstate->data;
  hwloc__libxml_export_state_data_t ldata = (void *) state->data;

  state->parent = parentstate;
  state->new_child = parentstate->new_child;
  state->new_prop = parentstate->new_prop;
  state->add_content = parentstate->add_content;
  state->end_object = parentstate->end_object;
  state->global = parentstate->global;

  ldata->current_node = xmlNewChild(lpdata->current_node, NULL, BAD_CAST name, NULL);
}

static void
hwloc__libxml_export_new_prop(hwloc__xml_export_state_t state, const char *name, const char *value)
{
  hwloc__libxml_export_state_data_t ldata = (void *) state->data;
  xmlNewProp(ldata->current_node, BAD_CAST name, BAD_CAST value);
}

static void
hwloc__libxml_export_end_object(hwloc__xml_export_state_t state __hwloc_attribute_unused, const char *name __hwloc_attribute_unused)
{
  /* nothing to do */
}

static void
hwloc__libxml_export_add_content(hwloc__xml_export_state_t state, const char *buffer, size_t length)
{
  hwloc__libxml_export_state_data_t ldata = (void *) state->data;
  xmlNodeAddContentLen(ldata->current_node, BAD_CAST buffer, length);
}

static xmlDocPtr
hwloc__libxml2_prepare_export(hwloc_topology_t topology, struct hwloc__xml_export_data_s *edata,
			      unsigned long flags)
{
  struct hwloc__xml_export_state_s state;
  hwloc__libxml_export_state_data_t data = (void *) state.data;
  int v1export = flags & HWLOC_TOPOLOGY_EXPORT_XML_FLAG_V1;
  xmlDocPtr doc = NULL;       /* document pointer */
  xmlNodePtr root_node = NULL; /* root pointer */

  HWLOC_BUILD_ASSERT(sizeof(*data) <= sizeof(state.data));

  LIBXML_TEST_VERSION;
  hwloc_libxml2_init_once();

  /* Creates a new document, a node and set it as a root node. */
  doc = xmlNewDoc(BAD_CAST "1.0");
  root_node = xmlNewNode(NULL, BAD_CAST "topology");
  if (!(flags & HWLOC_TOPOLOGY_EXPORT_XML_FLAG_V1))
    xmlNewProp(root_node, BAD_CAST "version", BAD_CAST "2.0");
  xmlDocSetRootElement(doc, root_node);

  /* Creates a DTD declaration. Isn't mandatory. */
  (void) xmlCreateIntSubset(doc, BAD_CAST "topology", NULL, v1export ? BAD_CAST "hwloc.dtd" : BAD_CAST "hwloc2.dtd");

  state.new_child = hwloc__libxml_export_new_child;
  state.new_prop = hwloc__libxml_export_new_prop;
  state.add_content = hwloc__libxml_export_add_content;
  state.end_object = hwloc__libxml_export_end_object;
  state.global = edata;

  data->current_node = root_node;

  hwloc__xml_export_topology (&state, topology, flags);

  return doc;
}

static int
hwloc_libxml_export_file(hwloc_topology_t topology, struct hwloc__xml_export_data_s *edata,
			 const char *filename, unsigned long flags)
{
  xmlDocPtr doc;
  int ret;

  errno = 0; /* set to 0 so that we know if libxml2 changed it */

  doc = hwloc__libxml2_prepare_export(topology, edata, flags);
  ret = xmlSaveFormatFileEnc(filename, doc, "UTF-8", 1);
  xmlFreeDoc(doc);
  hwloc_libxml2_cleanup();

  if (ret < 0) {
    if (!errno)
      /* libxml2 likely got an error before doing I/O */
      errno = EINVAL;
    return ret;
  }
  return 0;
}

static int
hwloc_libxml_export_buffer(hwloc_topology_t topology, struct hwloc__xml_export_data_s *edata,
			   char **xmlbuffer, int *buflen, unsigned long flags)
{
  xmlDocPtr doc;

  doc = hwloc__libxml2_prepare_export(topology, edata, flags);
  xmlDocDumpFormatMemoryEnc(doc, (xmlChar **)xmlbuffer, buflen, "UTF-8", 1);
  xmlFreeDoc(doc);
  hwloc_libxml2_cleanup();
  return 0;
}

static xmlDocPtr
hwloc__libxml2_prepare_export_diff(hwloc_topology_diff_t diff, const char *refname)
{
  struct hwloc__xml_export_state_s state;
  hwloc__libxml_export_state_data_t data = (void *) state.data;
  xmlDocPtr doc = NULL;       /* document pointer */
  xmlNodePtr root_node = NULL; /* root pointer */

  HWLOC_BUILD_ASSERT(sizeof(*data) <= sizeof(state.data));

  LIBXML_TEST_VERSION;
  hwloc_libxml2_init_once();

  /* Creates a new document, a node and set it as a root node. */
  doc = xmlNewDoc(BAD_CAST "1.0");
  root_node = xmlNewNode(NULL, BAD_CAST "topologydiff");
  if (refname)
    xmlNewProp(root_node, BAD_CAST "refname", BAD_CAST refname);
  xmlDocSetRootElement(doc, root_node);

  /* Creates a DTD declaration. Isn't mandatory. */
  (void) xmlCreateIntSubset(doc, BAD_CAST "topologydiff", NULL, BAD_CAST "hwloc2-diff.dtd");

  state.new_child = hwloc__libxml_export_new_child;
  state.new_prop = hwloc__libxml_export_new_prop;
  state.add_content = hwloc__libxml_export_add_content;
  state.end_object = hwloc__libxml_export_end_object;

  data->current_node = root_node;

  hwloc__xml_export_diff (&state, diff);

  return doc;
}

static int
hwloc_libxml_export_diff_file(hwloc_topology_diff_t diff, const char *refname, const char *filename)
{
  xmlDocPtr doc;
  int ret;

  errno = 0; /* set to 0 so that we know if libxml2 changed it */

  doc = hwloc__libxml2_prepare_export_diff(diff, refname);
  ret = xmlSaveFormatFileEnc(filename, doc, "UTF-8", 1);
  xmlFreeDoc(doc);
  hwloc_libxml2_cleanup();

  if (ret < 0) {
    if (!errno)
      /* libxml2 likely got an error before doing I/O */
      errno = EINVAL;
    return ret;
  }
  return 0;
}

static int
hwloc_libxml_export_diff_buffer(hwloc_topology_diff_t diff, const char *refname, char **xmlbuffer, int *buflen)
{
  xmlDocPtr doc;

  doc = hwloc__libxml2_prepare_export_diff(diff, refname);
  xmlDocDumpFormatMemoryEnc(doc, (xmlChar **)xmlbuffer, buflen, "UTF-8", 1);
  xmlFreeDoc(doc);
  hwloc_libxml2_cleanup();
  return 0;
}

static void
hwloc_libxml_free_buffer(void *xmlbuffer)
{
  xmlFree(BAD_CAST xmlbuffer);
}

/*************
 * Callbacks *
 *************/

static struct hwloc_xml_callbacks hwloc_xml_libxml_callbacks = {
  hwloc_libxml_backend_init,
  hwloc_libxml_export_file,
  hwloc_libxml_export_buffer,
  hwloc_libxml_free_buffer,
  hwloc_libxml_import_diff,
  hwloc_libxml_export_diff_file,
  hwloc_libxml_export_diff_buffer
};

static struct hwloc_xml_component hwloc_libxml_xml_component = {
  NULL,
  &hwloc_xml_libxml_callbacks
};

static int
hwloc_xml_libxml_component_init(unsigned long flags)
{
  if (flags)
    return -1;
  if (hwloc_plugin_check_namespace("xml_libxml", "hwloc__xml_verbose") < 0)
    return -1;
  return 0;
}

#ifdef HWLOC_INSIDE_PLUGIN
HWLOC_DECLSPEC extern const struct hwloc_component hwloc_xml_libxml_component;
#endif

const struct hwloc_component hwloc_xml_libxml_component = {
  HWLOC_COMPONENT_ABI,
  hwloc_xml_libxml_component_init, NULL,
  HWLOC_COMPONENT_TYPE_XML,
  0,
  &hwloc_libxml_xml_component
};
