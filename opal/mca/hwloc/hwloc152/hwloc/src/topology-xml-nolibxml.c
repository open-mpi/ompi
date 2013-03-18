/*
 * Copyright © 2009 CNRS
 * Copyright © 2009-2012 inria.  All rights reserved.
 * Copyright © 2009-2011 Université Bordeaux 1
 * Copyright © 2009-2011 Cisco Systems, Inc.  All rights reserved.
 * See COPYING in top-level directory.
 */

#include <private/autogen/config.h>
#include <hwloc.h>
#include <private/private.h>
#include <private/xml.h>
#include <private/misc.h>
#include <private/debug.h>

#include <string.h>
#include <assert.h>

/*******************
 * Import routines *
 *******************/

typedef struct hwloc__nolibxml_import_state_data_s {
  char *tagbuffer; /* buffer containing the next tag */
  char *attrbuffer; /* buffer containing the next attribute of the current node */
  char *tagname; /* tag name of the current node */
  int closed; /* set if the current node is auto-closing */
} * hwloc__nolibxml_import_state_data_t;

static char *
hwloc__nolibxml_import_ignore_spaces(char *buffer)
{
  return buffer + strspn(buffer, " \t\n");
}

static int
hwloc__nolibxml_import_next_attr(hwloc__xml_import_state_t state, char **namep, char **valuep)
{
  hwloc__nolibxml_import_state_data_t nstate = (void*) state->data;
  int namelen;
  size_t len, escaped;
  char *buffer, *value, *end;

  /* find the beginning of an attribute */
  buffer = hwloc__nolibxml_import_ignore_spaces(nstate->attrbuffer);
  namelen = strspn(buffer, "abcdefghijklmnopqrstuvwxyz_");
  if (buffer[namelen] != '=' || buffer[namelen+1] != '\"')
    return -1;
  buffer[namelen] = '\0';
  *namep = buffer;

  /* find the beginning of its value, and unescape it */
  *valuep = value = buffer+namelen+2;
  len = 0; escaped = 0;
  while (value[len+escaped] != '\"') {
    if (value[len+escaped] == '&') {
      if (!strncmp(&value[1+len+escaped], "#10;", 4)) {
	escaped += 4;
	value[len] = '\n';
      } else if (!strncmp(&value[1+len+escaped], "#13;", 4)) {
	escaped += 4;
	value[len] = '\r';
      } else if (!strncmp(&value[1+len+escaped], "#9;", 3)) {
	escaped += 3;
	value[len] = '\t';
      } else if (!strncmp(&value[1+len+escaped], "quot;", 5)) {
	escaped += 5;
	value[len] = '\"';
      } else if (!strncmp(&value[1+len+escaped], "lt;", 3)) {
	escaped += 3;
	value[len] = '<';
      } else if (!strncmp(&value[1+len+escaped], "gt;", 3)) {
	escaped += 3;
	value[len] = '>';
      } else if (!strncmp(&value[1+len+escaped], "amp;", 4)) {
	escaped += 4;
	value[len] = '&';
      } else {
	return -1;
      }
    } else {
      value[len] = value[len+escaped];
    }
    len++;
    if (value[len+escaped] == '\0')
      return -1;
  }
  value[len] = '\0';

  /* find next attribute */
  end = &value[len+escaped+1]; /* skip the ending " */
  nstate->attrbuffer = hwloc__nolibxml_import_ignore_spaces(end);
  return 0;
}

static int
hwloc__nolibxml_import_find_child(hwloc__xml_import_state_t state,
				  hwloc__xml_import_state_t childstate,
				  char **tagp)
{
  hwloc__nolibxml_import_state_data_t nstate = (void*) state->data;
  hwloc__nolibxml_import_state_data_t nchildstate = (void*) childstate->data;
  char *buffer = nstate->tagbuffer;
  char *end;
  int namelen;

  childstate->parent = state;
  childstate->next_attr = state->next_attr;
  childstate->find_child = state->find_child;
  childstate->close_tag = state->close_tag;
  childstate->close_child = state->close_child;

  /* auto-closed tags have no children */
  if (nstate->closed)
    return 0;

  /* find the beginning of the tag */
  buffer = hwloc__nolibxml_import_ignore_spaces(buffer);
  if (buffer[0] != '<')
    return -1;
  buffer++;

  /* if closing tag, return nothing and do not advance */
  if (buffer[0] == '/')
    return 0;

  /* normal tag */
  *tagp = nchildstate->tagname = buffer;

  /* find the end, mark it and return it */
  end = strchr(buffer, '>');
  if (!end)
    return -1;
  end[0] = '\0';
  nchildstate->tagbuffer = end+1;

  /* handle auto-closing tags */
  if (end[-1] == '/') {
    nchildstate->closed = 1;
    end[-1] = '\0';
  } else
    nchildstate->closed = 0;

  /* find attributes */
  namelen = strspn(buffer, "abcdefghijklmnopqrstuvwxyz_");
  /* cannot be without attributes */
  assert(buffer[namelen] != '\0');

  if (buffer[namelen] != ' ')
    return -1;

  /* found a space, likely starting attributes */
  buffer[namelen] = '\0';
  nchildstate->attrbuffer = buffer+namelen+1;
  return 1;
}

static int
hwloc__nolibxml_import_close_tag(hwloc__xml_import_state_t state)
{
  hwloc__nolibxml_import_state_data_t nstate = (void*) state->data;
  char *buffer = nstate->tagbuffer;
  char *end;

  /* auto-closed tags need nothing */
  if (nstate->closed)
    return 0;

  /* find the beginning of the tag */
  buffer = hwloc__nolibxml_import_ignore_spaces(buffer);
  if (buffer[0] != '<')
    return -1;
  buffer++;

  /* find the end, mark it and return it to the parent */
  end = strchr(buffer, '>');
  if (!end)
    return -1;
  end[0] = '\0';
  nstate->tagbuffer = end+1;

  /* if closing tag, return nothing */
  if (buffer[0] != '/' || strcmp(buffer+1, nstate->tagname) )
    return -1;
  return 0;
}

static void
hwloc__nolibxml_import_close_child(hwloc__xml_import_state_t state)
{
  hwloc__nolibxml_import_state_data_t nstate = (void*) state->data;
  hwloc__nolibxml_import_state_data_t nparent = (void*) state->parent->data;
  nparent->tagbuffer = nstate->tagbuffer;
}

static int
hwloc_nolibxml_look(struct hwloc_topology *topology,
		    struct hwloc__xml_import_state_s *state)
{
  hwloc__nolibxml_import_state_data_t nstate = (void*) state->data;
  char *buffer = topology->backend_params.xml.data;

  assert(sizeof(*nstate) <= sizeof(state->data));

  /* skip headers */
  while (!strncmp(buffer, "<?xml ", 6) || !strncmp(buffer, "<!DOCTYPE ", 10)) {
    buffer = strchr(buffer, '\n');
    if (!buffer)
      goto failed;
    buffer++;
  }

  /* find topology tag */
  if (strncmp(buffer, "<topology>", 10))
    goto failed;

  state->next_attr = hwloc__nolibxml_import_next_attr;
  state->find_child = hwloc__nolibxml_import_find_child;
  state->close_tag = hwloc__nolibxml_import_close_tag;
  state->close_child = hwloc__nolibxml_import_close_child;
  state->parent = NULL;
  nstate->closed = 0;
  nstate->tagbuffer = buffer+10;
  nstate->tagname = (char *) "topology";
  nstate->attrbuffer = NULL;
  return 0; /* success */

 failed:
  return -1; /* failed */
}

static void
hwloc_nolibxml_look_failed(struct hwloc_topology *topology __hwloc_attribute_unused)
{
  /* not only when verbose */
  fprintf(stderr, "Failed to parse XML input with the minimalistic parser. If it was not\n"
	  "generated by hwloc, try enabling full XML support with libxml2.\n");
}

/********************
 * Backend routines *
 ********************/

static void
hwloc_nolibxml_backend_exit(struct hwloc_topology *topology)
{
  free(topology->backend_params.xml.data);
}

int
hwloc_nolibxml_backend_init(struct hwloc_topology *topology, const char *xmlpath, const char *xmlbuffer, int xmlbuflen)
{
  if (xmlbuffer) {
    topology->backend_params.xml.data = malloc(xmlbuflen);
    memcpy(topology->backend_params.xml.data, xmlbuffer, xmlbuflen);
  } else {
    FILE * file;
    size_t buflen = 4096, offset, readlen;
    char *buffer = malloc(buflen+1);
    size_t ret;

    if (!strcmp(xmlpath, "-"))
      xmlpath = "/dev/stdin";

    file = fopen(xmlpath, "r");
    if (!file)
      return -1;

    offset = 0; readlen = buflen;
    while (1) {
      ret = fread(buffer+offset, 1, readlen, file);

      offset += ret;
      buffer[offset] = 0;

      if (ret != readlen)
        break;

      buflen *= 2;
      buffer = realloc(buffer, buflen+1);
      readlen = buflen/2;
    }

    fclose(file);

    topology->backend_params.xml.data = buffer;
    /* buflen = offset+1; */
  }

  topology->backend_params.xml.look = hwloc_nolibxml_look;
  topology->backend_params.xml.look_failed = hwloc_nolibxml_look_failed;
  topology->backend_params.xml.backend_exit = hwloc_nolibxml_backend_exit;
  return 0;
}

/*******************
 * Export routines *
 *******************/

typedef struct hwloc__nolibxml_export_output_data_s {
  struct hwloc__xml_export_output_s generic;

  char *buffer; /* (moving) buffer where to write */
  size_t written; /* how many bytes were written (or would have be written if not truncated) */
  size_t remaining; /* how many bytes are still available in the buffer */
  unsigned indent; /* indentation level for the next line */
} * hwloc__nolibxml_export_output_data_t;

static void
hwloc__nolibxml_export_update_buffer(hwloc__nolibxml_export_output_data_t ndata, int res)
{
  if (res >= 0) {
    ndata->written += res;
    if (res >= (int) ndata->remaining)
      res = ndata->remaining>0 ? ndata->remaining-1 : 0;
    ndata->buffer += res;
    ndata->remaining -= res;
  }
}

static char *
hwloc__nolibxml_export_escape_string(const char *src)
{
  int fulllen, sublen;
  char *escaped, *dst;

  fulllen = strlen(src);

  sublen = strcspn(src, "\n\r\t\"<>&");
  if (sublen == fulllen)
    return NULL; /* nothing to escape */

  escaped = malloc(fulllen*6+1); /* escaped chars are replaced by at most 6 char */
  dst = escaped;

  memcpy(dst, src, sublen);
  src += sublen;
  dst += sublen;

  while (*src) {
    int replen;
    switch (*src) {
    case '\n': strcpy(dst, "&#10;");  replen=5; break;
    case '\r': strcpy(dst, "&#13;");  replen=5; break;
    case '\t': strcpy(dst, "&#9;");   replen=4; break;
    case '\"': strcpy(dst, "&quot;"); replen=6; break;
    case '<':  strcpy(dst, "&lt;");   replen=4; break;
    case '>':  strcpy(dst, "&gt;");   replen=4; break;
    case '&':  strcpy(dst, "&amp;");  replen=5; break;
    default: replen=0; break;
    }
    dst+=replen; src++;

    sublen = strcspn(src, "\n\r\t\"<>&");
    memcpy(dst, src, sublen);
    src += sublen;
    dst += sublen;
  }

  *dst = 0;
  return escaped;
}

static void
hwloc__nolibxml_export_new_child(hwloc__xml_export_output_t output, const char *name)
{
  hwloc__nolibxml_export_output_data_t ndata = output->data;
  int res = hwloc_snprintf(ndata->buffer, ndata->remaining, "%*s<%s", ndata->indent, "", name);
  hwloc__nolibxml_export_update_buffer(ndata, res);
  ndata->indent += 2;
}

static void
hwloc__nolibxml_export_new_prop(hwloc__xml_export_output_t output, const char *name, const char *value)
{
  hwloc__nolibxml_export_output_data_t ndata = output->data;
  char *escaped = hwloc__nolibxml_export_escape_string(value);
  int res = hwloc_snprintf(ndata->buffer, ndata->remaining, " %s=\"%s\"", name, escaped ? (const char *) escaped : value);
  hwloc__nolibxml_export_update_buffer(ndata, res);
  free(escaped);
}

static void
hwloc__nolibxml_export_end_props(hwloc__xml_export_output_t output, unsigned nr_children)
{
  hwloc__nolibxml_export_output_data_t ndata = output->data;
  int res = hwloc_snprintf(ndata->buffer, ndata->remaining, nr_children ? ">\n" : "/>\n");
  hwloc__nolibxml_export_update_buffer(ndata, res);
}

static void
hwloc__nolibxml_export_end_child(hwloc__xml_export_output_t output, const char *name, unsigned nr_children)
{
  hwloc__nolibxml_export_output_data_t ndata = output->data;
  int res;
  ndata->indent -= 2;
  if (nr_children) {
    res = hwloc_snprintf(ndata->buffer, ndata->remaining, "%*s</%s>\n", ndata->indent, "", name);
    hwloc__nolibxml_export_update_buffer(ndata, res);
  }
}

static size_t
hwloc___nolibxml_prepare_export(hwloc_topology_t topology, char *xmlbuffer, int buflen)
{
  struct hwloc__xml_export_output_s output;
  struct hwloc__nolibxml_export_output_data_s ndata;
  int res;

  output.new_child = hwloc__nolibxml_export_new_child;
  output.end_child = hwloc__nolibxml_export_end_child;
  output.new_prop = hwloc__nolibxml_export_new_prop;
  output.end_props = hwloc__nolibxml_export_end_props;
  output.data = &ndata;

  ndata.indent = 0;
  ndata.written = 0;
  ndata.buffer = xmlbuffer;
  ndata.remaining = buflen;

  res = hwloc_snprintf(ndata.buffer, ndata.remaining,
		 "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
		 "<!DOCTYPE topology SYSTEM \"hwloc.dtd\">\n");
  hwloc__nolibxml_export_update_buffer(&ndata, res);
  hwloc__nolibxml_export_new_child(&output, "topology");
  hwloc__nolibxml_export_end_props(&output, 1);
  hwloc__xml_export_object (&output, topology, hwloc_get_root_obj(topology));
  hwloc__nolibxml_export_end_child(&output, "topology", 1);

  return ndata.written+1;
}

int
hwloc_nolibxml_export_buffer(hwloc_topology_t topology, char **bufferp, int *buflenp)
{
  char *buffer;
  size_t bufferlen, res;

  bufferlen = 16384; /* random guess for large enough default */
  buffer = malloc(bufferlen);
  res = hwloc___nolibxml_prepare_export(topology, buffer, bufferlen);

  if (res > bufferlen) {
    buffer = realloc(buffer, res);
    hwloc___nolibxml_prepare_export(topology, buffer, res);
  }

  *bufferp = buffer;
  *buflenp = res;
  return 0;
}

int
hwloc_nolibxml_export_file(hwloc_topology_t topology, const char *filename)
{
  FILE *file;
  char *buffer;
  int bufferlen;
  int ret;

  ret = hwloc_nolibxml_export_buffer(topology, &buffer, &bufferlen);
  if (ret < 0)
    return -1;

  if (!strcmp(filename, "-")) {
    file = stdout;
  } else {
    file = fopen(filename, "w");
    if (!file) {
      free(buffer);
      return -1;
    }
  }

  fwrite(buffer, bufferlen-1 /* don't write the ending \0 */, 1, file);
  free(buffer);

  if (file != stdout)
    fclose(file);
  return 0;
}

void
hwloc_nolibxml_free_buffer(void *xmlbuffer)
{
  free(xmlbuffer);
}
