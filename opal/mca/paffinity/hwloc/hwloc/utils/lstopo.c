/*
 * Copyright © 2009 CNRS, INRIA, Université Bordeaux 1
 * Copyright © 2009 Cisco Systems, Inc.  All rights reserved.
 * See COPYING in top-level directory.
 */

#include <private/config.h>
#include <hwloc.h>

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <dirent.h>
#include <fcntl.h>

#ifdef HWLOC_HAVE_CAIRO
#include <cairo.h>
#endif

#include "lstopo.h"

int logical = 1;
hwloc_obj_type_t show_only = (hwloc_obj_type_t) -1;
int show_cpuset = 0;
unsigned int fontsize = 10;
unsigned int gridsize = 10;
unsigned int force_horiz = 0;
unsigned int force_vert = 0;
unsigned int top = 0;
hwloc_pid_t pid = -1;

FILE *open_file(const char *filename, const char *mode)
{
  const char *extn = strrchr(filename, '.');

  if (filename[0] == '-' && extn == filename + 1)
    return stdout;

  return fopen(filename, mode);
}

static void add_process_objects(hwloc_topology_t topology)
{
  hwloc_obj_t root;
  hwloc_cpuset_t cpuset;
  DIR *dir;
  struct dirent *dirent;
  const struct hwloc_topology_support *support;

  root = hwloc_get_root_obj(topology);

  support = hwloc_topology_get_support(topology);

  if (!support->cpubind->get_thisproc_cpubind)
    return;

  dir  = opendir("/proc");
  if (!dir)
    return;
  cpuset = hwloc_cpuset_alloc();

  while ((dirent = readdir(dir))) {
    long pid;
    char *end;
    char name[64];

    pid = strtol(dirent->d_name, &end, 10);
    if (*end)
      /* Not a number */
      continue;

    snprintf(name, sizeof(name), "%ld", pid);

#ifdef HWLOC_LINUX_SYS
    {
      char path[6 + strlen(dirent->d_name) + 1 + 7 + 1];
      snprintf(path, sizeof(path), "/proc/%s/cmdline", dirent->d_name);
      char cmd[64], *c;
      int file;
      ssize_t n;

      if ((file = open(path, O_RDONLY)) >= 0) {
        n = read(file, cmd, sizeof(cmd) - 1);
        close(file);

        if (n <= 0)
          /* Ignore kernel threads and errors */
          continue;

        cmd[n] = 0;
        if ((c = strchr(cmd, ' ')))
          *c = 0;
        snprintf(name, sizeof(name), "%ld %s", pid, cmd);
      }
    }
#endif /* HWLOC_LINUX_SYS */

    if (hwloc_get_proc_cpubind(topology, pid, cpuset, 0))
      continue;

    if (hwloc_cpuset_isincluded(root->cpuset, cpuset))
      continue;

    hwloc_topology_insert_misc_object_by_cpuset(topology, cpuset, name);
  }

  hwloc_cpuset_free(cpuset);
  closedir(dir);
}

static void usage(char *name, FILE *where)
{
  fprintf (where, "Usage: %s [ options ] ... [ filename ]\n\n", name);
  fprintf (where, "See lstopo(1) for more details.\n\n");
  fprintf (where, "Supported output file formats: .txt, .fig"
#ifdef HWLOC_HAVE_CAIRO
#if CAIRO_HAS_PDF_SURFACE
		  ", .pdf"
#endif /* CAIRO_HAS_PDF_SURFACE */
#if CAIRO_HAS_PS_SURFACE
		  ", .ps"
#endif /* CAIRO_HAS_PS_SURFACE */
#if CAIRO_HAS_PNG_FUNCTIONS
		  ", .png"
#endif /* CAIRO_HAS_PNG_FUNCTIONS */
#if CAIRO_HAS_SVG_SURFACE
		  ", .svg"
#endif /* CAIRO_HAS_SVG_SURFACE */
#endif /* HWLOC_HAVE_CAIRO */
#ifdef HWLOC_HAVE_XML
		  ", .xml"
#endif /* HWLOC_HAVE_XML */
		  "\n");
  fprintf (where, "\nOptions:\n");
  fprintf (where, "   -l --logical          Display hwloc logical object indexes (default)\n");
  fprintf (where, "   -p --physical         Display physical object indexes\n");
  fprintf (where, "   -v --verbose          Include additional detail\n");
  fprintf (where, "   -s --silent           Opposite of --verbose (default)\n");
  fprintf (where, "   -c --cpuset           Show the cpuset of each object\n");
  fprintf (where, "   -C --cpuset-only      Only show the cpuset of each ofbject\n");
  fprintf (where, "   --only <type>         Only show the given type in the text output\n");
  fprintf (where, "   --ignore <type>       Ignore objects of the given type\n");
  fprintf (where, "   --no-caches           Do not show caches\n");
  fprintf (where, "   --no-useless-caches   Do not show caches which do not have a hierarchical\n"
                  "                         impact\n");
  fprintf (where, "   --whole-system        Do not consider administration limitations\n");
  fprintf (where, "   --merge               Do not show levels that do not have a hierarcical\n"
                  "                         impact\n");
#ifdef HWLOC_HAVE_XML
  fprintf (where, "   --xml <path>          Read topology from XML file <path>\n");
#endif
#ifdef HWLOC_LINUX_SYS
  fprintf (where, "   --fsys-root <path>    Chroot containing the /proc and /sys of another system\n");
#endif
  fprintf (where, "   --pid <pid>           Detect topology as seen by process <pid>\n");
  fprintf (where, "   --top                 Display processes within the hierarchy\n");
  fprintf (where, "   --synthetic \"n:2 2\"   Simulate a fake hierarchy, here with 2 NUMA nodes of 2\n"
                  "                         processors\n");
  fprintf (where, "   --fontsize 10         Set size of text font\n");
  fprintf (where, "   --gridsize 10         Set size of margin between elements\n");
  fprintf (where, "   --horiz               Horizontal graphic layout instead of nearly 4/3 ratio\n");
  fprintf (where, "   --vert                Vertical graphic layout instead of nearly 4/3 ratio\n");
  fprintf (where, "   --version             Report version and exit\n");
}

int
main (int argc, char *argv[])
{
  int err;
  int verbose_mode = 1;
  hwloc_topology_t topology;
  const char *filename = NULL;
  unsigned long flags = 0;
  int merge = 0;
  int ignorecache = 0;
  char * callname;
  char * synthetic = NULL;
  const char * xmlpath = NULL;
  char * fsysroot = NULL;
  int force_console = 0;
  int opt;

  callname = strrchr(argv[0], '/');
  if (!callname)
    callname = argv[0];
  else
    callname++;

  err = hwloc_topology_init (&topology);
  if (err)
    return EXIT_FAILURE;

  while (argc >= 2)
    {
      opt = 0;
      if (!strcmp (argv[1], "-v") || !strcmp (argv[1], "--verbose")) {
	verbose_mode++;
	force_console = 1;
      } else if (!strcmp (argv[1], "-s") || !strcmp (argv[1], "--silent")) {
	verbose_mode--;
	force_console = 1;
      } else if (!strcmp (argv[1], "-h") || !strcmp (argv[1], "--help")) {
	usage(callname, stdout);
        exit(EXIT_SUCCESS);
      } else if (!strcmp (argv[1], "-l") || !strcmp (argv[1], "--logical"))
	logical = 1;
      else if (!strcmp (argv[1], "-p") || !strcmp (argv[1], "--physical"))
	logical = 0;
      else if (!strcmp (argv[1], "-c") || !strcmp (argv[1], "--cpuset"))
	show_cpuset = 1;
      else if (!strcmp (argv[1], "-C") || !strcmp (argv[1], "--cpuset-only"))
	show_cpuset = 2;
      else if (!strcmp (argv[1], "--only")) {
	if (argc <= 2) {
	  usage (callname, stderr);
	  exit(EXIT_FAILURE);
	}
        show_only = hwloc_obj_type_of_string(argv[2]);
	opt = 1;
      }
      else if (!strcmp (argv[1], "--ignore")) {
	if (argc <= 2) {
	  usage (callname, stderr);
	  exit(EXIT_FAILURE);
	}
        hwloc_topology_ignore_type(topology, hwloc_obj_type_of_string(argv[2]));
	opt = 1;
      }
      else if (!strcmp (argv[1], "--no-caches"))
	ignorecache = 2;
      else if (!strcmp (argv[1], "--no-useless-caches"))
	ignorecache = 1;
      else if (!strcmp (argv[1], "--whole-system"))
	flags |= HWLOC_TOPOLOGY_FLAG_WHOLE_SYSTEM;
      else if (!strcmp (argv[1], "--merge"))
	merge = 1;
      else if (!strcmp (argv[1], "--horiz"))
        force_horiz = 1;
      else if (!strcmp (argv[1], "--vert"))
        force_vert = 1;
      else if (!strcmp (argv[1], "--fontsize")) {
	if (argc <= 2) {
	  usage (callname, stderr);
	  exit(EXIT_FAILURE);
	}
	fontsize = atoi(argv[2]);
	opt = 1;
      }
      else if (!strcmp (argv[1], "--gridsize")) {
	if (argc <= 2) {
	  usage (callname, stderr);
	  exit(EXIT_FAILURE);
	}
	gridsize = atoi(argv[2]);
	opt = 1;
      }
      else if (!strcmp (argv[1], "--synthetic")) {
	if (argc <= 2) {
	  usage (callname, stderr);
	  exit(EXIT_FAILURE);
	}
	synthetic = argv[2]; opt = 1;
      } else if (!strcmp (argv[1], "--xml")) {
#ifdef HWLOC_HAVE_XML
	if (argc <= 2) {
	  usage (callname, stderr);
	  exit(EXIT_FAILURE);
	}
	xmlpath = argv[2]; opt = 1;
	if (!strcmp(xmlpath, "-")) {
	  xmlpath = "/dev/stdin";
        }
#else /* HWLOC_HAVE_XML */
        fprintf(stderr, "This installation of hwloc does not support --xml, sorry.\n");
        exit(EXIT_FAILURE);
#endif /* HWLOC_HAVE_XML */
      } else if (!strcmp (argv[1], "--fsys-root")) {
#ifdef HWLOC_LINUX_SYS
	if (argc <= 2) {
	  usage (callname, stderr);
	  exit(EXIT_FAILURE);
	}
	fsysroot = argv[2]; opt = 1;
#else /* HWLOC_LINUX_SYS */
        fprintf(stderr, "This installation of hwloc does not support --fsys-root, sorry.\n");
        exit(EXIT_FAILURE);
#endif /* HWLOC_LINUX_SYS */
      } else if (!strcmp (argv[1], "--pid")) {
	if (argc <= 2) {
	  usage (callname, stderr);
	  exit(EXIT_FAILURE);
	}
	pid = atoi(argv[2]); opt = 1;
      } else if (!strcmp (argv[1], "--top"))
        top = 1;
      else if (!strcmp (argv[1], "--version")) {
          printf("%s %s\n", callname, VERSION);
          exit(EXIT_SUCCESS);
      } else {
	if (filename) {
	  fprintf (stderr, "Unrecognized options: %s\n", argv[1]);
	  usage (callname, stderr);
	  exit(EXIT_FAILURE);
	} else
	  filename = argv[1];
      }
      argc -= opt+1;
      argv += opt+1;
    }

  if (show_only != (hwloc_obj_type_t)-1) {
    merge = 0;
    force_console = 1;
  }
  if (show_cpuset)
    force_console = 1;

  hwloc_topology_set_flags(topology, flags);

  if (ignorecache > 1) {
    hwloc_topology_ignore_type(topology, HWLOC_OBJ_CACHE);
  } else if (ignorecache) {
    hwloc_topology_ignore_type_keep_structure(topology, HWLOC_OBJ_CACHE);
  }
  if (merge)
    hwloc_topology_ignore_all_keep_structure(topology);

  if (synthetic)
    if (hwloc_topology_set_synthetic(topology, synthetic))
      return EXIT_FAILURE;
  if (xmlpath) {
    if (hwloc_topology_set_xml(topology, xmlpath)) {
      perror("Setting target XML file");
      return EXIT_FAILURE;
    }
  }
  if (fsysroot) {
    if (hwloc_topology_set_fsroot(topology, fsysroot)) {
      perror("Setting target filesystem root");
      return EXIT_FAILURE;
    }
  }
  if (pid > 0) {
    if (hwloc_topology_set_pid(topology, pid)) {
      perror("Setting target pid");
      return EXIT_FAILURE;
    }
  }

  err = hwloc_topology_load (topology);
  if (err)
    return EXIT_FAILURE;

  if (top)
    add_process_objects(topology);

  if (!filename && !strcmp(callname,"hwloc-info")) {
    /* behave kind-of plpa-info */
    filename = "-";
    verbose_mode--;
  }

  if (!filename) {
#ifdef HWLOC_HAVE_CAIRO
#if CAIRO_HAS_XLIB_SURFACE && defined HWLOC_HAVE_X11
    if (!force_console && getenv("DISPLAY"))
      output_x11(topology, NULL, logical, verbose_mode);
    else
#endif /* CAIRO_HAS_XLIB_SURFACE */
#endif /* HWLOC_HAVE_CAIRO */
#ifdef HWLOC_WIN_SYS
      output_windows(topology, NULL, logical, verbose_mode);
#else
    output_console(topology, NULL, logical, verbose_mode);
#endif
  } else if (!strcmp(filename, "-")
	  || !strcmp(filename, "/dev/stdout"))
    output_console(topology, filename, logical, verbose_mode);
  else if (strstr(filename, ".txt"))
    output_text(topology, filename, logical, verbose_mode);
  else if (strstr(filename, ".fig"))
    output_fig(topology, filename, logical, verbose_mode);
#ifdef HWLOC_HAVE_CAIRO
#if CAIRO_HAS_PNG_FUNCTIONS
  else if (strstr(filename, ".png"))
    output_png(topology, filename, logical, verbose_mode);
#endif /* CAIRO_HAS_PNG_FUNCTIONS */
#if CAIRO_HAS_PDF_SURFACE
  else if (strstr(filename, ".pdf"))
    output_pdf(topology, filename, logical, verbose_mode);
#endif /* CAIRO_HAS_PDF_SURFACE */
#if CAIRO_HAS_PS_SURFACE
  else if (strstr(filename, ".ps"))
    output_ps(topology, filename, logical, verbose_mode);
#endif /* CAIRO_HAS_PS_SURFACE */
#if CAIRO_HAS_SVG_SURFACE
  else if (strstr(filename, ".svg"))
    output_svg(topology, filename, logical, verbose_mode);
#endif /* CAIRO_HAS_SVG_SURFACE */
#endif /* HWLOC_HAVE_CAIRO */
#ifdef HWLOC_HAVE_XML
  else if (strstr(filename, ".xml"))
    output_xml(topology, filename, logical, verbose_mode);
#endif
  else {
    fprintf(stderr, "file format not supported\n");
    usage(callname, stderr);
    exit(EXIT_FAILURE);
  }

  hwloc_topology_destroy (topology);

  return EXIT_SUCCESS;
}
