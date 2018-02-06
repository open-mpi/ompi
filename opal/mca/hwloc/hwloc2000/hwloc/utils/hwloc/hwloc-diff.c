/*
 * Copyright © 2013-2018 Inria.  All rights reserved.
 * See COPYING in top-level directory.
 */

#include <private/autogen/config.h>
#include <hwloc.h>
#include <hwloc/diff.h>

#include "misc.h"

void usage(const char *callname __hwloc_attribute_unused, FILE *where)
{
	fprintf(where, "Usage: hwloc-diff [options] <old.xml> <new.xml> [<output.diff.xml>]\n");
	fprintf(where, "Options:\n");
	fprintf(where, "  --refname        Change the XML reference identifier in the output\n");
	fprintf(where, "                   (default is the filename of the first topology\n");
	fprintf(where, "  --version        Report version and exit\n");
}

int main(int argc, char *argv[])
{
	hwloc_topology_t topo1, topo2;
	hwloc_topology_diff_t firstdiff = NULL, diff;
	unsigned long flags = HWLOC_TOPOLOGY_FLAG_WHOLE_SYSTEM;
	char *callname, *input1, *input2, *output, *outputname, *refname = NULL;
	char *xmlbuffer;
	int xmlbuflen;
	unsigned i, j;
	int err;

	callname = argv[0];
	/* skip argv[0], handle options */
	argc--;
	argv++;

	hwloc_utils_check_api_version(callname);

	putenv((char *) "HWLOC_XML_VERBOSE=1");

	while (argc && *argv[0] == '-') {
		if (!strcmp (argv[0], "--refname")) {
			refname = argv[1];
			argc--;
			argv++;
		} else if (!strcmp (argv[0], "--version")) {
			printf("%s %s\n", callname, HWLOC_VERSION);
			exit(EXIT_SUCCESS);
		} else {
			fprintf(stderr, "Unrecognized options: %s\n", argv[0]);
			usage(callname, stderr);
			exit(EXIT_FAILURE);
		}
		argc--;
		argv++;
	}

	if (argc < 2) {
		usage(callname, stderr);
		exit(EXIT_FAILURE);
	}
	input1 = argv[0];
	input2 = argv[1];
	argc -= 2;
	argv += 2;
	if (argc >= 1) {
		output = argv[0];
		outputname = argv[0];
		argc--;
		argv++;
	} else  {
		output = NULL;
		outputname = (char *) "stdout";
	}

	hwloc_topology_init(&topo1);
	hwloc_topology_set_all_types_filter(topo1, HWLOC_TYPE_FILTER_KEEP_ALL);
	hwloc_topology_set_flags(topo1, flags);
	err = hwloc_topology_set_xml(topo1, input1);
	if (err < 0) {
		fprintf(stderr, "Failed to load 1st XML topology %s\n", input1);
		goto out;
	}
	hwloc_topology_load(topo1);

	hwloc_topology_init(&topo2);
	hwloc_topology_set_all_types_filter(topo2, HWLOC_TYPE_FILTER_KEEP_ALL);
	hwloc_topology_set_flags(topo2, flags);
	err = hwloc_topology_set_xml(topo2, input2);
	if (err < 0) {
		fprintf(stderr, "Failed to load 2nd XML topology %s\n", input2);
		goto out_with_topo1;
	}
	hwloc_topology_load(topo2);

	if (!refname) {
		refname = strrchr(input1, '/');
		if (refname)
			refname++;
		else
			refname = input1;
	}

	err = hwloc_topology_diff_build(topo1, topo2, 0, &firstdiff);
	if (err < 0) {
		fprintf(stderr, "Failed to compute the diff (%s)\n", strerror(errno));
		goto out_with_topo2;
	}

	diff = firstdiff;
	i = 0, j = 0;
	while (diff) {
		i++;
		if (diff->generic.type == HWLOC_TOPOLOGY_DIFF_TOO_COMPLEX)
			j++;
		diff = diff->generic.next;
	}
	if (!i) {
		fprintf(stderr, "Found no difference, exporting empty topology diff to %s\n", outputname);
	} else if (!j) {
		fprintf(stderr, "Found %u differences, exporting to %s\n", i, outputname);
	} else {
		fprintf(stderr, "Found %u differences, including %u too complex ones.\n", i, j);
		fprintf(stderr, "Cannot export differences to %s\n", outputname);
	}
	if (!j) {
		if (output) {
			err = hwloc_topology_diff_export_xml(firstdiff, refname, output);
		} else {
			err = hwloc_topology_diff_export_xmlbuffer(firstdiff, refname, &xmlbuffer, &xmlbuflen);
			if (!err) {
				printf("%s\n", xmlbuffer);
				hwloc_free_xmlbuffer(topo1, xmlbuffer);
			}
		}
		if (err < 0)
			fprintf(stderr, "Failed to export topology diff %s\n", output);
	}

	hwloc_topology_diff_destroy(firstdiff);

	hwloc_topology_destroy(topo2);
	hwloc_topology_destroy(topo1);

	if (j)
	  exit(EXIT_FAILURE);
	else
	  exit(EXIT_SUCCESS);

out_with_topo2:
	hwloc_topology_destroy(topo2);
out_with_topo1:
	hwloc_topology_destroy(topo1);
out:
	exit(EXIT_FAILURE);
}
