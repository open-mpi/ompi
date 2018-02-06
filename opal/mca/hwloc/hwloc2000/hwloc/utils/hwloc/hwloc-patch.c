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
	fprintf(where, "Usage: hwloc-patch [options] [<old.xml> | refname] [<diff.xml> | -] [<output.xml>]\n");
	fprintf(where, "Options:\n");
	fprintf(where, "  -R --reverse     Reverse the sense of the difference\n");
	fprintf(where, "  --version        Report version and exit\n");
}

static int hwloc_diff_read(const char *inputdiff,
			   hwloc_topology_diff_t *firstdiffp, char **refnamep)
{
	size_t buflen, offset, readlen;
	char *buffer, *tmp;
	size_t ret;
	int err;

	if (strcmp(inputdiff, "-"))
		return hwloc_topology_diff_load_xml(inputdiff, firstdiffp, refnamep);

	buflen = 4096;
	buffer = malloc(buflen+1); /* one more byte for the ending \0 */
	if (!buffer)
		goto out;

	offset = 0; readlen = buflen;
	while (1) {
		ret = fread(buffer+offset, 1, readlen, stdin);

		offset += ret;
		buffer[offset] = 0;

		if (ret != readlen)
			break;

		buflen *= 2;
		tmp = realloc(buffer, buflen+1);
		if (!tmp) {
			fprintf(stderr, "Failed to realloc buffer for reading diff.\n");
			goto out_with_buffer;
		}
		buffer = tmp;
		readlen = buflen/2;
	}

	err = hwloc_topology_diff_load_xmlbuffer(buffer, (int)(offset+1), firstdiffp, refnamep);
	free(buffer);
	return err;

out_with_buffer:
	free(buffer);
out:
	return -1;
}

int main(int argc, char *argv[])
{
	hwloc_topology_t topo;
	hwloc_topology_diff_t firstdiff = NULL;
	unsigned long flags = HWLOC_TOPOLOGY_FLAG_WHOLE_SYSTEM;
	unsigned long patchflags = 0;
	char *callname, *input, *inputdiff, *output = NULL, *refname = NULL;
	int err;

	callname = argv[0];
	/* skip argv[0], handle options */
	argc--;
	argv++;

	hwloc_utils_check_api_version(callname);

	putenv((char *) "HWLOC_XML_VERBOSE=1");

	while (argc && *argv[0] == '-') {
		if (!strcmp (argv[0], "-R") || !strcmp (argv[0], "--reverse")) {
			patchflags ^= HWLOC_TOPOLOGY_DIFF_APPLY_REVERSE;
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
	input = argv[0];
	inputdiff = argv[1];
	argc -= 2;
	argv += 2;
	if (argc >= 1) {
		output = argv[0];
		argc--;
		argv++;
	}

	/* load the diff and get the refname */
	err = hwloc_diff_read(inputdiff, &firstdiff, &refname);
	if (err < 0) {
		fprintf(stderr, "Failed to load XML topology diff %s\n", inputdiff);
		goto out;
	}

	/* load the input topology */
	hwloc_topology_init(&topo);
	hwloc_topology_set_all_types_filter(topo, HWLOC_TYPE_FILTER_KEEP_ALL);
	hwloc_topology_set_flags(topo, flags);
	if (!strcmp(input, "refname")) {
		/* use the diff refname as input */
		if (!refname) {
			fprintf(stderr, "Couldn't find the reference topology name from the input diff %s\n", inputdiff);
			goto out_with_topo;
		}
		err = hwloc_topology_set_xml(topo, refname);
		if (err < 0) {
			fprintf(stderr, "Failed to load XML topology %s (from input diff %s refname)\n", refname, inputdiff);
			goto out_with_topo;
		}
	} else {
		/* use the given input */
		err = hwloc_topology_set_xml(topo, input);
		if (err < 0) {
			fprintf(stderr, "Failed to load XML topology %s\n", input);
			goto out_with_topo;
		}
	}

	hwloc_topology_load(topo);

	err = hwloc_topology_diff_apply(topo, firstdiff, patchflags);
	if (err < 0) {
		fprintf(stderr, "Failed to%s apply topology diff %s, failed for hunk #%d hunk\n",
			(patchflags & HWLOC_TOPOLOGY_DIFF_APPLY_REVERSE) ? " reverse" : "",
			inputdiff, -err);
		goto out_with_topo;
	}

	err = hwloc_topology_export_xml(topo, output ? output : input, 0);
	if (err < 0) {
		fprintf(stderr, "Failed to export patched topology %s\n", output);
		goto out_with_topo;
	}

	hwloc_topology_destroy(topo);
	hwloc_topology_diff_destroy(firstdiff);

	exit(EXIT_SUCCESS);

out_with_topo:
	hwloc_topology_destroy(topo);
	hwloc_topology_diff_destroy(firstdiff);
out:
	exit(EXIT_FAILURE);
}
