/*
 * Copyright (c) 2015-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2016      IBM Corporation.  All rights reserved.
 *
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "src/include/pmix_config.h"

#include <string.h>
#ifdef HAVE_UNISTD_H
#    include <unistd.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#    include <sys/types.h>
#endif
#ifdef HAVE_SYS_STAT_H
#    include <sys/stat.h>
#endif
#ifdef HAVE_FCNTL_H
#    include <fcntl.h>
#endif
#include <jansson.h>
#include <curl/curl.h>
#include <time.h>

#include "pmix_common.h"

#include "src/class/pmix_bitmap.h"
#include "src/class/pmix_list.h"
#include "src/class/pmix_pointer_array.h"
#include "src/include/pmix_globals.h"
#include "src/mca/preg/preg.h"
#include "src/threads/pmix_threads.h"
#include "src/util/pmix_argv.h"
#include "src/util/pmix_error.h"
#include "src/util/pmix_name_fns.h"
#include "src/util/pmix_output.h"
#include "src/util/pmix_environ.h"
#include "src/util/pmix_printf.h"
#include "src/util/pmix_show_help.h"

#include "pnet_sshot.h"
#include "src/mca/pnet/base/base.h"
#include "src/mca/pnet/pnet.h"

/* internal variables */
static pmix_pointer_array_t mygroups, node_names;

static const char *ama_object_string = "mac";
static const char *device_object_string = "device";
static const char *node_name_delim = ",";
static const char *json_string_fmt = "{\"hosts\":[%s]}";
static const char *ama_format = "%hhx:%hhx:%hhx:%hhx:%hhx:%hhx";
static const char *node_nic_name_format = "%s-%s";
static const char *switch_delim = ":";
static size_t node_nic_name_size = 128;

typedef struct
{
    char *payload;
    size_t size;
} vnid_response_t;

typedef struct
{
    unsigned int group_id;
    unsigned int switch_id;
    unsigned port_id;
} coordinates;

/*Joins host_name and device_name: host-device.*/
static char *join_names(size_t size, const char *node_name, const char *nic_name, const char *format)
{
    char buffer[node_nic_name_size];
    int n = snprintf(buffer, size, format, node_name, nic_name);
    if (n < 0) {
	return NULL;
    }
    return strdup(buffer);
}

/* Parse out nodenames passed in from RM (e.g., pbs). */
static void extract_node_names(pmix_pointer_array_t *name_arr, char *names)
{
    char *token = strtok(strdup(names), node_name_delim);
    while (token != NULL) {
	pmix_pointer_array_add(name_arr, strdup(token));
	token = strtok(NULL, node_name_delim);
    }
    for (int p = 0; p<name_arr->size; p++) {
	char *name = pmix_pointer_array_get_item(name_arr, p);
	if (!name) {
	    break;
	}
    }
}


/* Helper function to parse out string ama */
static int parse_ama(const char *ama, coordinates *coord)
{
    int mac_address[8] = {0};
    if (sscanf(ama, ama_format,
	       &mac_address[5],
	       &mac_address[4],
	       &mac_address[3],
	       &mac_address[2],
	       &mac_address[1],
	       &mac_address[0]
	    ) != 6)
	{

	    return 1;
	}

    // Fabric Group
    coord->group_id = ((mac_address[3] & 0xF) << 2) |
	((mac_address[4] & 0xC0) >> 6);

    // Switch ID
    coord->switch_id = ((mac_address[4] & 0x3F) << 2) |
	((mac_address[5] & 0xC0) >> 6);

    // Switch Port
    coord->port_id = mac_address[5] & 0x3F;
    return 0;
}

/* Internal structures to store topology state.*/
typedef struct {
    pmix_object_t super;
    int switch_id;
    char **members;
} sshot_switch_t;

static void switch_con(sshot_switch_t *swtch)
{
    swtch->switch_id = 0;
    swtch->members = NULL;
}

static void switch_des(sshot_switch_t *swtch)
{
    if (NULL != swtch->members) {
	PMIx_Argv_free(swtch->members);
    }
}

static PMIX_CLASS_INSTANCE(sshot_switch_t, pmix_object_t, switch_con, switch_des);

typedef struct {
    pmix_object_t super;
    int group_id;
    pmix_pointer_array_t switches;
} sshot_group_t;

static void group_con(sshot_group_t *group)
{
    group->group_id = 0;
    PMIX_CONSTRUCT(&group->switches, pmix_pointer_array_t);
    // overkill on the size
    pmix_pointer_array_init(&group->switches, 256, INT_MAX, 256);
}

static void group_des(sshot_group_t *group)
{
    PMIX_DESTRUCT(&group->switches);
}

static PMIX_CLASS_INSTANCE(sshot_group_t, pmix_object_t, group_con, group_des);

/* internal functions */
size_t curl_callback (void *contents, size_t size, size_t nmemb, void *userp);
static int ask_fabric_controller(char *vnid_url, char *vnid_username, char *credential, char *nodes, const char *fmt, vnid_response_t *response);

pmix_status_t pmix_pnet_sshot_register_fabric(pmix_fabric_t *fabric, const pmix_info_t directives[],
                                              size_t ndirs, pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    PMIX_HIDE_UNUSED_PARAMS(cbdata);
    PMIX_HIDE_UNUSED_PARAMS(ndirs);
    PMIX_HIDE_UNUSED_PARAMS(cbfunc);
    PMIX_HIDE_UNUSED_PARAMS(directives);

    int num_devices = 0, num_nodes = 0;
    int err = 0;
    json_t *root = NULL, *macs = NULL, *devices = NULL;
    json_error_t jerr;
    vnid_response_t response;
    response.size = 0;
    response.payload = NULL;

    if (NULL ==  pmix_mca_pnet_sshot_component.vnid_url) {
	return PMIX_ERROR;
    }
    if (NULL == pmix_mca_pnet_sshot_component.credential) {
	fprintf(stderr, "credential is %s\n", pmix_mca_pnet_sshot_component.credential);
	return PMIX_ERROR;
    }

    if (NULL == pmix_mca_pnet_sshot_component.nodes) {
      return PMIX_ERROR;
    }

    PMIX_CONSTRUCT(&node_names, pmix_pointer_array_t);
    pmix_pointer_array_init(&node_names, 128, INT_MAX, 128);
    extract_node_names(&node_names, pmix_mca_pnet_sshot_component.nodes);

    PMIX_CONSTRUCT(&mygroups, pmix_pointer_array_t);
    pmix_pointer_array_init(&mygroups, 64, INT_MAX, 64);

    err = ask_fabric_controller(pmix_mca_pnet_sshot_component.vnid_url,
				pmix_mca_pnet_sshot_component.vnid_username,
				pmix_mca_pnet_sshot_component.credential,
				pmix_mca_pnet_sshot_component.nodes,
				json_string_fmt,
				&response
				);
    if (err) {
	free(response.payload);
	return PMIX_ERROR;
    }
    root = json_loads(response.payload, 0, &jerr);
    free(response.payload);
    if (NULL == root) {
	return PMIX_ERROR;
    }

    macs = json_object_get(root, ama_object_string);
    if (!macs) {
	return PMIX_ERROR;
    }

    devices = json_object_get(root, device_object_string);
    if (!devices) {
	return PMIX_ERROR;
    }

    num_nodes = json_array_size(macs);
    num_devices = json_array_size(devices);

    if (num_nodes != num_devices) {
	return PMIX_ERROR;
    }

    for (int i = 0; i < num_nodes; i++) {
	json_t *mac_array = json_array_get(macs, i);
	json_t *dev_array = json_array_get(devices, i);
	char *nname = pmix_pointer_array_get_item(&node_names, i);
	int nics_per_node = json_array_size(mac_array);
	int macs_per_node = json_array_size(dev_array);
	if (nics_per_node != macs_per_node) {
	    return PMIX_ERROR;
	}
	for (int j = 0; j<nics_per_node; j++) {
	    json_t *node_macs = json_array_get(mac_array, j);
	    json_t *node_devs = json_array_get(dev_array, j);
	    const char *addr = json_string_value(node_macs);
	    const char *dev = json_string_value(node_devs);
	    coordinates coord = {0};
	    err = parse_ama(addr, &coord);
	    if (err) {
		return PMIX_ERROR;
	    }
	    sshot_group_t *grp;
	    sshot_switch_t *swtch;
	    // do we have this group already?
	    // no
	    if (NULL == (grp = (sshot_group_t*)pmix_pointer_array_get_item(&mygroups, coord.group_id))) {
		grp = PMIX_NEW(sshot_group_t);
		grp->group_id = coord.group_id;
		pmix_pointer_array_set_item(&mygroups, grp->group_id, grp);
	    }
		// now we add the switch.
	    if (NULL == (swtch = (sshot_switch_t*)pmix_pointer_array_get_item(&grp->switches, coord.switch_id))) {
		swtch = PMIX_NEW(sshot_switch_t);
		swtch->switch_id = coord.switch_id;
		pmix_pointer_array_set_item(&grp->switches, swtch->switch_id, swtch);
	    }
	    char *full_name = join_names(node_nic_name_size, nname, dev, node_nic_name_format);
	    PMIx_Argv_append_nosize(&swtch->members, full_name);
	}
    }
    // now loop through all group objs, and each switch, and generate the string
    // format is: group_id:switch_id:mem1,mem2,...,memn;group_id:switch_id:mem1,mem2,....
    char **group_members = NULL, *all_info = NULL;
    for (int i = 0; i<mygroups.size; i++) {
	sshot_group_t *grp = (sshot_group_t*)pmix_pointer_array_get_item(&mygroups, i);
	if (grp) {
	    char *switch_members = NULL;
	    for (int j = 0; j<grp->switches.size; j++) {

		sshot_switch_t *swtch = (sshot_switch_t*)pmix_pointer_array_get_item(&grp->switches, j);
		if (swtch) {
		    char *switch_members2 = PMIx_Argv_join(swtch->members, ',');
		    pmix_asprintf(&switch_members, "%d%s%s", swtch->switch_id, switch_delim, switch_members2);
		    free(switch_members2);
		} else {
		    continue;
		}
	    }
	    char *group_members2 = NULL;
	    pmix_asprintf(&group_members2, "%d:%s", grp->group_id, switch_members);
	    PMIx_Argv_append_nosize(&group_members, group_members2);
	    free(group_members2);
	} else {
	    continue;
	}
    }
    if (group_members) {
	all_info = PMIx_Argv_join(group_members, ';');
	PMIX_INFO_CREATE(fabric->info, 1);
	fabric->ninfo = 1;
	PMIX_INFO_LOAD(&fabric->info[0], PMIX_FABRIC_GROUPS, all_info, PMIX_STRING);
	free(all_info);
    }
    return PMIX_OPERATION_SUCCEEDED;
}

int ask_fabric_controller(char *vnid_url, char *vnid_username, char *credential, char *nodes, const char *fmt, vnid_response_t *response)
{
    char *query_str = NULL;
    int query_str_length = 0;
    char full_credential[128] = {0};

    sprintf(full_credential, "%s:%s", vnid_username, credential);

    query_str_length = strlen(nodes) + strlen(fmt);
    query_str = (char*)calloc(query_str_length, sizeof(*query_str));
    sprintf(query_str, fmt, nodes);

    CURL *curl = curl_easy_init();
    curl_easy_setopt(curl, CURLOPT_URL, vnid_url);
    curl_easy_setopt(curl, CURLOPT_POSTFIELDS, query_str);
    curl_easy_setopt(curl, CURLOPT_USERPWD, full_credential);
    curl_easy_setopt(curl, CURLOPT_SSL_VERIFYHOST, 0);
    curl_easy_setopt(curl, CURLOPT_SSL_VERIFYPEER, 0);
    curl_easy_setopt(curl, CURLOPT_TIMEOUT, 10);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, curl_callback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, response);

    CURLcode res = curl_easy_perform(curl);
    if (res != CURLE_OK) {
	free(query_str);
	return 1;
    }
    free(query_str);
    return 0;
}


/* callback for curl fetch */
size_t curl_callback (void *contents, size_t size, size_t nmemb, void *userp)
{
    size_t realsize = size * nmemb;                             /* calculate buffer size */
    vnid_response_t *p = (vnid_response_t *) userp;   /* cast pointer to fetch struct */
    /* expand buffer using a temporary pointer to avoid memory leaks */
    char *temp = realloc(p->payload, p->size + realsize + 1);

    /* check allocation */
    if (temp == NULL) {
	/* this isn't good */
	free(p->payload);
	return 1;
    }

    /* assign payload */
    p->payload = temp;

    /* copy contents to buffer */
    memcpy(&(p->payload[p->size]), contents, realsize);

    /* set new buffer size */
    p->size += realsize;
    /* ensure null termination */
    p->payload[p->size] = 0;
    /* return size */
    return realsize;
}
