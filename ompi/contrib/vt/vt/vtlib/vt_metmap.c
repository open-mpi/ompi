/**
 * VampirTrace
 * http://www.tu-dresden.de/zih/vampirtrace
 *
 * Copyright (c) 2005-2008, ZIH, TU Dresden, Federal Republic of Germany
 *
 * Copyright (c) 1998-2005, Forschungszentrum Juelich, Juelich Supercomputing
 *                          Centre, Federal Republic of Germany
 *
 * See the file COPYING in the package base directory for details
 **/

#include "config.h"

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>

#include "vt_env.h"
#include "vt_error.h"
#include "vt_metric.h"

/* metricmap operations */

static vt_metricmap_t* vt_metricmap_append(vt_metricmap_t* map,
					   vt_metmap_t type,
					   char* event, char* alias)
{
    /*printf("Def 0x%X %s = <%s>\n", type, event, alias);*/

    if (map == NULL) {
        map = (vt_metricmap_t*)calloc(1, sizeof(vt_metricmap_t));
        if (map == NULL) {
            vt_cntl_msg("Metricmap creation failed!");
            return NULL;
        }
        /*printf("Created new metricmap head @0x%p\n", map);*/
    } else {
        while (map->next != NULL) map = map->next;
        map->next = (vt_metricmap_t*)calloc(1, sizeof(vt_metricmap_t));
        if (map->next == NULL) {
            vt_cntl_msg("Metricmap append failed!");
            return NULL;
        }
        map = map->next;
        /*printf("Created new metricmap node @0x%p\n", map);*/
    }

    map->type = type;
    map->event_name = strdup(event);
    map->alias_name = strdup(alias);
    map->next = NULL;
    
    return map;
}

void vt_metricmap_dump(vt_metricmap_t* map)
{
    unsigned j=0;

    if (map == NULL) {
        printf("Can't dump empty metricmap!\n");
        return;
    }

    printf("Metricmap dump (head=0x%p):\n", (void*)map);
    while (map != NULL) {
        printf("m[%3u] 0x%X %s = %s\n", j, map->type,
                map->event_name, map->alias_name);
        j++;
        map = map->next;
    }
    printf("Metricmap dumped %u maps\n", j);
}

void vt_metricmap_free(vt_metricmap_t* map)
{
    if (map == NULL) {
        /*printf("Can't free empty metricmap!\n");*/
        return;
    }

    /*printf("Metricmap free (head=0x%p):\n", map);*/
    while (map != NULL) {
        vt_metricmap_t* next = map->next;
        if (map->event_name != NULL) free(map->event_name);
        if (map->alias_name != NULL) free(map->alias_name);
        free(map);
        map = next;
    }
}

/* METRICS.SPEC entry format: */
/* measure MEASURE_DEF = NAME                // no operators */
/* aggroup AGGROUP_DEF = NAME1 NAME2 ...     // no operators */
/* compose COMPOSE_DEF = NAME1 + NAME2 + ... // one or more "+" */
/* compute COMPUTE_DEF = NAME1 & NAME2 & ... // "&" is "+-* /" */

/* initialize metric map */
vt_metricmap_t* vt_metricmap_init(vt_metmap_t match)
{
    vt_metricmap_t* mapv = NULL, *map = NULL;
    char* specfile = vt_env_metrics_spec();
    unsigned lineno=0, defs=0;
    unsigned invalid_defs=0, unknown_defs=0;
    unsigned measure_defs=0, aggroup_defs=0, compose_defs=0, compute_defs=0;
    char line[1024];
    FILE *fp;

    if (!specfile) return NULL;

    fp = fopen(specfile, "r"); 
    if (fp == NULL) {
        vt_cntl_msg("Failed to open metric specification %s: %s",
		    specfile, strerror(errno));
        return NULL;
    }

    /*printf("specfile=%s match=0x%X\n", specfile, match);*/

    while (fgets(line, sizeof(line), fp)) {
        vt_metmap_t type=VT_METMAP_UNKNOWN;
        char* def_name, *def_args;
        int len = strcspn(line, "#\n"); /* length of non-comment string */
        while (len && ((line[len-1] == ' ') || (line[len-1] == '\t'))) len--;
        line[len] = '\0'; /* chop comment and return */
        lineno++;
        if (len <= 1) continue; 
        defs++;
        if      (!strncmp("measure", line, 7)) type=VT_METMAP_MEASURE;
        else if (!strncmp("compose", line, 7)) type=VT_METMAP_COMPOSE;
        else if (!strncmp("compute", line, 7)) type=VT_METMAP_COMPUTE;
        else if (!strncmp("aggroup", line, 7)) type=VT_METMAP_AGGROUP;
        /*printf("%3d:%2d %d-[%2d] %s\n", lineno, defs, type, len, line);*/
        if (type == VT_METMAP_UNKNOWN) {
            unknown_defs++;
            vt_cntl_msg("Failed to parse metric definition line %d: %s", lineno, line);
            continue;
        }
        line[7] = '\0'; /* terminate definition type */
        def_name = line + 8;
        def_name += strspn(def_name, " \t"); /* get start of definition name */
        len = strcspn(def_name, "= \t"); /* length of definition name */
        *(def_name+len)='\0'; /* terminate definition name */
        def_args = line + 8 + len + 1;
        def_args += strspn(def_args, "= \t"); /* get start of def argument */
        /*printf("Def %2d:<%s> %s <%s>\n", defs, def_name, line, def_args);*/
        len = strlen(def_args); /* length of definition arguments */
        if (((type == VT_METMAP_MEASURE) && (match & VT_METMAP_MEASURE)) ||
            ((type == VT_METMAP_AGGROUP) && (match & VT_METMAP_AGGROUP))) {
            if (((int)strcspn(def_args, "=+") != len) ||
                      (((int)strcspn(def_args, "=+-*/ \t") != len)
                          && (type == VT_METMAP_MEASURE))) {
                type = VT_METMAP_INVALID;
                invalid_defs++;
                vt_cntl_msg("XXXX Def %d:%s <%s> invalid!", lineno, line, def_name);
            } else {
                map = vt_metricmap_append(map, type, def_name, def_args);
                measure_defs++;
            }
        } else if ((type == VT_METMAP_COMPOSE) && (match & VT_METMAP_COMPOSE)) {
                map = vt_metricmap_append(map, type, def_name, def_args);
                compose_defs++;
        } else if ((type == VT_METMAP_COMPUTE) && (match & VT_METMAP_COMPUTE)) {
                map = vt_metricmap_append(map, type, def_name, def_args);
                compute_defs++;
        }
        if (mapv == NULL) mapv = map; /* initialise head of vector */
    }
    vt_cntl_msg("Mapped %d/%d defs from \"%s\"",
        measure_defs+aggroup_defs+compose_defs+compute_defs, defs, specfile);
#if 0
    printf("measure %d aggroup %d compose %d compute %d unknown %d invalid %d\n", 
            measure_defs, aggroup_defs, compose_defs, compute_defs,
            unknown_defs, invalid_defs);
#endif
    fclose(fp);
    return mapv;
}

