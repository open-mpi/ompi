/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
#include "orte_config.h"

#include <stdio.h>
#include <libxml/parser.h>
#include <libxml/tree.h>

#include "include/orte_constants.h"
#include "mca/base/mca_base_param.h"

#include "rds_resfile.h"

static void
process_resource(xmlChar *site, xmlDoc *doc, xmlNode *a_node)
{
    xmlNode *cur_node = NULL;
    xmlChar *resource;
    char *tokens[3];
    
    tokens[0] = strdup((char*)site);
    tokens[2] = NULL;
    
    cur_node = a_node->children;
    while (NULL != cur_node) {
        if ((!xmlStrcmp(cur_node->name, (const xmlChar *)"name"))) {
            resource = xmlNodeListGetString(doc, cur_node->children, 1);
            printf("site[resource]: %s[%s]\n", site, resource);
            tokens[1] = strdup((char*)resource);
            xmlFree(resource);
        }
        cur_node = cur_node->next;
    }
}

static void
process_site(xmlChar *site, xmlDoc *doc, xmlNode *a_node)
{
    xmlNode *cur_node = NULL;
    
    cur_node = a_node->children;
    while (NULL != cur_node) {
        if ((!xmlStrcmp(cur_node->name, (const xmlChar *)"resource"))) {
            process_resource(site, doc, cur_node);
        }
        cur_node = cur_node->next;
    }
}


int orte_rds_resfile_query(void)
{
#if 0
    xmlDoc *doc = NULL;
    xmlNode *root_element = NULL;
    xmlNode *cur = NULL;
    xmlChar *site;
    int fileid;
    char *resfile;
    
    /*
     * this initializes the library and checks potential ABI mismatches
     * between the version it was compiled for and the actual shared
     * library used.
     */
    LIBXML_TEST_VERSION

    /* get the resource filename */
     fileid = mca_base_param_register_string("rds", "resfile", "name", "RESOURCE_FILE", NULL);
     mca_base_param_lookup_string(fileid, &resfile);
     if (NULL == resfile) {  /* no resource file provided */
        return ORTE_ERR_NOT_FOUND;
     }
     
    /*parse the file and get the DOM */
    doc = xmlReadFile(resfile, NULL, 0);

    if (doc == NULL) {
        printf("error: could not parse file %s\n", resfile);
    }

    /*Get the root element node */
    root_element = xmlDocGetRootElement(doc);

    /* walk the document tree looking for sites and save their info to the registry */
    cur = root_element->children;
    while (NULL != cur) {
        if ((!xmlStrcmp(cur->name, (const xmlChar *)"site-name"))) {
            site = xmlNodeListGetString(doc, cur->children, 1);
            printf("site: %s\n", site);
            process_site(site, doc, cur);
            xmlFree(site);
        }
        cur = cur->next;
    }
    
    /*free the document */
    xmlFreeDoc(doc);

    /*
     *Free the global variables that may
     *have been allocated by the parser.
     */
    xmlCleanupParser();
#endif

    return ORTE_SUCCESS;
}


int orte_rds_resfile_finalize(void)
{
    return ORTE_SUCCESS;
}
