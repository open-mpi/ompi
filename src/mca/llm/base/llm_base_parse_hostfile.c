/*
 * $HEADER$
 */

#include "ompi_config.h"

#include "class/ompi_list.h"
#include "runtime/runtime.h"
#include "util/output.h"
#include "mca/mca.h"
#include "mca/base/base.h"
#include "mca/llm/llm.h"
#include "mca/llm/base/base.h"
#include "mca/llm/base/base_internal.h"
#include "mca/llm/base/llm_base_parse_hostfile_lex.h"
#include "runtime/runtime_types.h"

static void parse_error(void);
static int parse_keyval(int, mca_llm_base_hostfile_node_t*);

static void
parse_error()
{
    printf("hostfile: error reading hostfile at line %d, %s\n",
           mca_llm_base_yynewlines, mca_llm_base_string);
}

static
int
parse_keyval(int first, mca_llm_base_hostfile_node_t *node)
{
    int val;
    char *key, *value;
    ompi_rte_valuepair_t *keyval;

    if (MCA_LLM_BASE_STRING != first) {
        return OMPI_ERROR;
    }

    /* grab the key */
    key = strdup(mca_llm_base_string);
    if (NULL == key) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    /* find the equals */
    if (MCA_LLM_BASE_EQUAL != mca_llm_base_yylex()) {
        free(key);
        return OMPI_ERROR;
    }

    /* make sure we have a value */
    val = mca_llm_base_yylex();
    if (MCA_LLM_BASE_STRING != val && MCA_LLM_BASE_QUOTED_STRING != val) {
        free(key);
        return OMPI_ERROR;
    }

    /* grab the value */
    value = strdup(mca_llm_base_string);
    if (NULL == value) {
        free(key);
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    /* make a keyval and store it */
    keyval = OBJ_NEW(ompi_rte_valuepair_t);
    keyval->key = key;
    keyval->value = value;

    ompi_list_append(node->info, (ompi_list_item_t *) keyval); 

    return OMPI_SUCCESS;
}


static
int
parse_count(void)
{
    /* find the equals */
    if (MCA_LLM_BASE_EQUAL != mca_llm_base_yylex()) return -1;
    /* and now the string */
    if (MCA_LLM_BASE_STRING != mca_llm_base_yylex()) return -1;

    return atoi(mca_llm_base_string);
}


static
int
parse_line(int first, mca_llm_base_hostfile_node_t *node)
{
    int val;
    int ret;

    if (MCA_LLM_BASE_STRING == first) {
        strncpy(node->hostname, mca_llm_base_string, MAXHOSTNAMELEN);
        node->given_count = 1;
    } else {
        parse_error();
        return OMPI_ERROR;
    }

    while (!mca_llm_base_parse_done) {
        val = mca_llm_base_yylex();
        switch (val) {
        case MCA_LLM_BASE_DONE:
            return OMPI_SUCCESS;
            break;

        case MCA_LLM_BASE_NEWLINE:
            return OMPI_SUCCESS;
            break;

        case MCA_LLM_BASE_COUNT:
            ret = parse_count();
            if (ret < 0) return OMPI_ERROR;

            node->given_count = ret;
            break;

        case MCA_LLM_BASE_STRING:
            ret = parse_keyval(val, node);
            if (OMPI_SUCCESS != ret) return ret;
            break;

        default:
            parse_error();
            return OMPI_ERROR;
            break;
        }
    }

    return OMPI_SUCCESS;
}


ompi_list_t *
mca_llm_base_parse_hostfile(const char *hostfile)
{
    mca_llm_base_hostfile_node_t *newnode;
    ompi_list_t *list;
    int val, ret;

    OMPI_LOCK(&mca_llm_base_parse_mutex);

    list = OBJ_NEW(ompi_list_t);

    mca_llm_base_parse_done = false;

    mca_llm_base_yyin = fopen(hostfile, "r");
    if (NULL == mca_llm_base_yyin) {
        printf("hostfile: could not open %s\n", hostfile);
        OBJ_RELEASE(list);
        list = NULL;
        goto parse_exit;
    }

    while (!mca_llm_base_parse_done) {
        val = mca_llm_base_yylex();
        switch (val) {
        case MCA_LLM_BASE_DONE:
            goto parse_exit;
            break;

        case MCA_LLM_BASE_NEWLINE:
            /* blank line!  ignore it */
            break;

        case MCA_LLM_BASE_STRING:
            newnode = OBJ_NEW(mca_llm_base_hostfile_node_t);
            ret = parse_line(val, newnode);
            if (OMPI_SUCCESS != ret) {
                OBJ_RELEASE(newnode);
                OBJ_RELEASE(list);
                list = NULL;
                goto parse_exit;
            }

            ompi_list_append(list, (ompi_list_item_t *)newnode); 
            break;

        default:
            parse_error();
            OBJ_RELEASE(list);
            list = NULL;
            goto parse_exit;

            break;
        }
    }

parse_exit:
    OMPI_UNLOCK(&mca_llm_base_parse_mutex);

    return list;
}
