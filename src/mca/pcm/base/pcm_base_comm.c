/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <string.h>

#include "class/ompi_list.h"
#include "mca/pcm/base/base.h"

#define START_KEY "@MCA_PCM@\n"
#define END_KEY "@MCA_PCM_END@\n"
#define NODE_KEY "@MCA_PCM_NODE@\n"

#define PROTOCOL_VERSION 1

int
mca_pcm_base_send_schedule(FILE *fp, 
                          ompi_rte_node_schedule_t *sched,
                          ompi_list_t *nodelist)
{
    int i, envc;
    ompi_list_item_t *node_item, *info_item;
    ompi_rte_node_allocation_t *node;
    ompi_rte_valuepair_t *valpair;

    fprintf(fp, START_KEY);
    fprintf(fp, "%d\n", PROTOCOL_VERSION);

    /* ARGC */
    fprintf(fp, "%d\n", sched->argc);
    for (i = 0 ; i < sched->argc ; ++i) {
        fprintf(fp, "%d %s\n", (int) strlen((sched->argv)[i]), 
                (sched->argv)[i]);
    }

    /* ENV - since we don't have a envc, must create ourselves...*/
    for (envc = 0 ; (sched->env)[envc] != NULL ; ++envc) ;
    fprintf(fp, "%d\n", envc);
    for (i = 0 ; i < envc ; ++i) {
        fprintf(fp, "%d %s\n", (int) strlen((sched->env)[i]), 
                (sched->env)[i]);
    }
    
    /* CWD */
    fprintf(fp, "%d %s\n", (int) strlen(sched->cwd), 
            (strlen(sched->cwd) > 0) ? sched->cwd : "");
    fflush(fp);

    /* NODE LIST */
    fprintf(fp, "%d\n", (int) ompi_list_get_size(nodelist));
    for (node_item = ompi_list_get_first(nodelist) ;
         node_item != ompi_list_get_end(nodelist) ;
         node_item = ompi_list_get_next(node_item)) {
        node = (ompi_rte_node_allocation_t*) node_item;

        fprintf(fp, NODE_KEY);
        fprintf(fp, "%d %s\n", (int) strlen(node->hostname), 
                node->hostname);
        fprintf(fp, "%d\n", node->count);

        /* INFO */
        fprintf(fp, "%d\n", (int) ompi_list_get_size(node->info));
        for (info_item = ompi_list_get_first(node->info) ;
             info_item != ompi_list_get_end(node->info) ;
             info_item = ompi_list_get_next(info_item)) {
            valpair = (ompi_rte_valuepair_t*) info_item;

            fprintf(fp, "%d %d %s %s\n",
                    (int) strlen(valpair->key), (int) strlen(valpair->value),
                    valpair->key, valpair->value);
        }
    }

    /*
     * so we've basically ignored the fact we might error out up until
     * this point, which probably won't hurt anything.  But do a quick
     * check that the other side hasn't dropped our connection yet.
     *
     * Do this before the last print so we don't get swapped out and
     * accidently catch the expected eof or something
     */
    if (feof(fp) || ferror(fp)) return OMPI_ERROR;

    fprintf(fp, END_KEY);

    /*
     * don't check eof, because it is possible we got swapped out for
     * a bit and caught the other side going bye-bye 
     */
    if (ferror(fp)) return OMPI_ERROR;

    return OMPI_SUCCESS;
}


static int
get_key(FILE *fp, const char *key)
{
    size_t pos = 0;
    size_t len;
    int val;
    int countdown = 50;

    len = strlen(key);

    while (pos < len) {
        val = fgetc(fp);
        if (val == EOF) {
            if (feof(fp)) {
                countdown--;
                if (0 == countdown) return OMPI_ERROR; 
                /* BWB: probably want to back off at some point */
                clearerr(fp);
            } else {
                return OMPI_ERROR;
            }
        } else {
            if (val == key[pos]) {
                pos++;
            } else if (pos != 0) {
                /* this wasn't the key - start looking again */
                pos = 0;
            }
        }
    }

    return OMPI_SUCCESS;
}


static int
get_check_version(FILE *fp)
{
    int ret;
    int ver;

    ret = fscanf(fp, "%d\n", &ver);
    if (ret != 1) return OMPI_ERROR;
    if (ver != PROTOCOL_VERSION) return OMPI_ERROR;

    return OMPI_SUCCESS;
}


static int
get_string(FILE *fp, char **strp)
{
    int len;
    int ret;
    char *str;
    size_t str_read;;

    ret = fscanf(fp, "%d ", &len);
    if (ret != 1) return OMPI_ERROR;

    str = (char*) malloc(sizeof(char) * (len + 2));
    if (NULL == str) return OMPI_ERROR;

    str_read = fread(str, len, 1, fp);
    if (str_read != 1) {
        free(str);
        return OMPI_ERROR;
    }

    /* just in case */
    str[len] = '\0';

    ret = fgetc(fp);
    if (ret != '\n') {
        free(str);
        return OMPI_ERROR;
    }

    *strp = str;

    return OMPI_SUCCESS;
}


static int
get_argv_array(FILE *fp, int *argcp, char ***argvp)
{
    int ret;
    int len;
    int i, j;
    char **argv;

    ret = fscanf(fp, "%d\n", &len);
    if (ret != 1) return OMPI_ERROR;

    argv = (char**) malloc(sizeof(char*) * (len + 1));
    if (NULL == argv) return OMPI_ERROR;

    /* NULL terminiate the array */
    argv[len] = NULL;

    for (i = 0 ; i < len ; ++i) {
        ret = get_string(fp, &(argv[i]));
        if (OMPI_SUCCESS != ret) {
            for (j = 0 ; j < len ; ++i) {
                free(argv[j]);
            }
            free(argv);
            return OMPI_ERROR;
        }
    }

    *argcp = len;
    *argvp = argv;

    return OMPI_SUCCESS;
}


static int
get_keyval(FILE *fp, char **keyp, char **valp)
{
    int ret;
    char *key, *val;
    int keylen, vallen; 
    size_t str_read;;

    ret = fscanf(fp, "%d %d ", &keylen, &vallen);
    if (ret != 2) return OMPI_ERROR;

    key = (char*) malloc(sizeof(char) * (keylen + 2));
    if (NULL == key) return OMPI_ERROR;

    val = (char*) malloc(sizeof(char) * (vallen + 2));
    if (NULL == val) {
        free(key);
        return OMPI_ERROR;
    }

    /* get the key */
    str_read = fread(key, keylen, 1, fp);
    if (str_read != 1) {
        free(key);
        free(val);
        return OMPI_ERROR;
    }

    /* get the space */
    ret = fgetc(fp);
    if (ret != ' ') {
        free(key);
        free(val);
        return OMPI_ERROR;
    }

    /* get the value */
    str_read = fread(val, vallen, 1, fp);
    if (str_read != 1) {
        free(key);
        free(val);
        return OMPI_ERROR;
    }

    /* get the end of line newline */
    ret = fgetc(fp);
    if (ret != '\n') {
        free(key);
        free(val);
        return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
}


static int
get_nodeinfo(FILE *fp, ompi_list_t *info)
{
    ompi_rte_valuepair_t *newinfo;
    int ret;
    int info_len;
    int i;

    ret = fscanf(fp, "%d\n", &info_len);
    if (ret != 1) return OMPI_ERROR;

    for (i = 0 ; i < info_len ; ++i) {
        ret = get_keyval(fp, &(newinfo->key), &(newinfo->value));
        if (OMPI_SUCCESS != ret) {
            OBJ_RELEASE(newinfo);
            return ret;
        }

        ompi_list_append(info, (ompi_list_item_t*) newinfo);
    }

    return OMPI_SUCCESS;
}


static int
get_nodelist(FILE *fp, ompi_list_t *nodelist)
{
    int nodelist_len;
    int ret;
    ompi_rte_node_allocation_t *newnode;
    int i;
    char *tmpstr;

    ret = fscanf(fp, "%d\n", &nodelist_len);
    if (ret != 1) return OMPI_ERROR;

    for (i = 0 ; i < nodelist_len ; ++i) {
        /* make sure we have a key */
        ret = get_key(fp, NODE_KEY);
        if (OMPI_SUCCESS != ret) return ret;

        /* create the node */
        newnode = OBJ_NEW(ompi_rte_node_allocation_t);
        /* fill in fields */
        ret = get_string(fp, &tmpstr);
        if (OMPI_SUCCESS != ret) {
            OBJ_RELEASE(newnode);
            return OMPI_ERROR;
        }
        strncpy(newnode->hostname, tmpstr, sizeof(newnode->hostname));
        free(tmpstr);

        ret = fscanf(fp, "%d\n", &(newnode->count));
        if (ret != 1) {
            OBJ_RELEASE(newnode);
            return OMPI_ERROR;
        }

        ret = get_nodeinfo(fp, newnode->info);
        if (OMPI_SUCCESS != ret) {
            OBJ_RELEASE(newnode);
            return OMPI_ERROR;
        }

        ompi_list_append(nodelist, (ompi_list_item_t*) newnode);
    }

    return OMPI_SUCCESS;
}


int 
mca_pcm_base_recv_schedule(FILE *fp, 
                          ompi_rte_node_schedule_t *sched,
                          ompi_list_t *nodelist)
{
    int ret, val;

    /* try to get our starting key */
    ret = get_key(fp, START_KEY);
    if (OMPI_SUCCESS != ret) return ret;

    /* check our version */
    get_check_version(fp);
    if (OMPI_SUCCESS != ret) return ret;

    /* get argc */
    get_argv_array(fp, &(sched->argc), &(sched->argv));
    if (OMPI_SUCCESS != ret) return ret;

    /* get env */
    get_argv_array(fp, &val, &(sched->env));
    if (OMPI_SUCCESS != ret) return ret;

    /* get cwd */
    get_string(fp, &(sched->cwd));
    if (OMPI_SUCCESS != ret) return ret;

    /* get node list */
    get_nodelist(fp, nodelist);
    if (OMPI_SUCCESS != ret) return ret;

    /* make sure we have our end */
    ret = get_key(fp, END_KEY);
    if (OMPI_SUCCESS != ret) return ret;

    return OMPI_SUCCESS;
}

