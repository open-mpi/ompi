/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <string.h>

#include "include/constants.h"
#include "class/ompi_list.h"
#include "mca/pcm/base/base.h"
#include "mca/llm/base/base_internal.h"

#define START_KEY "@MCA_PCM@\n"
#define END_KEY "@MCA_PCM_END@\n"
#define NODE_KEY "@MCA_PCM_NODE@\n"

#define PROTOCOL_VERSION 1

int
mca_pcm_base_send_schedule(FILE *fp, 
                           mca_ns_base_cellid_t cellid,
                           mca_ns_base_jobid_t jobid,
                           mca_ns_base_vpid_t global_start_vpid,
                           int global_spawn_size,
                           ompi_rte_node_schedule_t *sched,
                           int num_procs)
{
    int i;

    fprintf(fp, START_KEY);
    fprintf(fp, "%d\n", PROTOCOL_VERSION);

    /* CELLID */
    fprintf(fp, "%d\n", cellid);

    /* JOBID */
    fprintf(fp, "%d\n", jobid);

    /* SPAWN-WIDE STARTING VPID */
    fprintf(fp, "%d\n", global_start_vpid);

    /* SPAWN-WIDE JOB SIZE */
    fprintf(fp, "%d\n", global_spawn_size);

    /* ARGC */
    fprintf(fp, "%d\n", sched->argc);
    for (i = 0 ; i < sched->argc ; ++i) {
        fprintf(fp, "%d %s\n", (int) strlen((sched->argv)[i]), 
                (sched->argv)[i]);
    }

    /* ENV */
    if (sched->env == NULL) {
        fprintf(fp, "%d\n", 0);
    } else {
        fprintf(fp, "%d\n", sched->envc);
        for (i = 0 ; i < sched->envc ; ++i) {
            fprintf(fp, "%d %s\n", (int) strlen((sched->env)[i]), 
                    (sched->env)[i]);
        }
    }
    
    /* CWD */
    fprintf(fp, "%d %s\n", (int) strlen(sched->cwd), 
            (strlen(sched->cwd) > 0) ? sched->cwd : "");
    fflush(fp);

    /* number of processes to start */
    fprintf(fp, "%d\n", num_procs);

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
get_int(FILE *fp, int *num)
{
    int ret;

    ret = fscanf(fp, "%d\n", num);
    if (ret != 1) return OMPI_ERROR;

    return OMPI_SUCCESS;
}


static int
get_uint(FILE *fp, unsigned int *num)
{
    int ret;

    ret = fscanf(fp, "%u\n", num);
    if (ret != 1) return OMPI_ERROR;

    return OMPI_SUCCESS;
}


static int
get_check_version(FILE *fp)
{
    int ret;
    int ver;

    ret = get_int(fp, &ver);
    if (OMPI_SUCCESS != ret) return ret;

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

    ret = get_int(fp, &len);
    if (OMPI_SUCCESS != ret) return ret;

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


int 
mca_pcm_base_recv_schedule(FILE *fp, 
                           mca_ns_base_cellid_t *cellid,
                           mca_ns_base_jobid_t *jobid,
                           mca_ns_base_vpid_t *starting_vpid,
                           int *spawn_size,
                           ompi_rte_node_schedule_t *sched,
                           int *num_procs)
{
    int ret, val;

    /* try to get our starting key */
    ret = get_key(fp, START_KEY);
    if (OMPI_SUCCESS != ret) return ret;

    /* check our version */
    ret = get_check_version(fp);
    if (OMPI_SUCCESS != ret) return ret;

    /* get our cellid */
    ret = get_uint(fp, cellid);
    if (OMPI_SUCCESS != ret) return ret;

    /* get our jobid */
    ret = get_uint(fp, jobid);
    if (OMPI_SUCCESS != ret) return ret;

    /* get our spawn-wide starting vpid */
    ret = get_uint(fp, starting_vpid);
    if (OMPI_SUCCESS != ret) return ret;

    /* get our spawn-wide job size */
    ret = get_int(fp, spawn_size);
    if (OMPI_SUCCESS != ret) return ret;

    /* get argc */
    ret = get_argv_array(fp, &(sched->argc), &(sched->argv));
    if (OMPI_SUCCESS != ret) return ret;

    /* get env */
    ret = get_argv_array(fp, &val, &(sched->env));
    if (OMPI_SUCCESS != ret) return ret;

    /* get cwd */
    ret = get_string(fp, &(sched->cwd));
    if (OMPI_SUCCESS != ret) return ret;

    /* get num procs */
    ret = get_int(fp, num_procs);

    /* make sure we have our end */
    ret = get_key(fp, END_KEY);
    if (OMPI_SUCCESS != ret) return ret;

    return OMPI_SUCCESS;
}

