/* -*- C -*-
 */

#include <time.h>
#include <stdio.h>

#include "orte/constants.h"

#include "opal/class/opal_list.h"
#include "opal/util/argv.h"

#include "orte/runtime/runtime.h"

typedef struct {
    opal_list_item_t super;
    char *word;
    int count;
} word_count_t;
OBJ_CLASS_INSTANCE(word_count_t,
                   opal_list_item_t,
                   NULL, NULL);

int main(int argc, char* argv[])
{
    char text[100];
    opal_list_t words;
    word_count_t *cnt;
    char **incnt;
    bool found;
    opal_list_item_t *item;

    if (ORTE_SUCCESS != orte_init(&argc, &argv, ORTE_PROC_NON_MPI)) {
        fprintf(stderr, "Failed orte_init\n");
        exit(1);
    }
    
    OBJ_CONSTRUCT(&words, opal_list_t);
    while (fgets(text, sizeof(text), stdin)) {
        /* remove trailing newline */
        if ('\n' == text[strlen(text)-1]) {
            text[strlen(text)-1] = '\0';
        }
        incnt = opal_argv_split(text, '\t');
        found = false;
        if (opal_argv_count(incnt) < 2) {
            opal_output(0, "INCORRECT WORD SPLIT: %s", text);
            opal_argv_free(incnt);
            continue;
        }
        for (item = opal_list_get_first(&words);
             item != opal_list_get_end(&words);
             item = opal_list_get_next(item)) {
            cnt = (word_count_t*)item;
            if (0 == strcmp(cnt->word, incnt[0])) {
                cnt->count += atoi(incnt[1]);
                found = true;
                break;
            }
        }
        if (!found) {
            cnt = OBJ_NEW(word_count_t);
            cnt->word = strdup(incnt[0]);
            cnt->count = atoi(incnt[1]);
            opal_list_append(&words, &cnt->super);
        }
        opal_argv_free(incnt);
    }

    fprintf(stdout, "FINAL COUNT:\n");
    while (NULL != (item = opal_list_remove_first(&words))) {
        cnt = (word_count_t*)item;
        fprintf(stdout, "%s: %d\n", cnt->word, cnt->count);
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&words);

    if (ORTE_SUCCESS != orte_finalize()) {
        fprintf(stderr, "Failed orte_finalize\n");
        exit(1);
    }
    return 0;
}
