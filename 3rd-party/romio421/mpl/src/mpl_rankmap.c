/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */

#include "mpl.h"
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <stdbool.h>
#include <assert.h>

/* rankmap string format (used in PMI_process_mapping) in ABNF:
 *
 * mapping = '(' format ',' (block / block_repeats) *[',' (block / block_repeats)] ')'
 * format = 'vector'
 * block = num / '(' num ',' num ',' num ')'
 * block_repeats = '[' block *[',' block] ']x' num
 * num = 1*DIGIT
 *
 * If a block is in 3-tuple of (X,Y,Z), the meaning is:
 *     X - node id start value
 *     Y - number of nodes with size Z
 *     Z - number of processes assigned to each node
 *
 * If a block is a single number id, it denotes the node id, which is equivallen to
 * (id, 1, 1).
 *
 * A block_repeat repeats the bracketed block list by num of times.
 */

/* START UNIT - MPL_rankmap_str_to_array */

#define RANKMAP_PARSE_ERROR() do { goto fn_fail; } while (0)

/* advance _c until we find a non whitespace character */
#define RANKMAP_SKIP_SPACE(_c) while (isspace(*(_c))) ++(_c)
/* return true iff _c points to a character valid as an identifier, i.e., [-_a-zA-Z0-9] */
#define RANKMAP_ISIDENT(_c) (isalnum(_c) || (_c) == '-' || (_c) == '_')

/* give an error iff *_c != _e */
#define RANKMAP_EXPECT_C(_c, _e) do { if (*(_c) != _e) RANKMAP_PARSE_ERROR(); } while (0)
#define RANKMAP_EXPECT_AND_SKIP_C(_c, _e) do { RANKMAP_EXPECT_C(_c, _e); ++_c; } while (0)
/* give an error iff the first |_m| characters of the string _s are equal to _e */
#define RANKMAP_EXPECT_S(_s, _e) (strncmp(_s, _e, strlen(_e)) == 0 && !RANKMAP_ISIDENT((_s)[strlen(_e)]))

#define RANKMAP_GET_INT(_s, val) do { \
    char * tmp; \
    val = (int) strtol(_s, &tmp, 0); \
    _s = tmp; \
} while (0)

/* (X,Y,Z) -> n1, n2, n3 */
#define RANKMAP_GET_TRIPLET(s, n1, n2, n3) do { \
    RANKMAP_SKIP_SPACE(s); \
    RANKMAP_GET_INT(s, n1); \
    RANKMAP_SKIP_SPACE(s); \
    RANKMAP_EXPECT_AND_SKIP_C(s, ','); \
    RANKMAP_SKIP_SPACE(s); \
    RANKMAP_GET_INT(s, n2); \
    RANKMAP_SKIP_SPACE(s); \
    RANKMAP_EXPECT_AND_SKIP_C(s, ','); \
    RANKMAP_SKIP_SPACE(s); \
    RANKMAP_GET_INT(s, n3); \
    RANKMAP_SKIP_SPACE(s); \
    RANKMAP_EXPECT_AND_SKIP_C(s, ')'); \
} while (0)

#define RANKMAP_VECTOR "vector"

/* Parsing result is a rank-to-nodeid map where rank is the array index.  */
int MPL_rankmap_str_to_array(char *mapping, int sz, int *out_rankmap)
{
    const char *s = mapping;
    const char *s_repeat_start = NULL;
    const char *s_repeat_cont = NULL;
    int num_repeat = -1;
    int rank = 0;

    if (!s || !strlen(s)) {
        /* An empty-string indicates an inability to determine or express the
         * process layout on the part of the process manager.  Consider this a
         * non-fatal error case. */
        goto fn_fail;
    }

    RANKMAP_SKIP_SPACE(s);
    RANKMAP_EXPECT_AND_SKIP_C(s, '(');
    RANKMAP_SKIP_SPACE(s);
    bool flag = RANKMAP_EXPECT_S(s, RANKMAP_VECTOR);
    if (!flag) {
        RANKMAP_PARSE_ERROR();
    }
    s += strlen(RANKMAP_VECTOR);
    RANKMAP_SKIP_SPACE(s);

    bool expect_comma = true;
    while (*s && rank < sz) {
        RANKMAP_SKIP_SPACE(s);
        if (expect_comma) {
            if (*s == ']' && s[1] == 'x') {
                s += 2;
                if (!isdigit(*s) || !s_repeat_start) {
                    RANKMAP_PARSE_ERROR();
                }
                if (num_repeat == -1) {
                    RANKMAP_GET_INT(s, num_repeat);
                    s_repeat_cont = s;
                }
                num_repeat--;
                if (num_repeat == 0) {
                    s = s_repeat_cont;
                } else {
                    s = s_repeat_start;
                    expect_comma = false;
                }
            } else if (*s == ')') {
                /* hack to support legacy format */
                if (rank < sz) {
                    if (s_repeat_start) {
                        s = s_repeat_start;
                        expect_comma = false;
                        continue;
                    }
                }
                break;
            } else {
                RANKMAP_EXPECT_AND_SKIP_C(s, ',');
                expect_comma = false;
            }
        } else {
            if (*s == '[') {
                s_repeat_start = s + 1;
                s++;
            } else if (*s == '(') {
                /* (X, Y, Z) */
                const char *s_save = s;
                int n1, n2, n3;
                s++;
                RANKMAP_GET_TRIPLET(s, n1, n2, n3);
                if (!s_repeat_start && n1 == 0) {
                    s_repeat_start = s_save;
                }
                for (int i = 0; i < n2; i++) {
                    for (int j = 0; j < n3; j++) {
                        out_rankmap[rank++] = n1 + i;
                        if (rank == sz) {
                            goto fn_exit;
                        }
                    }
                }
                expect_comma = true;
            } else {
                /* a single num */
                const char *s_save = s;
                int id;
                RANKMAP_GET_INT(s, id);
                if (!s_repeat_start && id == 0) {
                    s_repeat_start = s_save;
                }
                out_rankmap[rank] = id;
                rank++;
                expect_comma = true;
            }
        }
    }

  fn_exit:
    return MPL_SUCCESS;
  fn_fail:
    return MPL_ERR_FAIL;
}

/* END UNIT - MPL_rankmap_str_to_array */

/* START UNIT - MPL_rankmap_array_to_str */
/* Generate a string from the rankmap array */

#define MAPSTR_GROW(n) do { \
    if (pos + n >= str_size) { \
        if (str_size == 0) { \
            str_size = n * 3 / 2; \
            str = MPL_malloc(str_size, MPL_MEM_OTHER); \
        } else { \
            str_size = (str_size + n) * 3 / 2; \
            str = MPL_realloc(str, str_size, MPL_MEM_OTHER); \
        } \
        assert(str); \
    } \
} while (0)

#define MAPSTR_APPEND(...) do {\
    /* assuming every block fits in 20 spaces, true if node_id fits in 5 digits */ \
    MAPSTR_GROW(20); \
    pos += snprintf(str + pos, str_size - pos, __VA_ARGS__); \
} while (0)

/* get maximum node id and minimum node id in a rankmap array */
static void get_rankmap_range(int *rankmap, int sz, int *max_id_out, int *min_id_out)
{
    int max_id = rankmap[0];
    int min_id = rankmap[0];
    for (int rank = 1; rank < sz; rank++) {
        if (max_id < rankmap[rank]) {
            max_id = rankmap[rank];
        }
        if (min_id > rankmap[rank]) {
            min_id = rankmap[rank];
        }
    }
    *max_id_out = max_id;
    *min_id_out = min_id;
}

/* find number of repeats of rankmap[from_rank..cur_rank] */
static int check_repeats(int *rankmap, int sz, int from_rank, int cur_rank)
{
    /* NOTE: minimum repeats is 1 since anything repeat by 1 on its own */
    int num_repeats = 1;

    int num_ranks = cur_rank - from_rank;

    while (cur_rank + num_ranks <= sz) {
        for (int i = 0; i < num_ranks; i++) {
            if (rankmap[from_rank + i] != rankmap[cur_rank + i]) {
                return num_repeats;
            }
        }
        num_repeats++;
        cur_rank += num_ranks;
    }
    return num_repeats;
}

int MPL_rankmap_array_to_str(int *rankmap, int sz, char **out_mapping_str)
{
    int max_id, min_id, num_nodes;
    get_rankmap_range(rankmap, sz, &max_id, &min_id);
    num_nodes = max_id - min_id + 1;

    /* node_block for grouping ranks */
    struct node_block {
        int start_id;
        int num_nodes;
        int num_ranks;
        int start_rank;
        bool start_repeat;
    };
    struct node_block *node_blocks;
    node_blocks = MPL_malloc(sz * sizeof(struct node_block), MPL_MEM_OTHER);
    int i_blk = 0;

    /* node_markers[i] is the last block that start with node i (node id offset by min_id) */
    int *node_markers;
    node_markers = MPL_malloc(num_nodes * sizeof(int), MPL_MEM_OTHER);
    for (int i = 0; i < num_nodes; i++) {
        node_markers[i] = -1;
    }

    int skip_to_rank;
    skip_to_rank = -1;
    /* NOTE: last iteration is a sentinel for checking compression */
    for (int rank = 0; rank < sz + 1; rank++) {
        if (rank < skip_to_rank) {
            continue;
        } else if (rank > 0 && rank < sz && rankmap[rank] == rankmap[rank - 1]) {
            node_blocks[i_blk - 1].num_ranks++;
        } else {
            /* try compress previous node blocks */
            if (i_blk > 1) {
                struct node_block *p0 = &node_blocks[i_blk - 2];
                struct node_block *p1 = &node_blocks[i_blk - 1];
                if (p0->num_ranks == p1->num_ranks && p0->start_id + p0->num_nodes == p1->start_id) {
                    p0->num_nodes++;
                    node_markers[p1->start_id] = -1;
                    i_blk--;
                }
            }

            if (rank == sz) {
                /* sentinel, we are done */
                break;
            }

            /* check for repeats */
            int i_mark = rankmap[rank] - min_id;
            int mark_blk = node_markers[i_mark];
            if (mark_blk != -1) {
                int mark_rank = node_blocks[mark_blk].start_rank;
                int num_repeats = check_repeats(rankmap, sz, mark_rank, rank);
                if (num_repeats > 1) {
                    node_blocks[mark_blk].start_repeat = true;
                    /* insert a special repeat block */
                    node_blocks[i_blk].start_id = -1;
                    node_blocks[i_blk].num_nodes = num_repeats;
                    i_blk++;
                    /* clear node_markers */
                    for (int i = 0; i < num_nodes; i++) {
                        node_markers[i] = -1;
                    }
                    /* skip the repeated ranks */
                    skip_to_rank = mark_rank + (rank - mark_rank) * num_repeats;
                    continue;
                }
            }

            /* add a new node_block */
            int node_id = rankmap[rank];
            node_blocks[i_blk].start_id = node_id;
            node_blocks[i_blk].start_rank = rank;
            node_blocks[i_blk].num_nodes = 1;
            node_blocks[i_blk].num_ranks = 1;
            node_blocks[i_blk].start_repeat = false;
            node_markers[node_id] = i_blk;
            i_blk++;
        }
    }

    /* translate the node_blocks into string */
    char *str = NULL;
    int str_size = 0;
    int pos = 0;

    MAPSTR_APPEND("(vector");
    for (int i = 0; i < i_blk; i++) {
        struct node_block *blk = node_blocks + i;
        if (blk->start_id == -1) {
            MAPSTR_APPEND("]x%d", blk->num_nodes);
        } else {
            MAPSTR_APPEND(",");
            if (blk->start_repeat) {
                MAPSTR_APPEND("[");
            }
            if (blk->num_nodes == 1 && blk->num_ranks == 1) {
                MAPSTR_APPEND("%d", blk->start_id);
            } else {
                MAPSTR_APPEND("(%d,%d,%d)", blk->start_id, blk->num_nodes, blk->num_ranks);
            }
        }
    }
    MAPSTR_APPEND(")");

    MPL_free(node_blocks);
    MPL_free(node_markers);
    *out_mapping_str = str;

    return MPL_SUCCESS;
}

/* END UNIT - MPL_rankmap_array_to_str */
