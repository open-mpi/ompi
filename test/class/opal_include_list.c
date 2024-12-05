/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2024      Google, LLC. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"
#include <assert.h>

#include "opal/class/opal_include_list.h"
#include "opal/util/argv.h"
#include "opal/constants.h"
#include "opal/runtime/opal.h"
#include "support.h"


#define TEST_ASSERT(cond, failure_string)       \
    if (!(cond)) {                              \
        test_failure(failure_string);           \
        return test_finalize();                 \
    }

#define TEST_EXPECT(cond, failure_string)       \
    if (cond) {                                 \
        test_pass();                            \
    } else {                                    \
        test_failure(failure_string);           \
    }

static void test_pass(void) {
    fprintf (stderr, "PASS\n");
    test_success();
}

static void check_include_list_contents(const char *check_str, const char *value, const opal_include_list_t *include_list)
{
    bool is_exclude = false;

    if ('^' == value[0]) {
        is_exclude = true;
        ++value;
    }

    fprintf (stderr, "    Checking %s list contents: ", check_str);
    char **list = opal_argv_split(value, ',');
    bool contents_match = true;
    for (int i = 0 ; list[i] != NULL ; ++i) {
        if ((NULL == include_list->items[i] || 0 != strcasecmp(list[i], include_list->items[i])) ||
            (list[i+1] == NULL && include_list->items[i+1] != NULL)) {
            contents_match = false;
            break;
        }
    }
    TEST_EXPECT(contents_match, "contents do not match");
  
    fprintf (stderr, "    Checking %s include/exclude: ", check_str);
    TEST_EXPECT(is_exclude == include_list->is_exclude, "list type not correctly identified");
}

static int test_opal_include_list_deserialize(void)
{
    test_init("opal_include_list_deserialize");

    fprintf (stderr, "  Test opal_include_list_t construction: ");

    opal_include_list_t include_list;
    OBJ_CONSTRUCT(&include_list, opal_include_list_t);
    if (NULL != include_list.items || include_list.is_exclude == true) {
        OBJ_DESTRUCT(&include_list);
        test_failure("incorrectly initial state");
        return test_finalize();
    } else {
        test_pass();
    }

    fprintf (stderr, "  Test parsing an exclude list: ");
    char *exclude_list_string = "^foo,baz"; 
    int ret = include_list.super.deserialize(&include_list.super, exclude_list_string);
    TEST_EXPECT(ret == OPAL_SUCCESS, "");
    check_include_list_contents("exclude", exclude_list_string, &include_list);

    fprintf (stderr, "  Test parsing an include list: ");
    char *include_list_string = "foo,bar,baz"; 
    ret = include_list.super.deserialize(&include_list.super, include_list_string);
    TEST_EXPECT(ret == OPAL_SUCCESS, "");
    check_include_list_contents("include", include_list_string, &include_list);

    fprintf (stderr, "  Test opal_include_list_t destruction: ");
    OBJ_DESTRUCT(&include_list);
    if (NULL != include_list.items || include_list.is_exclude == true) {
        test_failure("incorrect state after destruct");
    } else {
        test_pass();
    }

    return test_finalize();
}

static void check_list_rank(opal_include_list_t *include_list, char *value, int expected_rank,
                           bool regex_match, bool case_sensitive)
{
    fprintf (stderr, "  Test matching item=%s, expected_rank=%d, regex_match=%d, "
             "case_sensitive=%d: ", value, expected_rank, regex_match, case_sensitive);
    int result = opal_include_list_match(include_list, value, regex_match, case_sensitive);
    if (result != expected_rank) {
        char *failure_message = NULL;
        asprintf(&failure_message, "actual rank=%d", result);
        test_failure(failure_message);
        free(failure_message);
    } else {
        test_pass();
    }
}

static void test_include_list_matching(opal_include_list_t *include_list)
{
    int rank_multiplier = include_list->is_exclude ? -1 : 1;
    int not_found_rank = include_list->is_exclude ? 1 : -1;
    for (int i = 0 ; include_list->items[i] != NULL ; ++i) {
        int rank = i + 1;
        int tmp_len = strlen(include_list->items[i]) + 4;
        char *tmp = alloca (tmp_len);
        strncpy(tmp, include_list->items[i], tmp_len);

        check_list_rank(include_list, tmp, rank_multiplier * rank,
                        /*regex_match=*/false, /*case_sensitive=*/true);

        /* verify case sensitivity */
        tmp[0] = isupper(tmp[0]) ? tolower(tmp[0]) : toupper(tmp[0]);
        check_list_rank(include_list, tmp, not_found_rank,
                         /*regex_match=*/false, /*case_sensitive=*/true);
        check_list_rank(include_list, tmp, rank_multiplier * rank,
                         /*regex_match=*/false, /*case_sensitive=*/false);

        /* include/exclude lists look for exact matches, not partial ones. verify
         * this behavior. */
        tmp[0] = isupper(tmp[0]) ? tolower(tmp[0]) : toupper(tmp[0]);
        strcat(tmp, "bar");
        check_list_rank(include_list, tmp, not_found_rank,
                         /*regex_match=*/false, /*case_sensitive=*/true);
    }
}

static int test_opal_include_list_match_include(void)
{
    test_init("opal_include_list_match");

    opal_include_list_t include_list;
    OBJ_CONSTRUCT(&include_list, opal_include_list_t);

    char *include_list_string = "foo,baz";
    fprintf (stderr, "  Test parsing an include list: ");
    int ret = include_list.super.deserialize(&include_list.super, include_list_string);
    TEST_EXPECT(ret == OPAL_SUCCESS, "");

    test_include_list_matching(&include_list);

    OBJ_DESTRUCT(&include_list);
    return test_finalize();
}

static int test_opal_include_list_match_exclude(void)
{
    test_init("opal_exclude_list_match");

    opal_include_list_t include_list;
    OBJ_CONSTRUCT(&include_list, opal_include_list_t);

    char *exclude_list_string = "^foo,baz";
    fprintf (stderr, "  Test parsing an exclude list: ");
    int ret = include_list.super.deserialize(&include_list.super, exclude_list_string);
    TEST_EXPECT(ret == OPAL_SUCCESS, "");

    test_include_list_matching(&include_list);

    OBJ_DESTRUCT(&include_list);
    return test_finalize();
}

static int test_opal_include_list_match_include_regex(void)
{
    test_init("opal_include_list_match_regex");

    opal_include_list_t include_list;
    OBJ_CONSTRUCT(&include_list, opal_include_list_t);

    char *include_list_string = "foo[BX]ar,b[aE]z";
    fprintf (stderr, "  Test parsing a regex include list: ");
    int ret = include_list.super.deserialize(&include_list.super, include_list_string);
    TEST_EXPECT(ret == OPAL_SUCCESS, "");

    check_list_rank(&include_list, "fooBar", /*expected_rank=*/1,
                    /*regex_match=*/true, /*case_sensitive=*/true);
    check_list_rank(&include_list, "fooXar", /*expected_rank=*/1,
                    /*regex_match=*/true, /*case_sensitive=*/true);
    check_list_rank(&include_list, "foobar", /*expected_rank=*/-1,
                    /*regex_match=*/true, /*case_sensitive=*/true);
    check_list_rank(&include_list, "fooxar", /*expected_rank=*/-1,
                    /*regex_match=*/true, /*case_sensitive=*/true);
    check_list_rank(&include_list, "foobar", /*expected_rank=*/1,
                    /*regex_match=*/true, /*case_sensitive=*/false);
    check_list_rank(&include_list, "fooxar", /*expected_rank=*/1,
                    /*regex_match=*/true, /*case_sensitive=*/false);
    check_list_rank(&include_list, "fooxar0000", /*expected_rank=*/1,
                    /*regex_match=*/true, /*case_sensitive=*/false);

    OBJ_DESTRUCT(&include_list);
    return test_finalize();
}

static int test_opal_include_list_match_exclude_regex(void)
{
    test_init("opal_exclude_list_match");

    opal_include_list_t include_list;
    OBJ_CONSTRUCT(&include_list, opal_include_list_t);

    char *exclude_list_string = "^foo[BX]ar,b[aE]z";;
    fprintf (stderr, "  Test parsing a regex exclude list: ");
    int ret = include_list.super.deserialize(&include_list.super, exclude_list_string);
    TEST_EXPECT(ret == OPAL_SUCCESS, "");

    check_list_rank(&include_list, "fooBar", /*expected_rank=*/-1,
                    /*regex_match=*/true, /*case_sensitive=*/true);
    check_list_rank(&include_list, "fooXar", /*expected_rank=*/-1,
                    /*regex_match=*/true, /*case_sensitive=*/true);
    check_list_rank(&include_list, "foobar", /*expected_rank=*/1,
                    /*regex_match=*/true, /*case_sensitive=*/true);
    check_list_rank(&include_list, "fooxar", /*expected_rank=*/1,
                    /*regex_match=*/true, /*case_sensitive=*/true);
    check_list_rank(&include_list, "foobar", /*expected_rank=*/-1,
                    /*regex_match=*/true, /*case_sensitive=*/false);
    check_list_rank(&include_list, "fooxar", /*expected_rank=*/-1,
                    /*regex_match=*/true, /*case_sensitive=*/false);
    check_list_rank(&include_list, "fooxar0000", /*expected_rank=*/-1,
                    /*regex_match=*/true, /*case_sensitive=*/false);

    OBJ_DESTRUCT(&include_list);
    return test_finalize();
}

int main(int argc, char *argv[])
{
    int ret = opal_init_util(&argc, &argv);
    if (OPAL_SUCCESS != ret) {
        fprintf (stderr, "could not initialize opal");
        exit(1);
    }

    test_opal_include_list_deserialize();
    test_opal_include_list_match_exclude();
    test_opal_include_list_match_include();
    test_opal_include_list_match_include_regex();
    test_opal_include_list_match_exclude_regex();

    opal_finalize_util();

    return ret;
}
