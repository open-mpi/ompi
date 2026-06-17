/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2021 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2018 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2012-2017 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/*
 * Unit tests for opal_infosubscriber_t -- the subscribe/notify
 * infrastructure layered on top of opal_info_t.
 *
 * opal_infosubscriber_t has OBJ_CLASS_INSTANCE(opal_object_t, ...),
 * so OBJ_NEW(opal_infosubscriber_t) works standalone without a
 * subclass.
 *
 * Library is compiled with -DNDEBUG, so assert() is a no-op.
 * All verification must go through test_verify().
 */

#include "opal_config.h"

#include "support.h"

#include "opal/util/info_subscriber.h"
#include "opal/util/info.h"
#include "opal/constants.h"
#include "opal/runtime/opal.h"

#include <stdbool.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

/* ------------------------------------------------------------------ */
/* Callback infrastructure                                             */
/* ------------------------------------------------------------------ */

/*
 * Invocation counters, one per callback variant.  Reset before each
 * test case that needs them.
 */
static int g_cb_invocations = 0;
static const char *g_cb_last_key = NULL;
static const char *g_cb_last_value = NULL;

/*
 * Simple pass-through callback: records arguments, returns value as-is.
 * Must return a string literal (the impl never frees the return value).
 */
static const char *cb_passthrough(opal_infosubscriber_t *obj,
                                  const char *key,
                                  const char *value)
{
    (void) obj;
    g_cb_invocations++;
    g_cb_last_key   = key;
    g_cb_last_value = value;
    /* Return the value unchanged.  Static literals are safe here. */
    return value;
}

/*
 * Override callback: always returns "overridden" regardless of input.
 */
static int g_override_invocations = 0;

static const char *cb_override(opal_infosubscriber_t *obj,
                               const char *key,
                               const char *value)
{
    (void) obj;
    (void) key;
    (void) value;
    g_override_invocations++;
    return "overridden";
}

/*
 * Suppressing callback: returns NULL, which causes the key to be
 * stored as internal (inaccessible to the public API).
 */
static int g_suppress_invocations = 0;

static const char *cb_suppress(opal_infosubscriber_t *obj,
                               const char *key,
                               const char *value)
{
    (void) obj;
    (void) key;
    (void) value;
    g_suppress_invocations++;
    return NULL;
}

/* ------------------------------------------------------------------ */
/* Forward declarations                                                */
/* ------------------------------------------------------------------ */

static void test_subscribe_fires_on_register(void);
static void test_subscribe_uses_existing_info(void);
static void test_change_info_fires_callback(void);
static void test_change_info_no_callback_is_internal(void);
static void test_override_callback(void);
static void test_suppress_callback(void);
static void test_multiple_subscriptions(void);

/* ------------------------------------------------------------------ */
/* main                                                                */
/* ------------------------------------------------------------------ */

int main(int argc, char *argv[])
{
    test_init("opal_infosubscriber_t");

    opal_init_util(&argc, &argv);

    test_subscribe_fires_on_register();
    test_subscribe_uses_existing_info();
    test_change_info_fires_callback();
    test_change_info_no_callback_is_internal();
    test_override_callback();
    test_suppress_callback();
    test_multiple_subscriptions();

    int r = test_finalize();
    opal_finalize_util();
    return r;
}

/* ------------------------------------------------------------------ */
/* Helper: read a value from obj->s_info by key; return 0 if missing  */
/* ------------------------------------------------------------------ */
static int read_sinfo(opal_infosubscriber_t *obj, const char *key,
                      char *buf, size_t buflen)
{
    if (NULL == obj->s_info) {
        return 0;
    }
    opal_cstring_t *val = NULL;
    int flag = 0;
    opal_info_get(obj->s_info, key, &val, &flag);
    if (flag && NULL != val) {
        snprintf(buf, buflen, "%s", val->string);
        OBJ_RELEASE(val);
        return 1;
    }
    return 0;
}

/* ------------------------------------------------------------------ */

/*
 * When opal_infosubscribe_subscribe is called, it immediately
 * invokes the callback with the default_value (or the value already
 * stored in s_info for that key, whichever applies).  Here s_info is
 * empty, so the default "default_val" is passed to the callback.
 */
static void test_subscribe_fires_on_register(void)
{
    g_cb_invocations = 0;
    g_cb_last_key    = NULL;
    g_cb_last_value  = NULL;

    opal_infosubscriber_t *obj = OBJ_NEW(opal_infosubscriber_t);

    int rc = opal_infosubscribe_subscribe(obj, "mykey", "default_val", cb_passthrough);
    test_verify("subscribe returns OPAL_SUCCESS", OPAL_SUCCESS == rc);
    test_verify("callback invoked once on subscribe", 1 == g_cb_invocations);
    test_verify("callback received correct key",
                NULL != g_cb_last_key && 0 == strcmp(g_cb_last_key, "mykey"));
    test_verify("callback received default value",
                NULL != g_cb_last_value && 0 == strcmp(g_cb_last_value, "default_val"));

    /* The returned value from the passthrough callback must be stored
     * in s_info under "mykey". */
    char buf[64] = {0};
    int found = read_sinfo(obj, "mykey", buf, sizeof(buf));
    test_verify("s_info populated after subscribe", 1 == found);
    test_verify("s_info value matches callback return",
                0 == strcmp(buf, "default_val"));

    OBJ_RELEASE(obj);
}

/* ------------------------------------------------------------------ */

/*
 * If s_info already has a value for the key when subscribe is called,
 * the callback receives that existing value, not the default.
 */
static void test_subscribe_uses_existing_info(void)
{
    g_cb_invocations = 0;
    g_cb_last_value  = NULL;

    opal_infosubscriber_t *obj = OBJ_NEW(opal_infosubscriber_t);

    /* Pre-populate s_info before subscribing */
    if (NULL == obj->s_info) {
        obj->s_info = OBJ_NEW(opal_info_t);
    }
    opal_info_set(obj->s_info, "prekey", "existing_val");

    int rc = opal_infosubscribe_subscribe(obj, "prekey", "fallback", cb_passthrough);
    test_verify("subscribe with pre-existing info returns OPAL_SUCCESS", OPAL_SUCCESS == rc);
    test_verify("callback still fires once", 1 == g_cb_invocations);
    test_verify("callback received existing value, not default",
                NULL != g_cb_last_value && 0 == strcmp(g_cb_last_value, "existing_val"));

    OBJ_RELEASE(obj);
}

/* ------------------------------------------------------------------ */

/*
 * opal_infosubscribe_change_info: iterates new_info keys, calls
 * subscribers for each key that has one, and stores the result.
 */
static void test_change_info_fires_callback(void)
{
    g_cb_invocations = 0;

    opal_infosubscriber_t *obj = OBJ_NEW(opal_infosubscriber_t);

    /* Subscribe first so the callback exists for "watched" */
    opal_infosubscribe_subscribe(obj, "watched", "init", cb_passthrough);
    /* Reset counter after the subscribe-time invocation */
    g_cb_invocations = 0;

    /* Build new_info with a value change for "watched" */
    opal_info_t *new_info = OBJ_NEW(opal_info_t);
    opal_info_set(new_info, "watched", "new_val");

    int rc = opal_infosubscribe_change_info(obj, new_info);
    test_verify("change_info returns OPAL_SUCCESS", OPAL_SUCCESS == rc);
    test_verify("callback fired by change_info", 1 == g_cb_invocations);

    /* The updated value must appear in s_info */
    char buf[64] = {0};
    int found = read_sinfo(obj, "watched", buf, sizeof(buf));
    test_verify("s_info updated by change_info", 1 == found);
    test_verify("s_info holds new value after change_info",
                0 == strcmp(buf, "new_val"));

    OBJ_RELEASE(new_info);
    OBJ_RELEASE(obj);
}

/* ------------------------------------------------------------------ */

/*
 * Keys that arrive via change_info with NO registered callback are
 * stored via opal_info_set_internal (ie_internal=true).  At the OPAL
 * layer, opal_info_get() finds the key regardless of ie_internal --
 * info_find_key() matches purely on the key string.  So flag comes
 * back 1.  The observable that discriminates internal from public keys
 * is opal_info_dup_public(): it skips ie_internal entries.
 *
 * This test verifies:
 *   1. The key is reachable via opal_info_get (flag==1).
 *   2. opal_info_dup (copies all)    -> key present in duplicate.
 *   3. opal_info_dup_public (public only) -> key absent in duplicate.
 */
static void test_change_info_no_callback_is_internal(void)
{
    opal_infosubscriber_t *obj = OBJ_NEW(opal_infosubscriber_t);

    /* No subscription for "unwatched" -- stored as internal */
    opal_info_t *new_info = OBJ_NEW(opal_info_t);
    opal_info_set(new_info, "unwatched", "some_val");

    opal_infosubscribe_change_info(obj, new_info);

    /*
     * opal_info_get at the OPAL layer does NOT filter by ie_internal.
     * The key is present and flag==1.
     */
    opal_cstring_t *val = NULL;
    int flag = 0;
    opal_info_get(obj->s_info, "unwatched", &val, &flag);
    test_verify("unsubscribed key is findable via opal_info_get (flag==1)",
                1 == flag);
    test_verify("unsubscribed key holds expected value",
                NULL != val && 0 == strcmp(val->string, "some_val"));
    if (NULL != val) {
        OBJ_RELEASE(val);
    }

    /*
     * opal_info_dup (all keys) must copy the internal key.
     */
    opal_info_t *dup_all = OBJ_NEW(opal_info_t);
    opal_info_dup(obj->s_info, &dup_all);
    int nkeys_all = -1;
    opal_info_get_nkeys(dup_all, &nkeys_all);
    test_verify("opal_info_dup copies internal key", 1 == nkeys_all);
    OBJ_RELEASE(dup_all);

    /*
     * opal_info_dup_public (public keys only) must SKIP the internal key.
     * This is the observable that proves the key was stored internally.
     */
    opal_info_t *dup_pub = OBJ_NEW(opal_info_t);
    opal_info_dup_public(obj->s_info, &dup_pub);
    int nkeys_pub = -1;
    opal_info_get_nkeys(dup_pub, &nkeys_pub);
    test_verify("opal_info_dup_public skips key stored via change_info (no callback)",
                0 == nkeys_pub);
    OBJ_RELEASE(dup_pub);

    OBJ_RELEASE(new_info);
    OBJ_RELEASE(obj);
}

/* ------------------------------------------------------------------ */

/*
 * A callback that returns a different value overrides what gets
 * stored in s_info.
 */
static void test_override_callback(void)
{
    g_override_invocations = 0;

    opal_infosubscriber_t *obj = OBJ_NEW(opal_infosubscriber_t);
    opal_infosubscribe_subscribe(obj, "k", "default", cb_override);
    /* subscribe-time invocation already happened */
    test_verify("override callback fired on subscribe", 1 == g_override_invocations);

    /* s_info must hold the overridden value, not the default */
    char buf[64] = {0};
    int found = read_sinfo(obj, "k", buf, sizeof(buf));
    test_verify("s_info holds overridden value after subscribe", 1 == found);
    test_verify("overridden value is \"overridden\"", 0 == strcmp(buf, "overridden"));

    /* A change_info must also produce the overridden value */
    g_override_invocations = 0;
    opal_info_t *new_info = OBJ_NEW(opal_info_t);
    opal_info_set(new_info, "k", "user_val");

    opal_infosubscribe_change_info(obj, new_info);
    test_verify("override callback fires on change_info", 1 == g_override_invocations);

    memset(buf, 0, sizeof(buf));
    found = read_sinfo(obj, "k", buf, sizeof(buf));
    test_verify("s_info still overridden after change_info", 1 == found);
    test_verify("value still \"overridden\" after change_info",
                0 == strcmp(buf, "overridden"));

    OBJ_RELEASE(new_info);
    OBJ_RELEASE(obj);
}

/* ------------------------------------------------------------------ */

/*
 * A callback that returns NULL causes the key to be deleted from s_info
 * (see info_subscriber.c: if (updated_value) set, else delete).
 */
static void test_suppress_callback(void)
{
    g_suppress_invocations = 0;

    opal_infosubscriber_t *obj = OBJ_NEW(opal_infosubscriber_t);
    opal_infosubscribe_subscribe(obj, "sk", "default_sk", cb_suppress);
    test_verify("suppress callback fires on subscribe", 1 == g_suppress_invocations);

    /* The key must NOT be present in s_info (callback returned NULL) */
    char buf[64] = {0};
    int found = read_sinfo(obj, "sk", buf, sizeof(buf));
    test_verify("key absent from s_info when callback returns NULL", 0 == found);

    OBJ_RELEASE(obj);
}

/* ------------------------------------------------------------------ */

/*
 * Subscribe two different callbacks on two different keys; verify
 * each fires independently via change_info.
 */
static void test_multiple_subscriptions(void)
{
    g_cb_invocations       = 0;
    g_override_invocations = 0;

    opal_infosubscriber_t *obj = OBJ_NEW(opal_infosubscriber_t);
    opal_infosubscribe_subscribe(obj, "key_a", "a_default", cb_passthrough);
    opal_infosubscribe_subscribe(obj, "key_b", "b_default", cb_override);

    /* Reset after subscribe-time invocations */
    g_cb_invocations       = 0;
    g_override_invocations = 0;

    opal_info_t *new_info = OBJ_NEW(opal_info_t);
    opal_info_set(new_info, "key_a", "new_a");
    opal_info_set(new_info, "key_b", "new_b");

    opal_infosubscribe_change_info(obj, new_info);

    test_verify("passthrough callback fired for key_a", 1 == g_cb_invocations);
    test_verify("override callback fired for key_b", 1 == g_override_invocations);

    char buf[64] = {0};
    int found = read_sinfo(obj, "key_a", buf, sizeof(buf));
    test_verify("key_a holds passthrough value", 1 == found &&
                0 == strcmp(buf, "new_a"));

    memset(buf, 0, sizeof(buf));
    found = read_sinfo(obj, "key_b", buf, sizeof(buf));
    test_verify("key_b holds overridden value", 1 == found &&
                0 == strcmp(buf, "overridden"));

    OBJ_RELEASE(new_info);
    OBJ_RELEASE(obj);
}
