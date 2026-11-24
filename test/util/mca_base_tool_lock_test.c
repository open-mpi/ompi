/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 * SPDX-License-Identifier: BSD-3-Clause-Open-MPI
 *
 * White-box unit test for the MCA tool lock (mca_base_tool_lock(), the
 * lock that MPI_T holds while it uses MCA variables).  Links libopen-pal
 * directly, with no MPI/MPI_T layer.  Runs as a 'make check' program.
 *
 * Registering, deregistering, and invalidating MCA variables take the
 * tool lock, and several of them do so on behalf of others: registering
 * a variable also registers its group and its synonyms (including the
 * synonyms for a component alias); registering a performance variable
 * registers its group; deregistering or invalidating a group deregisters
 * its variables and invalidates its performance variables and events;
 * and closing a component invalidates and deregisters its variables.
 * Each of these must leave the lock free when it returns, and must not
 * take the lock while it already holds it.  So must registering an
 * invalidated performance variable or event again.
 *
 * This test drives each of those paths, checks that each did its job,
 * and checks after each that the lock is free.  In a debug build, taking
 * the lock while holding it fails an assertion; in a release build it
 * may deadlock, which the watchdog turns into a failure.
 */

#include "opal_config.h"

#include <signal.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_alias.h"
#include "opal/mca/base/mca_base_event.h"
#include "opal/mca/base/mca_base_pvar.h"
#include "opal/mca/base/mca_base_var.h"
#include "opal/mca/base/mca_base_var_enum.h"
#include "opal/mca/base/mca_base_var_group.h"
#include "opal/runtime/opal.h"

#define PROJECT "opal"
#define FRAMEWORK "locktest"
#define COMPONENT "comp"
#define ALIAS "oldcomp"

/* Seconds before the watchdog declares a deadlock */
#define TIMEOUT 120

static int failures = 0;

static void check(const char *what, int cond)
{
    if (cond) {
        printf("PASS: %s\n", what);
    } else {
        ++failures;
        printf("FAIL: %s\n", what);
    }
    fflush(stdout);
}

static void watchdog(int sig)
{
    static const char msg[] = "FAIL: timed out (deadlocked on the MCA tool lock?)\n";

    ssize_t n;

    (void) sig;
    n = write(STDOUT_FILENO, msg, sizeof(msg) - 1);
    (void) n;
    _exit(1);
}

/* Check that the calling thread does not hold the tool lock, and that it
   can take and release it */
static void check_lock_free(const char *after)
{
    char what[256];

#if OPAL_ENABLE_DEBUG
    snprintf(what, sizeof(what), "the tool lock is free after %s", after);
    check(what, !mca_base_tool_lock_held());
    if (mca_base_tool_lock_held()) {
        return;
    }
#endif
    mca_base_tool_lock();
    mca_base_tool_unlock();
    snprintf(what, sizeof(what), "the tool lock can be taken after %s", after);
    check(what, 1);
}

static int get_value(const mca_base_pvar_t *pvar __opal_attribute_unused__, void *value,
                     void *obj __opal_attribute_unused__)
{
    *(unsigned long *) value = 42;
    return OPAL_SUCCESS;
}

static int register_pvar(void)
{
    return mca_base_pvar_register(PROJECT, FRAMEWORK, COMPONENT, "counter", "a test counter",
                                  OPAL_INFO_LVL_4, MCA_BASE_PVAR_CLASS_COUNTER,
                                  MCA_BASE_VAR_TYPE_UNSIGNED_LONG, NULL,
                                  MCA_BASE_VAR_BIND_NO_OBJECT,
                                  MCA_BASE_PVAR_FLAG_READONLY | MCA_BASE_PVAR_FLAG_CONTINUOUS
                                      | MCA_BASE_PVAR_FLAG_IWG,
                                  get_value, NULL, NULL, NULL);
}

static mca_base_event_source_t *event_source = NULL;

static int register_event(void)
{
    static const mca_base_var_type_t types[1] = {MCA_BASE_VAR_TYPE_INT32_T};
    static const ptrdiff_t offsets[1] = {0};

    return mca_base_event_register(PROJECT, FRAMEWORK, COMPONENT, "event", "a test event",
                                   OPAL_INFO_LVL_4, 1, types, offsets, NULL,
                                   MCA_BASE_VAR_BIND_NO_OBJECT, MCA_BASE_EVENT_FLAG_IWG,
                                   event_source, NULL);
}

static bool pvar_valid(int index)
{
    const mca_base_pvar_t *pvar;

    return OPAL_SUCCESS == mca_base_pvar_get(index, &pvar) && !mca_base_pvar_is_invalid(pvar);
}

static bool event_valid(int index)
{
    mca_base_event_t *event;

    /* mca_base_event_get_by_index() does not return an invalid event */
    return OPAL_SUCCESS == mca_base_event_get_by_index(index, &event);
}

static bool var_registered(const char *component, const char *name)
{
    return 0 <= mca_base_var_find(PROJECT, FRAMEWORK, component, name);
}

/* What a component's close function saw */
static int pvar_index = -1;
static int event_index = -1;
static bool close_called;
static bool close_saw_pvar_valid;
static bool close_saw_event_valid;
static bool close_saw_lock_held;

static void close_record(void)
{
    close_called = true;
    close_saw_pvar_valid = pvar_valid(pvar_index);
    close_saw_event_valid = event_valid(event_index);
#if OPAL_ENABLE_DEBUG
    close_saw_lock_held = mca_base_tool_lock_held();
#else
    close_saw_lock_held = false;
#endif
}

static int close_refuse(void)
{
    close_record();
    return OPAL_ERROR;
}

static int close_ok(void)
{
    close_record();
    return OPAL_SUCCESS;
}

static void init_component(mca_base_component_t *component, mca_base_close_component_1_0_0_fn_t close)
{
    memset(component, 0, sizeof(*component));
    strncpy(component->mca_project_name, PROJECT, sizeof(component->mca_project_name) - 1);
    strncpy(component->mca_type_name, FRAMEWORK, sizeof(component->mca_type_name) - 1);
    strncpy(component->mca_component_name, COMPONENT,
            sizeof(component->mca_component_name) - 1);
    component->mca_close_component = close;
}

int main(int argc, char *argv[])
{
    mca_base_var_enum_value_t enum_values[] = {{0, "zero"}, {1, "one"}, {0, NULL}};
    mca_base_var_enum_t *enumerator = NULL;
    const mca_base_var_group_t *cgroup;
    mca_base_var_group_t *group = NULL;
    mca_base_component_t component;
    int var_index, group_index, rc;
    int value = 0;

    signal(SIGALRM, watchdog);
    alarm(TIMEOUT);

    opal_init(&argc, &argv);
    check_lock_free("opal_init()");

    /* Registering a variable of a component that has an alias also
       registers a synonym for it under the alias */
    rc = mca_base_alias_register(PROJECT, FRAMEWORK, COMPONENT, ALIAS, MCA_BASE_ALIAS_FLAG_NONE);
    check("alias_register succeeds", OPAL_SUCCESS == rc);
    var_index = mca_base_var_register(PROJECT, FRAMEWORK, COMPONENT, "value", "a test variable",
                                      MCA_BASE_VAR_TYPE_INT, NULL, 0, MCA_BASE_VAR_FLAG_DWG,
                                      OPAL_INFO_LVL_4, MCA_BASE_VAR_SCOPE_LOCAL, &value);
    check("var_register succeeds", 0 <= var_index);
    check("var_register registers the variable", var_registered(COMPONENT, "value"));
    check("var_register registers a synonym under the component's alias",
          var_registered(ALIAS, "value"));
    check_lock_free("registering a variable");

    rc = mca_base_var_register_synonym(var_index, PROJECT, FRAMEWORK, COMPONENT, "value_syn",
                                       0);
    check("var_register_synonym succeeds", 0 <= rc);
    check("var_register_synonym registers the synonym", var_registered(COMPONENT, "value_syn"));
    check_lock_free("registering a synonym");

    group_index = mca_base_var_group_find(PROJECT, FRAMEWORK, COMPONENT);
    check("registering a variable registers its group", 0 <= group_index);
    if (0 > group_index) {
        opal_finalize();
        return 1;
    }

    /* An enumerator added to the group */
    rc = mca_base_var_enum_create("locktest_enum", enum_values, &enumerator);
    check("var_enum_create succeeds", OPAL_SUCCESS == rc);
    rc = mca_base_var_enum_register(PROJECT, FRAMEWORK, COMPONENT, "locktest_enum", &enumerator);
    check("var_enum_register succeeds", OPAL_SUCCESS == rc);
    rc = mca_base_var_group_get(group_index, &cgroup);
    check("var_group_get succeeds", OPAL_SUCCESS == rc);
    if (OPAL_SUCCESS != rc) {
        opal_finalize();
        return 1;
    }
    group = (mca_base_var_group_t *) cgroup;
    check("var_enum_register adds the enumerator to the group",
          1 == opal_value_array_get_size(&group->group_enums));
    check_lock_free("registering an enumerator");

    /* A performance variable and an event in the group, both
       invalidated with it */
    pvar_index = register_pvar();
    check("pvar_register succeeds", 0 <= pvar_index);
    check("pvar_register adds the variable to the group",
          1 == opal_value_array_get_size(&group->group_pvars));
    check_lock_free("registering a performance variable");

    rc = mca_base_event_source_register("locktest.src", "a test event source",
                                        MCA_BASE_EVENT_SOURCE_ORDERED, 1000000000,
                                        OPAL_COUNT_MAX, NULL, true, NULL);
    check("event_source_register succeeds", 0 <= rc);
    if (0 <= rc) {
        (void) mca_base_event_source_get_by_index(rc, &event_source);
    }
    event_index = register_event();
    check("event_register succeeds", 0 <= event_index);
    check("event_register adds the event to the group",
          1 == opal_value_array_get_size(&group->group_events));
    check_lock_free("registering an event");

    /* Invalidating the group invalidates its performance variable and
       event, and leaves its variable registered */
    rc = mca_base_var_group_invalidate(group_index);
    check("var_group_invalidate succeeds", OPAL_SUCCESS == rc);
    check("var_group_invalidate invalidates the performance variable", !pvar_valid(pvar_index));
    check("var_group_invalidate invalidates the event", !event_valid(event_index));
    check("var_group_invalidate leaves the variable registered",
          var_registered(COMPONENT, "value"));
    check_lock_free("invalidating a group");

    /* Registering them again makes them valid again.  An invalid
       performance variable is registered again as a new variable, with a
       new index; an event is made valid again under its old index. */
    rc = register_pvar();
    check("pvar_register of an invalid variable succeeds", 0 <= rc);
    check("pvar_register of an invalid variable registers a valid one", pvar_valid(rc));
    check("the invalid performance variable stays invalid", !pvar_valid(pvar_index));
    pvar_index = rc;
    check_lock_free("registering a performance variable again");
    check("event_register of an invalid event returns its index", event_index == register_event());
    check("event_register of an invalid event makes it valid", event_valid(event_index));
    check_lock_free("registering an event again");

    /* Closing a component that refuses to close: its performance
       variables and events are invalidated before its close function
       runs, without the lock held, and stay invalid; it stays loaded, so
       its variables stay registered */
    init_component(&component, close_refuse);
    close_called = false;
    mca_base_component_close(&component, 0);
    check("component_close calls the close function", close_called);
    check("the close function runs after the performance variable is invalidated",
          !close_saw_pvar_valid);
    check("the close function runs after the event is invalidated", !close_saw_event_valid);
    check("the close function runs without the tool lock held", !close_saw_lock_held);
    check("a component that refuses to close keeps its performance variable invalid",
          !pvar_valid(pvar_index));
    check("a component that refuses to close keeps its variables",
          var_registered(COMPONENT, "value"));
    check_lock_free("closing a component that refuses to close");

    pvar_index = register_pvar();
    (void) register_event();
    check("registering them again makes the performance variable valid", pvar_valid(pvar_index));
    check("registering them again makes the event valid", event_valid(event_index));

    /* Closing a component that closes: it is unloaded, which deregisters
       its variables and releases its group's enumerator */
    init_component(&component, close_ok);
    close_called = false;
    mca_base_component_close(&component, 0);
    check("component_close calls the close function", close_called);
    check("the close function runs after the performance variable is invalidated",
          !close_saw_pvar_valid);
    check("the close function runs without the tool lock held", !close_saw_lock_held);
    check("closing a component deregisters its variables", !var_registered(COMPONENT, "value"));
    check("closing a component invalidates its performance variable", !pvar_valid(pvar_index));
    check("closing a component invalidates its event", !event_valid(event_index));
    check_lock_free("closing a component");

    /* Unloading the component deregistered its group, so deregistering
       it again fails -- and must still release the lock */
    rc = mca_base_var_group_deregister(group_index);
    check("var_group_deregister of a deregistered group fails", OPAL_SUCCESS != rc);
    check_lock_free("failing to deregister a group");

    opal_finalize();

    printf("RESULT: %s (%d failures)\n", 0 == failures ? "PASS" : "FAIL", failures);
    return 0 == failures ? 0 : 1;
}
