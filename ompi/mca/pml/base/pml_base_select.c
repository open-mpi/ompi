/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2007 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2012      Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2013-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2015-2020 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2020-2022 Amazon.com, Inc. or its affiliates.  All Rights
 * Copyright (c) 2018-2020 Triad National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2026      NVIDIA Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include <string.h>

#include "opal/class/opal_list.h"
#include "opal/util/output.h"
#include "opal/util/show_help.h"
#include "opal/util/string_copy.h"
#include "opal/runtime/opal_progress.h"
#include "ompi/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/runtime/opal.h"
#include "opal/mca/pmix/pmix-internal.h"

#include "ompi/constants.h"
#include "ompi/instance/instance.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/pml/base/base.h"
#include "ompi/proc/proc.h"
#include "ompi/runtime/ompi_modex.h"
#include "ompi/runtime/ompi_rte.h"

typedef struct opened_component_t {
  opal_list_item_t super;
  mca_pml_base_component_t *om_component;
} opened_component_t;


static int mca_pml_base_finalize (void) {
  if (NULL != mca_pml_base_selected_component.pmlm_finalize) {
      return mca_pml_base_selected_component.pmlm_finalize();
  }

  return OMPI_SUCCESS;
}

/**
 * Function for selecting one component from all those that are
 * available.
 *
 * Call the init function on all available components and get their
 * priorities.  Select the component with the highest priority.  All
 * other components will be closed and unloaded.  The selected component
 * will have all of its function pointers saved and returned to the
 * caller.
 */
int mca_pml_base_select(bool enable_progress_threads,
                        bool enable_mpi_threads)
{
    int i, priority = 0, best_priority = -1, ret = 0;
    opal_list_item_t *item = NULL;
    mca_base_component_list_item_t *cli = NULL;
    mca_pml_base_component_t *component = NULL, *best_component = NULL;
    mca_pml_base_module_t *module = NULL, *best_module = NULL;
    opal_list_t opened;
    opened_component_t *om = NULL;
    bool found_pml;

    /* Traverse the list of available components; call their init
       functions. */

    OBJ_CONSTRUCT(&opened, opal_list_t);
    OPAL_LIST_FOREACH(cli, &ompi_pml_base_framework.framework_components, mca_base_component_list_item_t) {
        component = (mca_pml_base_component_t *) cli->cli_component;

        /* if there is an include list - item must be in the list to be included */
        found_pml = false;
        for( i = 0; i < opal_pointer_array_get_size(&mca_pml_base_pml); i++) {
            char * tmp_val = NULL;
            tmp_val = (char *) opal_pointer_array_get_item(&mca_pml_base_pml, i);
            if( NULL == tmp_val) {
                continue;
            }

            if(0 == strncmp(component->pmlm_version.mca_component_name,
                            tmp_val, strlen(component->pmlm_version.mca_component_name)) ) {
                found_pml = true;
                break;
            }
        }

        if(!found_pml && opal_pointer_array_get_size(&mca_pml_base_pml)) {
            opal_output_verbose( 10, ompi_pml_base_framework.framework_output,
                                     "select: component %s not in the include list",
                                     component->pmlm_version.mca_component_name );

            continue;
        }

        /* if there is no init function - ignore it */
        if (NULL == component->pmlm_init) {
            opal_output_verbose( 10, ompi_pml_base_framework.framework_output,
                                 "select: no init function; ignoring component %s",
                                 component->pmlm_version.mca_component_name );
            continue;
        }

        /* Init component to get its priority */
        opal_output_verbose( 10, ompi_pml_base_framework.framework_output,
                             "select: initializing %s component %s",
                             component->pmlm_version.mca_type_name,
                             component->pmlm_version.mca_component_name );
        priority = best_priority;
        module = component->pmlm_init(&priority, enable_progress_threads,
                                      enable_mpi_threads);
        if (NULL == module) {
            opal_output_verbose( 10, ompi_pml_base_framework.framework_output,
                                 "select: init returned failure for component %s",
                                 component->pmlm_version.mca_component_name );
            continue;
        }

        opal_output_verbose( 10, ompi_pml_base_framework.framework_output,
                             "select: init returned priority %d", priority );
        if (priority > best_priority) {
            best_priority = priority;
            best_component = component;
            best_module = module;
        }

        om = (opened_component_t*)malloc(sizeof(opened_component_t));
        if (NULL == om) {
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
        OBJ_CONSTRUCT(om, opal_list_item_t);
        om->om_component = component;
        opal_list_append(&opened, (opal_list_item_t*) om);
    }

    /* Finished querying all components.  Check for the bozo case. */

    if( NULL == best_component ) {
        opal_show_help("help-mca-base.txt", "find-available:none found",
                       true, "pml",
                       opal_process_info.nodename,
                       "pml");
        for( i = 0; i < opal_pointer_array_get_size(&mca_pml_base_pml); i++) {
            char * tmp_val = NULL;
            tmp_val = (char *) opal_pointer_array_get_item(&mca_pml_base_pml, i);
            if( NULL == tmp_val) {
                continue;
            }
            ompi_rte_abort(1, "PML %s cannot be selected", tmp_val);
        }
        if(0 == i) {
            ompi_rte_abort(2, "No pml component available.  This shouldn't happen.");
        }
    }

    opal_output_verbose( 10, ompi_pml_base_framework.framework_output,
                         "selected %s best priority %d\n",
                         best_component->pmlm_version.mca_component_name, best_priority);

    /* Save the winner */

    mca_pml_base_selected_component = *best_component;
    mca_pml = *best_module;
    opal_output_verbose( 10, ompi_pml_base_framework.framework_output,
                         "select: component %s selected",
                         mca_pml_base_selected_component.pmlm_version.mca_component_name );

    /* Finalize all non-selected components */

    for (item = opal_list_remove_first(&opened);
         NULL != item;
         item = opal_list_remove_first(&opened)) {
        om = (opened_component_t *) item;

        if (om->om_component != best_component ) {
            /* Finalize */

            if (NULL != om->om_component->pmlm_finalize) {

                /* Blatantly ignore the return code (what would we do to
                   recover, anyway?  This component is going away, so errors
                   don't matter anymore) */

                om->om_component->pmlm_finalize();
                opal_output_verbose(10, ompi_pml_base_framework.framework_output,
                                    "select: component %s not selected / finalized",
                                    om->om_component->pmlm_version.mca_component_name);
            }
        }
        OBJ_DESTRUCT( om );
        free(om);
    }
    OBJ_DESTRUCT( &opened );

    /* This base function closes, unloads, and removes from the
       available list all unselected components.  The available list will
       contain only the selected component. */

    mca_base_components_close(ompi_pml_base_framework.framework_output,
                              &ompi_pml_base_framework.framework_components,
                              (mca_base_component_t *) best_component);

    /* register the winner's callback */
    if( NULL != mca_pml.pml_progress ) {
        opal_progress_register(mca_pml.pml_progress);
    }

#if OPAL_ENABLE_FT_MPI
    if( NULL == mca_pml.pml_revoke_comm ) {
        /* do not crash when calling a not implemented function after a failure is
         * reported, return a NOT_IMPLEMENTED error */
        mca_pml.pml_revoke_comm = mca_pml_base_revoke_comm;
    }
#endif /* OPAL_ENABLE_FT_MPI */

    /* register winner in the modex */
    ret = mca_pml_base_pml_selected(best_component->pmlm_version.mca_component_name);

    /* All done */
    ompi_mpi_instance_append_finalize (mca_pml_base_finalize);

    return ret;
}

/* need a "commonly" named PML structure so everything ends up in the
   same modex field. Not static: ompi_modex.c probes the same key to learn
   whether a peer has committed, and two copies of this would drift into a
   Get on a key nobody published. */
mca_base_component_t mca_pml_base_modex_component = {
    OMPI_MCA_BASE_VERSION_2_1_0("pml", 2, 0, 0),
    .mca_component_name = "base",
    .mca_component_major_version = 2,
    .mca_component_minor_version = 0,
    .mca_component_release_version = 0,
};


/* What this process put in the modex, which is what a peer compares
 * itself against. Kept here rather than read back from
 * mca_pml_base_selected_component: vprotocol renames that copy
 * ("ob1]vpessimist") as the losing components close, while what went
 * into the modex is the host PML's own name. */
static char mca_pml_base_pml_name[MCA_BASE_MAX_COMPONENT_NAME_LEN + 1] = {0};

/*
 * If direct modex, then publish PML for all procs. If full modex then
 * publish PML for rank 0 only, to keep the collected payload small.
 * Which of the two it is decides how the check below can be made: a
 * name per rank can be checked per peer, rank 0's alone can only be
 * checked against rank 0 -- which is enough, since every rank runs that
 * comparison and a job that disagrees has at least one rank that
 * differs from rank 0.
 *
 * This is a within-job affair either way. Two jobs being connected
 * compare through the exchange the connect itself makes, since neither
 * can name a rank of the other whose data it is sure to hold.
 *
 * Direct Modex is performed when collect_all_data is false, as we do
 * not perform a fence operation during MPI_Init if async_modex is true.
 * If async_modex is false and collect_all_data is false then we do a
 * zero-byte barrier and we would still require direct modex during
 * add_procs
 */
int
mca_pml_base_pml_selected(const char *name)
{
    int rc = 0;

    opal_string_copy(mca_pml_base_pml_name, name, sizeof(mca_pml_base_pml_name));

    if (!opal_pmix_collect_all_data || 0 == OMPI_PROC_MY_NAME->vpid) {
        OPAL_MODEX_SEND(rc, PMIX_GLOBAL, &mca_pml_base_modex_component, name,
                        strlen(name) + 1);
    }
    return rc;
}

const char *mca_pml_base_pml_selected_name(void)
{
    return mca_pml_base_pml_name;
}

/*
 * Compare this process's PML against what one peer published.
 *
 * cache_only asks for what is already local instead of fetching: a
 * caller that cannot afford a round trip, or a blocking Get, passes
 * true and retries later.
 *
 * @retval OMPI_SUCCESS       the same PML, or the peer is us.
 * @retval OMPI_ERR_NOT_READY the peer's blob has not reached us yet.
 * @retval OMPI_ERR_NOT_FOUND the peer published nothing under this key.
 * @retval OMPI_ERR_UNREACH   another PML. Reported here, since this is
 *                            the only place that holds both names.
 */
static int pml_check_one(opal_process_name_t proc_name, bool cache_only)
{
    size_t size;
    int ret = 0;
    char *remote_pml;
    char *key;

    /* if we are proc_name=OMPI_PROC_MY_NAME, then we can also assume success */
    if (0 == opal_compare_proc(ompi_proc_local()->super.proc_name, proc_name)) {
        opal_output_verbose( 10, ompi_pml_base_framework.framework_output,
                            "check:select: PML check not necessary on self");
        return OMPI_SUCCESS;
    }
    if ('\0' == mca_pml_base_pml_name[0]) {
        /* Nothing selected yet, so nothing to compare against. */
        return OMPI_SUCCESS;
    }

    /* Built once and freed: the macro expands its key argument more than
     * once, so a call there allocates twice and leaks both. */
    key = mca_base_component_to_string(&mca_pml_base_modex_component);
    if (NULL == key) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    if (cache_only) {
        OPAL_MODEX_RECV_STRING_OPTIONAL(ret, key, &proc_name,
                                        (void**) &remote_pml, &size);
    } else {
        OPAL_MODEX_RECV_STRING(ret, key, &proc_name,
                               (void**) &remote_pml, &size);
    }
    free(key);
    if (OPAL_ERR_NOT_READY == ret) {
        /* The peer published its choice, but the blob has not reached us
         * yet. Distinct from NOT_FOUND: the caller must come back later
         * rather than declare the peer unreachable. */
        opal_output_verbose( 10, ompi_pml_base_framework.framework_output,
                            "check:select: PML modex for process %s not local yet",
                            OMPI_NAME_PRINT(&proc_name));
        return OMPI_ERR_NOT_READY;
    }
    /* Both spellings, because the two macros above do not agree: the
     * fetching one folds every flavour of "no such key" onto the OPAL
     * status, the cache-only one hands back the PMIx status untouched. */
    if (OPAL_ERR_NOT_FOUND == ret || PMIX_ERR_NOT_FOUND == ret) {
        opal_output_verbose( 10, ompi_pml_base_framework.framework_output,
                            "check:select: PML modex for process %s not found",
                            OMPI_NAME_PRINT(&proc_name));
        return OMPI_ERR_NOT_FOUND;
    }

    /* the remote pml returned should never be NULL if an error
     * wasn't returned, but just to be safe, and since the check
     * is fast...let's be sure
     */
    if (NULL == remote_pml) {
        opal_output_verbose( 10, ompi_pml_base_framework.framework_output,
                            "check:select: got a NULL pml from process %s",
                            OMPI_NAME_PRINT(&proc_name));
        return OMPI_ERR_UNREACH;
    }

    opal_output_verbose( 10, ompi_pml_base_framework.framework_output,
                        "check:select: checking my pml %s against process %s"
                        " pml %s", mca_pml_base_pml_name,
                        OMPI_NAME_PRINT(&proc_name), remote_pml);

    /* if that module doesn't match my own, return an error */
    if ((size != strlen(mca_pml_base_pml_name) + 1) ||
        (0 != strcmp(mca_pml_base_pml_name, remote_pml))) {
        char *errhost = NULL;
        OPAL_MODEX_RECV_VALUE_OPTIONAL(ret, PMIX_HOSTNAME, &proc_name,
                                       &(errhost), PMIX_STRING);
        opal_output(0, "%s selected pml %s, but peer %s on %s selected pml %s",
                    OMPI_NAME_PRINT(&ompi_proc_local()->super.proc_name),
                    mca_pml_base_pml_name, OMPI_NAME_PRINT(&proc_name),
                    (NULL == errhost) ? "unknown" : errhost,
                    remote_pml);
        free(remote_pml);
        free(errhost);
         /* cleanup before returning */
        return OMPI_ERR_UNREACH;
    }

    free(remote_pml);
    return OMPI_SUCCESS;
}

/* Compare against this job's rank 0. One rank answers for the whole job,
 * and within our own job rank 0 is the one every rank can name without
 * holding a proc for it -- its data comes out of the same exchange as
 * everybody else's. */
static int pml_check_own_job(void)
{
    opal_process_name_t rank0 = {.jobid = OMPI_PROC_MY_NAME->jobid, .vpid = 0};

    return pml_check_one(rank0, false);
}

/* One comparison answers for a whole job exchanged under a fence, so this
 * records that it has been made. Raced at worst into being made twice,
 * which costs a local read and reaches the same verdict. */
static bool pml_check_job_done = false;

/* Make that comparison as soon as the exchange it reads has landed, from
 * whichever of the paths below gets there first. Called where the job is
 * about to rely on every rank having chosen the same PML, so up to the
 * first wire-up, which is as late as it can matter.
 *
 * Nobody to hand a failure to: a job whose ranks chose different PMLs
 * cannot send a single message, retry or not, and pml_check_one() has
 * already said which two names disagree. */
static void pml_check_own_job_once(void)
{
    if (pml_check_job_done || !ompi_modex_all_ready()) {
        return;
    }
    pml_check_job_done = true;
    if (OMPI_ERR_UNREACH == pml_check_own_job()) {
        ompi_rte_abort(1, NULL);
    }
}

int mca_pml_base_pml_check_start(void)
{
    if (!ompi_pml_base_check_pml || !opal_pmix_collect_all_data) {
        return OMPI_SUCCESS;
    }

    /* Compare here if rank 0's data is already local, so that a
     * misconfigured job dies in MPI_Init. The fence was only just started,
     * so usually it is not, and then the first wire-up makes the
     * comparison -- either the one MPI_Init itself does for the PMLs that
     * need every peer wired, or the first one a send provokes. */
    pml_check_own_job_once();
    return OMPI_SUCCESS;
}

int mca_pml_base_pml_check_peer(ompi_proc_t *proc)
{
    bool in_flight;
    int ret;

    if (!ompi_pml_base_check_pml) {
        return OMPI_SUCCESS;
    }

    if (opal_pmix_collect_all_data) {
        /* Under a fence one comparison covers the job, and this peer is
         * about to be wired, which cannot happen before that fence has
         * landed -- so here it is certain to be made. */
        pml_check_own_job_once();
        return OMPI_SUCCESS;
    }

    /* While this peer's blob can still arrive, ask only what is local:
     * this runs from a first send or an arriving fragment, where a round
     * trip -- and a blocking one at that -- does not belong, and coming
     * back later costs nothing because the key this reads is the very one
     * ompi_modex fetches to decide the peer has committed. Asking is what
     * starts that fetch, so the retry is not a spin. Once the blob is
     * here there is no later, and the same read is a cache hit. */
    in_flight = !ompi_modex_proc_ready(proc);

    ret = pml_check_one(proc->super.proc_name, in_flight);
    if (OMPI_ERR_UNREACH == ret) {
        ompi_rte_abort(1, NULL);
    }
    if (OMPI_ERR_NOT_FOUND == ret) {
        /* Every rank publishes this key in this mode, so while the
         * exchange is in flight a miss is a peer that has not committed
         * yet -- and the wire-up this precedes would fail the same way.
         * A miss after we have actually asked is final: there is nothing
         * to compare against, and refusing to talk to a peer over a
         * check we cannot make would be worse than not making it. */
        return in_flight ? OMPI_ERR_NOT_READY : OMPI_SUCCESS;
    }

    return ret;
}

int
mca_pml_base_pml_check_selected(ompi_proc_t **procs,
                                size_t nprocs)
{
    opal_jobid_t my_jobid = OMPI_PROC_MY_NAME->jobid;
    int ret;

    if (!ompi_pml_base_check_pml) {
        return OMPI_SUCCESS;
    }

    /* Only this job's own procs, and only in the mode where a rank has to
     * be read one at a time. Under a fence every rank is compared against
     * rank 0, which covers the job transitively, and this is one of the
     * places that comparison can first be made.
     *
     * Procs of another job are not compared here. Neither job can name a
     * rank of the other whose published data it is sure to hold -- accept
     * can hand back a subgroup, so even that job's rank 0 may be a name
     * nothing local has ever heard of -- so the two compare through the
     * exchange their roots already make in ompi_dpm_connect_accept(). */
    if (opal_pmix_collect_all_data) {
        pml_check_own_job_once();
    } else {
        for (size_t i = 0; i < nprocs; ++i) {
            if (procs[i]->super.proc_name.jobid != my_jobid) {
                continue;
            }
            ret = pml_check_one(procs[i]->super.proc_name, false);
            if (OMPI_ERR_NOT_FOUND == ret) {
                ret = OMPI_ERR_NOT_READY;
            }
            if (OMPI_SUCCESS != ret) {
                return ret;
            }
        }
    }

    return OMPI_SUCCESS;
}
