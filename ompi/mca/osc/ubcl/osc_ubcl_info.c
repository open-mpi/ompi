/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2025 Bull SAS.  All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi/mca/osc/ubcl/osc_ubcl_info.h"
#include "osc_ubcl_utils.h"

static void update_same_disp_unit_info(mca_osc_ubcl_module_t *module, bool value)
{
    if (value != module->same_disp_unit){
        int my_rank;
        int my_disp;

        my_rank = ompi_comm_rank(module->comm);
        my_disp = osc_ubcl_get_disp_unit(module, my_rank);

        /* Disp_unit array need to be freed or allocated */
        osc_ubcl_fini_disp_unit(module);
        module->same_disp_unit = value;
        osc_ubcl_sync_disp_unit(module, my_disp, false);
    }
}

static const char* update_local_copy(opal_infosubscriber_t *obj, const char *key, const char *value)
{
    bool bval;
    mca_osc_ubcl_module_t *module;
    struct ompi_win_t *win = (struct ompi_win_t*) obj;
    module = (mca_osc_ubcl_module_t *) win->w_osc_module;

    bval = opal_str_to_bool(value);
    mca_osc_ubcl_log(20, "%s updated to %s", key, value);

    if(0 == strcmp(key, "no_locks")) {
        module->no_locks = bval;
    } else if(0 == strcmp(key, "same_disp_unit")) {
        update_same_disp_unit_info(module, bval);
    }

    /* Do not change the official value. We just needed to update our copy */
    return value;
}

static bool get_win_info_bool(struct ompi_win_t *win, char *info_name)
{
    bool ret = false;
    int found;

    opal_info_get_bool(win->super.s_info, info_name, &ret, &found);
    return ret;
}

int osc_ubcl_read_info(struct opal_info_t *info, struct ompi_win_t *win)
{
    mca_osc_ubcl_module_t *module;

    /* Windows inherits from opal_infosubscriber_t class. Use it to keep
     * duplicated value up-to-date */
    opal_infosubscribe_subscribe(&win->super, "no_locks", "false", update_local_copy);
    opal_infosubscribe_subscribe(&win->super, "same_disp_unit", "false", update_local_copy);

    module = (mca_osc_ubcl_module_t *) win->w_osc_module;
    module->no_locks = get_win_info_bool(module->win, "no_locks");
    module->same_disp_unit = get_win_info_bool(module->win, "same_disp_unit");

    return OMPI_SUCCESS;
}

int osc_ubcl_get_disp_unit(mca_osc_ubcl_module_t *module, int target)
{
    if (module->same_disp_unit) {
        return module->disp_unit.uniq;
    } else {
        return module->disp_unit.all[target];
    }
}

int osc_ubcl_sync_disp_unit(mca_osc_ubcl_module_t *module, int disp_unit, bool need_synchro)
{
    int ret = OMPI_SUCCESS;

    if(! module->same_disp_unit) {
        int comm_size = ompi_comm_size(module->comm);
        int my_rank = ompi_comm_rank(module->comm);
        module->disp_unit.all =  malloc(comm_size * sizeof(int));
        if (NULL == module->disp_unit.all) {
            ret = OMPI_ERR_OUT_OF_RESOURCE;
            goto exit;
        }
        module->disp_unit.all[my_rank] = disp_unit;
        ret = module->comm->c_coll->coll_allgather(&disp_unit, 1, MPI_INT, module->disp_unit.all, 1,
                                                   MPI_INT, module->comm,
                                                   module->comm->c_coll->coll_allgather_module);
    } else if (need_synchro) {
        module->disp_unit.uniq = disp_unit;
        ret = module->comm->c_coll->coll_barrier(module->comm,
                                                 module->comm->c_coll->coll_barrier_module);
    }

exit:
    return ret;
}

void osc_ubcl_fini_disp_unit(mca_osc_ubcl_module_t *module)
{
    if(! module->same_disp_unit) {
        free(module->disp_unit.all);
        module->disp_unit.all = NULL;
    }
}
