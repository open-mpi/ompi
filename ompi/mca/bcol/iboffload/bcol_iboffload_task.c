 /*
  * Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
  * Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
  * $COPYRIGHT$
  *
  * Additional copyrights may follow
  *
  * $HEADER$
  */

#include "ompi_config.h"

#include "bcol_iboffload.h"
#include "bcol_iboffload_frag.h"
#include "bcol_iboffload_task.h"

static void task_constructor(mca_bcol_iboffload_task_t *task)
{
    task->frag = NULL;
    task->collfrag = NULL;
    task->endpoint = NULL;
    task->next_task = NULL;

    task->sg_entries = NULL;
    task->sg_entries_num = 0;

    task->task_list = NULL;

    memset(&task->wr, 0, sizeof(task->wr));

    memset(&task->element, 0, sizeof(struct mqe_task));
    memset(&task->task_mqe_qp_entry, 0, sizeof(struct mqe_qp_entry));
}

static void task_destructor(mca_bcol_iboffload_task_t *task)
{
    if (NULL != task->sg_entries) {
        free(task->sg_entries);
    }
}

OBJ_CLASS_INSTANCE(
        mca_bcol_iboffload_task_t,
        ompi_free_list_item_t,
        task_constructor,
        task_destructor);

void
mca_bcol_iboffload_calc_task_init(ompi_free_list_item_t* item, void* ctx)
{
    mca_bcol_iboffload_task_t *calc_task =
                    (mca_bcol_iboffload_task_t *) item;

    calc_task->task_list = (ompi_free_list_t *) ctx;

    calc_task->sg_entries_num = 2;
    calc_task->sg_entries = (struct ibv_sge *) malloc (2 * sizeof(struct ibv_sge));
}

void
mca_bcol_iboffload_iovec_task_init(ompi_free_list_item_t* item, void* ctx)
{
    mca_bcol_iboffload_task_t *iovec_task =
                    (mca_bcol_iboffload_task_t *) item;

    mca_bcol_iboffload_module_t *iboffload_module =
                   (mca_bcol_iboffload_module_t *) ctx;

    int nitems, group_size = iboffload_module->group_size;

    nitems = group_size / 2 + group_size % 2;
    if (nitems > iboffload_module->device->ib_dev_attr.max_sge) {
        nitems = iboffload_module->device->ib_dev_attr.max_sge;
    }

    iovec_task->sg_entries_num = nitems;
    iovec_task->task_list = &iboffload_module->iovec_tasks_free;

    iovec_task->sg_entries = (struct ibv_sge *)
                       malloc(nitems * sizeof(struct ibv_sge));
}
