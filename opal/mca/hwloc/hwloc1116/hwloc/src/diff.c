/*
 * Copyright © 2013-2014 Inria.  All rights reserved.
 * See COPYING in top-level directory.
 */

#include <private/autogen/config.h>
#include <private/private.h>
#include <private/misc.h>

int hwloc_topology_diff_destroy(hwloc_topology_t topology __hwloc_attribute_unused,
				hwloc_topology_diff_t diff)
{
	hwloc_topology_diff_t next;
	while (diff) {
		next = diff->generic.next;
		switch (diff->generic.type) {
		default:
			break;
		case HWLOC_TOPOLOGY_DIFF_OBJ_ATTR:
			switch (diff->obj_attr.diff.generic.type) {
			default:
				break;
			case HWLOC_TOPOLOGY_DIFF_OBJ_ATTR_NAME:
			case HWLOC_TOPOLOGY_DIFF_OBJ_ATTR_INFO:
				free(diff->obj_attr.diff.string.name);
				free(diff->obj_attr.diff.string.oldvalue);
				free(diff->obj_attr.diff.string.newvalue);
				break;
			}
			break;
		}
		free(diff);
		diff = next;
	}
	return 0;
}

/************************
 * Computing diffs
 */

static void hwloc_append_diff(hwloc_topology_diff_t newdiff,
			      hwloc_topology_diff_t *firstdiffp,
			      hwloc_topology_diff_t *lastdiffp)
{
	if (*firstdiffp)
		(*lastdiffp)->generic.next = newdiff;
	else
		*firstdiffp = newdiff;
	*lastdiffp = newdiff;
	newdiff->generic.next = NULL;
}

static int hwloc_append_diff_too_complex(hwloc_obj_t obj1,
					 hwloc_topology_diff_t *firstdiffp,
					 hwloc_topology_diff_t *lastdiffp)
{
	hwloc_topology_diff_t newdiff;
	newdiff = malloc(sizeof(*newdiff));
	if (!newdiff)
		return -1;

	newdiff->too_complex.type = HWLOC_TOPOLOGY_DIFF_TOO_COMPLEX;
	newdiff->too_complex.obj_depth = obj1->depth;
	newdiff->too_complex.obj_index = obj1->logical_index;
	hwloc_append_diff(newdiff, firstdiffp, lastdiffp);
	return 0;
}

static int hwloc_append_diff_obj_attr_string(hwloc_obj_t obj,
					     hwloc_topology_diff_obj_attr_type_t type,
					     const char *name,
					     const char *oldvalue,
					     const char *newvalue,
					     hwloc_topology_diff_t *firstdiffp,
					     hwloc_topology_diff_t *lastdiffp)
{
	hwloc_topology_diff_t newdiff;

	if (obj->type == HWLOC_OBJ_MISC)
		/* TODO: add a custom level/depth for Misc */
		return hwloc_append_diff_too_complex(obj, firstdiffp, lastdiffp);

	newdiff = malloc(sizeof(*newdiff));
	if (!newdiff)
		return -1;

	newdiff->obj_attr.type = HWLOC_TOPOLOGY_DIFF_OBJ_ATTR;
	newdiff->obj_attr.obj_depth = obj->depth;
	newdiff->obj_attr.obj_index = obj->logical_index;
	newdiff->obj_attr.diff.string.type = type;
	newdiff->obj_attr.diff.string.name = name ? strdup(name) : NULL;
	newdiff->obj_attr.diff.string.oldvalue = oldvalue ? strdup(oldvalue) : NULL;
	newdiff->obj_attr.diff.string.newvalue = newvalue ? strdup(newvalue) : NULL;
	hwloc_append_diff(newdiff, firstdiffp, lastdiffp);
	return 0;
}

static int hwloc_append_diff_obj_attr_uint64(hwloc_obj_t obj,
					     hwloc_topology_diff_obj_attr_type_t type,
					     hwloc_uint64_t idx,
					     hwloc_uint64_t oldvalue,
					     hwloc_uint64_t newvalue,
					     hwloc_topology_diff_t *firstdiffp,
					     hwloc_topology_diff_t *lastdiffp)
{
	hwloc_topology_diff_t newdiff;

	if (obj->type == HWLOC_OBJ_MISC)
		/* TODO: add a custom level/depth for Misc */
		return hwloc_append_diff_too_complex(obj, firstdiffp, lastdiffp);

	newdiff = malloc(sizeof(*newdiff));
	if (!newdiff)
		return -1;

	newdiff->obj_attr.type = HWLOC_TOPOLOGY_DIFF_OBJ_ATTR;
	newdiff->obj_attr.obj_depth = obj->depth;
	newdiff->obj_attr.obj_index = obj->logical_index;
	newdiff->obj_attr.diff.uint64.type = type;
	newdiff->obj_attr.diff.uint64.index = idx;
	newdiff->obj_attr.diff.uint64.oldvalue = oldvalue;
	newdiff->obj_attr.diff.uint64.newvalue = newvalue;
	hwloc_append_diff(newdiff, firstdiffp, lastdiffp);
	return 0;
}

static int
hwloc_diff_trees(hwloc_topology_t topo1, hwloc_obj_t obj1,
		 hwloc_topology_t topo2, hwloc_obj_t obj2,
		 unsigned flags,
		 hwloc_topology_diff_t *firstdiffp, hwloc_topology_diff_t *lastdiffp)
{
	unsigned i;
	int err;

	if (obj1->depth != obj2->depth)
		goto out_too_complex;
	if (obj1->type != obj2->type)
		goto out_too_complex;

	if (obj1->os_index != obj2->os_index)
		/* we could allow different os_index for non-PU non-NUMAnode objects
		 * but it's likely useless anyway */
		goto out_too_complex;

#define _SETS_DIFFERENT(_set1, _set2) \
 (   ( !(_set1) != !(_set2) ) \
  || ( (_set1) && !hwloc_bitmap_isequal(_set1, _set2) ) )
#define SETS_DIFFERENT(_set, _obj1, _obj2) _SETS_DIFFERENT((_obj1)->_set, (_obj2)->_set)
	if (SETS_DIFFERENT(cpuset, obj1, obj2)
	    || SETS_DIFFERENT(complete_cpuset, obj1, obj2)
	    || SETS_DIFFERENT(online_cpuset, obj1, obj2)
	    || SETS_DIFFERENT(allowed_cpuset, obj1, obj2)
	    || SETS_DIFFERENT(nodeset, obj1, obj2)
	    || SETS_DIFFERENT(complete_nodeset, obj1, obj2)
	    || SETS_DIFFERENT(allowed_nodeset, obj1, obj2))
		goto out_too_complex;

	/* no need to check logical_index, sibling_rank, symmetric_subtree,
	 * the parents did it */

	if ((!obj1->name) != (!obj2->name)
	    || (obj1->name && strcmp(obj1->name, obj2->name))) {
		err = hwloc_append_diff_obj_attr_string(obj1,
						       HWLOC_TOPOLOGY_DIFF_OBJ_ATTR_NAME,
						       NULL,
						       obj1->name,
						       obj2->name,
						       firstdiffp, lastdiffp);
		if (err < 0)
			return err;
	}

	/* memory */
	if (obj1->memory.local_memory != obj2->memory.local_memory) {
		err = hwloc_append_diff_obj_attr_uint64(obj1,
						       HWLOC_TOPOLOGY_DIFF_OBJ_ATTR_SIZE,
						       0,
						       obj1->memory.local_memory,
						       obj2->memory.local_memory,
						       firstdiffp, lastdiffp);
		if (err < 0)
			return err;
	}
	/* ignore memory page_types */

	/* ignore os_level */

	/* type-specific attrs */
	switch (obj1->type) {
	default:
		break;
	case HWLOC_OBJ_CACHE:
		if (memcmp(obj1->attr, obj2->attr, sizeof(obj1->attr->cache)))
			goto out_too_complex;
		break;
	case HWLOC_OBJ_GROUP:
		if (memcmp(obj1->attr, obj2->attr, sizeof(obj1->attr->group)))
			goto out_too_complex;
		break;
	case HWLOC_OBJ_PCI_DEVICE:
		if (memcmp(obj1->attr, obj2->attr, sizeof(obj1->attr->pcidev)))
			goto out_too_complex;
		break;
	case HWLOC_OBJ_BRIDGE:
		if (memcmp(obj1->attr, obj2->attr, sizeof(obj1->attr->bridge)))
			goto out_too_complex;
		break;
	case HWLOC_OBJ_OS_DEVICE:
		if (memcmp(obj1->attr, obj2->attr, sizeof(obj1->attr->osdev)))
			goto out_too_complex;
		break;
	}

	/* distances */
	if (obj1->distances_count != obj2->distances_count)
		goto out_too_complex;
	for(i=0; i<obj1->distances_count; i++) {
		struct hwloc_distances_s *d1 = obj1->distances[i], *d2 = obj2->distances[i];
		if (d1->relative_depth != d2->relative_depth
		    || d1->nbobjs != d2->nbobjs
		    || d1->latency_max != d2->latency_max
		    || d1->latency_base != d2->latency_base
		    || memcmp(d1->latency, d2->latency, d1->nbobjs * d1->nbobjs * sizeof(*d1->latency)))
			goto out_too_complex;
	}

	/* infos */
	if (obj1->infos_count != obj2->infos_count)
		goto out_too_complex;
	for(i=0; i<obj1->infos_count; i++) {
		if (strcmp(obj1->infos[i].name, obj2->infos[i].name))
			goto out_too_complex;
		if (strcmp(obj1->infos[i].value, obj2->infos[i].value)) {
			err = hwloc_append_diff_obj_attr_string(obj1,
							       HWLOC_TOPOLOGY_DIFF_OBJ_ATTR_INFO,
							       obj1->infos[i].name,
							       obj1->infos[i].value,
							       obj2->infos[i].value,
							       firstdiffp, lastdiffp);
			if (err < 0)
				return err;
		}
	}

	/* ignore userdata */

	/* children */
	if (obj1->arity != obj2->arity)
		goto out_too_complex;
	for(i=0; i<obj1->arity; i++) {
		err = hwloc_diff_trees(topo1, obj1->children[i],
				       topo2, obj2->children[i],
				       flags,
				       firstdiffp, lastdiffp);
		if (err < 0)
			return err;
	}

	return 0;

out_too_complex:
	hwloc_append_diff_too_complex(obj1, firstdiffp, lastdiffp);
	return 0;
}

int hwloc_topology_diff_build(hwloc_topology_t topo1,
			      hwloc_topology_t topo2,
			      unsigned long flags,
			      hwloc_topology_diff_t *diffp)
{
	hwloc_topology_diff_t lastdiff, tmpdiff;
	int err;

	if (flags != 0) {
		errno = EINVAL;
		return -1;
	}

	*diffp = NULL;
	err = hwloc_diff_trees(topo1, hwloc_get_root_obj(topo1),
			       topo2, hwloc_get_root_obj(topo2),
			       flags,
			       diffp, &lastdiff);

	if (!err) {
		tmpdiff = *diffp;
		while (tmpdiff) {
			if (tmpdiff->generic.type == HWLOC_TOPOLOGY_DIFF_TOO_COMPLEX) {
				err = 1;
				break;
			}
			tmpdiff = tmpdiff->generic.next;
		}
	}

	return err;
}

/********************
 * Applying diffs
 */

static int
hwloc_apply_diff_one(hwloc_topology_t topology,
		     hwloc_topology_diff_t diff,
		     unsigned long flags)
{
	int reverse = !!(flags & HWLOC_TOPOLOGY_DIFF_APPLY_REVERSE);

	switch (diff->generic.type) {
	case HWLOC_TOPOLOGY_DIFF_OBJ_ATTR: {
		struct hwloc_topology_diff_obj_attr_s *obj_attr = &diff->obj_attr;
		hwloc_obj_t obj = hwloc_get_obj_by_depth(topology, obj_attr->obj_depth, obj_attr->obj_index);
		if (!obj)
			return -1;

		switch (obj_attr->diff.generic.type) {
		case HWLOC_TOPOLOGY_DIFF_OBJ_ATTR_SIZE: {
			hwloc_obj_t tmpobj;
			hwloc_uint64_t oldvalue = reverse ? obj_attr->diff.uint64.newvalue : obj_attr->diff.uint64.oldvalue;
			hwloc_uint64_t newvalue = reverse ? obj_attr->diff.uint64.oldvalue : obj_attr->diff.uint64.newvalue;
			hwloc_uint64_t valuediff = newvalue - oldvalue;
			if (obj->memory.local_memory != oldvalue)
				return -1;
			obj->memory.local_memory = newvalue;
			tmpobj = obj;
			while (tmpobj) {
				tmpobj->memory.total_memory += valuediff;
				tmpobj = tmpobj->parent;
			}
			break;
		}
		case HWLOC_TOPOLOGY_DIFF_OBJ_ATTR_NAME: {
			const char *oldvalue = reverse ? obj_attr->diff.string.newvalue : obj_attr->diff.string.oldvalue;
			const char *newvalue = reverse ? obj_attr->diff.string.oldvalue : obj_attr->diff.string.newvalue;
			if (!obj->name || strcmp(obj->name, oldvalue))
				return -1;
			free(obj->name);
			obj->name = strdup(newvalue);
			break;
		}
		case HWLOC_TOPOLOGY_DIFF_OBJ_ATTR_INFO: {
			const char *name = obj_attr->diff.string.name;
			const char *oldvalue = reverse ? obj_attr->diff.string.newvalue : obj_attr->diff.string.oldvalue;
			const char *newvalue = reverse ? obj_attr->diff.string.oldvalue : obj_attr->diff.string.newvalue;
			unsigned i;
			int found = 0;
			for(i=0; i<obj->infos_count; i++) {
				if (!strcmp(obj->infos[i].name, name)
				    && !strcmp(obj->infos[i].value, oldvalue)) {
					free(obj->infos[i].value);
					obj->infos[i].value = strdup(newvalue);
					found = 1;
					break;
				}
			}
			if (!found)
				return -1;
			break;
		}
		default:
			return -1;
		}

		break;
	}
	default:
		return -1;
	}

	return 0;
}

int hwloc_topology_diff_apply(hwloc_topology_t topology,
			      hwloc_topology_diff_t diff,
			      unsigned long flags)
{
	hwloc_topology_diff_t tmpdiff, tmpdiff2;
	int err, nr;

	if (flags & ~HWLOC_TOPOLOGY_DIFF_APPLY_REVERSE) {
		errno = EINVAL;
		return -1;
	}

	tmpdiff = diff;
	nr = 0;
	while (tmpdiff) {
		nr++;
		err = hwloc_apply_diff_one(topology, tmpdiff, flags);
		if (err < 0)
			goto cancel;
		tmpdiff = tmpdiff->generic.next;
	}
	return 0;

cancel:
	tmpdiff2 = tmpdiff;
	tmpdiff = diff;
	while (tmpdiff != tmpdiff2) {
		hwloc_apply_diff_one(topology, tmpdiff, flags ^ HWLOC_TOPOLOGY_DIFF_APPLY_REVERSE);
		tmpdiff = tmpdiff->generic.next;
	}
	errno = EINVAL;
	return -nr; /* return the index (starting at 1) of the first element that couldn't be applied */
}
