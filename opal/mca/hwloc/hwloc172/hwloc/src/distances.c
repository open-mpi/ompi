/*
 * Copyright © 2010-2013 Inria.  All rights reserved.
 * Copyright © 2011-2012 Université Bordeaux 1
 * Copyright © 2011 Cisco Systems, Inc.  All rights reserved.
 * See COPYING in top-level directory.
 */

#include <private/autogen/config.h>
#include <hwloc.h>
#include <private/private.h>
#include <private/debug.h>

#include <float.h>
#include <math.h>

/**************************
 * Main Init/Clear/Destroy
 */

/* called during topology init */
void hwloc_distances_init(struct hwloc_topology *topology)
{
  topology->first_osdist = topology->last_osdist = NULL;
}

/* called when reloading a topology.
 * keep initial parameters (from set_distances and environment),
 * but drop what was generated during previous load().
 */
void hwloc_distances_clear(struct hwloc_topology *topology)
{
  struct hwloc_os_distances_s * osdist;
  for(osdist = topology->first_osdist; osdist; osdist = osdist->next) {
    /* remove final distance matrices, but keep physically-ordered ones */
    free(osdist->objs);
    osdist->objs = NULL;
  }
}

/* called during topology destroy */
void hwloc_distances_destroy(struct hwloc_topology * topology)
{
  struct hwloc_os_distances_s *osdist, *next = topology->first_osdist;
  while ((osdist = next) != NULL) {
    next = osdist->next;
    /* remove final distance matrics AND physically-ordered ones */
    free(osdist->indexes);
    free(osdist->objs);
    free(osdist->distances);
    free(osdist);
  }
  topology->first_osdist = topology->last_osdist = NULL;
}

/******************************************************
 * Inserting distances in the topology
 * from a backend, from the environment or by the user
 */

/* insert a distance matrix in the topology.
 * the caller gives us those pointers, we take care of freeing them later and so on.
 */
void hwloc_distances_set(hwloc_topology_t __hwloc_restrict topology, hwloc_obj_type_t type,
			 unsigned nbobjs, unsigned *indexes, hwloc_obj_t *objs, float *distances,
			 int force)
{
  struct hwloc_os_distances_s *osdist, *next = topology->first_osdist;
  /* look for existing distances for the same type */
  while ((osdist = next) != NULL) {
    next = osdist->next;
    if (osdist->type == type) {
      if (osdist->forced && !force) {
	/* there is a forced distance element, ignore the new non-forced one */
	free(indexes);
	free(objs);
	free(distances);
	return;
      } else if (force) {
	/* we're forcing a new distance, remove the old ones */
	free(osdist->indexes);
	free(osdist->objs);
	free(osdist->distances);
	/* remove current object */
	if (osdist->prev)
	  osdist->prev->next = next;
	else
	  topology->first_osdist = next;
	if (next)
	  next->prev = osdist->prev;
	else
	  topology->last_osdist = osdist->prev;
	/* free current object */
	free(osdist);
      }
    }
  }

  if (!nbobjs)
    /* we're just clearing, return now */
    return;

  /* create the new element */
  osdist = malloc(sizeof(struct hwloc_os_distances_s));
  osdist->nbobjs = nbobjs;
  osdist->indexes = indexes;
  osdist->objs = objs;
  osdist->distances = distances;
  osdist->forced = force;
  osdist->type = type;
  /* insert it */
  osdist->next = NULL;
  osdist->prev = topology->last_osdist;
  if (topology->last_osdist)
    topology->last_osdist->next = osdist;
  else
    topology->first_osdist = osdist;
  topology->last_osdist = osdist;
}

/* make sure a user-given distance matrix is sane */
static int hwloc_distances__check_matrix(hwloc_topology_t __hwloc_restrict topology __hwloc_attribute_unused, hwloc_obj_type_t type __hwloc_attribute_unused,
					 unsigned nbobjs, unsigned *indexes, hwloc_obj_t *objs __hwloc_attribute_unused, float *distances __hwloc_attribute_unused)
{
  unsigned i,j;
  /* make sure we don't have the same index twice */
  for(i=0; i<nbobjs; i++)
    for(j=i+1; j<nbobjs; j++)
      if (indexes[i] == indexes[j]) {
	errno = EINVAL;
	return -1;
      }
  return 0;
}

static void hwloc_distances__set_from_string(struct hwloc_topology *topology,
					     hwloc_obj_type_t type, char *string)
{
  /* the string format is: "index[0],...,index[N-1]:distance[0],...,distance[N*N-1]"
   * or "index[0],...,index[N-1]:X*Y" or "index[0],...,index[N-1]:X*Y*Z"
   */
  char *tmp = string, *next;
  unsigned *indexes;
  float *distances;
  unsigned nbobjs = 0, i, j, x, y, z;

  if (!strcmp(string, "none")) {
    hwloc_distances_set(topology, type, 0, NULL, NULL, NULL, 1 /* force */);
    return;
  }

  if (sscanf(string, "%u-%u:", &i, &j) == 2) {
    /* range i-j */
    nbobjs = j-i+1;
    indexes = calloc(nbobjs, sizeof(unsigned));
    distances = calloc(nbobjs*nbobjs, sizeof(float));
    /* make sure the user didn't give a veeeeery large range */
    if (!indexes || !distances) {
      free(indexes);
      free(distances);
      return;
    }
    for(j=0; j<nbobjs; j++)
      indexes[j] = j+i;
    tmp = strchr(string, ':') + 1;

  } else {
    /* explicit list of indexes, count them */
    while (1) {
      size_t size = strspn(tmp, "0123456789");
      if (tmp[size] != ',') {
	/* last element */
	tmp += size;
	nbobjs++;
	break;
      }
      /* another index */
      tmp += size+1;
      nbobjs++;
    }

    if (*tmp != ':') {
      fprintf(stderr, "Ignoring %s distances from environment variable, missing colon\n",
	      hwloc_obj_type_string(type));
      return;
    }

    indexes = calloc(nbobjs, sizeof(unsigned));
    distances = calloc(nbobjs*nbobjs, sizeof(float));
    tmp = string;
    
    /* parse indexes */
    for(i=0; i<nbobjs; i++) {
      indexes[i] = strtoul(tmp, &next, 0);
      tmp = next+1;
    }
  }


  /* parse distances */
  z=1; /* default if sscanf finds only 2 values below */
  if (sscanf(tmp, "%u*%u*%u", &x, &y, &z) >= 2) {
    /* generate the matrix to create x groups of y elements */
    if (x*y*z != nbobjs) {
      fprintf(stderr, "Ignoring %s distances from environment variable, invalid grouping (%u*%u*%u=%u instead of %u)\n",
	      hwloc_obj_type_string(type), x, y, z, x*y*z, nbobjs);
      free(indexes);
      free(distances);
      return;
    }
    for(i=0; i<nbobjs; i++)
      for(j=0; j<nbobjs; j++)
	if (i==j)
	  distances[i*nbobjs+j] = 1;
	else if (i/z == j/z)
	  distances[i*nbobjs+j] = 2;
	else if (i/z/y == j/z/y)
	  distances[i*nbobjs+j] = 4;
	else
	  distances[i*nbobjs+j] = 8;

  } else {
    /* parse a comma separated list of distances */
    for(i=0; i<nbobjs*nbobjs; i++) {
      distances[i] = (float) atof(tmp);
      next = strchr(tmp, ',');
      if (next) {
        tmp = next+1;
      } else if (i!=nbobjs*nbobjs-1) {
	fprintf(stderr, "Ignoring %s distances from environment variable, not enough values (%u out of %u)\n",
		hwloc_obj_type_string(type), i+1, nbobjs*nbobjs);
	free(indexes);
	free(distances);
	return;
      }
    }
  }

  if (hwloc_distances__check_matrix(topology, type, nbobjs, indexes, NULL, distances) < 0) {
    fprintf(stderr, "Ignoring invalid %s distances from environment variable\n", hwloc_obj_type_string(type));
    free(indexes);
    free(distances);
    return;
  }

  hwloc_distances_set(topology, type, nbobjs, indexes, NULL, distances, 1 /* force */);
}

/* take distances in the environment, store them as is in the topology.
 * we'll convert them into object later once the tree is filled
 */
void hwloc_distances_set_from_env(struct hwloc_topology *topology)
{
  hwloc_obj_type_t type;
  for(type = HWLOC_OBJ_SYSTEM; type < HWLOC_OBJ_TYPE_MAX; type++) {
    char *env, envname[64];
    snprintf(envname, sizeof(envname), "HWLOC_%s_DISTANCES", hwloc_obj_type_string(type));
    env = getenv(envname);
    if (env) {
      hwloc_localeswitch_declare;
      hwloc_localeswitch_init();
      hwloc_distances__set_from_string(topology, type, env);
      hwloc_localeswitch_fini();
    }
  }
}

/* The actual set() function exported to the user
 *
 * take the given distance, store them as is in the topology.
 * we'll convert them into object later once the tree is filled.
 */
int hwloc_topology_set_distance_matrix(hwloc_topology_t __hwloc_restrict topology, hwloc_obj_type_t type,
				       unsigned nbobjs, unsigned *indexes, float *distances)
{
  unsigned *_indexes;
  float *_distances;

  if (!nbobjs && !indexes && !distances) {
    hwloc_distances_set(topology, type, 0, NULL, NULL, NULL, 1 /* force */);
    return 0;
  }

  if (!nbobjs || !indexes || !distances)
    return -1;

  if (hwloc_distances__check_matrix(topology, type, nbobjs, indexes, NULL, distances) < 0)
    return -1;

  /* copy the input arrays and give them to the topology */
  _indexes = malloc(nbobjs*sizeof(unsigned));
  memcpy(_indexes, indexes, nbobjs*sizeof(unsigned));
  _distances = malloc(nbobjs*nbobjs*sizeof(float));
  memcpy(_distances, distances, nbobjs*nbobjs*sizeof(float));
  hwloc_distances_set(topology, type, nbobjs, _indexes, NULL, _distances, 1 /* force */);

  return 0;
}

/************************
 * Restricting distances
 */

/* called when some objects have been removed because empty/ignored/cgroup/restrict,
 * we must rebuild the list of objects from indexes (in hwloc_distances_finalize_os())
 */
void hwloc_distances_restrict_os(struct hwloc_topology *topology)
{
  struct hwloc_os_distances_s * osdist;
  for(osdist = topology->first_osdist; osdist; osdist = osdist->next) {
    /* remove the objs array, we'll rebuild it from the indexes
     * depending on remaining objects */
    free(osdist->objs);
    osdist->objs = NULL;
  }
}


/* cleanup everything we created from distances so that we may rebuild them
 * at the end of restrict()
 */
void hwloc_distances_restrict(struct hwloc_topology *topology, unsigned long flags)
{
  if (flags & HWLOC_RESTRICT_FLAG_ADAPT_DISTANCES) {
    /* some objects may have been removed, clear objects arrays so that finalize_os rebuilds them properly */
    hwloc_distances_restrict_os(topology);
  } else {
    /* if not adapting distances, drop everything */
    hwloc_distances_destroy(topology);
  }
}

/**************************************************************
 * Convert user/env given array of indexes into actual objects
 */

static hwloc_obj_t hwloc_find_obj_by_type_and_os_index(hwloc_obj_t root, hwloc_obj_type_t type, unsigned os_index)
{
  hwloc_obj_t child;
  if (root->type == type && root->os_index == os_index)
    return root;
  child = root->first_child;
  while (child) {
    hwloc_obj_t found = hwloc_find_obj_by_type_and_os_index(child, type, os_index);
    if (found)
      return found;
    child = child->next_sibling;
  }
  return NULL;
}

/* convert distance indexes that were previously stored in the topology
 * into actual objects if not done already.
 * it's already done when distances come from backends (this function should not be called then).
 * it's not done when distances come from the user.
 *
 * returns -1 if the matrix was invalid
 */
static int
hwloc_distances__finalize_os(struct hwloc_topology *topology, struct hwloc_os_distances_s *osdist)
{
  unsigned nbobjs = osdist->nbobjs;
  unsigned *indexes = osdist->indexes;
  float *distances = osdist->distances;
  unsigned i, j;
  hwloc_obj_type_t type = osdist->type;
  hwloc_obj_t *objs = calloc(nbobjs, sizeof(hwloc_obj_t));

  assert(!osdist->objs);

  /* traverse the topology and look for the relevant objects */
  for(i=0; i<nbobjs; i++) {
    hwloc_obj_t obj = hwloc_find_obj_by_type_and_os_index(topology->levels[0][0], type, indexes[i]);
    if (!obj) {

      /* shift the matrix */
#define OLDPOS(i,j) (distances+(i)*nbobjs+(j))
#define NEWPOS(i,j) (distances+(i)*(nbobjs-1)+(j))
      if (i>0) {
	/** no need to move beginning of 0th line */
	for(j=0; j<i-1; j++)
	  /** move end of jth line + beginning of (j+1)th line */
	  memmove(NEWPOS(j,i), OLDPOS(j,i+1), (nbobjs-1)*sizeof(*distances));
	/** move end of (i-1)th line */
	memmove(NEWPOS(i-1,i), OLDPOS(i-1,i+1), (nbobjs-i-1)*sizeof(*distances));
      }
      if (i<nbobjs-1) {
	/** move beginning of (i+1)th line */
	memmove(NEWPOS(i,0), OLDPOS(i+1,0), i*sizeof(*distances));
	/** move end of jth line + beginning of (j+1)th line */
	for(j=i; j<nbobjs-2; j++)
	  memmove(NEWPOS(j,i), OLDPOS(j+1,i+1), (nbobjs-1)*sizeof(*distances));
	/** move end of (nbobjs-2)th line */
	memmove(NEWPOS(nbobjs-2,i), OLDPOS(nbobjs-1,i+1), (nbobjs-i-1)*sizeof(*distances));
      }

      /* shift the indexes array */
      memmove(indexes+i, indexes+i+1, (nbobjs-i-1)*sizeof(*indexes));

      /* update counters */
      nbobjs--;
      i--;
      continue;
    }
    objs[i] = obj;
  }

  osdist->nbobjs = nbobjs;
  if (!nbobjs) {
    /* the whole matrix was invalid, let the caller remove this distances */
    free(objs);
    return -1;
  }

  /* setup the objs array */
  osdist->objs = objs;
  return 0;
}


void hwloc_distances_finalize_os(struct hwloc_topology *topology)
{
  int dropall = !topology->levels[0][0]->cpuset; /* we don't support distances on multinode systems */

  struct hwloc_os_distances_s *osdist, *next = topology->first_osdist;
  while ((osdist = next) != NULL) {
    int err;
    next = osdist->next;

    if (dropall)
      goto drop;

    /* remove final distance matrics AND physically-ordered ones */

    if (osdist->objs)
      /* nothing to do, switch to the next element */
      continue;

    err = hwloc_distances__finalize_os(topology, osdist);
    if (!err)
      /* convert ok, switch to the next element */
      continue;

   drop:
    /* remove this element */
    free(osdist->indexes);
    free(osdist->distances);
    /* remove current object */
    if (osdist->prev)
      osdist->prev->next = next;
    else
      topology->first_osdist = next;
    if (next)
      next->prev = osdist->prev;
    else
      topology->last_osdist = osdist->prev;
    /* free current object */
    free(osdist);
  }
}

/***********************************************************
 * Convert internal distances given by the backend/env/user
 * into exported logical distances attached to objects
 */

static hwloc_obj_t
hwloc_get_obj_covering_cpuset_nodeset(struct hwloc_topology *topology,
				      hwloc_const_cpuset_t cpuset,
				      hwloc_const_nodeset_t nodeset)
{
  hwloc_obj_t parent = hwloc_get_root_obj(topology), child;

  assert(cpuset);
  assert(nodeset);
  assert(hwloc_bitmap_isincluded(cpuset, parent->cpuset));
  assert(!nodeset || hwloc_bitmap_isincluded(nodeset, parent->nodeset));

 trychildren:
  child = parent->first_child;
  while (child) {
    /* look for a child with a cpuset containing ours.
     * if it has a nodeset, it must also contain ours.
     */
    if (child->cpuset && hwloc_bitmap_isincluded(cpuset, child->cpuset)
	&& (!child->nodeset || hwloc_bitmap_isincluded(nodeset, child->nodeset))) {
      parent = child;
      goto trychildren;
    }
    child = child->next_sibling;
  }
  return parent;
}

static void
hwloc_distances__finalize_logical(struct hwloc_topology *topology,
				  unsigned nbobjs,
				  hwloc_obj_t *objs, float *osmatrix)
{
  unsigned i, j, li, lj, minl;
  float min = FLT_MAX, max = FLT_MIN;
  hwloc_obj_t root;
  float *matrix;
  hwloc_cpuset_t cpuset;
  hwloc_nodeset_t nodeset;
  unsigned relative_depth;
  int idx;

  /* find the root */
  cpuset = hwloc_bitmap_alloc();
  nodeset = hwloc_bitmap_alloc();
  for(i=0; i<nbobjs; i++) {
    hwloc_bitmap_or(cpuset, cpuset, objs[i]->cpuset);
    if (objs[i]->nodeset)
      hwloc_bitmap_or(nodeset, nodeset, objs[i]->nodeset);
  }
  /* find the object covering cpuset AND nodeset (can't use hwloc_get_obj_covering_cpuset()) */
  root = hwloc_get_obj_covering_cpuset_nodeset(topology, cpuset, nodeset);
  if (!root) {
    /* should not happen, ignore the distance matrix and report an error. */
    if (!hwloc_hide_errors()) {
      char *a, *b;
      hwloc_bitmap_asprintf(&a, cpuset);
      hwloc_bitmap_asprintf(&b, nodeset);
      fprintf(stderr, "****************************************************************************\n");
      fprintf(stderr, "* hwloc has encountered an error when adding a distance matrix to the topology.\n");
      fprintf(stderr, "*\n");
      fprintf(stderr, "* hwloc_distances__finalize_logical() could not find any object covering\n");
      fprintf(stderr, "* cpuset %s and nodeset %s\n", a, b);
      fprintf(stderr, "*\n");
      fprintf(stderr, "* Please report this error message to the hwloc user's mailing list,\n");
#ifdef HWLOC_LINUX_SYS
      fprintf(stderr, "* along with the output from the hwloc-gather-topology.sh script.\n");
#else
      fprintf(stderr, "* along with any relevant topology information from your platform.\n");
#endif
      fprintf(stderr, "****************************************************************************\n");
      free(a);
      free(b);
    }
    hwloc_bitmap_free(cpuset);
    hwloc_bitmap_free(nodeset);
    return;
  }
  /* ideally, root has the exact cpuset and nodeset.
   * but ignoring or other things that remove objects may cause the object array to reduce */
  assert(hwloc_bitmap_isincluded(cpuset, root->cpuset));
  assert(hwloc_bitmap_isincluded(nodeset, root->nodeset));
  hwloc_bitmap_free(cpuset);
  hwloc_bitmap_free(nodeset);
  if (root->depth >= objs[0]->depth) {
    /* strange topology led us to find invalid relative depth, ignore */
    return;
  }
  relative_depth = objs[0]->depth - root->depth; /* this assume that we have distances between objects of the same level */

  if (nbobjs != hwloc_get_nbobjs_inside_cpuset_by_depth(topology, root->cpuset, root->depth + relative_depth))
    /* the root does not cover the right number of objects, maybe we failed to insert a root (bad intersect or so). */
    return;

  /* get the logical index offset, it's the min of all logical indexes */
  minl = UINT_MAX;
  for(i=0; i<nbobjs; i++)
    if (minl > objs[i]->logical_index)
      minl = objs[i]->logical_index;

  /* compute/check min/max values */
  for(i=0; i<nbobjs; i++)
    for(j=0; j<nbobjs; j++) {
      float val = osmatrix[i*nbobjs+j];
      if (val < min)
	min = val;
      if (val > max)
	max = val;
    }
  if (!min) {
    /* Linux up to 2.6.36 reports ACPI SLIT distances, which should be memory latencies.
     * Except of SGI IP27 (SGI Origin 200/2000 with MIPS processors) where the distances
     * are the number of hops between routers.
     */
    hwloc_debug("%s", "minimal distance is 0, matrix does not seem to contain latencies, ignoring\n");
    return;
  }

  /* store the normalized latency matrix in the root object */
  idx = root->distances_count++;
  root->distances = realloc(root->distances, root->distances_count * sizeof(struct hwloc_distances_s *));
  root->distances[idx] = malloc(sizeof(struct hwloc_distances_s));
  root->distances[idx]->relative_depth = relative_depth;
  root->distances[idx]->nbobjs = nbobjs;
  root->distances[idx]->latency = matrix = malloc(nbobjs*nbobjs*sizeof(float));
  root->distances[idx]->latency_base = (float) min;
#define NORMALIZE_LATENCY(d) ((d)/(min))
  root->distances[idx]->latency_max = NORMALIZE_LATENCY(max);
  for(i=0; i<nbobjs; i++) {
    li = objs[i]->logical_index - minl;
    matrix[li*nbobjs+li] = NORMALIZE_LATENCY(osmatrix[i*nbobjs+i]);
    for(j=i+1; j<nbobjs; j++) {
      lj = objs[j]->logical_index - minl;
      matrix[li*nbobjs+lj] = NORMALIZE_LATENCY(osmatrix[i*nbobjs+j]);
      matrix[lj*nbobjs+li] = NORMALIZE_LATENCY(osmatrix[j*nbobjs+i]);
    }
  }
}

/* convert internal distances into logically-ordered distances
 * that can be exposed in the API
 */
void
hwloc_distances_finalize_logical(struct hwloc_topology *topology)
{
  unsigned nbobjs;
  int depth;
  struct hwloc_os_distances_s * osdist;
  for(osdist = topology->first_osdist; osdist; osdist = osdist->next) {

    nbobjs = osdist->nbobjs;
    if (!nbobjs)
      continue;

    depth = hwloc_get_type_depth(topology, osdist->type);
    if (depth == HWLOC_TYPE_DEPTH_UNKNOWN || depth == HWLOC_TYPE_DEPTH_MULTIPLE)
      continue;

    if (osdist->objs) {
      assert(osdist->distances);
      hwloc_distances__finalize_logical(topology, nbobjs,
					osdist->objs,
					osdist->distances);
    }
  }
}

/***************************************************
 * Destroying logical distances attached to objects
 */

/* destroy an object distances structure */
void
hwloc_clear_object_distances_one(struct hwloc_distances_s * distances)
{
  free(distances->latency);
  free(distances);

}

void
hwloc_clear_object_distances(hwloc_obj_t obj)
{
  unsigned i;
  for (i=0; i<obj->distances_count; i++)
    hwloc_clear_object_distances_one(obj->distances[i]);
  free(obj->distances);
  obj->distances = NULL;
  obj->distances_count = 0;
}

/******************************************
 * Grouping objects according to distances
 */

static void hwloc_report_user_distance_error(const char *msg, int line)
{
    static int reported = 0;

    if (!reported && !hwloc_hide_errors()) {
        fprintf(stderr, "****************************************************************************\n");
        fprintf(stderr, "* hwloc has encountered what looks like an error from user-given distances.\n");
        fprintf(stderr, "*\n");
        fprintf(stderr, "* %s\n", msg);
        fprintf(stderr, "* Error occurred in topology.c line %d\n", line);
        fprintf(stderr, "*\n");
        fprintf(stderr, "* Please make sure that distances given through the interface or environment\n");
        fprintf(stderr, "* variables do not contradict any other topology information.\n");
        fprintf(stderr, "****************************************************************************\n");
        reported = 1;
    }
}

static int hwloc_compare_distances(float a, float b, float accuracy)
{
  if (accuracy != 0.0 && fabsf(a-b) < a * accuracy)
    return 0;
  return a < b ? -1 : a == b ? 0 : 1;
}

/*
 * Place objects in groups if they are in a transitive graph of minimal distances.
 * Return how many groups were created, or 0 if some incomplete distance graphs were found.
 */
static unsigned
hwloc__find_groups_by_min_distance(unsigned nbobjs,
				   float *_distances,
				   float accuracy,
				   unsigned *groupids,
				   int verbose)
{
  float min_distance = FLT_MAX;
  unsigned groupid = 1;
  unsigned i,j,k;
  unsigned skipped = 0;

#define DISTANCE(i, j) _distances[(i) * nbobjs + (j)]

  memset(groupids, 0, nbobjs*sizeof(*groupids));

  /* find the minimal distance */
  for(i=0; i<nbobjs; i++)
    for(j=0; j<nbobjs; j++) /* check the entire matrix, it may not be perfectly symmetric depending on the accuracy */
      if (i != j && DISTANCE(i, j) < min_distance) /* no accuracy here, we want the real minimal */
        min_distance = DISTANCE(i, j);
  hwloc_debug("found minimal distance %f between objects\n", min_distance);

  if (min_distance == FLT_MAX)
    return 0;

  /* build groups of objects connected with this distance */
  for(i=0; i<nbobjs; i++) {
    unsigned size;
    int firstfound;

    /* if already grouped, skip */
    if (groupids[i])
      continue;

    /* start a new group */
    groupids[i] = groupid;
    size = 1;
    firstfound = i;

    while (firstfound != -1) {
      /* we added new objects to the group, the first one was firstfound.
       * rescan all connections from these new objects (starting at first found) to any other objects,
       * so as to find new objects minimally-connected by transivity.
       */
      int newfirstfound = -1;
      for(j=firstfound; j<nbobjs; j++)
	if (groupids[j] == groupid)
	  for(k=0; k<nbobjs; k++)
              if (!groupids[k] && !hwloc_compare_distances(DISTANCE(j, k), min_distance, accuracy)) {
	      groupids[k] = groupid;
	      size++;
	      if (newfirstfound == -1)
		newfirstfound = k;
	      if (i == j)
		hwloc_debug("object %u is minimally connected to %u\n", k, i);
	      else
	        hwloc_debug("object %u is minimally connected to %u through %u\n", k, i, j);
	    }
      firstfound = newfirstfound;
    }

    if (size == 1) {
      /* cancel this useless group, ignore this object and try from the next one */
      groupids[i] = 0;
      skipped++;
      continue;
    }

    /* valid this group */
    groupid++;
    if (verbose)
      fprintf(stderr, "Found transitive graph with %u objects with minimal distance %f accuracy %f\n",
	      size, min_distance, accuracy);
  }

  if (groupid == 2 && !skipped)
    /* we created a single group containing all objects, ignore it */
    return 0;

  /* return the last id, since it's also the number of used group ids */
  return groupid-1;
}

/* check that the matrix is ok */
static int
hwloc__check_grouping_matrix(unsigned nbobjs, float *_distances, float accuracy, int verbose)
{
  unsigned i,j;
  for(i=0; i<nbobjs; i++) {
    for(j=i+1; j<nbobjs; j++) {
      /* should be symmetric */
      if (hwloc_compare_distances(DISTANCE(i, j), DISTANCE(j, i), accuracy)) {
	if (verbose)
	  fprintf(stderr, "Distance matrix asymmetric ([%u,%u]=%f != [%u,%u]=%f), aborting\n",
		  i, j, DISTANCE(i, j), j, i, DISTANCE(j, i));
	return -1;
      }
      /* diagonal is smaller than everything else */
      if (hwloc_compare_distances(DISTANCE(i, j), DISTANCE(i, i), accuracy) <= 0) {
	if (verbose)
	  fprintf(stderr, "Distance to self not strictly minimal ([%u,%u]=%f <= [%u,%u]=%f), aborting\n",
		  i, j, DISTANCE(i, j), i, i, DISTANCE(i, i));
	return -1;
      }
    }
  }
  return 0;
}

/*
 * Look at object physical distances to group them.
 */
static void
hwloc__groups_by_distances(struct hwloc_topology *topology,
			   unsigned nbobjs,
			   struct hwloc_obj **objs,
			   float *_distances,
			   unsigned nbaccuracies, float *accuracies,
			   int fromuser,
			   int needcheck,
			   int verbose)
{
  unsigned *groupids = NULL;
  unsigned nbgroups = 0;
  unsigned i,j;

  if (nbobjs <= 2) {
      return;
  }

  groupids = malloc(sizeof(unsigned) * nbobjs);
  if (NULL == groupids) {
      return;
  }

  for(i=0; i<nbaccuracies; i++) {
    if (verbose)
      fprintf(stderr, "Trying to group %u %s objects according to physical distances with accuracy %f\n",
	      nbobjs, hwloc_obj_type_string(objs[0]->type), accuracies[i]);
    if (needcheck && hwloc__check_grouping_matrix(nbobjs, _distances, accuracies[i], verbose) < 0)
      continue;
    nbgroups = hwloc__find_groups_by_min_distance(nbobjs, _distances, accuracies[i], groupids, verbose);
    if (nbgroups)
      break;
  }
  if (!nbgroups)
    goto outter_free;

  /* For convenience, put these declarations inside a block.  It's a
     crying shame we can't use C99 syntax here, and have to do a bunch
     of mallocs. :-( */
  {
      hwloc_obj_t *groupobjs = NULL;
      unsigned *groupsizes = NULL;
      float *groupdistances = NULL;

      groupobjs = malloc(sizeof(hwloc_obj_t) * nbgroups);
      groupsizes = malloc(sizeof(unsigned) * nbgroups);
      groupdistances = malloc(sizeof(float) * nbgroups * nbgroups);
      if (NULL == groupobjs || NULL == groupsizes || NULL == groupdistances) {
          goto inner_free;
      }
      /* create new Group objects and record their size */
      memset(&(groupsizes[0]), 0, sizeof(groupsizes[0]) * nbgroups);
      for(i=0; i<nbgroups; i++) {
          /* create the Group object */
          hwloc_obj_t group_obj, res_obj;
          group_obj = hwloc_alloc_setup_object(HWLOC_OBJ_GROUP, -1);
          group_obj->cpuset = hwloc_bitmap_alloc();
          group_obj->attr->group.depth = topology->next_group_depth;
          for (j=0; j<nbobjs; j++)
	    if (groupids[j] == i+1) {
	      /* assemble the group cpuset */
	      hwloc_bitmap_or(group_obj->cpuset, group_obj->cpuset, objs[j]->cpuset);
	      /* if one obj has a nodeset, assemble a group nodeset */
	      if (objs[j]->nodeset) {
		if (!group_obj->nodeset)
		  group_obj->nodeset = hwloc_bitmap_alloc();
		hwloc_bitmap_or(group_obj->nodeset, group_obj->nodeset, objs[j]->nodeset);
	      }
              groupsizes[i]++;
            }
          hwloc_debug_1arg_bitmap("adding Group object with %u objects and cpuset %s\n",
                                  groupsizes[i], group_obj->cpuset);
          res_obj = hwloc__insert_object_by_cpuset(topology, group_obj,
						   fromuser ? hwloc_report_user_distance_error : hwloc_report_os_error);
	  /* res_obj may be different from group_objs if we got groups from XML import before grouping */
          groupobjs[i] = res_obj;
      }

      /* factorize distances */
      memset(&(groupdistances[0]), 0, sizeof(groupdistances[0]) * nbgroups * nbgroups);
#undef DISTANCE
#define DISTANCE(i, j) _distances[(i) * nbobjs + (j)]
#define GROUP_DISTANCE(i, j) groupdistances[(i) * nbgroups + (j)]
      for(i=0; i<nbobjs; i++)
	if (groupids[i])
	  for(j=0; j<nbobjs; j++)
	    if (groupids[j])
                GROUP_DISTANCE(groupids[i]-1, groupids[j]-1) += DISTANCE(i, j);
      for(i=0; i<nbgroups; i++)
          for(j=0; j<nbgroups; j++) {
              unsigned groupsize = groupsizes[i]*groupsizes[j];
              float groupsizef = (float) groupsize;
              GROUP_DISTANCE(i, j) /= groupsizef;
          }
#ifdef HWLOC_DEBUG
      hwloc_debug("%s", "generated new distance matrix between groups:\n");
      hwloc_debug("%s", "  index");
      for(j=0; j<nbgroups; j++)
	hwloc_debug(" % 5d", (int) j); /* print index because os_index is -1 for Groups */
      hwloc_debug("%s", "\n");
      for(i=0; i<nbgroups; i++) {
	hwloc_debug("  % 5d", (int) i);
	for(j=0; j<nbgroups; j++)
	  hwloc_debug(" %2.3f", GROUP_DISTANCE(i, j));
	hwloc_debug("%s", "\n");
      }
#endif

      topology->next_group_depth++;
      hwloc__groups_by_distances(topology, nbgroups, groupobjs, (float*) groupdistances, nbaccuracies, accuracies, fromuser, 0 /* no need to check generated matrix */, verbose);

  inner_free:
      /* Safely free everything */
      if (NULL != groupobjs) {
          free(groupobjs);
      }
      if (NULL != groupsizes) {
          free(groupsizes);
      }
      if (NULL != groupdistances) {
          free(groupdistances);
      }
  }

 outter_free:
  if (NULL != groupids) {
      free(groupids);
  }
}

void
hwloc_group_by_distances(struct hwloc_topology *topology)
{
  unsigned nbobjs;
  struct hwloc_os_distances_s * osdist;
  char *env;
  float accuracies[5] = { 0.0f, 0.01f, 0.02f, 0.05f, 0.1f };
  unsigned nbaccuracies = 5;
  hwloc_obj_t group_obj;
  int verbose = 0;
  unsigned i;
#ifdef HWLOC_DEBUG
  unsigned j;
#endif

  env = getenv("HWLOC_GROUPING");
  if (env && !atoi(env))
    return;
  /* backward compat with v1.2 */
  if (getenv("HWLOC_IGNORE_DISTANCES"))
    return;

  env = getenv("HWLOC_GROUPING_ACCURACY");
  if (!env) {
    /* only use 0.0 */
    nbaccuracies = 1;
  } else if (strcmp(env, "try")) {
    /* use the given value */
    nbaccuracies = 1;
    accuracies[0] = (float) atof(env);
  } /* otherwise try all values */

#ifdef HWLOC_DEBUG
  verbose = 1;
#else
  env = getenv("HWLOC_GROUPING_VERBOSE");
  if (env)
    verbose = atoi(env);
#endif

  for(osdist = topology->first_osdist; osdist; osdist = osdist->next) {

    nbobjs = osdist->nbobjs;
    if (!nbobjs)
      continue;

    if (osdist->objs) {
      /* if we have objs, we must have distances as well,
       * thanks to hwloc_convert_distances_indexes_into_objects()
       */
      assert(osdist->distances);

#ifdef HWLOC_DEBUG
      hwloc_debug("%s", "trying to group objects using distance matrix:\n");
      hwloc_debug("%s", "  index");
      for(j=0; j<nbobjs; j++)
	hwloc_debug(" % 5d", (int) osdist->objs[j]->os_index);
      hwloc_debug("%s", "\n");
      for(i=0; i<nbobjs; i++) {
	hwloc_debug("  % 5d", (int) osdist->objs[i]->os_index);
	for(j=0; j<nbobjs; j++)
	  hwloc_debug(" %2.3f", osdist->distances[i*nbobjs + j]);
	hwloc_debug("%s", "\n");
      }
#endif

      hwloc__groups_by_distances(topology, nbobjs,
				 osdist->objs,
				 osdist->distances,
				 nbaccuracies, accuracies,
				 osdist->indexes != NULL,
				 1 /* check the first matrice */,
				 verbose);

      /* add a final group object covering everybody so that the distance matrix can be stored somewhere.
       * this group will be merged into a regular object if the matrix isn't strangely incomplete
       */
      group_obj = hwloc_alloc_setup_object(HWLOC_OBJ_GROUP, -1);
      group_obj->attr->group.depth = (unsigned) -1;
      group_obj->cpuset = hwloc_bitmap_alloc();
      for(i=0; i<nbobjs; i++) {
	/* assemble the group cpuset */
	hwloc_bitmap_or(group_obj->cpuset, group_obj->cpuset, osdist->objs[i]->cpuset);
	/* if one obj has a nodeset, assemble a group nodeset */
	if (osdist->objs[i]->nodeset) {
	  if (!group_obj->nodeset)
	    group_obj->nodeset = hwloc_bitmap_alloc();
	  hwloc_bitmap_or(group_obj->nodeset, group_obj->nodeset, osdist->objs[i]->nodeset);
	}
      }
      hwloc_debug_1arg_bitmap("adding Group object (as root of distance matrix with %u objects) with cpuset %s\n",
			      nbobjs, group_obj->cpuset);
      hwloc__insert_object_by_cpuset(topology, group_obj,
				     osdist->indexes != NULL ? hwloc_report_user_distance_error : hwloc_report_os_error);
    }
  }
}
