/**
 * VampirTrace
 * http://www.tu-dresden.de/zih/vampirtrace
 *
 * Copyright (c) 2005-2008, ZIH, TU Dresden, Federal Republic of Germany
 *
 * Copyright (c) 1998-2005, Forschungszentrum Juelich, Juelich Supercomputing
 *                          Centre, Federal Republic of Germany
 *
 * See the file COPYING in the package base directory for details
 **/

#include "config.h"

#include "vt_otf_gen.h"
#include "vt_otf_sum.h"
#include "vt_error.h"
#include "vt_inttypes.h"
#include "vt_pform.h"
#include "vt_trc.h"

#include <stdio.h>
#include <stdlib.h>

#include "otf.h"

#define VTSUM_STACK_BSIZE 100
#define VTSUM_STAT_BSIZE  500

/*
 *-----------------------------------------------------------------------------
 * Macro functions
 *-----------------------------------------------------------------------------
 */

#define VTSUM_CHECK(sum) \
  if (sum == NULL) vt_error_msg("Abort: Uninitialized summary generator")

#define VTSUM_FUNC_STAT_ADD(_sum, _rid, _stat)                        \
{                                                                     \
  if (_sum->func_stat_num == _sum->func_stat_size)                    \
  {                                                                   \
    _sum->func_stat = (VTSum_funcStat*)realloc(_sum->func_stat,       \
				    (_sum->func_stat_size             \
				     + VTSUM_STAT_BSIZE)              \
				    * sizeof(VTSum_funcStat));        \
    _sum->func_stat_size += VTSUM_STAT_BSIZE;                         \
  }                                                                   \
                                                                      \
  _stat = &(_sum->func_stat[_sum->func_stat_num++]);                  \
  _stat->rid  = _rid;                                                 \
  _stat->cnt  = 0;                                                    \
  _stat->excl = 0;                                                    \
  _stat->incl = 0;                                                    \
}

#define VTSUM_MSG_STAT_ADD(_sum, _peer, _cid, _tag, _stat)            \
{                                                                     \
  if (_sum->msg_stat_num == _sum->msg_stat_size)                      \
  {                                                                   \
    _sum->msg_stat = (VTSum_msgStat*)realloc(_sum->msg_stat,          \
				  (_sum->msg_stat_size                \
				   + VTSUM_STAT_BSIZE)                \
				  * sizeof(VTSum_msgStat));           \
    _sum->msg_stat_size += VTSUM_STAT_BSIZE;                          \
  }                                                                   \
                                                                      \
  _stat = &(_sum->msg_stat[_sum->msg_stat_num++]);                    \
  _stat->peer  = _peer;                                               \
  _stat->cid   = _cid;                                                \
  _stat->tag   = _tag;                                                \
  _stat->scnt  = 0;                                                   \
  _stat->rcnt  = 0;                                                   \
  _stat->sent  = 0;                                                   \
  _stat->recvd = 0;                                                   \
}

#define VTSUM_FOP_STAT_ADD(_sum, _fid, _stat)                         \
{                                                                     \
  if (_sum->fop_stat_num == _sum->fop_stat_size)                      \
  {                                                                   \
    _sum->fop_stat = (VTSum_fopStat*)realloc(_sum->fop_stat,          \
				    (_sum->fop_stat_size              \
				     + VTSUM_STAT_BSIZE)              \
				    * sizeof(VTSum_fopStat));         \
    _sum->fop_stat_size += VTSUM_STAT_BSIZE;                          \
  }                                                                   \
                                                                      \
  _stat = &(_sum->fop_stat[_sum->fop_stat_num++]);                    \
  _stat->fid    = _fid;                                               \
  _stat->nopen  = 0;                                                  \
  _stat->nclose = 0;                                                  \
  _stat->nread  = 0;                                                  \
  _stat->nwrite = 0;                                                  \
  _stat->nseek  = 0;                                                  \
  _stat->read   = 0;                                                  \
  _stat->wrote  = 0;                                                  \
}

#define VTSUM_STACK_PUSH(_sum, _stat, _time)                          \
{                                                                     \
  if (_sum->stack_pos+1 == (int32_t)_sum->stack_size)                 \
  {                                                                   \
    _sum->stack = (VTSum_stack*)realloc(_sum->stack,                  \
			       (_sum->stack_size                      \
				+ VTSUM_STACK_BSIZE)                  \
			       * sizeof(VTSum_stack));                \
    _sum->stack_size += VTSUM_STACK_BSIZE;                            \
  }                                                                   \
                                                                      \
  _sum->stack_pos++;                                                  \
  _sum->stack[_sum->stack_pos].stat = _stat;                          \
  _sum->stack[_sum->stack_pos].stat->cnt++;                           \
  _sum->stack[_sum->stack_pos].hexcl = *_time;                        \
  _sum->stack[_sum->stack_pos].hincl = *_time;                        \
                                                                      \
  if (_sum->stack_pos > 0)                                            \
  {                                                                   \
    _sum->stack[_sum->stack_pos-1].stat->excl +=                      \
       (*_time - _sum->stack[_sum->stack_pos-1].hexcl);               \
  }                                                                   \
}

#define VTSUM_STACK_POP(_sum, _time)                                  \
{                                                                     \
  if (_sum->stack_pos == -1)                                          \
    vt_error_msg("Abort: Stack underflow");                           \
                                                                      \
  _sum->stack_pos--;                                                  \
  _sum->stack[_sum->stack_pos+1].stat->excl +=                        \
     (*_time - _sum->stack[_sum->stack_pos+1].hexcl);                 \
  _sum->stack[_sum->stack_pos+1].stat->incl +=                        \
     (*_time - _sum->stack[_sum->stack_pos+1].hincl);                 \
  if (_sum->stack_pos != -1)                                          \
     _sum->stack[_sum->stack_pos].hexcl = *_time;                     \
}

#define VT_CHECK_DUMP(_sum, _time)                                    \
  if (*_time >= _sum->next_dump) VTSum_dump(_sum, 1);


/* Robert Jenkins' 96 bit mix function */
/* (http://www.cris.com/~Ttwang/tech/inthash.htm) */

#define RJ96MIX(a, b, c, h)                                           \
{                                                                     \
  a=a-b;  a=a-c;  a=a^(c >> 13);                                      \
  b=b-c;  b=b-a;  b=b^(a << 8);                                       \
  c=c-a;  c=c-b;  c=c^(b >> 13);                                      \
  a=a-b;  a=a-c;  a=a^(c >> 12);                                      \
  b=b-c;  b=b-a;  b=b^(a << 16);                                      \
  c=c-a;  c=c-b;  c=c^(b >> 5);                                       \
  a=a-b;  a=a-c;  a=a^(c >> 3);                                       \
  b=b-c;  b=b-a;  b=b^(a << 10);                                      \
  c=c-a;  c=c-b;  c=c^(b >> 15);                                      \
  h=c;                                                                \
}

/*
 *-----------------------------------------------------------------------------
 * VTSum
 *-----------------------------------------------------------------------------
 */

/* Data structure for function statistic */

typedef struct
{
  uint32_t         rid;
  uint64_t         cnt;
  uint64_t         excl;
  uint64_t         incl;
} VTSum_funcStat;

/* Data structure for message statistic */

typedef struct
{
  uint32_t         peer;
  uint32_t         cid;
  uint32_t         tag;
  uint64_t         scnt;
  uint64_t         rcnt;
  uint64_t         sent;
  uint64_t         recvd;
} VTSum_msgStat;

/* Data structure for file operation statistic */

typedef struct
{
  uint32_t         fid;
  uint64_t         nopen;
  uint64_t         nclose;
  uint64_t         nread;
  uint64_t         nwrite;
  uint64_t         nseek;
  uint64_t         read;
  uint64_t         wrote;
} VTSum_fopStat;

/* Data structure for call stack */

typedef struct
{
  uint64_t         hexcl;
  uint64_t         hincl;
  VTSum_funcStat*  stat;
} VTSum_stack;

/* VTSum record */

struct VTSum_struct
{
  VTGen*           gen;
  VTSum_stack*     stack;
  VTSum_funcStat*  func_stat;
  VTSum_msgStat*   msg_stat;
  VTSum_fopStat*   fop_stat;
  uint32_t         stack_size;
  int32_t          stack_pos;
  uint64_t         func_stat_size;
  uint64_t         func_stat_num;
  uint64_t         msg_stat_size;
  uint64_t         msg_stat_num;
  uint64_t         fop_stat_size;
  uint64_t         fop_stat_num;
  uint64_t         next_dump;
};

/* Summary interval */
static uint64_t SumIntv = 0;

/* Hash table to map function ids to statistic */

typedef struct HN_func {
  uint32_t id;            /* hash code (identifier of region) */
  VTSum_funcStat* stat;   /* associated statistic             */
  struct HN_func* next;
} HashNode_func;

/* Hash table to map message peer, comm, and tag to statistic */

typedef struct HN_msg {
  uint32_t id;              /* hash code (generated by RJ96MIX) */
  uint32_t peer, cid, tag;  /* peer, comm, tag of message       */
  VTSum_msgStat* stat;      /* associated statistic             */
  struct HN_msg* next;
} HashNode_msg;

/* Hash table to map file op. ids to statistic */

typedef struct HN_fop {
  uint32_t id;            /* hash code (identifier of file op.) */
  VTSum_fopStat* stat;    /* associated statistic               */
  struct HN_fop* next;
} HashNode_fop;

#define HASH_MAX 1021
static HashNode_func* htab_func[HASH_MAX];
static HashNode_msg*  htab_msg[HASH_MAX];
static HashNode_fop*  htab_fop[HASH_MAX];

/* Stores function statistic `stat' under hash code `h' */

static void hash_put_func(uint32_t h, VTSum_funcStat* stat) {
  uint32_t id = h % HASH_MAX;
  HashNode_func *add = (HashNode_func*)malloc(sizeof(HashNode_func));
  add->id = h;
  add->stat = stat;
  add->next = htab_func[id];
  htab_func[id] = add;
}

/* Lookup hash code `h'
 * Returns hash table entry if already stored, otherwise NULL */

static HashNode_func* hash_get_func(uint32_t h) {
  uint32_t id = h % HASH_MAX;
  HashNode_func *curr = htab_func[id];
  while ( curr ) {
    if ( curr->id == h ) {
      return curr;
    }
    curr = curr->next;
  }
  return NULL;
}

/* Stores message statistic `stat' under hash code input `peer',`cid',`tag' */

static void hash_put_msg(uint32_t peer, uint32_t cid, uint32_t tag,
			 VTSum_msgStat* stat)
{
  uint32_t id;
  uint32_t h;
  uint32_t a=peer, b=cid, c=tag;
  HashNode_msg *add = (HashNode_msg*)malloc(sizeof(HashNode_msg));
  RJ96MIX(a,b,c,h);
  id = h % HASH_MAX;
  add->id = h;
  add->peer = peer;
  add->cid  = cid;
  add->tag  = tag;
  add->stat = stat;
  add->next = htab_msg[id];
  htab_msg[id] = add;
}

/* Lookup hash code input `peer',`cid',`tag'
 * Returns hash table entry if already stored, otherwise NULL */

static HashNode_msg* hash_get_msg(uint32_t peer, uint32_t cid, uint32_t tag) {
  uint32_t h;
  uint32_t a=peer, b=cid, c=tag;
  HashNode_msg *curr;
  RJ96MIX(a,b,c,h);
  curr = htab_msg[h % HASH_MAX];
  while ( curr ) {
    if ( curr->id == h &&
	 curr->peer == peer &&
	 curr->cid == cid &&
	 curr->tag == tag ) {
       return curr;
    }
    curr = curr->next;
  }
  return NULL;
}

/* Stores file operation statistic `stat' under hash code `h' */

static void hash_put_fop(uint32_t h, VTSum_fopStat* stat) {
  uint32_t id = h % HASH_MAX;
  HashNode_fop *add = (HashNode_fop*)malloc(sizeof(HashNode_fop));
  add->id = h;
  add->stat = stat;
  add->next = htab_fop[id];
  htab_fop[id] = add;
}

/* Lookup hash code `h'
 * Returns hash table entry if already stored, otherwise NULL */

static HashNode_fop* hash_get_fop(uint32_t h) {
  uint32_t id = h % HASH_MAX;
  HashNode_fop *curr = htab_fop[id];
  while ( curr ) {
    if ( curr->id == h ) {
      return curr;
    }
    curr = curr->next;
  }
  return NULL;
}


VTSum* VTSum_open(VTGen* gen, uint32_t intv)
{
  VTSum* sum;

  /* allocate VTSum record */

  sum = (VTSum*)malloc(sizeof(VTSum));
  if (sum == NULL) 
    vt_error();

  /* set pointer to corresponding VTGen record */
  sum->gen = gen;

  /* initialize call stack */

  sum->stack = (VTSum_stack*)malloc(VTSUM_STACK_BSIZE * sizeof(VTSum_stack));
  if (sum->stack == NULL)
    vt_error();
  sum->stack_size = VTSUM_STACK_BSIZE;
  sum->stack_pos = -1;

  /* initialize function statistics */

  sum->func_stat = (VTSum_funcStat*)malloc(VTSUM_STAT_BSIZE
					   * sizeof(VTSum_funcStat));
  if (sum->func_stat == NULL)
    vt_error();
  sum->func_stat_size = VTSUM_STAT_BSIZE;
  sum->func_stat_num = 0;

  /* initialize message statistics */

  sum->msg_stat = (VTSum_msgStat*)malloc(VTSUM_STAT_BSIZE
					 * sizeof(VTSum_msgStat));
  if (sum->msg_stat == NULL)
    vt_error();
  sum->msg_stat_size = VTSUM_STAT_BSIZE;
  sum->msg_stat_num = 0;

  /* initialize file operation statistics */

  sum->fop_stat = (VTSum_fopStat*)malloc(VTSUM_STAT_BSIZE
					 * sizeof(VTSum_fopStat));
  if (sum->fop_stat == NULL)
    vt_error();
  sum->fop_stat_size = VTSUM_STAT_BSIZE;
  sum->fop_stat_num = 0;

  /* set summary interval */

  if (intv > 0)
  {
    SumIntv = (vt_pform_clockres() * intv) / 1000;
    sum->next_dump = SumIntv;
  }
  else
  {
    sum->next_dump = (uint64_t)-1;
  }

  /* return */
  return sum;
}

void VTSum_dump(VTSum* sum, uint8_t markDump)
{
  uint64_t time = vt_pform_wtime();
  uint32_t i;


  /* mark begin of statistics dump */
  if (markDump)
    vt_enter_stat(&time);

  /* dump function statistics */

  for(i=0; i < sum->func_stat_num; i++)
  {
    VTGen_write_FUNCTION_SUMMARY(sum->gen, &time,
      sum->func_stat[i].rid,
      sum->func_stat[i].cnt,
      sum->func_stat[i].excl,
      sum->func_stat[i].incl);
  }

  /* dump message statistics */

  for(i=0; i < sum->msg_stat_num; i++)
  {
    VTGen_write_MESSAGE_SUMMARY(sum->gen, &time,
      sum->msg_stat[i].peer,
      sum->msg_stat[i].cid,
      sum->msg_stat[i].tag,
      sum->msg_stat[i].scnt,
      sum->msg_stat[i].rcnt,
      sum->msg_stat[i].sent,
      sum->msg_stat[i].recvd);
  }

  /* dump file operation statistics */

  for(i=0; i < sum->fop_stat_num; i++)
  {
    VTGen_write_FILE_OPERATION_SUMMARY(sum->gen, &time,
      sum->fop_stat[i].fid,
      sum->fop_stat[i].nopen,
      sum->fop_stat[i].nclose,
      sum->fop_stat[i].nread,
      sum->fop_stat[i].nwrite,
      sum->fop_stat[i].nseek,
      sum->fop_stat[i].read,
      sum->fop_stat[i].wrote);
  }

  time = vt_pform_wtime();

  /* mark end of statistics dump */
  if (markDump)
    vt_exit_stat(&time);

  if (sum->next_dump != (uint64_t)-1)
    sum->next_dump = time + SumIntv;
}

void VTSum_close(VTSum* sum)
{
  /* dump summaries */
  VTSum_dump(sum, 0);

  /* free call stack */
  free(sum->stack);

  /* free function statistics */
  free(sum->func_stat);

  /* free message statistics */
  free(sum->msg_stat);

  /* free file operation statistics */
  free(sum->fop_stat);
}


/* -- Region -- */

void VTSum_enter(VTSum* sum, uint64_t* time, uint32_t rid)
{
  HashNode_func* hn;
  VTSum_funcStat* stat = NULL;

  VTSUM_CHECK(sum);

  if ( (hn = hash_get_func(rid)) )
  {
    stat = hn->stat;
  }
  else
  {
    VTSUM_FUNC_STAT_ADD(sum, rid, stat);
    hash_put_func(rid, stat);
  }

  VTSUM_STACK_PUSH(sum, stat, time);

  VT_CHECK_DUMP(sum, time);
}

void VTSum_exit(VTSum* sum, uint64_t* time, uint32_t rid)
{
  VTSUM_CHECK(sum);

  VTSUM_STACK_POP(sum, time);

  VT_CHECK_DUMP(sum, time);
}


/* -- Message -- */

#define VTSum_mpi(_sum, _time, _peer, _cid, _tag, _stat)    \
{                                                           \
  HashNode_msg* hn;                                         \
                                                            \
  if ( (hn = hash_get_msg(_peer, _cid, _tag)) )             \
  {                                                         \
    _stat = hn->stat;                                       \
  }                                                         \
  else                                                      \
  {                                                         \
    VTSUM_MSG_STAT_ADD(_sum, _peer, _cid, _tag, _stat);     \
    hash_put_msg(_peer, _cid, _tag, _stat);                 \
  }                                                         \
}

void VTSum_mpi_send(VTSum* sum, uint64_t* time, uint32_t dpid, uint32_t cid,
		    uint32_t tag, uint64_t sent)
{
  VTSum_msgStat* stat;

  VTSUM_CHECK(sum);

  VTSum_mpi(sum, time, dpid, cid, tag, stat);
  
  stat->scnt++;
  stat->sent += sent;
  
  VT_CHECK_DUMP(sum, time);
}

void VTSum_mpi_recv(VTSum* sum, uint64_t* time, uint32_t spid, uint32_t cid,
		    uint32_t tag, uint64_t recvd)
{
  VTSum_msgStat* stat;

  VTSUM_CHECK(sum);
  
  VTSum_mpi(sum, time, spid, cid, tag, stat);

  stat->rcnt++;
  stat->recvd += recvd;
  
  VT_CHECK_DUMP(sum, time);
}


/* -- File I/O -- */

#define VTSum_fop(_sum, _time, _fid, _stat)                 \
{                                                           \
  HashNode_fop* hn;                                         \
                                                            \
  if ( (hn = hash_get_fop(_fid)) )                          \
  {                                                         \
    _stat = hn->stat;                                       \
  }                                                         \
  else                                                      \
  {                                                         \
    VTSUM_FOP_STAT_ADD(_sum, _fid, _stat);                  \
    hash_put_fop(_fid, _stat);                              \
  }                                                         \
}

void VTSum_fop_open(VTSum* sum, uint64_t* time, uint32_t fid)
{
  VTSum_fopStat* stat = NULL;

  VTSUM_CHECK(sum);

  VTSum_fop(sum, time, fid, stat);

  stat->nopen++;

  VT_CHECK_DUMP(sum, time);
}

void VTSum_fop_close(VTSum* sum, uint64_t* time, uint32_t fid)
{
  VTSum_fopStat* stat = NULL;

  VTSUM_CHECK(sum);

  VTSum_fop(sum, time, fid, stat);

  stat->nclose++;

  VT_CHECK_DUMP(sum, time);
}

void VTSum_fop_read(VTSum* sum, uint64_t* time, uint32_t fid, uint64_t read)
{
  VTSum_fopStat* stat = NULL;

  VTSUM_CHECK(sum);

  VTSum_fop(sum, time, fid, stat);

  stat->nread++;
  stat->read += read;

  VT_CHECK_DUMP(sum, time);
}

void VTSum_fop_write(VTSum* sum, uint64_t* time, uint32_t fid, uint64_t wrote)
{
  VTSum_fopStat* stat = NULL;

  VTSUM_CHECK(sum);

  VTSum_fop(sum, time, fid, stat);

  stat->nwrite++;
  stat->wrote += wrote;

  VT_CHECK_DUMP(sum, time);
}

void VTSum_fop_seek(VTSum* sum, uint64_t* time, uint32_t fid)
{
  VTSum_fopStat* stat = NULL;

  VTSUM_CHECK(sum);

  VTSum_fop(sum, time, fid, stat);

  stat->nseek++;

  VT_CHECK_DUMP(sum, time);
}
