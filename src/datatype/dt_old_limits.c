#include "datatype.h"

int dt_type_ub( dt_desc_t* pData, long* disp )
{
   *disp = pData->ub;
   return 0;
}

int dt_type_lb( dt_desc_t* pData, long* disp )
{
   *disp = pData->lb;
   return 0;
}

int dt_type_extent( dt_desc_t* pData, long* extent )
{
   *extent = pData->ub - pData->lb;
   return 0;
}

int dt_type_size ( dt_desc_t* pData, int *size )
{
   *size = pData->size;
   return 0;
}

int dt_type_resize( dt_desc_t* pOld, long lb, long extent, dt_desc_t** pNew )
{
   return 0;
}

int dt_get_extent( dt_desc_t* datatype, long* lb, long* extent)
{
   dt_desc_t* pData = (dt_desc_t*)datatype;

   *lb = pData->lb;
   *extent = pData->ub - pData->lb;
   return 0;
}

int dt_get_true_extent( dt_desc_t* datatype, long* true_lb, long* true_extent)
{
   dt_desc_t* pData = (dt_desc_t*)datatype;

   *true_lb = pData->true_lb;
   *true_extent = pData->true_ub - pData->true_lb;
   return 0;
}
