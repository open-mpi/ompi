#ifndef _RFG_STRMKRS_H
#define _RFG_STRMKRS_H

/* some macros for string manipulation like perl
 */

/* macro to remove newline character from string */

#define chomp(str)                                                    \
{                                                                     \
  if( str[strlen(str)-1] == '\n' )                                    \
    str[strlen(str)-1] = '\0';                                        \
}

/* macro to strip whitespace from string */

#define trim(str)                                                     \
{                                                                     \
  int _trim_start_idx_ = 0;                                           \
  int _trim_stop_idx_ = strlen( str );                                \
  int i, j;                                                           \
                                                                      \
  if( strlen( str ) > 0 )                                             \
  {                                                                   \
    for( i = 0; i < (int)strlen( str )                                \
         && str[i] == ' '; i++ ) _trim_start_idx_++;                  \
                                                                      \
    for( i = strlen( str ) - 1; i >= 0                                \
         && str[i] == ' '; i-- ) _trim_stop_idx_--;                   \
                                                                      \
    for( j = 0, i = _trim_start_idx_; i < _trim_stop_idx_; i++, j++ ) \
      str[j] = str[i];                                                \
    str[j] = '\0';                                                    \
  }                                                                   \
}
#endif
