/*
 * $HEADER$
 */

#ifndef LAM_RUNTIME_H
#define LAM_RUNTIME_H

#ifdef __cplusplus
extern "C" {
#endif

  int lam_init(int argc, char* argv[]);
  int lam_finalize(void);

#ifdef __cplusplus
}
#endif

#endif /* LAM_RUNTIME_H */
