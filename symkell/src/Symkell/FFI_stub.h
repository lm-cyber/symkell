#include <HsFFI.h>
#if defined(__cplusplus)
extern "C" {
#endif
extern HsPtr symkell_differentiate(HsPtr a1, HsPtr a2);
extern HsPtr symkell_integrate(HsPtr a1, HsPtr a2);
extern HsPtr symkell_simplify(HsPtr a1);
extern HsPtr symkell_limit(HsPtr a1, HsPtr a2, HsPtr a3);
extern HsPtr symkell_taylor_series(HsPtr a1, HsPtr a2, HsInt32 a3, HsInt32 a4);
extern HsPtr symkell_laurent_series(HsPtr a1, HsPtr a2, HsInt32 a3, HsInt32 a4, HsInt32 a5);
extern HsDouble symkell_evaluate(HsPtr a1, HsPtr a2, HsPtr a3);
extern void hs_init_symkell(void);
extern void hs_exit_symkell(void);
#if defined(__cplusplus)
}
#endif

