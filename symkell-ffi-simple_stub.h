#include <HsFFI.h>
#if defined(__cplusplus)
extern "C" {
#endif
extern HsPtr symkell_differentiate(HsPtr a1, HsPtr a2);
extern HsDouble symkell_evaluate(HsPtr a1, HsPtr a2, HsPtr a3);
extern void hs_init_symkell(void);
extern void hs_exit_symkell(void);
#if defined(__cplusplus)
}
#endif

