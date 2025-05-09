#ifndef FFI_EXPORT_H
#define FFI_EXPORT_H

#ifdef __cplusplus
extern "C" {
#endif

/* Add two integers */
extern int add_ints(int x, int y);

/* Subtract the second integer from the first */
extern int subtract_ints(int x, int y);

/* Multiply two integers */
extern int multiply_ints(int x, int y);

/* Divide the first float by the second */
extern float divide_floats(float x, float y);

/* Calculate the factorial of an integer */
extern int factorial_int(int x);

#ifdef __cplusplus
}
#endif

#endif /* FFI_EXPORT_H */ 