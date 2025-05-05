# PySymkell

A Python wrapper for the Symkell symbolic mathematics library.

## Building the Shared Library

Before using the Python wrapper, you need to compile the Haskell library into a shared library. Here's how:

### Prerequisites

- GHC (Glasgow Haskell Compiler)
- Cabal or Stack (Haskell build tools)
- Python 3.6+

### Compiling the Shared Library

The easiest way to compile the shared library is to use the provided script:

```bash
# From the project root
./compile_extended.sh
```

This will compile the enhanced version of the library with support for all functions.

Alternatively, you can compile manually:

```bash
# Using GHC directly
ghc -O2 -dynamic -shared -fPIC -o libsymkell.so symkell-ffi-extended.hs \
    -package text

# For minimal version with fewer dependencies
ghc -O2 -dynamic -shared -fPIC -o libsymkell.so symkell-ffi-minimal.hs \
    -package text
```

This will generate a `libsymkell.so` file (on Linux, or `.dylib` on macOS, `.dll` on Windows).

## Installation

Once you've built the shared library, you can install the Python package:

```bash
# From the symkell project root
cp libsymkell.so pysymkell/
cd pysymkell
pip install -e .
```

Or simply make sure the `libsymkell.so` file is in a location where Python can find it (current directory, system library path, etc.).

## Usage

```python
from symkell import (
    Expression,
    differentiate, 
    integrate,
    simplify,
    limit,
    taylor_series,
    laurent_series,
    evaluate
)

# Create an expression
expr = Expression("x :*: x :+: Number 2 :*: x :+: Number 1")  # x^2 + 2x + 1

# Differentiate with respect to x
derivative = differentiate(expr, "x")
print(f"Derivative: {derivative}")

# Integrate with respect to x
integral = integrate(expr, "x")
if integral:
    print(f"Integral: {integral}")

# Simplify an expression
simplified = simplify(expr)
print(f"Simplified: {simplified}")

# Compute a limit
lim = limit(expr, "x", 1)  # limit as x approaches 1
if lim:
    print(f"Limit: {lim}")

# Compute Taylor series
series = taylor_series(expr, "x", 0, 3)  # Around x=0, order 3
print(f"Taylor series: {series}")

# Evaluate at a point
value = evaluate(expr, "x", 2)  # Evaluate at x=2
print(f"Value at x=2: {value}")
```

See `example.py` for a more complete example.

## API Reference

- `differentiate(expr, variable)`: Differentiate an expression with respect to a variable
- `integrate(expr, variable)`: Integrate an expression with respect to a variable
- `simplify(expr)`: Simplify an expression
- `limit(expr, variable, target)`: Compute the limit of an expression as a variable approaches a target
- `taylor_series(expr, variable, point, order)`: Compute the Taylor series expansion
- `laurent_series(expr, variable, point, pos_terms, neg_terms)`: Compute the Laurent series expansion
- `evaluate(expr, variable, value)`: Evaluate an expression by substituting a value for a variable

## Memory Management

The wrapper handles memory management internally. CStrings returned by the Haskell library are automatically converted to Python strings.

## Error Handling

Functions that can fail (like `integrate` and `limit`) return `None` if the operation cannot be performed. Other functions raise exceptions with descriptive error messages.

## Troubleshooting

If you get an error about not being able to load the library, check that:
1. The shared library (`libsymkell.so`, `.dylib`, or `.dll`) is built correctly
2. The library is in a location where Python can find it
3. The library dependencies are satisfied (you may need to set `LD_LIBRARY_PATH` on Linux or `DYLD_LIBRARY_PATH` on macOS)