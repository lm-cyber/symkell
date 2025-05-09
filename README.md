# Symkell

Symkell is a symbolic mathematics library written in Haskell with Python bindings. It provides powerful tools for symbolic computation, including differentiation, integration, limits, polynomial operations, and more.

## Overview

Symkell enables symbolic mathematical operations through a well-structured Haskell core with FFI bindings to make the functionality accessible from Python. The project is designed for researchers, educators, and developers working with symbolic mathematics.

## TODO

- Fix FFI implementation for better interoperability with Python
- Refactor codebase to improve maintainability and performance
- Fix and enhance limits computation algorithm
- Improve Series expansion functionality
- Implement CI/CD pipeline for automated testing and deployment

## Project Structure

- **symkell_core**: The Haskell core library implementing symbolic mathematics operations
- **ffi_export_lib**: FFI (Foreign Function Interface) library for exporting Haskell functions
- **pysymkell**: Python bindings for accessing Symkell functionality from Python

## Features

- Symbolic expression representation and manipulation
- Differentiation of expressions
- Integration (with support for various techniques)
- Symbolic limits
- Series expansions
- Polynomial operations (including rational functions)
- Expression simplification
- LaTeX conversion for displaying expressions
- Haskell expression generation

## Algorithms

### Symbolic Integration

Symkell implements a sophisticated symbolic integration algorithm:

1. **Preprocessing & Simplification**
   - Numerical evaluation of constants
   - Symbolic simplification and canonicalization
   - Term collection and ordering

2. **Basic Integration Methods**
   - Table lookup for known integrals
   - Linearity application for sums/differences

3. **Advanced Techniques**
   - Integration by substitution (u-substitution)
   - Integration by parts
   - Specialized handling for rational functions
   - Trigonometric, exponential, and logarithmic integration

4. **Structural Methods**
   - Partial implementation of Risch algorithm for determining elementary antiderivatives
   - Special function handling for non-elementary integrals

### Symbolic Limits

Symkell uses a multi-step approach for computing limits:

1. **Preprocessing**
   - Handling limit direction and infinite limits
   - Resolving trivial cases

2. **Core Algorithms**
   - Leading term analysis
   - Series expansion techniques (similar to Gruntz algorithm)
   - L'Hôpital's rule for indeterminate forms

3. **Recursive Methods**
   - Term-wise limit computation when possible
   - Algebraic simplifications for resolving indeterminacies


## Installation

### Prerequisites

- GHC (Glasgow Haskell Compiler)
- Stack (Haskell build tool)
- Python 3.6+

### Building from Source

1. Clone the repository:
   ```
   git clone https://github.com/yourusername/symkell.git
   cd symkell
   ```

2. Build the Haskell core and FFI libraries:
   ```
   cd symkell
   stack build
   ```

3. Build and install the Python package:
   ```
   cd ../pysymkell
   pip install -e .
   ```

## Usage

### Haskell API

```haskell
import Symkell

-- Create a symbolic expression
-- Perform differentiation
expr = ...
diff = differentiate "x" expr

-- Perform integration
integral = integrate "x" expr

-- Convert to LaTeX
latex = toLaTeX expr

-- Simplify an expression
simplified = simplify expr
```

### Python API

```python
import pysymkell as sk

# Create symbolic expressions
# Perform operations on them
# Output results
```

## Development

This project is built using Stack with GHC and uses standard Python package development tools.

## License

This project is licensed under the BSD 3-Clause License - see the LICENSE file for details. 