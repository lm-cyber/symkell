import ctypes
import os
import sys
import platform
from pathlib import Path
from typing import Dict, Union, List, Tuple, Optional

def _find_lib() -> str:
    """Find the Haskell shared library."""
    # Determine library extension based on platform
    if platform.system() == "Darwin":
        ext = "dylib"
    else:
        ext = "so"
    
    # Look for the library in the same directory as this file
    lib_name = f"libsymkell.{ext}"
    search_paths = [
        os.path.dirname(os.path.abspath(__file__)),  # Current directory
        os.path.abspath(os.path.join(os.path.dirname(__file__), '..')),  # Parent directory
    ]
    
    for path in search_paths:
        lib_path = os.path.join(path, lib_name)
        if os.path.exists(lib_path):
            return lib_path
    
    raise FileNotFoundError(f"Could not find {lib_name} in {', '.join(search_paths)}")

class Expression:
    """Represents a symbolic mathematical expression."""
    
    def __init__(self, expr_str: str):
        """Initialize with an expression string."""
        self.expr_str = expr_str
    
    @classmethod
    def from_int(cls, value: int) -> 'Expression':
        """Create an expression from an integer."""
        return cls(str(value))
    
    @classmethod
    def symbol(cls, name: str) -> 'Expression':
        """Create a symbolic variable expression."""
        return cls(name)
    
    def __str__(self) -> str:
        return self.expr_str

# Find and load the Haskell library
_lib_path = _find_lib()
_lib = ctypes.CDLL(_lib_path)

# Initialize Haskell runtime
_lib.hs_init_c.argtypes = []
_lib.hs_init_c.restype = None
_lib.hs_init_c()

# Set up the function definitions
_lib.differentiate_c.argtypes = [ctypes.c_char_p, ctypes.c_char_p]
_lib.differentiate_c.restype = ctypes.c_char_p

_lib.evaluate_c.argtypes = [ctypes.c_char_p, ctypes.c_char_p, ctypes.c_double]
_lib.evaluate_c.restype = ctypes.c_double

# New function definitions
_lib.integrate_c.argtypes = [ctypes.c_char_p, ctypes.c_char_p]
_lib.integrate_c.restype = ctypes.c_char_p

_lib.simplify_c.argtypes = [ctypes.c_char_p]
_lib.simplify_c.restype = ctypes.c_char_p

_lib.limit_c.argtypes = [ctypes.c_char_p, ctypes.c_char_p, ctypes.c_char_p]
_lib.limit_c.restype = ctypes.c_char_p

_lib.taylor_series_c.argtypes = [ctypes.c_char_p, ctypes.c_char_p, ctypes.c_int, ctypes.c_int]
_lib.taylor_series_c.restype = ctypes.c_char_p

_lib.laurent_series_c.argtypes = [ctypes.c_char_p, ctypes.c_char_p, ctypes.c_int, ctypes.c_int, ctypes.c_int]
_lib.laurent_series_c.restype = ctypes.c_char_p

# Cleanup function
_lib.hs_exit_c.argtypes = []
_lib.hs_exit_c.restype = None

# Register cleanup at exit
import atexit
atexit.register(_lib.hs_exit_c)

def _to_expr_string(expr) -> str:
    """Convert various Python types to expression strings."""
    if isinstance(expr, Expression):
        return expr.expr_str
    elif isinstance(expr, str):
        return expr
    elif isinstance(expr, int):
        return str(expr)
    else:
        raise TypeError(f"Unsupported type: {type(expr)}")

def differentiate(expr, var: str = "x") -> Expression:
    """Differentiate an expression with respect to a variable."""
    expr_str = _to_expr_string(expr)
    var_bytes = var.encode('utf-8')
    expr_bytes = expr_str.encode('utf-8')
    
    result_ptr = _lib.differentiate_c(var_bytes, expr_bytes)
    result = ctypes.string_at(result_ptr).decode('utf-8')
    
    # Free the C string allocated by Haskell
    libc = ctypes.CDLL(None)
    libc.free(result_ptr)
    
    return Expression(result)

def evaluate(expr, var: str = "x", value: float = 0.0) -> float:
    """Evaluate an expression by substituting a value for a variable."""
    expr_str = _to_expr_string(expr)
    var_bytes = var.encode('utf-8')
    expr_bytes = expr_str.encode('utf-8')
    
    result = _lib.evaluate_c(var_bytes, expr_bytes, ctypes.c_double(value))
    return result 

def integrate(expr, var: str = "x") -> Optional[Expression]:
    """Integrate an expression with respect to a variable."""
    expr_str = _to_expr_string(expr)
    var_bytes = var.encode('utf-8')
    expr_bytes = expr_str.encode('utf-8')
    
    result_ptr = _lib.integrate_c(var_bytes, expr_bytes)
    if not result_ptr:
        return None
        
    result = ctypes.string_at(result_ptr).decode('utf-8')
    
    # Free the C string allocated by Haskell
    libc = ctypes.CDLL(None)
    libc.free(result_ptr)
    
    return Expression(result)

def simplify(expr) -> Expression:
    """Simplify an expression."""
    expr_str = _to_expr_string(expr)
    expr_bytes = expr_str.encode('utf-8')
    
    result_ptr = _lib.simplify_c(expr_bytes)
    result = ctypes.string_at(result_ptr).decode('utf-8')
    
    # Free the C string allocated by Haskell
    libc = ctypes.CDLL(None)
    libc.free(result_ptr)
    
    return Expression(result)

def limit(expr, var: str = "x", target = 0) -> Optional[Expression]:
    """Compute the limit of an expression as a variable approaches a target value."""
    expr_str = _to_expr_string(expr)
    var_bytes = var.encode('utf-8')
    expr_bytes = expr_str.encode('utf-8')
    target_str = _to_expr_string(target)
    target_bytes = target_str.encode('utf-8')
    
    result_ptr = _lib.limit_c(var_bytes, target_bytes, expr_bytes)
    if not result_ptr:
        return None
        
    result = ctypes.string_at(result_ptr).decode('utf-8')
    
    # Free the C string allocated by Haskell
    libc = ctypes.CDLL(None)
    libc.free(result_ptr)
    
    return Expression(result)

def taylor_series(expr, var: str = "x", point: int = 0, order: int = 3) -> Expression:
    """Compute the Taylor series expansion of an expression around a point to a given order."""
    expr_str = _to_expr_string(expr)
    var_bytes = var.encode('utf-8')
    expr_bytes = expr_str.encode('utf-8')
    
    result_ptr = _lib.taylor_series_c(expr_bytes, var_bytes, ctypes.c_int(point), ctypes.c_int(order))
    result = ctypes.string_at(result_ptr).decode('utf-8')
    
    # Free the C string allocated by Haskell
    libc = ctypes.CDLL(None)
    libc.free(result_ptr)
    
    return Expression(result)

def laurent_series(expr, var: str = "x", point: int = 0, pos_terms: int = 3, neg_terms: int = 1) -> Expression:
    """Compute the Laurent series expansion of an expression."""
    expr_str = _to_expr_string(expr)
    var_bytes = var.encode('utf-8')
    expr_bytes = expr_str.encode('utf-8')
    
    result_ptr = _lib.laurent_series_c(expr_bytes, var_bytes, ctypes.c_int(point), 
                                      ctypes.c_int(pos_terms), ctypes.c_int(neg_terms))
    result = ctypes.string_at(result_ptr).decode('utf-8')
    
    # Free the C string allocated by Haskell
    libc = ctypes.CDLL(None)
    libc.free(result_ptr)
    
    return Expression(result) 