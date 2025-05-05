#!/usr/bin/env python3

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

def main():
    # Create an expression (x^2 + 2x)
    expr_str = "(x ^ 2) + (2 * x)"
    expr = Expression(expr_str)
    
    print(f"Original expression: {expr}")
    
    # Differentiate the expression with respect to x
    # Result should be 2x + 2
    derivative = differentiate(expr, "x")
    print(f"Derivative: {derivative}")
    
    # Integrate the expression with respect to x
    # Result should be x^3/3 + x^2
    integral = integrate(expr, "x")
    if integral:
        print(f"Integral: {integral}")
    else:
        print("Could not compute integral")
    
    # Simplify an expression
    complex_expr = Expression("(0 + x) * (1 * y)")
    simplified = simplify(complex_expr)
    print(f"Simplified expression: {simplified}")
    
    # Calculate a limit
    lim_expr = Expression("(x ^ 2) - 4")
    lim_result = limit(lim_expr, "x", 2)
    if lim_result:
        print(f"Limit of {lim_expr} as x approaches 2: {lim_result}")
    else:
        print(f"Could not compute limit")
    
    # Calculate a Taylor series
    ts_result = taylor_series(expr, "x", 0, 3)
    print(f"Taylor series expansion around x=0 (order 3): {ts_result}")
    
    # Calculate a Laurent series
    ls_result = laurent_series(expr, "x", 0, 3, 1)
    print(f"Laurent series expansion around x=0: {ls_result}")
    
    # Evaluate the original expression at x = 2
    # Should be 8 (2^2 + 2*2)
    x_value = 2.0
    orig_result = evaluate(expr, "x", x_value)
    print(f"f({x_value}) = {orig_result}")
    
    # Evaluate the derivative at x = 2
    # Should be 6 (2*2 + 2)
    deriv_result = evaluate(derivative, "x", x_value)
    print(f"f'({x_value}) = {deriv_result}")

if __name__ == "__main__":
    main() 