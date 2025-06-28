def function_table(a, b, h):
    
    print("x      y")
    print("-----------------")
    
    x = a
    while x <= b:
        y = 3 * x**2 + 5 * x / 13
        print(f"{x:6.2f}   {y:8.2f}")
        x += h

function_table(1.0, 5.0, 0.5)

