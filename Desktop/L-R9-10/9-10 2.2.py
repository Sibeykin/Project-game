import random

def create_2d_array(rows, cols):
    return [[random.randint(-10, 10) for _ in range(cols)] for _ in range(rows)]

def check_rows_for_negatives(array):
    result = []
    for row in array:
        if all(num >= 0 for num in row):
            result.append(1)  
        else:
            result.append(-1) 
    return result

def main():
    rows, cols = 8, 6
    array_2d = create_2d_array(rows, cols)
    
    print("Двумерный массив (8x6):")
    for row in array_2d:
        print(row)
    
    array_1d = check_rows_for_negatives(array_2d)
    
    print("\nОдномерный массив:")
    print(array_1d)

if __name__ == "__main__":
    main()