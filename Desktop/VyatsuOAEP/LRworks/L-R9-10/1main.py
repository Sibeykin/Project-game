def find_elements(arr):
    max_element = None
    min_absolute_element = None
    max_negative_element = None

    for i, num in enumerate(arr):
        if max_element is None or num > max_element:
            max_element = num
            max_index = i  

        if min_absolute_element is None or abs(num) < abs(min_absolute_element):
            min_absolute_element = num
        
        if num < 0 and (max_negative_element is None or num > max_negative_element):
            max_negative_element = num

    return {
        "max_element": max_element,
        "max_index": max_index,
        "min_absolute_element": min_absolute_element,
        "max_negative_element": max_negative_element
    }

array = [3, -5, 1, 2, -4, 100, 0]
results = find_elements(array)

print("Максимальный элемент:", results["max_element"], "Индекс:", results["max_index"])
print("Минимальный по модулю элемент:", results["min_absolute_element"])
print("Максимальный отрицательный элемент:", results["max_negative_element"])
