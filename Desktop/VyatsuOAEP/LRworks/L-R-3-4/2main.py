def get_room_areas(num_rooms):

    total_area = 0
    for i in range(1, num_rooms + 1):
        print(f"Комната {i}:")
        length = float(input("  Введите длину: "))
        width = float(input("  Введите ширину: "))
        area = length * width
        print(f"  Площадь комнаты: {area:.2f} кв.м")
        total_area += area
    return total_area

num_rooms = int(input("Введите количество комнат в доме: "))
total_area = get_room_areas(num_rooms)
print(f"Суммарная площадь комнат в доме: {total_area:.2f} кв.м")


