
def get_cell_color(coord):
    column = ord(coord[0].upper()) - ord('A')
    row = int(coord[1]) - 1

    if (column + row) % 2 == 0:
        return "WHITE"
    else:
        return "BLACK"

cell_coord = input("Введите координату клетки (например, 'a1'): ")

print(get_cell_color(cell_coord))
