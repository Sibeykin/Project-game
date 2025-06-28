
apple_price = 5
pear_price = 7
orange_price = 8

x1 = int(input("Введите количество проданных яблок в понедельник: "))
y1 = int(input("Введите количество проданных груш в понедельник: "))
z1 = int(input("Введите количество проданных апельсинов в понедельник: "))

x2 = int(input("Введите количество проданных яблок во вторник: "))
y2 = int(input("Введите количество проданных груш во вторник: "))
z2 = int(input("Введите количество проданных апельсинов во вторник: "))

monday_revenue = x1 * apple_price + y1 * pear_price + z1 * orange_price

tuesday_revenue = x2 * apple_price + y2 * pear_price + z2 * orange_price

total_revenue = monday_revenue + tuesday_revenue

print(f"Выручка в понедельник: {monday_revenue} рублей")
print(f"Выручка во вторник: {tuesday_revenue} рублей")
print(f"Общая выручка за два дня: {total_revenue} рублей")
