
num = int(input("Введите трехзначное число: "))

a = num // 100
b = (num // 10) % 10
c = num % 10

reverse_num = c * 100 + b * 10 + a

difference = num - reverse_num

print("Разность чисел", difference)
