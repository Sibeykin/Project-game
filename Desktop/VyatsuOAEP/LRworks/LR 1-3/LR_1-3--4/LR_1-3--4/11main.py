
n = int(input("Введите число n (n<100): "))

if n % 10 == 1 and n % 100 != 11:
    ending = "korova"
elif 2 <= n % 10 <= 4 and (n % 100 < 10 or n % 100 >= 20):
    ending = "korovy"
else:
    ending = "korov"

print(f"На лугу пасется {n} {ending}")

