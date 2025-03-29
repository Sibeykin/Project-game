import random

whole_choice = ["камень", "ножницы", "бумага"]  
random_number = random.choice(whole_choice)  
user_number = input("Выбери: камень, ножницы, бумага: ")

if user_number not in whole_choice:
    print("Ошибка: выберите только 'камень', 'ножницы' или 'бумагу'!")
else:
    print(f"Бот выбрал: {random_number}")

    if user_number == random_number:
        print("Ничья!")
    elif (user_number == "камень" and random_number == "ножницы"):
         (user_number == "ножницы" and random_number == "бумага") 
         (user_number == "бумага" and random_number == "камень")
         print("Вы победили!")
    else:
        print("Вы проиграли!")
