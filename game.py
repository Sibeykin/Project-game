import random, os

choices = ["камень", "ножницы", "бумага"]
scores = {"игрок": 0, "бот": 0, "ничья": 0}

def clear(): os.system('cls' if os.name == 'nt' else 'clear')

def get_choice():
    while True:
        choice = input("Выбери: камень, ножницы, бумага (или 'выход'): ").lower().strip()
        if choice == "выход" or choice in choices: return choice
        print("Ошибка: выберите только 'камень', 'ножницы', 'бумагу' или 'выход'!")

def play_game():
    while True:
        clear()
        print(f"\nСчет: Игрок: {scores['игрок']} | Бот: {scores['бот']} | Ничьи: {scores['ничья']}\n")
        bot = random.choice(choices)
        user = get_choice()
        
        if user == "выход":
            print(f"\nИтог: Игрок: {scores['игрок']} | Бот: {scores['бот']} | Ничьи: {scores['ничья']}")
            break
            
        print(f"\nБот выбрал: {bot}")
        if user == bot:
            print("Ничья!")
            scores["ничья"] += 1
        elif (user == "камень" and bot == "ножницы") or (user == "ножницы" and bot == "бумага") or (user == "бумага" and bot == "камень"):
            print("Вы победили!")
            scores["игрок"] += 1
        else:
            print("Вы проиграли!")
            scores["бот"] += 1
        input("\nEnter для продолжения...")

def main():
    while True:
        clear()
        print("1. Начать игру\n2. Выход")
        if input("Выбор (1-2): ").strip() == "1": play_game()
        else: break