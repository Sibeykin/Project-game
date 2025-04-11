import random, os

choices = ["камень", "ножницы", "бумага"]
scores = {"игрок1": 0, "игрок2": 0, "бот": 0}

def clear():
    if os.name == 'nt':
        os.system('cls')
    else:
        os.system('clear')
    print("\n" * 2)

def get_choice(player_name=""):
    while True:
        choice = input(f"{player_name}Выбери: камень, ножницы, бумага (или 'выход'): ").lower().strip()
        if choice == "выход" or choice in choices: return choice
        print("Ошибка: выберите только 'камень', 'ножницы', 'бумагу' или 'выход'!")

def play_game():
    while True:
        clear()
        print(f"Счет: Игрок: {scores['игрок1']} | Бот: {scores['бот']}\n")
        bot = random.choice(choices)
        user = get_choice()
        
        if user == "выход":
            print(f"\nИтог: Игрок: {scores['игрок1']} | Бот: {scores['бот']}")
            break
            
        print(f"\nБот выбрал: {bot}")
        if user == bot:
            print("Ничья!")
        elif (user == "камень" and bot == "ножницы") or (user == "ножницы" and bot == "бумага") or (user == "бумага" and bot == "камень"):
            print("Вы победили!")
            scores["игрок1"] += 1
        else:
            print("Вы проиграли!")
            scores["бот"] += 1
        input()

def play_two_players():
    while True:
        clear()
        print(f"Счет: Игрок 1: {scores['игрок1']} | Игрок 2: {scores['игрок2']}\n")
        
        player1 = get_choice("Игрок 1: ")
        if player1 == "выход":
            print(f"\nИтог: Игрок 1: {scores['игрок1']} | Игрок 2: {scores['игрок2']}")
            break
            
        player2 = get_choice("Игрок 2: ")
        if player2 == "выход":
            print(f"\nИтог: Игрок 1: {scores['игрок1']} | Игрок 2: {scores['игрок2']}")
            break
            
        if player1 == player2:
            print("Ничья!")
        elif (player1 == "камень" and player2 == "ножницы") or \
             (player1 == "ножницы" and player2 == "бумага") or \
             (player1 == "бумага" and player2 == "камень"):
            print("Игрок 1 победил!")
            scores["игрок1"] += 1
        else:
            print("Игрок 2 победил!")
            scores["игрок2"] += 1
        input()

def main():
    print("Добро пожаловать в игру 'Камень, ножницы, бумага'!")
    while True:
        clear()
        print("1. Игра против бота")
        print("2. Игра для двоих игроков")
        print("3. Выход")
        choice = input("Выбор (1-3): ").strip()
        if choice == "1": 
            play_game()
        elif choice == "2":
            play_two_players()
        else: 
            print("\nДо свидания!")
            break