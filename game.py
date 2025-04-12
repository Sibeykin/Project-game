import random
from datetime import datetime

choices = ["камень", "ножницы", "бумага"]
scores = {"игрок1": 0, "игрок2": 0, "бот": 0}

def save_game_history(game_type, player1_score, player2_score):
    try:
        with open("history.txt", "a", encoding="utf-8") as file:
            date = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
            file.write(f"{date}  {game_type}  Игрок 1: {player1_score}  Игрок 2: {player2_score}\n")
    except Exception as e:
        print(f"Ошибка при сохранении истории: {e}")

def get_choice(player_name=""):
    while True:
        choice = input(f"{player_name}Выбери: камень, ножницы, бумага (или 'выход'): ").lower().strip()
        if choice == "выход" or choice in choices: 
            return choice
        print("Ошибка: выберите только 'камень', 'ножницы', 'бумагу' или 'выход'!")

def play_game():

    while True:
        print(f"Счет: Игрок: {scores['игрок1']} | Бот: {scores['бот']}\n")
        bot = random.choice(choices)
        user = get_choice()
        
        if user == "выход":
            print(f"\nИтог: Игрок: {scores['игрок1']}  Бот: {scores['бот']}")
            save_game_history("Игра против бота", scores['игрок1'], scores['бот'])
            break
            
        print(f"\nБот выбрал: {bot}")
        if user == bot:
            print("Ничья!")
        elif (user == "камень" and bot == "ножницы") or \
             (user == "ножницы" and bot == "бумага") or \
             (user == "бумага" and bot == "камень"):
            print("Вы победили!")
            scores["игрок1"] += 1
        else:
            print("Вы проиграли!")
            scores["бот"] += 1
        input()

def play_two_players():
    while True:
        print(f"Счет: Игрок 1: {scores['игрок1']}  Игрок 2: {scores['игрок2']}\n")
        
        player1 = get_choice("Игрок 1: ")
        if player1 == "выход":
            print(f"\nИтог: Игрок 1: {scores['игрок1']}  Игрок 2: {scores['игрок2']}")
            save_game_history("Игра для двоих игроков", scores['игрок1'], scores['игрок2'])
            break
            
        player2 = get_choice("Игрок 2: ")
        if player2 == "выход":
            print(f"\nИтог: Игрок 1: {scores['игрок1']}  Игрок 2: {scores['игрок2']}")
            save_game_history("Игра для двоих игроков", scores['игрок1'], scores['игрок2'])
            break
            
        if player1 == player2:
            print("Ничья!")
        elif (player1 == "камень" and player2 == "ножницы") or -\
             (player1 == "ножницы" and player2 == "бумага") or \
             (player1 == "бумага" and player2 == "камень"):
            print("Игрок 1 победил!")
            scores["игрок1"] += 1
        else:
            print("Игрок 2 победил!")
            scores["игрок2"] += 1
        input()

def show_history():
    try:
        with open("history.txt", "r", encoding="utf-8") as file:
            print("\nИстория игр:")
            for line in file:
                print(line.strip())
    except FileNotFoundError:
        print("\nИстория игр пуста")
    except Exception as e:
        print(f"Ошибка при чтении истории: {e}")
    input()

def main():
    print("Добро пожаловать в игру 'Камень, ножницы, бумага'!")
    while True:
        print("1. Игра против бота")
        print("2. Игра для двоих игроков")
        print("3. История игр")
        print("4. Выход")
        choice = input("Выбор (1-4): ").strip()
        if choice == "1": 
            play_game()
        elif choice == "2":
            play_two_players()
        elif choice == "3":
            show_history()
        else: 
            print("\nДо свидания!")
            break

if __name__ == "__main__":
    main() 