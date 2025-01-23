import random
import tkinter as tk
from tkinter import messagebox

user_score = 0
computer_score = 0
rounds_played = 0

def play_round(user_choice):
    global user_score, computer_score, rounds_played
    
    options = ['камень', 'ножницы', 'бумага']
    
    computer_choice = random.choice(options)
    result_text.set(f"Компьютер выбрал: {computer_choice}")
    
    if user_choice == computer_choice:
        result_text.set(result_text.get() + "\nНичья! Раунд засчитан снова.")
        return  
    
    elif (user_choice == 'камень' and computer_choice == 'ножницы') or \
         (user_choice == 'ножницы' and computer_choice == 'бумага') or \
         (user_choice == 'бумага' and computer_choice == 'камень'):
        user_score += 1
        result_text.set(result_text.get() + "\nВы победили этот раунд!")
    else:
        computer_score += 1
        result_text.set(result_text.get() + "\nКомпьютер победил этот раунд!")
    
    rounds_played += 1

    score_text.set(f"Текущий счет: Вы - {user_score}, Компьютер - {computer_score}")
    
    if rounds_played == 3:
        if user_score > computer_score:
            messagebox.showinfo("Игра окончена", "Поздравляем, вы победили!")
        elif user_score < computer_score:
            messagebox.showinfo("Игра окончена", "Компьютер победил!")
        else:
            messagebox.showinfo("Игра окончена", "Ничья!")

        reset_game()

def reset_game():
    global user_score, computer_score, rounds_played
    user_score = 0
    computer_score = 0
    rounds_played = 0
    score_text.set("Текущий счет: Вы - 0, Компьютер - 0")
    result_text.set("")

root = tk.Tk()
root.title("Камень, Ножницы, Бумага")

result_text = tk.StringVar()
score_text = tk.StringVar()

tk.Label(root, text="Выберите: камень, ножницы или бумага", font=("Arial", 14)).pack(pady=10)
tk.Button(root, text="Камень", font=("Arial", 12), command=lambda: play_round('камень')).pack(pady=5)
tk.Button(root, text="Ножницы", font=("Arial", 12), command=lambda: play_round('ножницы')).pack(pady=5)
tk.Button(root, text="Бумага", font=("Arial", 12), command=lambda: play_round('бумага')).pack(pady=5)

tk.Label(root, textvariable=score_text, font=("Arial", 12)).pack(pady=10)
tk.Label(root, textvariable=result_text, font=("Arial", 12)).pack(pady=10)

root.mainloop()
