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
    result_text.set(f"компьютер Анатолий выбрал: {computer_choice}")
    
    if user_choice == computer_choice:
        result_text.set(result_text.get() + "\nничья! раунд засчитан снова.")
        return  
    
    elif (user_choice == 'камень' and computer_choice == 'ножницы') or \
         (user_choice == 'ножницы' and computer_choice == 'бумага') or \
         (user_choice == 'бумага' and computer_choice == 'камень'):
        user_score += 1
        result_text.set(result_text.get() + "\nвы победили этот раунд!")
    else:
        computer_score += 1
        result_text.set(result_text.get() + "\nкомпьютер Анатолий победил этот раунд!")
    
    rounds_played += 1

    score_text.set(f"счет: вы - {user_score}, компьютер Анатолий - {computer_score}")
    
    if rounds_played == 3:
        if user_score > computer_score:
            messagebox.showinfo("игра окончена", "поздравляем, ты победил у тебя хорошая удача")
        elif user_score < computer_score:
            messagebox.showinfo("игра окончена", "ты проиграл, плохая у тебя удача")
        else:
            messagebox.showinfo("игра окончена", "ничья!")

        reset_game()

def reset_game():
    global user_score, computer_score, rounds_played
    user_score = 0
    computer_score = 0
    rounds_played = 0
    score_text.set("счет: вы - 0, компьютер Анатолий - 0")
    result_text.set("")

root = tk.Tk()
root.title("камень, ножницы, бумага")

result_text = tk.StringVar()
score_text = tk.StringVar()

tk.Label(root, text="выберите: камень, ножницы или бумага", font=("Arial", 14)).pack(pady=10)
tk.Button(root, text="камень", font=("Arial", 12), command=lambda: play_round('камень')).pack(pady=5)
tk.Button(root, text="гожницы", font=("Arial", 12), command=lambda: play_round('ножницы')).pack(pady=5)
tk.Button(root, text="бумага", font=("Arial", 12), command=lambda: play_round('бумага')).pack(pady=5)

tk.Label(root, textvariable=score_text, font=("Arial", 12)).pack(pady=10)
tk.Label(root, textvariable=result_text, font=("Arial", 12)).pack(pady=10)

root.mainloop()

