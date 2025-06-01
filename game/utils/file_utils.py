import os
import json
import datetime

# директория к игре
GAME_DIR = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
HISTORY_FILE = os.path.join('data', 'history.json')
KEYBINDINGS_FILE = os.path.join('data', 'keybindings.json')

def ensure_data_dir():
    if not os.path.exists('data'):
        os.makedirs('data')

def save_game_history(game_type, player1_score, player2_score):
    try:
        ensure_data_dir()
        history = []
        try:
            if os.path.exists(HISTORY_FILE):
                with open(HISTORY_FILE, "r", encoding="utf-8") as file:
                    history = json.load(file)
        except (FileNotFoundError, json.JSONDecodeError, PermissionError):
            history = []

        history.append({
            "date": datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S"),
            "game_type": game_type,
            "player1_score": player1_score,
            "player2_score": player2_score
        })

        try:
            with open(HISTORY_FILE, "w", encoding="utf-8") as file:
                json.dump(history, file, ensure_ascii=False, indent=4)
        except PermissionError:
            temp_file = HISTORY_FILE + ".tmp"
            with open(temp_file, "w", encoding="utf-8") as file:
                json.dump(history, file, ensure_ascii=False, indent=4)
            if os.path.exists(HISTORY_FILE):
                os.remove(HISTORY_FILE)
            os.rename(temp_file, HISTORY_FILE)
    except Exception as e:
        print(f"Ошибка сохранения истории: {e}")

def save_key_bindings(bindings):
    try:
        ensure_data_dir()
        with open(KEYBINDINGS_FILE, "w", encoding="utf-8") as file:
            json.dump(bindings, file, ensure_ascii=False, indent=4)
    except Exception as e:
        print(f"Ошибка сохранения биндов: {e}")

def load_key_bindings():
    try:
        ensure_data_dir()
        with open(KEYBINDINGS_FILE, "r", encoding="utf-8") as file:
            return json.load(file)
    except (FileNotFoundError, json.JSONDecodeError):
        return {
            "player1_rock": "A",
            "player1_scissors": "S",
            "player1_paper": "D",
            "player2_rock": "J",
            "player2_scissors": "K",
            "player2_paper": "L"
        }

def init_files():
    ensure_data_dir()
    
    if not os.path.exists(HISTORY_FILE):
        try:
            with open(HISTORY_FILE, "w", encoding="utf-8") as file:
                json.dump([], file, ensure_ascii=False, indent=4)
            os.chmod(HISTORY_FILE, 0o666)
        except Exception as e:
            print(f"Ошибка создания history.json: {e}")
    
    if not os.path.exists(KEYBINDINGS_FILE):
        try:
            default_bindings = {
                "player1_rock": "A",
                "player1_scissors": "S",
                "player1_paper": "D",
                "player2_rock": "J",
                "player2_scissors": "K",
                "player2_paper": "L"
            }
            with open(KEYBINDINGS_FILE, "w", encoding="utf-8") as file:
                json.dump(default_bindings, file, ensure_ascii=False, indent=4)
            os.chmod(KEYBINDINGS_FILE, 0o666)
        except Exception as e:
            print(f"Ошибка создания keybindings.json: {e}") 