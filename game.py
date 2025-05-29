from PyQt6.QtWidgets import (QApplication, QMainWindow, QWidget, QVBoxLayout, QHBoxLayout, QPushButton, QLabel, QGridLayout, QSpacerItem, QSizePolicy, 
                             QStackedWidget, QScrollArea, QTableWidget, QTableWidgetItem, QHeaderView, QMessageBox, QStatusBar)
from PyQt6.QtCore import Qt, QTimer
import random
import json
import datetime
# константы стилей
BUTTON_STYLE = """
    QPushButton {
        background-color: white;
        border: 2px solid black;
        border-radius: 10px;
        padding: 20px 40px;
        font-size: 18px;
        min-width: 220px;
    }
    QPushButton:hover { background-color: #EEEEEE; }
    QPushButton:pressed { background-color: #CCCCCC; }
"""
SMALL_BUTTON_STYLE = """
    QPushButton {
        background-color: white;
        border: 2px solid black;
        border-radius: 5px;
        padding: 8px 20px;
        font-size: 14px;
        min-width: 100px;
    }
    QPushButton:hover { background-color: #EEEEEE; }
    QPushButton:pressed { background-color: #CCCCCC; }
"""
TINY_BUTTON_STYLE = """
    QPushButton {
        background-color: white;
        border: 2px solid black;
        border-radius: 15px;
        padding: 5px 15px;
        font-size: 12px;
        min-width: 80px;
    }
    QPushButton:hover { background-color: #EEEEEE; }
    QPushButton:pressed { background-color: #CCCCCC; }
"""
# константы 
CHOICES = ["камень", "ножницы", "бумага"]
EMOJIS = {"rock": "🗿", "scissors": "✂️", "paper": "📄"}
MAX_SCORE = 3

scores = {"игрок1": 0, "игрок2": 0, "бот": 0}

# константы для валидации клавиш
VALID_KEYS = {
    'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M',
    'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
    '1', '2', '3', '4', '5', '6', '7', '8', '9', '0'
}

def save_game_history(game_type, player1_score, player2_score):
    try:
        try:
            with open("history.json", "r", encoding="utf-8") as file:
                history = json.load(file)
        except (FileNotFoundError, json.JSONDecodeError):
            history = []

        history.append({
            "date": "2024-03-21 12:00:00",  
            "game_type": game_type,
            "player1_score": player1_score,
            "player2_score": player2_score
        })

        with open("history.json", "w", encoding="utf-8") as file:
            json.dump(history, file, ensure_ascii=False, indent=4)
    except Exception as e:
        print(f"Ошибка сохранения истории: {e}")

def save_key_bindings(bindings):
    try:
        with open("keybindings.json", "w", encoding="utf-8") as file:
            json.dump(bindings, file, ensure_ascii=False, indent=4)
    except Exception as e:
        print(f"Ошибка сохранения биндов: {e}")

def load_key_bindings():
    try:
        with open("keybindings.json", "r", encoding="utf-8") as file:
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

def set_button_style(button, style=BUTTON_STYLE):
    button.setStyleSheet(style)

def set_small_button_style(button):
    button.setStyleSheet(SMALL_BUTTON_STYLE)

def set_tiny_button_style(button):
    button.setStyleSheet(TINY_BUTTON_STYLE)

class MainMenuWidget(QWidget):
    def __init__(self):
        super().__init__()
        self.initUI()

    def initUI(self):
        layout = QVBoxLayout()
        layout.setSpacing(15)
        self.setLayout(layout)

        # настройки кнопка
        top_layout = QHBoxLayout()
        top_layout.addSpacerItem(QSpacerItem(0, 0, QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Minimum))
        self.btn_settings = QPushButton("Настройки")
        self.btn_settings.setFixedSize(100, 30)
        set_button_style(self.btn_settings, TINY_BUTTON_STYLE)
        top_layout.addWidget(self.btn_settings)
        layout.addLayout(top_layout)
        # кмн и выберите режим
        for text, size in [("Камень, ножницы, бумага", 30), ("Выберите режим", 18)]:
            label = QLabel(text)
            label.setAlignment(Qt.AlignmentFlag.AlignCenter)
            label.setStyleSheet(f"font-size: {size}px; {'font-weight: bold;' if size == 30 else ''}")
            layout.addWidget(label)

        # эмодзи
        emoji_layout = QHBoxLayout()
        emoji_layout.addStretch()
        for emoji in ["🗿", "✂️", "📄"]:
            label = QLabel(emoji)
            label.setAlignment(Qt.AlignmentFlag.AlignCenter)
            label.setStyleSheet("font-size: 60px;")
            emoji_layout.addWidget(label)
        emoji_layout.addStretch()
        layout.addLayout(emoji_layout)
        layout.addSpacerItem(QSpacerItem(0, 50, QSizePolicy.Policy.Minimum, QSizePolicy.Policy.Fixed))
        # кнопки режимов
        game_modes_layout = QGridLayout()
        game_modes_layout.setSpacing(20)
        buttons = [
            ("Игра с ботом", 0, 0),
            ("Игра с другом", 0, 1),
            ("История игр", 1, 0),
            ("Выход", 1, 1)
        ]
        for text, row, col in buttons:
            btn = QPushButton(text)
            btn.setFixedSize(250, 45)
            btn.setStyleSheet("""
                QPushButton {
                    background-color: white;
                    border: 2px solid black;
                    border-radius: 10px;
                    padding: 10px 25px;
                    font-size: 16px;
                    min-width: 250px;
                    text-align: center;
                    line-height: 25px;
                }
                QPushButton:hover { background-color: #EEEEEE; }
                QPushButton:pressed { background-color: #CCCCCC; }
            """)
            game_modes_layout.addWidget(btn, row, col)
            setattr(self, f"btn_{text.lower().replace(' ', '_')}", btn)
        layout.addLayout(game_modes_layout)
        layout.addStretch()

class GameResultWidget(QWidget):
    def __init__(self, player_score, bot_score, game_mode, parent=None):
        super().__init__(parent)
        self.player_score = player_score
        self.bot_score = bot_score
        self.game_mode = game_mode
        self.parent_window = parent
        self.initUI()

    def initUI(self):
        layout = QVBoxLayout()
        layout.setSpacing(1)
        layout.setContentsMargins(30, 15, 30, 30)
        self.setLayout(layout)
        # кто победил
        winner = self.get_winner_text()
        title = QLabel(winner)
        title.setAlignment(Qt.AlignmentFlag.AlignCenter)
        title.setStyleSheet("""
            font-size: 24px;
            font-weight: bold;
            color: #2c3e50;
            margin: 2px 0 -4px 0;
        """)
        layout.addWidget(title)

        # Статистика игры
        stats_label = QLabel("Статистика игры:")
        stats_label.setAlignment(Qt.AlignmentFlag.AlignCenter)
        stats_label.setStyleSheet("""
            font-size: 18px;
            font-weight: bold;
            color: #2c3e50;
            margin: -4px 0 5px 0;
        """)
        layout.addWidget(stats_label)
        # виджет статистики
        stats_widget = QWidget()
        stats_widget.setStyleSheet("""
            QWidget {
                background-color: #f8f9fa;
                border: 2px solid #e9ecef;
                border-radius: 15px;
                padding: 10px;
            }
        """)
        stats_layout = QVBoxLayout()
        stats_layout.setSpacing(5)
        stats_widget.setLayout(stats_layout)
        # статистика
        total_games = self.player_score + self.bot_score
        player1_percent = (self.player_score / total_games * 100) if total_games > 0 else 0
        player2_percent = (self.bot_score / total_games * 100) if total_games > 0 else 0
        stats = [
            f"{'Игрок' if self.game_mode == 'bot' else 'Игрок 1'}: {self.player_score}",
            f"{'Бот' if self.game_mode == 'bot' else 'Игрок 2'}: {self.bot_score}",
            f"Процент побед {'Игрока' if self.game_mode == 'bot' else 'Игрока 1'}: {player1_percent:.1f}%",
            f"Процент побед {'Бота' if self.game_mode == 'bot' else 'Игрока 2'}: {player2_percent:.1f}%"
        ]
        for stat in stats:
            label = QLabel(stat)
            label.setAlignment(Qt.AlignmentFlag.AlignCenter)
            label.setStyleSheet("""
                font-size: 16px;
                color: #495057;
                margin: 2px 0;
            """)
            stats_layout.addWidget(label)
        layout.addWidget(stats_widget)
        layout.addSpacerItem(QSpacerItem(0, 45, QSizePolicy.Policy.Minimum, QSizePolicy.Policy.Fixed))
        buttons_layout = QHBoxLayout()
        buttons_layout.setSpacing(20)
        for text, slot in [("Сыграть снова", self.play_again), ("Главное меню", self.show_main_menu)]:
            btn = QPushButton(text)
            btn.setFixedSize(200, 40)
            btn.setStyleSheet("""
                QPushButton {
                    background-color: white;
                    border: 2px solid black;
                    border-radius: 10px;
                    padding: 10px 20px;
                    font-size: 16px;
                    min-width: 200px;
                }
                QPushButton:hover { background-color: #EEEEEE; }
                QPushButton:pressed { background-color: #CCCCCC; }
            """)
            btn.clicked.connect(slot)
            buttons_layout.addWidget(btn)
        layout.addLayout(buttons_layout)
        exit_btn = QPushButton("Выход из игры")
        exit_btn.setFixedSize(200, 40)
        exit_btn.setStyleSheet("""
            QPushButton {
                background-color: white;
                border: 2px solid black;
                border-radius: 10px;
                padding: 10px 20px;
                font-size: 16px;
                min-width: 200px;
            }
            QPushButton:hover { background-color: #EEEEEE; }
            QPushButton:pressed { background-color: #CCCCCC; }
        """)
        exit_btn.clicked.connect(self.close_game)
        layout.addWidget(exit_btn, alignment=Qt.AlignmentFlag.AlignCenter)

    def get_winner_text(self):
        if self.game_mode == "bot":
            return "Игрок победил!" if self.player_score > self.bot_score else "Бот победил!"
        else:
            if self.player_score > self.bot_score:
                return "Игрок 1 победил!"
            elif self.bot_score > self.player_score:
                return "Игрок 2 победил!"
            return "Ничья!"

    def play_again(self):
        if self.parent_window:
            if self.game_mode == "bot":
                self.parent_window.reset_game_bot()
                self.parent_window.show_game_vs_bot()
            else:
                self.parent_window.reset_game_friend()
                self.parent_window.show_game_vs_friend()
        self.close()

    def show_main_menu(self):
        if self.parent_window:
            if self.game_mode == "bot":
                self.parent_window.reset_game_bot()
            else:
                self.parent_window.reset_game_friend()
            self.parent_window.show_main_menu()

    def close_game(self):
        if self.parent_window:
            self.parent_window.close()

class BaseGameWidget(QWidget):
    def __init__(self, parent=None, key_assignments=None):
        super().__init__(parent)
        self.parent_window = parent
        self.key_assignments = key_assignments or {
            "player1_rock": "A",
            "player1_scissors": "S",
            "player1_paper": "D"
        }
        self.score1 = self.score2 = 0
        self.choice1 = self.choice2 = None
        self.is_remapping = False
        self.setFocusPolicy(Qt.FocusPolicy.StrongFocus)
        self.initUI()

    def initUI(self):
        layout = QVBoxLayout()
        layout.setSpacing(0)
        self.setLayout(layout)

        # верхняя панель назад и настройки
        top_layout = QHBoxLayout()
        self.btn_back = QPushButton("Назад")
        self.btn_back.setFixedSize(100, 30)
        set_button_style(self.btn_back, TINY_BUTTON_STYLE)
        top_layout.addWidget(self.btn_back)
        top_layout.addStretch()
        self.btn_settings = QPushButton("Настройки")
        self.btn_settings.setFixedSize(100, 30)
        set_button_style(self.btn_settings, TINY_BUTTON_STYLE)
        top_layout.addWidget(self.btn_settings)
        layout.addLayout(top_layout)

        # счет и иконки
        players_layout = QHBoxLayout()
        players_layout.addStretch()

        # игрок 1
        player1_layout = QVBoxLayout()
        player1_layout.setSpacing(0)
        self.score1_label = QLabel(f"{self.get_player1_name()}: {self.score1}")
        self.score1_label.setStyleSheet("font-size: 28px; font-weight: bold;")
        player1_layout.addWidget(self.score1_label, alignment=Qt.AlignmentFlag.AlignCenter)
        self.player1_icon = QLabel("👤")
        self.player1_icon.setStyleSheet("font-size: 40px;")
        player1_layout.addWidget(self.player1_icon, alignment=Qt.AlignmentFlag.AlignCenter)
        players_layout.addLayout(player1_layout)

        players_layout.addSpacing(20)

        # игрок 2
        player2_layout = QVBoxLayout()
        player2_layout.setSpacing(0)
        self.score2_label = QLabel(f"{self.get_player2_name()}: {self.score2}")
        self.score2_label.setStyleSheet("font-size: 24px; font-weight: bold;")
        player2_layout.addWidget(self.score2_label, alignment=Qt.AlignmentFlag.AlignCenter)
        self.player2_icon = QLabel("🤖" if isinstance(self, GameVsBotWidget) else "👤")
        self.player2_icon.setStyleSheet("font-size: 40px;")
        player2_layout.addWidget(self.player2_icon, alignment=Qt.AlignmentFlag.AlignCenter)
        players_layout.addLayout(player2_layout)
        players_layout.addStretch()
        layout.addLayout(players_layout)

        # подсказка
        self.hint_label = QLabel(self.get_hint_text())
        self.hint_label.setAlignment(Qt.AlignmentFlag.AlignCenter)
        self.hint_label.setStyleSheet("font-size: 20px; color: #FF4500; margin: 20px 0;")
        layout.addWidget(self.hint_label)

        # отображение выбора
        choices_layout = QVBoxLayout()
        choices_layout.setSpacing(10)
        choices_layout.setContentsMargins(0, 0, 0, 0)

        # строки выбора
        self.choice1_display = QLabel("Игрок 1 выбрал:")
        self.choice2_display = QLabel("Игрок 2 выбрал:")
        self.result_display = QLabel("")
        for label in [self.choice1_display, self.choice2_display, self.result_display]:
            label.setAlignment(Qt.AlignmentFlag.AlignCenter)
            label.setStyleSheet("font-size: 20px; margin: 10px 0;")
            choices_layout.addWidget(label)

        # знаки вопроса и VS
        choice_icons_layout = QHBoxLayout()
        choice_icons_layout.setSpacing(5)
        choice_icons_layout.addStretch()
        
        self.choice1_icon = QLabel("❓")
        self.choice1_icon.setAlignment(Qt.AlignmentFlag.AlignCenter)
        self.choice1_icon.setStyleSheet("font-size: 60px; padding: 12px;")
        choice_icons_layout.addWidget(self.choice1_icon)
        
        # VS
        vs_label = QLabel("VS")
        vs_label.setStyleSheet("font-size: 24px; font-weight: bold; color: #FF9800;")
        choice_icons_layout.addWidget(vs_label)
        
        self.choice2_icon = QLabel("❓")
        self.choice2_icon.setAlignment(Qt.AlignmentFlag.AlignCenter)
        self.choice2_icon.setStyleSheet("font-size: 60px; padding: 12px;")
        choice_icons_layout.addWidget(self.choice2_icon)
        
        choice_icons_layout.addStretch()
        choices_layout.addLayout(choice_icons_layout)

        layout.addLayout(choices_layout)
        self.setup_choice_buttons(layout)

    def get_player1_name(self): return "Игрок"
    def get_player2_name(self): return "Бот" if isinstance(self, GameVsBotWidget) else "Игрок 2"
    def get_hint_text(self): return "Игрок, сделайте свой выбор"

    def setup_choice_buttons(self, layout):
        choices_layout = QVBoxLayout()
        choices_layout.setSpacing(10)
        choices_layout.setAlignment(Qt.AlignmentFlag.AlignCenter) 
        choices_layout.setContentsMargins(0, -50, 0, 0)  
        
        for action, key in [("rock", "A"), ("scissors", "S"), ("paper", "D")]:
            btn_layout = QHBoxLayout()
            btn_layout.setAlignment(Qt.AlignmentFlag.AlignCenter)  
            key_label = QLabel(f"[{self.key_assignments.get(f'player1_{action}', key)}]")
            key_label.setObjectName(f"player1_{action}_key")
            key_label.setStyleSheet("font-size: 16px; margin-right: 5px;")
            btn_layout.addWidget(key_label)
            btn = QPushButton(self.get_choice_text(action))
            set_button_style(btn)
            btn.clicked.connect(lambda checked, a=action: self.make_choice(a))
            btn_layout.addWidget(btn)
            choices_layout.addLayout(btn_layout)
        layout.addLayout(choices_layout)

    def get_choice_text(self, action):
        return {"rock": "Камень", "scissors": "Ножницы", "paper": "Бумага"}[action]

    def update_key_assignments(self, assignments):
        self.key_assignments = assignments
        # обновляем отображение букв для каждой кнопки
        for action in ["rock", "scissors", "paper"]:
            key_label = self.findChild(QLabel, f"player1_{action}_key")
            if key_label:
                key_label.setText(self.key_assignments[f"player1_{action}"])

    def reset_game(self):
        self.score1 = self.score2 = 0
        self.score1_label.setText(f"{self.get_player1_name()}: {self.score1}")
        self.score2_label.setText(f"{self.get_player2_name()}: {self.score2}")
        self.choice1_display.setText(f"{self.get_player1_name()} выбрал:")
        self.choice2_display.setText(f"{self.get_player2_name()} выбрал:")
        self.result_display.setText("")
        self.choice1 = self.choice2 = None
        self.choice1_icon.setText("❓")
        self.choice2_icon.setText("❓")

    def make_choice(self, choice):
        if self.score1 >= MAX_SCORE or self.score2 >= MAX_SCORE:
            return
        player_emoji = EMOJIS[choice]
        self.choice1_display.setText(f"{self.get_player1_name()} выбрал: {player_emoji}")
        self.choice1_icon.setText(player_emoji)
        bot_choice = random.choice(["rock", "scissors", "paper"])
        bot_emoji = EMOJIS[bot_choice]
        self.choice2_display.setText(f"{self.get_player2_name()} выбрал: {bot_emoji}")
        self.choice2_icon.setText(bot_emoji)
        if choice == bot_choice:
            result = "Ничья!"
        elif (choice == "rock" and bot_choice == "scissors") or \
             (choice == "scissors" and bot_choice == "paper") or \
             (choice == "paper" and bot_choice == "rock"):
            result = "Вы победили!"
            self.score1 += 1
            self.score1_label.setText(f"{self.get_player1_name()}: {self.score1}")
        else:
            result = "Бот победил!"
            self.score2 += 1
            self.score2_label.setText(f"{self.get_player2_name()}: {self.score2}")
        self.result_display.setText(result)
        if self.score1 >= MAX_SCORE or self.score2 >= MAX_SCORE:
            save_game_history("Игра с ботом", self.score1, self.score2)
            self.show_result_window()
        else:
            QTimer.singleShot(2000, self.start_new_round)  

    def start_new_round(self):
        self.choice1_display.setText(f"{self.get_player1_name()} выбрал:")
        self.choice2_display.setText(f"{self.get_player2_name()} выбрал:")
        self.choice1_icon.setText("❓")
        self.choice2_icon.setText("❓")
        self.result_display.setText("")
        self.hint_label.setText("Игрок, сделайте свой выбор")

    def show_result_window(self):
        if self.parent_window:
            self.parent_window.show_results(self.score1, self.score2)

    def keyPressEvent(self, event):
        if self.is_remapping:
            return super().keyPressEvent(event)
        key_name = event.text().upper()
        if not key_name:
            return super().keyPressEvent(event)
        # проверяем привязки клавиш для игрока 1
        for action, key in self.key_assignments.items():
            if action.startswith("player1_") and key_name == key:
                choice = action.split("player1_")[1]
                self.make_choice(choice)
                return
        super().keyPressEvent(event)

class GameVsBotWidget(BaseGameWidget):
    pass

class GameVsFriendWidget(BaseGameWidget):
    def __init__(self, parent=None, key_assignments=None):
        super().__init__(parent, key_assignments)
        self.key_assignments.update({
            "player2_rock": key_assignments.get("player2_rock", "J"),
            "player2_scissors": key_assignments.get("player2_scissors", "K"),
            "player2_paper": key_assignments.get("player2_paper", "L")
        })
        self.choice1 = None
        self.choice2 = None

    def get_player1_name(self): return "Игрок 1"
    def get_player2_name(self): return "Игрок 2"
    def get_hint_text(self): return "Игрок 1, сделайте свой выбор"

    def setup_choice_buttons(self, layout):
        choices_layout = QHBoxLayout()
        choices_layout.setSpacing(30)
        choices_layout.setAlignment(Qt.AlignmentFlag.AlignCenter)
        
        # кнопки для игрока 1
        player1_layout = QVBoxLayout()
        player1_layout.setSpacing(10)
        player1_layout.setAlignment(Qt.AlignmentFlag.AlignCenter)
        player1_label = QLabel("Игрок 1")
        player1_label.setStyleSheet("font-size: 24px; font-weight: bold;")
        player1_layout.addWidget(player1_label, alignment=Qt.AlignmentFlag.AlignCenter)
        
        for action, key in [("rock", "A"), ("scissors", "S"), ("paper", "D")]:
            btn_layout = QHBoxLayout()
            btn_layout.setAlignment(Qt.AlignmentFlag.AlignCenter)
            key_label = QLabel(f"[{self.key_assignments.get(f'player1_{action}', key)}]")
            key_label.setObjectName(f"player1_{action}_key")
            key_label.setStyleSheet("font-size: 16px; margin-right: 5px;")
            btn_layout.addWidget(key_label)
            btn = QPushButton(self.get_choice_text(action))
            set_button_style(btn)
            btn.clicked.connect(lambda checked, a=action: self.handle_button_click("player1", a))
            btn_layout.addWidget(btn)
            player1_layout.addLayout(btn_layout)
        choices_layout.addLayout(player1_layout)

        # кнопки для игрока 2
        player2_layout = QVBoxLayout()
        player2_layout.setSpacing(10)
        player2_layout.setAlignment(Qt.AlignmentFlag.AlignCenter)
        player2_label = QLabel("Игрок 2")
        player2_label.setStyleSheet("font-size: 24px; font-weight: bold;")
        player2_layout.addWidget(player2_label, alignment=Qt.AlignmentFlag.AlignCenter)
        
        for action, key in [("rock", "J"), ("scissors", "K"), ("paper", "L")]:
            btn_layout = QHBoxLayout()
            btn_layout.setAlignment(Qt.AlignmentFlag.AlignCenter)
            key_label = QLabel(f"[{self.key_assignments.get(f'player2_{action}', key)}]")
            key_label.setObjectName(f"player2_{action}_key")
            key_label.setStyleSheet("font-size: 16px; margin-right: 5px;")
            btn_layout.addWidget(key_label)
            btn = QPushButton(self.get_choice_text(action))
            set_button_style(btn)
            btn.clicked.connect(lambda checked, a=action: self.handle_button_click("player2", a))
            btn_layout.addWidget(btn)
            player2_layout.addLayout(btn_layout)
        choices_layout.addLayout(player2_layout)
        
        layout.addLayout(choices_layout)

    def handle_button_click(self, player, choice):
        if player == "player1" and self.choice1 is None:
            self.update_choice(1, choice)
        elif player == "player2" and self.choice2 is None:
            self.update_choice(2, choice)

    def update_choice(self, player_num, choice):
        if player_num == 1 and self.choice1 is None:
            self.choice1 = choice
            self.choice1_display.setText("Игрок 1 выбрал:")
            self.choice1_icon.setText("❓")
            self.hint_label.setText("Игрок 2, сделайте свой выбор")
        elif player_num == 2 and self.choice2 is None:
            self.choice2 = choice
            self.choice2_display.setText("Игрок 2 выбрал:")
            self.choice2_icon.setText("❓")
        if self.choice1 and self.choice2:
            self.make_choice(self.choice1, self.choice2)

    def make_choice(self, choice1, choice2):
        if self.score1 >= MAX_SCORE or self.score2 >= MAX_SCORE:
            return
        emoji1 = EMOJIS[choice1]
        self.choice1_display.setText(f"Игрок 1 выбрал: {emoji1}")
        self.choice1_icon.setText(emoji1)
        emoji2 = EMOJIS[choice2]
        self.choice2_display.setText(f"Игрок 2 выбрал: {emoji2}")
        self.choice2_icon.setText(emoji2)
        if choice1 == choice2:
            result = "Ничья!"
        elif (choice1 == "rock" and choice2 == "scissors") or \
             (choice1 == "scissors" and choice2 == "paper") or \
             (choice1 == "paper" and choice2 == "rock"):
            result = "Игрок 1 победил!"
            self.score1 += 1
            self.score1_label.setText(f"Игрок 1: {self.score1}")
        else:
            result = "Игрок 2 победил!"
            self.score2 += 1
            self.score2_label.setText(f"Игрок 2: {self.score2}")
        self.result_display.setText(result)
        QTimer.singleShot(2000, self.start_new_round)

    def start_new_round(self):
        self.choice1 = self.choice2 = None
        self.choice1_display.setText("Игрок 1 выбрал:")
        self.choice2_display.setText("Игрок 2 выбрал:")
        self.choice1_icon.setText("❓")
        self.choice2_icon.setText("❓")
        self.result_display.setText("")
        self.hint_label.setText("Игрок 1, сделайте свой выбор")
        if self.score1 >= MAX_SCORE or self.score2 >= MAX_SCORE:
            save_game_history("Игра с другом", self.score1, self.score2)
            self.show_result_window()

    def update_key_assignments(self, assignments):
        super().update_key_assignments(assignments)

        for action in ["rock", "scissors", "paper"]:
            key_label = self.findChild(QLabel, f"player2_{action}_key")
            if key_label:
                key_label.setText(self.key_assignments[f"player2_{action}"])

    def keyPressEvent(self, event):
        if self.is_remapping:
            return super().keyPressEvent(event)

        key_name = event.text().upper()
        if not key_name:
            return super().keyPressEvent(event)

        # проверяем привязки клавиш 
        for action, key in self.key_assignments.items():
            if action.startswith("player1_") and key_name == key and self.choice1 is None:
                choice = action.split("player1_")[1]
                self.update_choice(1, choice)
                return
            elif action.startswith("player2_") and key_name == key and self.choice2 is None:
                choice = action.split("player2_")[1]
                self.update_choice(2, choice)
                return
        event.accept()

def clean_date_string(date_str):
    """Очистка строки даты от нежелательных символов"""
    cleaned = ''.join(c for c in date_str if c.isdigit() or c in '-: ')
    if len(cleaned) == 19:  
        return cleaned
    return "2024-03-21 12:00:00"  

def clean_game_type(game_type):
    """Очистка типа игры от нежелательных символов"""
    if game_type == "Игра с ботом":
        return "Игра с ботом"
    elif game_type == "Игра с другом":
        return "Игра с другом"
    elif "бот" in game_type.lower():
        return "Игра с ботом"
    return "Игра с другом"

def validate_history_entry(entry):
    """Валидация записи в истории игр"""
    required_fields = ["date", "game_type", "player1_score", "player2_score"]
    
    if not all(field in entry for field in required_fields):
        return False
    try:
        datetime.strptime(clean_date_string(entry["date"]), "%Y-%m-%d %H:%M:%S")
    except ValueError:
        return False
    
    # тип игры
    if clean_game_type(entry["game_type"]) not in ["Игра с ботом", "Игра с другом"]:
        return False
    
    # счет 
    try:
        player1_score = int(entry["player1_score"])
        player2_score = int(entry["player2_score"])
        if player1_score < 0 or player2_score < 0:
            return False
    except (ValueError, TypeError):
        return False
    
    return True

class HistoryWidget(QWidget):
    def __init__(self, parent=None):
        super().__init__(parent)
        self.parent_window = parent
        self.initUI()
        self.load_history()

    def initUI(self):
        layout = QVBoxLayout()
        self.setLayout(layout)
        layout.setSpacing(10)

        # назад и настройки
        top_layout = QHBoxLayout()
        self.btn_back = QPushButton("Назад")
        self.btn_back.setFixedSize(100, 30)
        set_button_style(self.btn_back, TINY_BUTTON_STYLE)
        top_layout.addWidget(self.btn_back)
        top_layout.addStretch()

        self.btn_settings = QPushButton("Настройки")
        self.btn_settings.setFixedSize(100, 30)
        set_button_style(self.btn_settings, TINY_BUTTON_STYLE)
        top_layout.addWidget(self.btn_settings)
        layout.addLayout(top_layout)

        # история игр
        title_label = QLabel("История игр")
        title_label.setAlignment(Qt.AlignmentFlag.AlignCenter)
        title_label.setStyleSheet("font-size: 24px; font-weight: bold; margin: 20px 0;")
        layout.addWidget(title_label)

        # таблица
        scroll_area = QScrollArea()
        scroll_area.setWidgetResizable(True)
        scroll_area.setStyleSheet("""
            QScrollArea { border: none; }
            QScrollBar:vertical {
                border: none;
                background: #f0f0f0;
                width: 10px;
                margin: 0px;
            }
            QScrollBar::handle:vertical {
                background: #c0c0c0;
                min-height: 20px;
                border-radius: 5px;
            }
            QScrollBar::add-line:vertical, QScrollBar::sub-line:vertical { height: 0px; }
        """)
        layout.addWidget(scroll_area)
        self.table = QTableWidget()
        self.table.setColumnCount(4)
        self.table.setHorizontalHeaderLabels(["Дата", "Режим", "Игрок 1", "Игрок 2"])
        self.table.horizontalHeader().setSectionResizeMode(QHeaderView.ResizeMode.Stretch)
        self.table.setStyleSheet("""
            QTableWidget {
                border: 2px solid #e0e0e0;
                border-radius: 10px;
                background-color: white;
            }
            QHeaderView::section {
                background-color: #f8f9fa;
                padding: 8px;
                border: none;
                font-weight: bold;
                text-align: center;
            }
            QTableWidget::item {
                padding: 8px;
                text-align: center;
            }
        """)
        scroll_area.setWidget(self.table)

    def load_history(self):
        try:
            with open("history.json", "r", encoding="utf-8") as file:
                history = json.load(file)
                self.table.setRowCount(len(history))
                for i, game in enumerate(history):
                    cleaned_date = clean_date_string(game["date"])
                    cleaned_game_type = clean_game_type(game["game_type"])
                    for col, text in enumerate([
                        cleaned_date,
                        cleaned_game_type,
                        str(game["player1_score"]),
                        str(game["player2_score"])
                    ]):
                        item = QTableWidgetItem(text)
                        item.setTextAlignment(Qt.AlignmentFlag.AlignCenter)
                        self.table.setItem(i, col, item)
        except Exception as e:
            print(f"Ошибка загрузки истории: {e}")

class SettingsWidget(QWidget):
    def __init__(self, parent=None):
        super().__init__(parent)
        self.parent_window = parent
        self.remapping_button = None
        self.is_remapping = False
        self.key_assignments = load_key_bindings()
        self.pending_key_assignments = {}
        self.previous_widget = None  
        self.initUI()
        self.setFocus()

    def initUI(self):
        layout = QVBoxLayout()
        self.setLayout(layout)
        layout.setSpacing(10)

        # верхняя панель с кнопкой назад
        top_layout = QHBoxLayout()
        self.btn_back = QPushButton("Назад")
        self.btn_back.setFixedSize(100, 30)
        set_button_style(self.btn_back, TINY_BUTTON_STYLE)
        self.btn_back.clicked.connect(self.go_back)
        top_layout.addWidget(self.btn_back)
        top_layout.addStretch()
        layout.addLayout(top_layout)

        # смена кнопок управления
        title_label = QLabel("Смена кнопок управления")
        title_label.setAlignment(Qt.AlignmentFlag.AlignCenter)
        title_label.setStyleSheet("font-size: 24px; font-weight: bold; margin: 20px 0;")
        layout.addWidget(title_label)

        # контейнер управления
        controls_container = QWidget()
        controls_container.setStyleSheet("""
            QWidget {
                background-color: white;
                border: 1px solid #e0e0e0;
                border-radius: 10px;
                padding: 20px;
            }
        """)
        controls_layout = QGridLayout()
        controls_container.setLayout(controls_layout)
        controls_layout.setSpacing(15)

        # заголовки игроков
        for col, text in [(0, "1 игрок"), (2, "2 игрок")]:
            label = QLabel(text)
            label.setAlignment(Qt.AlignmentFlag.AlignCenter)
            label.setStyleSheet("font-size: 18px; font-weight: bold;")
            controls_layout.addWidget(label, 0, col, alignment=Qt.AlignmentFlag.AlignCenter)

        # привязки клавиш
        self.key_buttons = {}
        for i, (action, icon) in enumerate([("rock", "🗿"), ("scissors", "✂️"), ("paper", "📄")]):
            for player_num, col, default_key in [(1, 0, "A"), (2, 2, "J")]:
                action_key = f"player{player_num}_{action}"
                key = self.key_assignments.get(action_key, default_key)
                key_button = QPushButton(f"[{key}]")
                key_button.setStyleSheet("""
                    QPushButton {
                        background-color: white;
                        border: 1px solid #2196F3;
                        border-radius: 5px;
                        padding: 5px 15px;
                        font-size: 18px;
                        min-width: 80px;
                    }
                    QPushButton:hover { background-color: #EEEEEE; }
                    QPushButton:pressed { background-color: #CCCCCC; }
                """)
                key_button.setFixedSize(80, 40)
                controls_layout.addWidget(key_button, i + 1, col, alignment=Qt.AlignmentFlag.AlignCenter)
                key_button.clicked.connect(lambda checked, btn=key_button, act=action_key: self.start_key_remapping(btn, act))
                self.key_buttons[action_key] = key_button

            icon_label = QLabel(icon)
            icon_label.setStyleSheet("font-size: 40px;")
            icon_label.setAlignment(Qt.AlignmentFlag.AlignCenter)
            controls_layout.addWidget(icon_label, i + 1, 1, alignment=Qt.AlignmentFlag.AlignCenter)

        controls_layout.setColumnStretch(0, 1)
        controls_layout.setColumnStretch(1, 0)
        controls_layout.setColumnStretch(2, 1)
        layout.addWidget(controls_container)

        # сообщение по центру
        self.message_label = QLabel()
        self.message_label.setAlignment(Qt.AlignmentFlag.AlignCenter)
        self.message_label.setStyleSheet("""
            QLabel {
                font-size: 16px;
                color: #000000;
                margin: 10px 0;
            }
        """)
        layout.addWidget(self.message_label)

    def start_key_remapping(self, button, action_key):
        if self.is_remapping:
            self.remapping_button.setStyleSheet(self.remapping_button.styleSheet().replace("border: 2px solid #2196F3;", "border: 1px solid #2196F3;"))
        self.is_remapping = True
        self.remapping_button = button
        self.remapping_action_key = action_key
        button.setStyleSheet(button.styleSheet().replace("border: 1px solid #2196F3;", "border: 2px solid #2196F3;"))

    def keyPressEvent(self, event):
        if not (self.is_remapping and self.remapping_button and self.remapping_action_key):
            return super().keyPressEvent(event)
        key_name = event.text().upper()
        if not (key_name and key_name.strip() and key_name in VALID_KEYS):
            self.message_label.setText("Используйте только английский алфавит и цифры")
            QTimer.singleShot(3000, lambda: self.message_label.setText(""))
            return
        is_already_assigned = any(
            (action_key != self.remapping_action_key and assigned_key == key_name)
            for action_key, assigned_key in {**self.key_assignments, **self.pending_key_assignments}.items()
        )
        if is_already_assigned:
            self.message_label.setText(f"Клавиша '{key_name}' уже используется")
            QTimer.singleShot(3000, lambda: self.message_label.setText(""))
            return
        self.remapping_button.setText(f"[{key_name}]")
        self.pending_key_assignments[self.remapping_action_key] = key_name
        self.remapping_button.setStyleSheet(self.remapping_button.styleSheet().replace("border: 2px solid #2196F3;", "border: 1px solid #4CAF50;"))
        self.is_remapping = False
        self.remapping_button = None
        self.remapping_action_key = None
        
        # сохраняем изменения
        new_assignments = {**self.key_assignments, **self.pending_key_assignments}
        save_key_bindings(new_assignments)
        self.key_assignments = new_assignments
        self.pending_key_assignments = {}
        
        # обновляем все кнопки
        for action_key, button in self.key_buttons.items():
            button.setText(f"[{self.key_assignments[action_key]}]")
        self.message_label.setText("Настройки сохранены")
        QTimer.singleShot(3000, lambda: self.message_label.setText(""))

    def get_key_assignments(self):
        return {**self.key_assignments, **self.pending_key_assignments}

    def update_key_assignments_display(self, assignments):
        self.key_assignments = assignments
        self.pending_key_assignments = {}
        for action_key, button in self.key_buttons.items():
            button.setText(f"[{self.key_assignments.get(action_key, '?')}]")

    def go_back(self):
        if self.parent_window:
            new_assignments = {**self.key_assignments, **self.pending_key_assignments}
            save_key_bindings(new_assignments)
            self.parent_window.key_assignments = new_assignments.copy()
            if self.previous_widget:
                self.parent_window.stacked_widget.setCurrentWidget(self.previous_widget)
                if hasattr(self.previous_widget, 'update_key_assignments'):
                    self.previous_widget.update_key_assignments(new_assignments)
            else:
                self.parent_window.show_main_menu()
            self.previous_widget = None 

class GameWindow(QMainWindow):
    def __init__(self):
        super().__init__()
        self.setWindowTitle("Камень, ножницы, бумага")
        self.setGeometry(100, 100, 600, 700)
        self.setStyleSheet("background-color: #FFFFFF; color: #000000;")
        self.stacked_widget = QStackedWidget()
        self.setCentralWidget(self.stacked_widget)
        self.key_assignments = load_key_bindings()
        self.init_widgets()
        self.connect_signals()

    def init_widgets(self):
        self.main_menu_widget = MainMenuWidget()
        self.game_vs_bot_widget = GameVsBotWidget(self, key_assignments=self.key_assignments)
        self.game_vs_friend_widget = GameVsFriendWidget(self, key_assignments=self.key_assignments)
        self.history_widget = HistoryWidget(self)
        self.settings_widget = SettingsWidget(self)
        for widget in [self.main_menu_widget, self.game_vs_bot_widget, self.game_vs_friend_widget, 
                      self.history_widget, self.settings_widget]:
            self.stacked_widget.addWidget(widget)
        self.stacked_widget.setCurrentWidget(self.main_menu_widget)

    def connect_signals(self):
        # главное меню
        self.main_menu_widget.btn_выход.clicked.connect(self.close)
        self.main_menu_widget.btn_игра_с_ботом.clicked.connect(self.show_game_vs_bot)
        self.main_menu_widget.btn_игра_с_другом.clicked.connect(self.show_game_vs_friend)
        self.main_menu_widget.btn_история_игр.clicked.connect(self.show_history)
        self.main_menu_widget.btn_settings.clicked.connect(self.show_settings)

        # Кнопки "Назад" 
        for widget in [self.game_vs_bot_widget, self.game_vs_friend_widget, self.history_widget]:
            widget.btn_back.clicked.connect(self.show_main_menu)

        # Кнопки "Настройки"
        for widget in [self.game_vs_bot_widget, self.game_vs_friend_widget, self.history_widget]:
            widget.btn_settings.clicked.connect(self.show_settings)

    def show_main_menu(self):
        self.stacked_widget.setCurrentWidget(self.main_menu_widget)

    def show_game_vs_bot(self):
        self.game_vs_bot_widget.reset_game()
        self.stacked_widget.setCurrentWidget(self.game_vs_bot_widget)
        self.game_vs_bot_widget.setFocus()

    def show_game_vs_friend(self):
        self.game_vs_friend_widget.reset_game()
        self.stacked_widget.setCurrentWidget(self.game_vs_friend_widget)
        self.game_vs_friend_widget.setFocus()

    def show_history(self):
        self.history_widget.load_history()
        self.stacked_widget.setCurrentWidget(self.history_widget)

    def show_settings(self):
        self.settings_widget.previous_widget = self.stacked_widget.currentWidget()
        self.settings_widget.update_key_assignments_display(self.key_assignments)
        self.stacked_widget.setCurrentWidget(self.settings_widget)
        self.settings_widget.setFocus()

    def show_results(self, player_score, bot_score):
        if hasattr(self, 'result_widget'):
            self.stacked_widget.removeWidget(self.result_widget)
            self.result_widget.deleteLater()
        current_widget = self.stacked_widget.currentWidget()
        game_mode = "bot" if current_widget == self.game_vs_bot_widget else "friend"
        self.result_widget = GameResultWidget(player_score, bot_score, game_mode, self)
        self.stacked_widget.addWidget(self.result_widget)
        self.stacked_widget.setCurrentWidget(self.result_widget)

    def reset_game_bot(self):
        self.game_vs_bot_widget.reset_game()

    def reset_game_friend(self):
        self.game_vs_friend_widget.reset_game()

    def save_key_settings(self):
        new_assignments = self.settings_widget.get_key_assignments()
        self.key_assignments = new_assignments.copy()  
        self.game_vs_bot_widget.update_key_assignments(new_assignments)
        self.game_vs_friend_widget.update_key_assignments(new_assignments)
        save_key_bindings(new_assignments)

def main():
    app = QApplication([])
    game_window = GameWindow()
    game_window.show()
    app.exec()
    
if __name__ == "__main__":
    main() 