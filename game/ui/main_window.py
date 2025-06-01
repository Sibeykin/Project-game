from PyQt6.QtWidgets import QMainWindow, QStackedWidget
from .widgets import (MainMenuWidget, GameVsBotWidget, GameVsFriendWidget, 
                              HistoryWidget, SettingsWidget, GameResultWidget)
from utils.file_utils import load_key_bindings, save_key_bindings

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

        # кнопки "Назад"
        for widget in [self.game_vs_bot_widget, self.game_vs_friend_widget, self.history_widget]:
            widget.btn_back.clicked.connect(self.show_main_menu)

        # кнопки "Настройки"
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

    def go_back_from_settings(self):
        new_assignments = self.settings_widget.get_key_assignments()
        save_key_bindings(new_assignments)
        self.key_assignments = new_assignments.copy()

        # обновляем привязки в игровых виджетах
        self.game_vs_bot_widget.update_key_assignments(self.key_assignments)
        self.game_vs_friend_widget.update_key_assignments(self.key_assignments)

        if self.settings_widget.previous_widget:
            self.stacked_widget.setCurrentWidget(self.settings_widget.previous_widget)
            self.settings_widget.previous_widget = None 
        else:
            self.show_main_menu()

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