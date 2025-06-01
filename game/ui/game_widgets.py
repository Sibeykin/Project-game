from PyQt6.QtWidgets import (QWidget, QVBoxLayout, QPushButton, 
                             QLabel, QScrollArea, QFrame, QHBoxLayout)
from PyQt6.QtCore import Qt
from PyQt6.QtGui import QFont
from utils.file_utils import load_history, load_keybindings

class MainMenuWidget(QWidget):
    def __init__(self):
        super().__init__()
        self.init_ui()
        
    def init_ui(self):
        layout = QVBoxLayout()
        layout.setSpacing(20)
        layout.setContentsMargins(50, 50, 50, 50)
        
        # заголовок
        title = QLabel("Камень, ножницы, бумага")
        title.setFont(QFont("Arial", 24, QFont.Weight.Bold))
        title.setAlignment(Qt.AlignmentFlag.AlignCenter)
        layout.addWidget(title)
        
        # кнопки
        buttons = [
            ("Играть", "play"),
            ("История", "history"),
            ("Настройки", "settings"),
            ("Выход", "exit")
        ]
        
        for text, name in buttons:
            btn = QPushButton(text)
            btn.setObjectName(name)
            btn.setFont(QFont("Arial", 14))
            btn.setMinimumHeight(50)
            btn.setStyleSheet("""
                QPushButton {
                    background-color: #4CAF50;
                    color: white;
                    border: none;
                    border-radius: 5px;
                }
                QPushButton:hover {
                    background-color: #45a049;
                }
                QPushButton:pressed {
                    background-color: #3d8b40;
                }
            """)
            layout.addWidget(btn)
            
        layout.addStretch()
        self.setLayout(layout)

class GameModesWidget(QWidget):
    def __init__(self):
        super().__init__()
        self.init_ui()
        
    def init_ui(self):
        layout = QVBoxLayout()
        layout.setSpacing(20)
        layout.setContentsMargins(50, 50, 50, 50)
        
        # заголовок
        title = QLabel("Выберите режим")
        title.setFont(QFont("Arial", 24, QFont.Weight.Bold))
        title.setAlignment(Qt.AlignmentFlag.AlignCenter)
        layout.addWidget(title)
    
        for text, name in modes:
            btn = QPushButton(text)
            btn.setObjectName(name)
            btn.setFont(QFont("Arial", 14))
            btn.setMinimumHeight(50)
            btn.setStyleSheet("""
                QPushButton {
                    background-color: #4CAF50;
                    color: white;
                    border: none;
                    border-radius: 5px;
                }
                QPushButton:hover {
                    background-color: #45a049;
                }
                QPushButton:pressed {
                    background-color: #3d8b40;
                }
            """)
            layout.addWidget(btn)
        layout.addStretch()
        self.setLayout(layout)

class HistoryWidget(QWidget):
    def __init__(self):
        super().__init__()
        self.init_ui()
        
    def init_ui(self):
        layout = QVBoxLayout()
        layout.setSpacing(20)
        layout.setContentsMargins(50, 50, 50, 50)
        
        # история игр заголовок
        title = QLabel("История игр")
        title.setFont(QFont("Arial", 24, QFont.Weight.Bold))
        title.setAlignment(Qt.AlignmentFlag.AlignCenter)
        layout.addWidget(title)
        
        # прокрут
        scroll = QScrollArea()
        scroll.setWidgetResizable(True)
        scroll.setStyleSheet("""
            QScrollArea {
                border: none;
                background-color: transparent;
            }
        """)
        
        # контейнер для записей
        self.history_container = QWidget()
        self.history_layout = QVBoxLayout()
        self.history_layout.setSpacing(10)
        self.history_container.setLayout(self.history_layout)
        scroll.setWidget(self.history_container)
        layout.addWidget(scroll)
        
        # кнопка "Назад" в истории игр 
        back_btn = QPushButton("Назад")
        back_btn.setObjectName("back")
        back_btn.setFont(QFont("Arial", 14))
        back_btn.setMinimumHeight(50)
        back_btn.setStyleSheet("""
            QPushButton {
                background-color: #4CAF50;
                color: white;
                border: none;
                border-radius: 5px;
            }
            QPushButton:hover {
                background-color: #45a049;
            }
            QPushButton:pressed {
                background-color: #3d8b40;
            }
        """)
        layout.addWidget(back_btn)
        
        self.setLayout(layout)
        
    def update_history(self):
        while self.history_layout.count():
            item = self.history_layout.takeAt(0)
            if item.widget():
                item.widget().deleteLater()
        
        # загружаем и отображаем историю
        history = load_history()
        for entry in history:
            frame = QFrame()
            frame.setFrameStyle(QFrame.Shape.StyledPanel)
            frame.setStyleSheet("""
                QFrame {
                    background-color: white;
                    border-radius: 5px;
                    padding: 10px;
                }
            """)
            
            entry_layout = QVBoxLayout()
            
            # дата и время для истории игр
            date_label = QLabel(entry.get('date', ''))
            date_label.setFont(QFont("Arial", 10))
            entry_layout.addWidget(date_label)
            
            # режим и результат в истории игр
            mode_label = QLabel(f"Режим: {entry.get('mode', '')}")
            mode_label.setFont(QFont("Arial", 12))
            entry_layout.addWidget(mode_label)
            result_label = QLabel(f"Результат: {entry.get('result', '')}")
            result_label.setFont(QFont("Arial", 12))
            entry_layout.addWidget(result_label)
            frame.setLayout(entry_layout)
            self.history_layout.addWidget(frame)
        self.history_layout.addStretch()

class SettingsWidget(QWidget):
    def __init__(self):
        super().__init__()
        self.init_ui()
        
    def init_ui(self):
        layout = QVBoxLayout()
        layout.setSpacing(20)
        layout.setContentsMargins(50, 50, 50, 50)
        
        # заголовок
        title = QLabel("Настройки")
        title.setFont(QFont("Arial", 24, QFont.Weight.Bold))
        title.setAlignment(Qt.AlignmentFlag.AlignCenter)
        layout.addWidget(title)
        
        # настройки клавиш
        keybindings = load_keybindings()
        
        for action, key in keybindings.items():
            frame = QFrame()
            frame.setFrameStyle(QFrame.Shape.StyledPanel)
            frame.setStyleSheet("""
                QFrame {
                    background-color: white;
                    border-radius: 5px;
                    padding: 10px;
                }
            """)
            
            key_layout = QHBoxLayout()
            action_label = QLabel(action)
            action_label.setFont(QFont("Arial", 12))
            key_layout.addWidget(action_label)
            key_btn = QPushButton(key)
            key_btn.setObjectName(f"key_{action}")
            key_btn.setFont(QFont("Arial", 12))
            key_btn.setMinimumWidth(100)
            key_btn.setStyleSheet("""
                QPushButton {
                    background-color: #4CAF50;
                    color: white;
                    border: none;
                    border-radius: 5px;
                }
                QPushButton:hover {
                    background-color: #45a049;
                }
                QPushButton:pressed {
                    background-color: #3d8b40;
                }
            """)
            key_layout.addWidget(key_btn)
            
            frame.setLayout(key_layout)
            layout.addWidget(frame)
        # кнопка "Назад" в настройках
        back_btn = QPushButton("Назад")
        back_btn.setObjectName("back")
        back_btn.setFont(QFont("Arial", 14))
        back_btn.setMinimumHeight(50)
        back_btn.setStyleSheet("""
            QPushButton {
                background-color: #4CAF50;
                color: white;
                border: none;
                border-radius: 5px;
            }
            QPushButton:hover {
                background-color: #45a049;
            }
            QPushButton:pressed {
                background-color: #3d8b40;
            }
        """)
        layout.addWidget(back_btn)
        
        layout.addStretch()
        self.setLayout(layout) 