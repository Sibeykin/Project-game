# максимальный счет 
MAX_SCORE = 3

# эмодзи для выбора
EMOJIS = {
    "rock": "🗿",
    "scissors": "✂️",
    "paper": "📄"
}

# игровые константы
CHOICES = ["камень", "ножницы", "бумага"]

# стили кнопок
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

# валидные клавиши
VALID_KEYS = {
    'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M',
    'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
    '1', '2', '3', '4', '5', '6', '7', '8', '9', '0'
} 