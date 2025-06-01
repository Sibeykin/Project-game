from PyQt6.QtWidgets import QApplication

from ui.main_window import GameWindow
from utils.file_utils import init_files

def main():
    app = QApplication([])
    
    # инициализация файлов 
    init_files()
    
    game_window = GameWindow()
    game_window.show()
    app.exec()

if __name__ == "__main__":
    main() 
