text = input("напишите предложение состоящее токо из чисел: ")
digit_count = sum(1 for char in text if char.isdigit())
print("количество цифр в строке:", digit_count)
