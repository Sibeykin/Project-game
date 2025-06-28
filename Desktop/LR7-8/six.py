text = input("напиши  предложение: ")
if len(text) >= 3:
    print("3 символ:", text[2])
if len(text) >= 6:
    print("6 символ:", text[5])
if len(text) >= 9:
    print("9 символ:", text[8])
else:
    print("напиши предложение с большим количеством символов для вывода ")
