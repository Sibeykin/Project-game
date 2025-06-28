text = input("напишите предложения ")
if text.startswith('abc'):
    result = 'www' + text[3:]
else:
    result = text + 'zzz'
print("результат предложения:", result)
