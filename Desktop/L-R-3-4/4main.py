def get_final_score(scores):
   
    max_score = max(scores)
    min_score = min(scores)
    scores.remove(max_score)
    scores.remove(min_score)

    final_score = sum(scores) / len(scores)

    return final_score

def get_scores():
 
    num_judges = int(input("Введите количество судей (не менее 3): "))
    while num_judges < 3:
        num_judges = int(input("Количество судей должно быть не менее 3. Введите еще раз: "))

    scores = []
    for i in range(1, num_judges + 1):
        score = float(input(f"Введите оценку судьи {i}: "))
        scores.append(score)

    return scores

scores = get_scores()
final_score = get_final_score(scores)
print(f"Итоговая оценка спортсмена: {final_score:.2f}")




