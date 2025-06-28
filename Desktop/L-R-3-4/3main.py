def calculate_student_scores():
  
    failed_students = 0
    total_score = 0
    num_students = 0

    while True:
        print(f"Студент {num_students + 1}:")
        scores = []
        for i in range(1, 5):
            score = float(input(f"  Оценка {i}: "))
            if score == 0:
                break
            scores.append(score)

        if not scores:
            break

        if any(score < 3 for score in scores):
            failed_students += 1

        total_score += sum(scores)

        num_students += 1

    average_score = total_score / (num_students * 4)

    return failed_students, average_score

failed, average = calculate_student_scores()
print(f"Количество неуспевающих студентов: {failed}")
print(f"Средний балл группы: {average:.2f}")



