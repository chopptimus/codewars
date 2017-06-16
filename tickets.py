def tickets(people):
    twenty_fives = 0
    fifties = 0
    for bill in people:
        if bill == 25:
            twenty_fives += 1
        elif bill == 50:
            twenty_fives -= 1
            fifties += 1
            if twenty_fives < 0:
                return "NO"
        else:
            if fifties == 0:
                twenty_fives -= 3
                if twenty_fives < 0:
                    return "NO"
            else:
                fifties -= 1
                twenty_fives -= 1
                if twenty_fives < 0 or fifties < 0:
                    return "NO"

    return "YES"

