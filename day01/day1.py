# load and sort is too easy
calories_per_elf = []
with open("day1.txt", "r") as f:
    calories = 0
    for entry in f:
        if entry == "\n":
            calories_per_elf.append(calories)
            calories = 0
        else:
            calories += int(entry)
# sort in the ascending order
sorted(calories_per_elf, reverse=True)[0:3] # [70698, 69773, 66172]


# we keep track of the top N elfs with most calories while reading the list
N = 3        
with open("day1.txt", "r") as f:
    reverse_topN_calories = [0]*N
    tmp_calories = 0
    # read all lines from the file
    for entry in f:
        if entry == "\n":
            pivot = -1
            for i, most_calories in enumerate(reverse_topN_calories):
                if tmp_calories > most_calories:
                    pivot = i
                    break
            if pivot > -1:
                tail = reverse_topN_calories[pivot:-1]
                reverse_topN_calories = reverse_topN_calories[0:pivot] + [tmp_calories] + tail
            tmp_calories = 0
        else:
            tmp_calories += int(entry)

    print(reverse_topN_calories) # [70698, 69773, 66172]
