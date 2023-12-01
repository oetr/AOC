def priority(c):
    if c >= 'a' and c <= 'z':
        return ord(c) - ord('a') + 1
    return ord(c) - ord('A') + 27


# part 1
with open("day3.txt", "r") as f:
    sum_priorities = 0
    for contents in f:
        half = round((len(contents)-1)/2)
        compartment_1 = contents[0:half]
        compartment_2 = contents[half:-1] # get rid of '\n'
        for c in compartment_1:
            if c in compartment_2:
                duplicate = c

        sum_priorities += priority(duplicate)

print("part1: {}".format(sum_priorities))


# part 2
def add_chars_to_set(s):
    char_set = set()
    for c in s:
        char_set.add(c)

    return char_set


with open("day3_2.txt", "r") as f:
    sum_priorities = 0
    elf_counter = 0
    sets = [0]*3
    sum_priorities = 0
    for i, contents in enumerate(f):
        sets[i % 3] = add_chars_to_set(contents[:-1])  # get rid of '\n'
        if (((i+1) % 3) == 0):
            badge = sets[0] & sets[1] & sets[2]
            sum_priorities += priority(badge.pop())

print("part2: {}".format(sum_priorities))
