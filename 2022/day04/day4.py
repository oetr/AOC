def contains(rooms):
    return (rooms[0] >= rooms[2] and rooms[1] <= rooms[3]) or (rooms[2] >= rooms[0] and rooms[3] <= rooms[1])


def overlaps(rooms):
    return (rooms[0] >= rooms[2] and rooms[0] <= rooms[3]) or \
            (rooms[1] >= rooms[2] and rooms[1] <= rooms[3]) or \
            (rooms[2] >= rooms[0] and rooms[2] <= rooms[1]) or \
            (rooms[3] >= rooms[0] and rooms[3] <= rooms[1])


# count the number of true values in a list
def count_true(l):
    return sum([1 for x in l if x])


def parse_line(line, part):
    two_entries = line.strip().split(",")
    rooms = [int(num) for entry in two_entries for num in entry.split("-")]
    if part == 1:
        c = contains(rooms)
    else:
        c = overlaps(rooms)
    return c


# part 1
with open("day4.txt", "r") as f:
    data = [parse_line(line, 1) for line in f]

count_true(data)

# part 2
with open("day4.txt", "r") as f:
    data = [parse_line(line, 2) for line in f]

count_true(data)
