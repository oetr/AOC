import csv

def day2(filename, part_no):
    enemy_lut = {'A': 0, 'B': 1, 'C': 2}
    player_lut = {'X': 0, 'Y': 1, 'Z': 2}
    needs_to_lut = {'X': -1, 'Y': 0, 'Z': 1}
    move_lut = [1, 2, 3]
    diff_lut = {0: 3,        # draw
                1: 6, -2: 6, # win
                2: 0, -1: 0} # lose
    score = 0
    with open(filename) as f:
        for row in csv.reader(f, delimiter=" "):
            enemy_move = enemy_lut[row[0]]
            if (part_no == 1):
                player_move = player_lut[row[1]]
            else:
                player_move = (enemy_move + needs_to_lut[row[1]]) % 3
            score += move_lut[player_move] + diff_lut[player_move - enemy_move]
    return score

part1_score = day2("day2.txt", 1) # 9177
part2_score = day2("day2.txt", 2) # 12111
