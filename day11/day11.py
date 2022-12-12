import re
import numpy as np

def eval_operand(old_level, v):
    if v != 'old':
        return int(v)
    else:
        return old_level

def update_worry_level(worry_level, val0, op, val1):
    val0 = eval_operand(worry_level, val0)
    val1 = eval_operand(worry_level, val1)
    match op:
        case '+': return val0 + val1
        case '-': return val0 - val1
        case '*': return val0 * val1
        case other: raise ValueError()


def make_complex(n, n_monkeys):
    return np.array([n]*n_monkeys, np.int16)

# operations mod tests
def plus_mod(complex_n, n, tests):
    return np.mod(complex_n + n, tests)

def minus_mod(complex_n, n, tests):
    return np.mod(complex_n - n, tests)

def mult_mod(complex_n, n, tests):
    return np.mod(complex_n * n, tests)


def update_worry_level_complex(worry_level, val0, op, val1, tests):
    val0 = eval_operand(worry_level, val0)
    val1 = eval_operand(worry_level, val1)
    match op:
        case '+': return plus_mod(val0, val1, tests)
        case '-': return minus_mod(val0, val1, tests)
        case '*': return mult_mod(val0, val1, tests)
        case other: raise ValueError()

def monkey_throw(monkey_i, part_no):
    def throw(monkey_nr, worry_level):
        levels[monkey_nr].append(worry_level)
    
    for level in levels[monkey_i]:
        times_inspected[monkey_i] += 1
        if part_no == 1:
            new_level = int(np.floor(update_worry_level(level, *operations[monkey_i]) / 3.0))
            if new_level % tests[monkey_i] == 0:
                throw(if_trues[monkey_i], new_level)
            else:
                throw(if_falses[monkey_i], new_level)
        else:
            new_level = update_worry_level_complex(level, *operations[monkey_i], tests)
            if new_level[monkey_i] == 0:
                throw(if_trues[monkey_i], new_level)
            else:
                throw(if_falses[monkey_i], new_level)
        
    levels[monkey_i] = []

# read
part_no = 2
with open("day11.txt", "r") as f:
    levels = []
    operations = []
    tests = []
    if_trues = []
    if_falses = []
    

    while True:
        monkey_nr = f.readline()
        if not monkey_nr:
            break

        levels.append([int(n) for n in re.findall(r'\d+', f.readline())])        
            
        operations.append(list(re.findall(r'new = ([old|0-9]+) ([\+\-\*]) ([old|0-9]+)', f.readline())[0]))
        # operations.append()
        np.array(tests.append(int(re.findall(r'\d+', f.readline())[0])))
        if_trues.append(int(re.findall(r'\d+', f.readline())[0]))
        if_falses.append(int(re.findall(r'\d+', f.readline())[0]))
        newline = f.readline()

        if part_no == 2:
            n_monkeys = len(levels)
            part2_levels = []
            for monkey_levels in levels:
                levels_tmp = []
                for level in monkey_levels:
                    levels_tmp.append(make_complex(level, n_monkeys))
                part2_levels.append(levels_tmp)

    if part_no == 2:
        levels = part2_levels

    times_inspected = [0]*len(levels)


# run for N rounds
for i in range(10000):
    print(i)
    for monkey_i in range(len(levels)):
        monkey_throw(monkey_i, part_no)

times_inspected.sort(reverse=True)
times_inspected[0] * times_inspected[1]
