import re

def print_state(state):
    top = []
    for i, stack in enumerate(state):
        if len(stack) > 0:
            top.append(stack[-1])
        print("{}: {}".format(i+1, stack))

    print("top: {}".format("".join(top)))


def make_state(stacks, n_stacks):
    state = [[] for _ in range(n_stacks)]
    for i, stack in stacks:
        print_state(state)
        state[i].append(stack)
    return state


def move9000(state, n, src, dst):
    print("move: {} {} {}".format(n, src, dst))
    if n == 0:
        return
    container = state[src][-1]
    state[src] = state[src][:-1]
    state[dst].append(container)
    move(state, n-1, src, dst)


def move(state, n, src, dst):
    print("move 9001: {} {} {}".format(n, src, dst))
    if n == 0:
        return
    container = state[src][-n:]
    state[src] = state[src][:-n]
    state[dst] += container


# part 1
header = []
actions = []
with open("day5.txt", "r") as f:
    # header
    n_stacks = 0
    while True:
        if line == "\n":
            header.reverse()
            n_stacks += 1
            state = make_state(header, n_stacks)
            break
        line = f.readline()
        m = re.findall(r"\[([A-Z]+?)\]|(    )", line)
        for i, entry in enumerate(m):
            if entry[0] != '':
                if i > n_stacks:
                    n_stacks = i
                header.append([i, entry[0]])
    # actions
    for line in f:
        m = re.match(r"move ([0-9]+?) from ([0-9]+?) to ([0-9]+?)", line)
        action = list(map(int, m.groups()))
        actions.append(action)

        move(state, action[0], action[1]-1, action[2]-1)


print_state(state)
