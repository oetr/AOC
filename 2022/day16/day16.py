# pages 155, 207 in the ADM-book
import numpy as np
import re

# done: make graph
# done: find shortest path to target
# go to target if cost < time allows
# open target
# remove target from the list
# continue


def read_input(filepath):
    def line_to_data(line):
        matches = re.findall(r'Valve ([A-Z]{2}) has flow rate=([0-9]+); tunnel[s]* lead[s]* to valve[s]* ([A-Z, ]*)', line)[0]
        return [matches[0], int(matches[1]), matches[2].split(', ')]

    with open(filepath, "r") as f:
        all_data = [line_to_data(line.strip()) for line in f]
        valves_to_i = dict([[d[0], i] for i, d in enumerate(all_data)])
        i_to_valves = [d[0] for d in all_data]
        adjacency_matrix = np.zeros((len(all_data), len(all_data)), bool)
        pressures = np.zeros(len(all_data), np.uint32)
        adjacent_valves = [[] for _ in range(len(all_data))]
        for i, entry in enumerate(all_data):
            pressures[i] = all_data[i][1]
            for vertex in entry[2]:
                adjacent_valves[i].append(valves_to_i[vertex])
                adjacency_matrix[i, valves_to_i[vertex]] = True

        return adjacency_matrix, pressures, adjacent_valves, valves_to_i, i_to_valves


adjacency_matrix, pressures, adjacent_valves, valves_to_i, i_to_valves = read_input("day16.txt")

n_vertices = len(pressures)
weight = np.ones((n_vertices, n_vertices), np.uint64)*(2**32-1)
weight[np.where(adjacency_matrix)] = 1
np.fill_diagonal(weight, 0)

def floyd(weight):
    for k in range(n_vertices):
        for i in range(n_vertices):
            for j in range(n_vertices):
                through_k = weight[i, k] + weight[k, j]
                if through_k < weight[i, j]:
                    weight[i, j] = through_k

    return weight

weight = floyd(weight)

# go, open, compute total pressure
open_valves = np.zeros(len(i_to_valves), bool)
open_valves[np.where(pressures==0)] = True
valves = np.where(np.bitwise_and(np.bitwise_not(open_valves), pressures > 0))[0]


def find_best_path(open_valves, current_valve):
    valves = np.where(np.bitwise_and(np.bitwise_not(open_valves), pressures > 0))[0]
    if len(valves) == 0:
        print("DONE!")
        return
    for valve in valves:
        open_valves_copy = open_valves.copy()
        open_valves_copy[valve] = True
        find_best_path(open_valves, valve)

#find_best_path(open_valves, valves_to_i['AA'])

possible_pressures_indices = np.where(pressures>0)

class State:
    positions = [0, 0]
    pressure = 0
    time_left = 0
    path = []
    open_valves = []
    targets = []
    n_workers = 1
    current_targets = []

    def __init__(self, positions, pressure, time_left, path, open_valves, current_targets, n_workers=1):
        self.positions = positions
        self.pressure = pressure
        self.time_left = time_left
        self.path = path.copy()
        self.open_valves = open_valves.copy()
        self.targets = []
        self.current_targets = []
        self.n_workers = n_workers
        for i, v in enumerate(open_valves):
            if (not v) and pressures[i] > 0:
                self.targets.append(i)

    def move(self):
        # get all unopen valves with nonzero pressure
        # add reacheable ones to the next state
        possible_actions = []
        for target in self.targets:
            # can target valve be reached and opened in time?
            target_time_left = self.time_left - weight[self.valve, target] - 1
            if target_time_left >= 0:
                pressure = self.pressure + pressures[target]*target_time_left
                open_valves = self.open_valves.copy()
                open_valves[target] = True
                possible_actions.append(State(target, pressure, target_time_left,
                                              self.path + [target, 'open'], open_valves))

        return possible_actions

    def print(self):
        print("{}['{}'], time: {}, total pressure: {}, len so far: {}".format(self.valve, i_to_valves[self.valve], self.time_left, self.pressure, len(self.path)))

    def is_open(self):
        return self.open_valves[self.valve]


# do exhaustive search until time runs out
# get current position
# add all possible actions to the queue
# decrement time
open_valves = np.zeros(len(i_to_valves), bool)
pressures[np.where(np.bitwise_not(open_valves))]
possible_pressures_indices = np.where(pressures>0)

initial_valve = valves_to_i['AA']
queue = [State(valve=initial_valve, pressure=0, time_left=30, path=[initial_valve], open_valves=open_valves)]
last_time_left = 30
iterations = 0
best_total = 0
while len(queue) > 0:
    current_state = queue.pop(-1)

    if np.sum(current_state.open_valves[possible_pressures_indices]) == len(possible_pressures_indices[0]):
        if current_state.pressure > 0:
            if best_total < current_state.pressure:
                print("Total: {}".format(current_state.pressure))
            best_total = max(best_total, current_state.pressure)
        continue

    if current_state.time_left == 0:
        if current_state.pressure > 0:
            if best_total < current_state.pressure:
                print("Total: {}".format(current_state.pressure))

            best_total = max(best_total, current_state.pressure)
        continue

    possible_actions = current_state.move()

    if len(possible_actions) == 0:
        if current_state.pressure > 0:
            if best_total < current_state.pressure:
                print("Total+: {}".format(current_state.pressure))
            best_total = max(best_total, current_state.pressure)
    else:
        queue = queue + possible_actions
