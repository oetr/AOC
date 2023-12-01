import numpy as np

start_marker = 100
goal_marker = 200
start_elevation = 0
goal_elevation = ord('z') - ord('a')


def read_input(filepath):
    def line_to_data(line):
        result = []
        for c in line:
            if c >= 'a' and c <= 'z':
                result.append(ord(c) - ord('a'))
            elif c == 'S':
                result.append(start_marker)
            elif c == 'E':
                result.append(goal_marker)
        return np.array(result, np.uint8)

    with open(filepath, "r") as f:
        return np.array([line_to_data(line.strip()) for line in f])


def get_position(position, y_add, x_add, y_range, x_range):
    new_y = position[0] + y_add
    new_x = position[1] + x_add
    if new_y >= 0 and new_x >= 0 and new_y < y_range and new_x < x_range:
        return (new_y, new_x)
    else:
        return False


def find_neighbors(position, visited, data_shape):
    results = []
    left = get_position(position, 0, -1, *data_shape)
    right = get_position(position, 0, 1, *data_shape)
    down = get_position(position, 1, 0, *data_shape)
    up = get_position(position, -1, 0, *data_shape)
    if left and (not visited[left]): results.append(left)
    if right and (not visited[right]): results.append(right)
    if down and (not visited[down]): results.append(down)
    if up and (not visited[up]): results.append(up)
    return results


def substitute_markers(elevation):
    if elevation == goal_marker: return goal_elevation
    if elevation == start_marker: return start_elevation
    return elevation


def is_step_allowed(data, from_position, to_position):
    from_elevation = substitute_markers(data[from_position])
    to_elevation = substitute_markers(data[to_position])
    return (to_elevation == from_elevation + 1) or (to_elevation <= from_elevation)


def find_distance_to_goal(data, start, goal, distances = None):
    visited = np.zeros(data.shape, bool)
    frontier = [start]  # add on the right, take from the left
    distance_from_start = np.zeros(data.shape) + np.inf
    distance_from_start[start] = 0

    # pick one, find neighbors, add them to frontier, if we have reached the goal: done
    while True:
        if frontier == []:
            return np.inf

        position = frontier.pop(0)
        visited[position] = True
        neighbors = find_neighbors(position, visited, data.shape)

        if position == goal:
            # TODO: update distances for all positions seen along the way
            return distance_from_start[position]

        for neighbor in neighbors:
            if not (neighbor in frontier) and is_step_allowed(data, position, neighbor):
                frontier.append(neighbor)
                if distance_from_start[neighbor] == np.inf:
                    distance_from_start[neighbor] = distance_from_start[position] + 1


# part 1
data = read_input("day12.txt")
start = np.where(data==start_marker)
goal = np.where(data==goal_marker)
find_distance_to_goal(data, start, goal)

# part 2
distances = np.zeros(data.shape) + np.inf
start = np.where(data==start_marker)
distances[start] = find_distance_to_goal(data, start, goal, distances)
suitable_start_indices = np.where(data == 0)
for y, x in zip(suitable_start_indices[0], suitable_start_indices[1]):
    start = (y, x)
    distances[start] = find_distance_to_goal(data, start, goal, distances)

np.min(distances)
