import numpy as np


def read_input(filename):
    with open(filename, 'r') as f:
        return [int(line.strip()) for line in f]
    pass


def list_to_linked_list(data):
    result = []
    for i in range(len(data)):
        result.append(Node(data[i], None, None))

    # connect the nodes
    for i in range(len(data)):
        result[i].prev = result[i-1 % len(data)]
        result[i].next = result[(i+1) % len(data)]
        result[i].next_to_move = result[(i+1) % len(data)]
    return result[0]


def find_zero(node):
    if node.data == 0:
        return node

    initial_node = node
    node = node.next
    while node.data != 0:
        node = node.next
        if node == initial_node:
            return False

    return node


def print_list(d, multby=1):
    i = 0
    initial_node = d
    while True:
        print("{}: {}".format(i, d.data*multby))
        d = d.next
        if d == initial_node:
            break
        i += 1


def linked_list_to_list(d):
    result = []
    i = 0
    initial_node = d
    while True:
        result.append(d.data)
        d = d.next
        if d == initial_node:
            break
        i += 1

    return result


# double linked list node (actually the back links are not needed here)
class Node:
    data = None
    next = None
    prev = None
    next_to_move = None

    def __init__(self, data, prev, next):
        self.data = data
        self.prev = prev
        self.next = next

    # move the node "data" steps (forward or backward)
    # return the node that was next before moving
    def move(self, length, multby=1):
        if self.data == 0 or self.data*multby % (abs(length) - 1) == 0:
            return

        # link prev to next
        self.prev.next = self.next
        self.next.prev = self.prev

        n_steps = self.data * multby % (length - 1) - 1
        node_to_insert_after = self.next
        for i in range(n_steps):
            node_to_insert_after = node_to_insert_after.next

        self.next = node_to_insert_after.next
        self.prev = node_to_insert_after
        node_to_insert_after.next = self
        self.next.prev = self


# data = read_input("day20-test.txt")
data = read_input("day20.txt")


if True: # part 2
    multby = 811589153
    n_rounds = 10
else: # part 1
    multby = 1
    n_rounds = 1

d = list_to_linked_list(data)

start = d
for _ in range(n_rounds):
    for _ in range(len(data)):
        d.move(len(data), multby)
        d = d.next_to_move

cipher = np.array(linked_list_to_list(start))*multby
zero = np.where(cipher==0)[0][0]
print(np.sum(cipher[np.mod(zero + np.array([1000, 2000, 3000]), len(data))]))
