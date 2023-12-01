import csv
import numpy as np

def read_input(filename):
    with open(filename) as f:
        return [list(map(int, row)) for row in csv.reader(f, delimiter=",")]

up = np.array([0,0,+1])
down = np.array([0,0,-1])
left = np.array([-1,0,0])
right = np.array([+1,0,0])
back = np.array([0,-1,0])
front = np.array([0,+1,0])
neighbor_coordinates = [up, down, left, right, back, front]


# union-find
class UF:
    ids = []
    szs = []
    count = 0

    def __init__(self, n):
        self.count = n
        self.ids = [i for i in range(n)]
        self.szs = [1 for _ in range(n)]

    def connected(self, p, q):
        return self.find(p) == self.find(q)

    def find(self, p):
        while (p != self.ids[p]):
            p = self.ids[p]
        return p

    def union(self, p, q):
        i = self.find(p)
        j = self.find(q)
        if i == j:
            return

        if self.szs[i] < self.szs[j]:
            self.ids[i] = j
            self.szs[j] += self.szs[i]
        else:
            self.ids[j] = i
            self.szs[i] += self.szs[j]

        self.count -= 1

    def is_only_connected_to_self(self, ID):
        return self.ids[ID] == ID

    def print(self):
        for i, ID in enumerate(self.ids):
            print("{} -> {}".format(i, ID))

    def find_all_connected_to_self(self):
        result = []
        for i, ID in enumerate(self.ids):
            if i == ID:
                result.append(ID)
                print("{} -> {}".format(i, ID))

        return result

data = np.array(read_input("day18.txt"))
data = np.array(read_input("day18-test.txt"))

# MAP: coordinates to droplet ids
droplets = {}
for i, pose in enumerate(data):
    droplets[str(pose)] = i

air_cubes = {}
air_coords = []
air_cube_id = 0
for pose in data:
    for neighbor_coords in neighbor_coordinates:
        xyz = pose + neighbor_coords
        if not (str(xyz) in droplets):
            if not (str(xyz) in air_cubes):
                print("{} -- {}".format(pose, xyz))
                air_coords.append(xyz)
                air_cubes[str(xyz)] = air_cube_id
                air_cube_id += 1
    pass

open_air_id = air_cube_id
air_cubes['open air'] = open_air_id

cc = UF(len(air_cubes))

# compute connected components for the air cubes
for xyz in air_coords:
    address = str(xyz)
    for neighbor_coords in neighbor_coordinates:
        neighbor = str(xyz + neighbor_coords)
        if not (neighbor in droplets):
            if neighbor in air_cubes:  # if neighbor is a known air cube
                cc.union(air_cubes[address], air_cubes[neighbor])
            else:  # connected to open air
                cc.union(air_cubes[address], open_air_id)
    pass


# traverse and add all surfaces without neighbors
# TODO: air pockets can be connected to each other also!
surface_area = 0
for ID in [ID for ID in range(len(air_cubes)-1) if cc.connected(open_air_id, ID)]:
    pose = air_coords[ID]
    for neighbor_coords in neighbor_coordinates:
        address = str(pose + neighbor_coords)
        if address in droplets:
            surface_area += 1

for pose in data:
    for neighbor_coords in neighbor_coordinates:
        address = str(pose + neighbor_coords)
        if address in air_cubes and cc.connected(open_air_id, air_cubes[address]):
            surface_area += 1
    pass

print(surface_area)
not_connected_to_air = [ID for ID in range(len(air_cubes)-1) if not cc.connected(open_air_id, ID)]
connected_to_self = cc.find_all_connected_to_self()

len(cc.find_all_connected_to_self())
for xyz in droplets:
    if xyz in air_cubes:
        print("a")
