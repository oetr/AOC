import csv
import numpy as np
import re

def read_input(filename):
    with open(filename) as f:
        return [list(map(int, row)) for row in csv.reader(f, delimiter=",")]
            
data = np.array(read_input("day18.txt"))
#data = np.array(read_input("day18-test.txt"))

# MAP: coordinates to droplet ids
pose_lut = {}
up = np.array([0,0,+1])
down = np.array([0,0,-1])
left = np.array([-1,0,0])
right = np.array([+1,0,0])
back = np.array([0,-1,0])
front = np.array([0,+1,0])
neighbor_coordinates = [up, down, left, right, back, front]
for pose in data:
    for neighbor_coords in neighbor_coordinates:
        pose_lut[str(pose + neighbor_coords)] = None

# fill pose ids
for i, pose in enumerate(data):
    pose_lut[str(pose)] = i

# traverse and add all surfaces without neighbors
surface_area = 0
for pose in data:
    for neighbor_coords in neighbor_coordinates:
        if pose_lut[str(pose + neighbor_coords)] is None:
            surface_area += 1
