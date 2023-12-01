import numpy as np
from z3 import *
import re

def parse_line(line):
    m = re.findall(r"([a-z]+): ([a-z]+) ([*\-+/]) ([a-z]+)", line)
    if len(m) == 0:
        m = re.findall(r"([a-z]+): ([0-9]+)", line)[0]
        return [m[0], int(m[1])]
    return list(m[0])


def read_input(filename):
    with open(filename, 'r') as f:
        return [parse_line(line.strip()) for line in f]
    pass

def lval(instruction):
    return instruction[0]

def var0(instruction):
    return instruction[1]

def var1(instruction):
    return instruction[3]

def op(instruction):
    return instruction[2]

def op_apply(op, v0, v1):
    if op=='-': return v0 - v1
    if op=='*': return v0 * v1
    if op=='/': return v0 / v1
    if op=='+': return v0 + v1


instructions = read_input("day21.txt")
instructions = read_input("day21-test.txt")
vars = {}
vals = []
# oversee and tag
for i, instruction in enumerate(instructions):
    vars[lval(instruction)] = i
    if len(instruction) == 2:
        if lval(instruction) == 'humn':
            # vals.append(None)
            print('--------------- {}'.format(var0(instruction)))
            vals.append(var0(instruction))
        else:
            vals.append(var0(instruction))
    else:
        vals.append(None)

#for starting_monkey in vars:
stack = [vars['root']]
while len(stack) > 0:
    # get var from stack
    var = stack[-1]
    if vals[var]: # evaled?
        stack.pop(-1)
    else: # eval each
        instruction = instructions[var]
        # if lval(instruction) == 'humn':
        #     vals[vars['humn']] = 3509819803065
        #     stack.pop(-1)
        v0 = vars[var0(instruction)]
        v1 = vars[var1(instruction)]
        if vals[v0] and vals[v1]:
            val0 = vals[v0]
            val1 = vals[v1]
            print("{} => ({} {} {})".format(instructions[var], op(instruction), val0, val1))
            vals[var] = op_apply(op(instruction), val0, val1)
            stack.pop(-1)
        else:
            print("[{}]{} => ({} {} {})".format(len(stack),instructions[var], op(instruction), v0, v1))
            if vals[v0] is None:
                stack.append(v0)
            if vals[v1] is None:
                stack.append(v1)

    pass


print(vals[vars['root']])
print(vals[vars['rcsj']])
print(vals[vars['zfsf']])


def eval_var(var_id, z_vars):
    if vals[var_id]:
        return vals[var_id]
    return z_vars[var_id]

def instr_to_z3_str(instr, z_vars):
    var = vars[lval(instr)]
    if vals[var]:
        z3expr = 'z_vars[{}] == RealVal({})'.format(var, vals[var])
    else:
        v0 = vars[var0(instr)]
        v1 = vars[var1(instr)]
        operator = op(instr)
        if operator == '/':
            z3expr = 'And(z_vars[{}] == z_vars[{}] / z_vars[{}], z_vars[{}] > 0.0)'.format(var, v0, v1, v1)
        else:
            v0_val = eval_var(v0, z_vars)
            v1_val = eval_var(v1, z_vars)
            z3expr = 'z_vars[{}] == z_vars[{}] {} z_vars[{}]'.format(var, v0, operator, v1)

    print('{} --- {}'.format(var, z3expr))
    return z3expr

z_vars = Reals(' '.join([lval(instruction) for instruction in instructions]))
s = Solver()
for instr in instructions:
    if lval(instr) == 'root':
        print("ROOT: {}".format(instr))
        v0 = vars[var0(instr)]
        v1 = vars[var1(instr)]
        #clause = eval('z_vars[{}] == z_vars[{}]'.format(v0, v1))
        #clause = eval(instr_to_z3_str(instr, z_vars))
    elif lval(instr) == 'humn':
        # clause = eval('z_vars[{}] < 1000000'.format(vars['humn']))
        # clause = eval(instr_to_z3_str(instr, z_vars))
        #continue
    else:
        clause = eval(instr_to_z3_str(instr, z_vars))

    print(clause)
    s.add(clause)

s.add(z_vars[vars['humn']] > 0)

s.check()
model = s.model()
model[z_vars[vars['root']]]
model[z_vars[vars['humn']]]
model[z_vars[vars['rcsj']]]
model[z_vars[vars['zfsf']]]
model[z_vars[vars['root']]]
