import z3
import sys


def main():
    variable_names = sys.argv[1].split(',')
    constraint_strs = sys.argv[2].split(',')
    constraints = []

    for var in variable_names:
        exec('{n} = z3.Int(\'{n}\')'.format(n=var))

    for constraint in constraint_strs:
        constraints.append(eval(constraint))
    z3.solve(*constraints)


if __name__ == '__main__':
    main()
