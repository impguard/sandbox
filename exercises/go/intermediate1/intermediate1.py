import sys


def part(first, rest, total):
    if first == total and len(rest) == 0:
        return [[first]]
    elif len(rest) == 0:
        return []

    # include me with +
    plus_results = part(rest[0], rest[1:], total - first)
    plus_results = [r + ['+', first] for r in plus_results]

    # include me with -
    minus_results = part(rest[0], rest[1:], total + first)
    minus_results = [r + ['-', first] for r in minus_results]

    # include me with combine
    combine_results = part(int(f'{rest[0]}{first}'), rest[1:], total)

    return plus_results + minus_results + combine_results


def sum_possibilities(lower, upper, total):
    results = part(upper, list(range(upper - 1, lower - 1, -1)), total)

    for result in results:
        print(' '.join(str(r) for r in result))

if __name__ == '__main__':
    args = sys.argv
    lower, upper, total = int(args[1]), int(args[2]), int(args[3])
    sum_possibilities(lower, upper, total)

