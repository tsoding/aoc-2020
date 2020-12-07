#!/usr/bin/env python3

if __name__ == "__main__":
    data = [
        list(map(set, group.split("\n")))
        for group in open("input.txt", "r").read().split("\n\n")
    ]
    part1 = sum(len(  set.union(*group)  ) 
                      for group in data)
    part2 = sum(len(  set.intersection(*group) ) 
                      for group in data)
    print(part1, part2)
