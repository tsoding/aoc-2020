#!/usr/bin/env python3

def every_yesu(answers):
    result = {}
    for answer in answers:
        for x in answer:
            if x in result:
                result[x] += 1
            else:
                result[x] = 1
    return len([x for x in result.values() if x == len(answers)])

def any_yesu(answers):
    result = set()
    for answer in answers:
        for x in answer:
            result.add(x)
    return len(result)

if __name__ == "__main__":
    with open('input.txt', 'r') as file:
        lines = map(
            lambda x: x.strip(),
            [line for line in file.readlines()])
        answers = []
        any_result = 0
        every_result = 0
        for line in lines:
            if line:
                answers.append(line)
            else:
                any_result += any_yesu(answers)
                every_result += every_yesu(answers)
                answers = []
        any_result += any_yesu(answers)
        every_result += every_yesu(answers)
        print(any_result)
        print(every_result)
