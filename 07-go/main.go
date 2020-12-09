package main

import (
	"fmt"
	"strings"
	"regexp"
	"strconv"
	"os"
	"bufio"
	"container/list"
)

type Color = string

type Child struct {
	color Color
	amount int
}

type Rule struct {
	container Color
	children []Child
}

type Rules = map[Color][]Child

func lineToRule(line string) Rule {
	a := strings.Split(line, "contain")
	container := strings.TrimRight(strings.Split(a[0], "bags")[0], " ")
	children := []Child{}

	r := regexp.MustCompile(" ([0-9]+) (.*) bags?")
	if a[1] != " no other bags." {
		for _, child := range strings.Split(a[1], ",") {
			b := r.FindStringSubmatch(child)
			amount, err := strconv.Atoi(b[1])
			if err != nil {
				panic(err)
			}
			children = append(children, Child{
				amount: amount,
				color: b[2],
			})
		}
	}

	return Rule{
		container: container,
		children: children,
	}
}

func findContainers(rules Rules, color Color) []Color {
	result := []Color{}
	for container, children := range rules {
		for _, child := range children {
			if child.color == color {
				result = append(result, container)
				break
			}
		}
	}
	return result
}

func rulesFromFile(filePath string) Rules {
	file, err := os.Open(filePath)
	if err != nil {
		panic(err)
	}
	defer file.Close()

	rules := map[Color][]Child{};

	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := scanner.Text()
		rule := lineToRule(line);
		rules[rule.container] = rule.children;
	}

	if err := scanner.Err(); err != nil {
		panic(err)
	}

	return rules
}

func countBagsV1(rules Rules, color Color) int {
	visited := map[Color]bool{}
	queue := list.New()

	queue.PushBack(color);
	for queue.Len() > 0 {
		next := queue.Front()

		if _, ok := visited[next.Value.(Color)]; !ok {
			for _, container := range findContainers(rules, next.Value.(Color)) {
				queue.PushBack(container)
			}
		}

		visited[next.Value.(Color)] = true;
		queue.Remove(next)
	}

	return len(visited) - 1
}

func countBagsV2(rules Rules, color Color) int {
	result := 0

	if children, ok := rules[color]; ok {
		for _, child := range children {
			result += child.amount + child.amount * countBagsV2(rules, child.color)
		}
	}

	return result
}

func createDotFileForRules(rules Rules, filePath string) {
	file, err := os.Create(filePath)
	if err != nil {
		panic(err)
	}
	defer file.Close()

	fmt.Fprintf(file, "digraph D {\n");
	for parent, children := range rules {
		for _, child := range children {
			fmt.Fprintf(file, "    %#v -> %#v\n", parent, child.color);
		}
	}
	fmt.Fprintf(file, "}\n");
}

func solveFile(filePath string) {
	rules := rulesFromFile(filePath)
	createDotFileForRules(rules, filePath + ".dot")
	fmt.Printf("Part 1: %d\n", countBagsV1(rules, "shiny gold"))
	fmt.Printf("Part 2: %d\n", countBagsV2(rules, "shiny gold"))
}

func main() {
	for _, filePath := range os.Args[1:] {
		fmt.Printf("Input file: %s\n", filePath)
		solveFile(filePath)
	}
}
