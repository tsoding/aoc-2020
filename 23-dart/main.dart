import 'dart:io';

String part1(String current) {
  int getDestIndex(String current, int max) {
    int t = int.parse(current[0]) - 1;
    int result = current.indexOf(t.toString());
    while (result < 0) {
      t -= 1;
      if (t < 1) t = max;
      result = current.indexOf(t.toString());
    }
    return result;
  }

  for (int i = 0; i < 100; ++i) {
    // print(current);
    var pick3 = current.substring(1, 4);
    var smol = current.split(pick3).join();
    var destIndex = getDestIndex(smol, 9);
    current = smol.substring(1, destIndex + 1) + pick3 + smol.substring(destIndex + 1) + smol[0];
  }
  
  return current.split("1").reversed.join();
}

class Node {
  int value;
  int next;

  Node(this.value, this.next);

  @override
  String toString() {
    return "(value: ${value}, next: ${next})";
  }
}

int part2(String content) {
  var nodes = new List<Node>();
  var addrs = new List<int>(1000 * 1000 + 10);

  for (int i = 0; i < content.length; ++i) {
    var value = int.parse(content[i]);
    nodes.add(new Node(value, i + 1));
    addrs[value] = i;
  }

  for (int value = 10; value <= 1000 * 1000; ++value) {
    int addr = value - 1;
    nodes.add(new Node(value, addr + 1));
    addrs[value] = addr;
  }

  nodes[1000 * 1000 - 1].next = 0;

  int current = 0;

  int jump(int addr, int n) {
    for (int i = 0; i < n; ++i) {
      addr = nodes[addr].next;
    }
    return addr;
  }

  bool isValueRemoved(int value, int addr, int n) {
    for (int i = 0; i < n; ++i) {
      if (nodes[addr].value == value) {
        return true;
      }
      addr = nodes[addr].next;
    }
    return false;
  }

  int getDestAddr(int pick3) {
    int result = nodes[current].value - 1;
    if (result < 1) result = 1000 * 1000;

    while (isValueRemoved(result, pick3, 3)) {
      result -= 1;
      if (result < 1) result = 1000 * 1000;
    }

    return addrs[result];
  }

  for (int i = 0; i < 10 * 1000 * 1000; ++i) {
    var pick3 = nodes[current].next;
    nodes[current].next = jump(current, 4);
    var dest = getDestAddr(pick3);
    // print(jump(pick3, 2));
    nodes[jump(pick3, 2)].next = nodes[dest].next;
    nodes[dest].next = pick3;
    current = nodes[current].next;
  }

  return nodes[jump(addrs[1], 1)].value * nodes[jump(addrs[1], 2)].value;
}

void solveFile(String filePath) {
  new File(filePath).readAsString()
  .then((String content) { return content.trim(); })
  .then((String content) {
      print('Input file: ${filePath}');
      print("Part 1: ${part1(content)}");
      print('Part 2: ${part2(content)}');
  });
}

void main(List<String> args) {
  for (String filePath in args) {
    solveFile(filePath);
  }
}
