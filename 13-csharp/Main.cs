using System;
using System.Collections.Generic;
using System.Diagnostics;

class Bus {
    public ulong Id;
    public ulong Offset;
    public Bus(ulong Id, ulong Offset) {
        this.Id = Id;
        this.Offset = Offset;
    }
}

class MainClass
{
    static int Part1(string filePath)
    {
        string[] lines = System.IO.File.ReadAllLines(filePath);
        var start = int.Parse(lines[0]);
        int answerWait = int.MaxValue;
        int answerBusId = -1;
        foreach (var bus in lines[1].Split(',')) {
            if (bus != "x") {
                var busId = int.Parse(bus);
                var wait = start % busId == 0 ? 0 : busId - start % busId;
                if (wait < answerWait) {
                    answerWait = wait;
                    answerBusId = busId;
                }
            }
        }
        return answerWait * answerBusId;
    }

    static ulong nextLineUp(List<Bus> buses, int n, ulong start, ulong step)
    {
        ulong t = start;
        bool found = false;
        while (!found) {
            found = true;
            for (int i = 0; found && i < n; ++i) {
                if ((t + buses[i].Offset) % buses[i].Id != 0) {
                    found = false;
                }
            }

            if (!found) {
                t += step;
            }
        }

        return t;
    }

    static ulong Part2(string filePath)
    {
        string[] lines = System.IO.File.ReadAllLines(filePath);
        List<Bus> buses = new List<Bus>();

        ulong offset = 0;
        foreach (var bus in lines[1].Split(',')) {
            if (bus != "x") {
                buses.Add(new Bus(ulong.Parse(bus), offset));
            }
            offset += 1;
        }

        buses.Sort(delegate(Bus bus1, Bus bus2) {
            return bus2.Id.CompareTo(bus1.Id);
        });

        ulong step = 1;
        int n = 2;
        ulong t = 0;
        while (n < buses.Count) {
            ulong t0 = nextLineUp(buses, n, t, step);
            ulong t1 = nextLineUp(buses, n, t0 + step, step);
            step = t1 - t0;
            t = t0;
            n += 1;
        }

        return nextLineUp(buses, n, t, step);
    }

    static void SolveFile(string filePath)
    {
        Console.WriteLine("Input file: {0}", filePath);
        Console.WriteLine("Part 1: {0}", Part1(filePath));
        Console.WriteLine("Part 2: {0}", Part2(filePath));
    }

    static void Main(string[] args)
    {
        foreach (var filePath in args) {
            SolveFile(filePath);
        }
    }
}

