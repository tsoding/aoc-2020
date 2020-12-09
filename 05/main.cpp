#include <cassert>
#include <iostream>
#include <string>
#include <fstream>
#include <limits>
#include <set>

using namespace std;

int row_of_pass(string pass)
{
    assert(pass.size() == 10);

    int low = 0;
    int high = 127;

    for (int i = 0; i < 7; ++i) {
        const int middle = (high - low) / 2 + low;
        switch (pass[i]) {
        case 'F':
            high = middle;
            break;
        case 'B':
            low = middle + 1;
            break;
        default:
            assert(0 && "unreachable");
        }
    }

    assert(low == high);

    return low;
}

int col_of_pass(string pass)
{
    assert(pass.size() == 10);

    int low = 0;
    int high = 7;

    for (int i = 7; i < 10; ++i) {
        const int middle = (high - low) / 2 + low;
        switch (pass[i]) {
        case 'L':
            high = middle;
            break;
        case 'R':
            low = middle + 1;
            break;
        default:
            assert(0 && "unreachable");
        }
    }

    assert(low == high);

    return low;
}

int id_of_pass(string pass)
{
    return row_of_pass(pass) * 8 + col_of_pass(pass);
}

void part_1(const char *file_path)
{
    ifstream fin(file_path);

    int ans = numeric_limits<int>::min();
    cout << "Part 1: " << endl;
    while (fin) {
        string pass;
        getline(fin, pass);
        if (pass.size() == 10) {
            auto id = id_of_pass(pass);
            ans = max(ans, id);
            cout << "  " << pass << " -> " << id_of_pass(pass) << endl;
        }
    }
    cout << "  Answer: " << ans << endl;
}

void part_2(const char *file_path)
{
    ifstream fin(file_path);

    set<int> ids_nuts;
    while (fin) {
        string pass;
        getline(fin, pass);
        if (pass.size() == 10) {
            auto id = id_of_pass(pass);
            ids_nuts.insert(id);
        }
    }

    std::cout << "Part 2: " << std::endl;
    for (int row = 0; row < 128; ++row) {
        cout << "  ";
        for (int col = 0; col < 8; ++col) {
            if (ids_nuts.find(row * 8 + col) != ids_nuts.end()) {
                cout << "* ";
            } else {
                cout << ". ";
            }
        }
        cout << endl;
    }
}

void solve_file(const char *file_path)
{
    cout << "Input file: " << file_path << endl;
    part_1(file_path);
    part_2(file_path);
}

int main(int argc, char **argv)
{
    for (int i = 1; i < argc; ++i) {
        solve_file(argv[i]);
    }

    return 0;
}
