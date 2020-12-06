import std.stdio;
import std.file;
import std.string;
import std.array;
import std.algorithm;
import std.conv;
import std.typecons;

struct Field
{
    string name;
    string value;
};

bool value_between(string value, int low, int high)
{
    assert(low <= high);
    try {
        auto x = value.to!int;
        return low <= x && x <= high;
    } catch (ConvException e) {
        return false;
    }
}

bool is_byr_valid(string value)
{
    return value_between(value, 1920, 2002);
}

bool is_iyr_valid(string value)
{
    return value_between(value, 2010, 2020);
}

bool is_eyr_valid(string value)
{
    return value_between(value, 2020, 2030);
}

bool is_hgt_valid(string value)
{
    if (value.endsWith("cm")) {
        return value_between(value[0..$-2], 150, 193);
    }

    if (value.endsWith("in")) { // KKoooooooona
        return value_between(value[0..$-2], 59, 76);
    }

    return false;
}

bool is_hcl_valid(string value)
{
    if (value.length != 7) {
        return false;
    }
    
    if (value[0] != '#') {
        return false;
    }

    for (int i = 1; i < 7; ++i) {
        if (!('0' <= value[i] && value[i] <= '9') &&
            !('a' <= value[i] && value[i] <= 'f'))
        {
            return false;
        }
    }

    return true;
}

bool is_ecl_valid(string value)
{
    string[] colors = ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"];
    return colors.canFind(value);
}

bool is_pid_valid(string value)
{
    if (value.length != 9) {
        return false;
    }

    for (int i = 0; i < 9; ++i) {
        if (!('0' <= value[i] && value[i] <= '9')) {
            return false;
        }
    }

    return true;
}

struct RequiredField
{
    string name;
    bool function(string) is_valid;    
}

bool is_valid(Field[] fields)
{
    RequiredField[] required_fields = [
        RequiredField("byr", &is_byr_valid),
        RequiredField("iyr", &is_iyr_valid),
        RequiredField("eyr", &is_eyr_valid),
        RequiredField("hgt", &is_hgt_valid),
        RequiredField("hcl", &is_hcl_valid),
        RequiredField("ecl", &is_ecl_valid),
        RequiredField("pid", &is_pid_valid)
    ];

    foreach (required_field; required_fields) {
        bool exists = false;
        foreach (field; fields) {
            if (field.name == required_field.name) {
                exists = true;
                if (!required_field.is_valid(field.value)) {
                    return false;
                }
                break;
            }
        }
        if (!exists) {
            return false;
        }
    }
    
    return true;
}

void parse_fields(ref Field[] fields, string line)
{
    foreach (field; line.split(' ')) {
        auto keyvalue = field.split(':');
        fields ~= Field(keyvalue[0], keyvalue[1]);
    }
}

void main()
{
    Field[] fields = [];
    int result = 0;
    foreach (line; readText("input.txt").splitLines()) {
        if (line.empty) {
            if (is_valid(fields)) {
                result += 1;
            }
            fields.length = 0;
        } else {
            parse_fields(fields, line);
        }
    }

    if (is_valid(fields)) {
        result += 1;
    }

    writeln(result);
}
