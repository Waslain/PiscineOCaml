// test_ft_print_comb2.cpp
#include <iostream>
#include <iomanip>
#include <sstream>
#include <cstdio>
#include <vector>
#include <string>
#include <cstdlib>

std::string build_expected() {
    std::ostringstream oss;
    for (int a = 0; a <= 98; ++a) {
        for (int b = a + 1; b <= 99; ++b) {
            oss << std::setfill('0') << std::setw(2) << a
                << ' '
                << std::setfill('0') << std::setw(2) << b;
            if (!(a == 98 && b == 99))
                oss << ", ";
        }
    }
    oss << '\n';
    return oss.str();
}

std::string run_prog(const char* path) {
    std::string cmd = std::string(path);
    FILE* pipe = popen(cmd.c_str(), "r");
    if (!pipe) {
        std::cerr << "Cannot run program\n";
        std::exit(2);
    }
    std::string out;
    char buf[4096];
    while (size_t n = std::fread(buf, 1, sizeof(buf), pipe))
        out.append(buf, n);
    pclose(pipe);
    return out;
}

int main(int argc, char** argv) {
    if (argc != 2) {
        std::cerr << "Usage: ./tester ./a.out\n";
        return 2;
    }
    std::string actual = run_prog(argv[1]);
    std::string expected = build_expected();

    if (actual == expected) {
        std::cout << "OK\n";
        return 0;
    }
    std::cout << "FAIL\n";
    // Show a small diff context
    size_t m = std::min(actual.size(), expected.size());
    for (size_t i = 0; i < m; ++i) {
        if (actual[i] != expected[i]) {
            std::cout << "First diff at index " << i << "\n";
            std::cout << "Expected: '" << expected.substr(i, 40) << "'\n";
            std::cout << "Actual  : '" << actual.substr(i, 40) << "'\n";
            break;
        }
    }
    if (actual.size() != expected.size())
        std::cout << "Size differs. Expected " << expected.size()
                  << " got " << actual.size() << "\n";
    return 1;
}