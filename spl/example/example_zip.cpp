// SPL Zip Example
// Demonstrates combining multiple streams with zip

#include <spl/spl.h>
#include <iostream>
#include <vector>
#include <string>

int main() {
    // Basic zip - combine two ranges
    std::cout << "=== Basic Zip ===\n";
    std::vector<int> numbers = {1, 2, 3, 4, 5};
    std::vector<std::string> words = {"one", "two", "three", "four", "five"};

    spl::apply(numbers,
        spl::zip(words),
        spl::for_each([](int n, const std::string& w) {
            std::cout << n << " -> " << w << "\n";
        })
    );

    // Zip for computation - dot product
    std::cout << "\n=== Dot Product ===\n";
    std::vector<double> a = {1.0, 2.0, 3.0};
    std::vector<double> b = {4.0, 5.0, 6.0};

    auto dot_product = spl::apply(a,
        spl::zip(b),
        spl::transform([](double x, double y) { return x * y; }),
        spl::sum()
    );
    std::cout << "Dot product: " << dot_product << "\n";  // 1*4 + 2*5 + 3*6 = 32

    // Zip with enumerate
    std::cout << "\n=== Enumerate (Zip with Index) ===\n";
    std::vector<std::string> items = {"apple", "banana", "cherry"};

    spl::apply(items,
        spl::enumerate(),
        spl::for_each([](size_t i, const std::string& item) {
            std::cout << "[" << i << "] " << item << "\n";
        })
    );

    // Zip multiple ranges
    std::cout << "\n=== Zip Three Ranges ===\n";
    std::vector<int> xs = {1, 2, 3};
    std::vector<int> ys = {10, 20, 30};
    std::vector<int> zs = {100, 200, 300};

    auto sums = spl::apply(xs,
        spl::zip(ys, zs),
        spl::transform([](int x, int y, int z) { return x + y + z; }),
        spl::to_vector()
    );

    std::cout << "Sums: ";
    for (int s : sums) std::cout << s << " ";
    std::cout << "\n";  // Output: 111 222 333

    // zip_result - add computed values to stream
    std::cout << "\n=== Zip Result ===\n";
    std::vector<int> vals = {1, 2, 3, 4, 5};

    spl::apply(vals,
        spl::zip_result([](int x) { return x * x; }),  // Add square
        spl::for_each([](int x, int sq) {
            std::cout << x << " squared is " << sq << "\n";
        })
    );

    // Parallel iteration with early termination
    std::cout << "\n=== Zip with Take ===\n";
    std::vector<int> long_range = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
    std::vector<char> letters = {'a', 'b', 'c'};  // Shorter range

    spl::apply(long_range,
        spl::zip(letters),
        spl::for_each([](int n, char c) {
            std::cout << n << "-" << c << " ";
        })
    );
    std::cout << "\n";  // Output: 1-a 2-b 3-c (stops at shorter range)

    return 0;
}
