// SPL Generators Example
// Demonstrates iota generator

#include <spl/spl.h>
#include <iostream>
#include <vector>

int main() {
    // iota - bounded range
    std::cout << "=== Iota (Bounded) ===\n";
    auto range = spl::apply(spl::iota(1, 11),
        spl::to_vector()
    );

    std::cout << "iota(1, 11): ";
    for (size_t x : range) std::cout << x << " ";
    std::cout << "\n";  // Output: 1 2 3 4 5 6 7 8 9 10

    // iota with processing
    std::cout << "\n=== Sum of 1 to 100 ===\n";
    auto sum_1_to_100 = spl::apply(spl::iota(1, 101), spl::sum());
    std::cout << "Sum 1 to 100: " << sum_1_to_100 << "\n";  // Output: 5050

    // iota - unbounded with take
    std::cout << "\n=== Iota (Unbounded with Take) ===\n";
    auto first_squares = spl::apply(spl::iota(1),  // Unbounded starting at 1
        spl::transform([](size_t x) { return x * x; }),
        spl::take(10),
        spl::to_vector()
    );

    std::cout << "First 10 squares: ";
    for (auto x : first_squares) std::cout << x << " ";
    std::cout << "\n";  // Output: 1 4 9 16 25 36 49 64 81 100

    // Fizz Buzz with iota
    std::cout << "\n=== FizzBuzz (1-20) ===\n";
    spl::apply(spl::iota(1, 21),
        spl::for_each([](size_t n) {
            if (n % 15 == 0) std::cout << "FizzBuzz ";
            else if (n % 3 == 0) std::cout << "Fizz ";
            else if (n % 5 == 0) std::cout << "Buzz ";
            else std::cout << n << " ";
        })
    );
    std::cout << "\n";

    // Enumerate using enumerate() stage
    std::cout << "\n=== Enumerate ===\n";
    std::vector<std::string> items = {"apple", "banana", "cherry", "date"};

    spl::apply(items,
        spl::enumerate(),
        spl::for_each([](size_t idx, const std::string& item) {
            std::cout << idx << ": " << item << "\n";
        })
    );

    // Primes using iota and filter
    std::cout << "\n=== Prime Numbers ===\n";
    auto is_prime = [](size_t n) {
        if (n < 2) return false;
        if (n == 2) return true;
        if (n % 2 == 0) return false;
        for (size_t i = 3; i * i <= n; i += 2) {
            if (n % i == 0) return false;
        }
        return true;
    };

    auto primes = spl::apply(spl::iota(2),
        spl::filter(is_prime),
        spl::take(20),
        spl::to_vector()
    );

    std::cout << "First 20 primes: ";
    for (auto p : primes) std::cout << p << " ";
    std::cout << "\n";

    // Zip with vectors (instead of generators)
    std::cout << "\n=== Zip Vectors ===\n";
    std::vector<int> a = {1, 2, 3, 4, 5};
    std::vector<int> b = {10, 20, 30, 40, 50};

    auto sums = spl::apply(a,
        spl::zip(b),
        spl::transform([](int x, int y) { return x + y; }),
        spl::to_vector()
    );

    std::cout << "Pairwise sums: ";
    for (int s : sums) std::cout << s << " ";
    std::cout << "\n";  // Output: 11 22 33 44 55

    // Triangular numbers
    std::cout << "\n=== Triangular Numbers ===\n";
    // T(n) = n * (n + 1) / 2
    auto triangular = spl::apply(spl::iota(1, 11),
        spl::transform([](size_t n) { return n * (n + 1) / 2; }),
        spl::to_vector()
    );

    std::cout << "First 10 triangular numbers: ";
    for (auto t : triangular) std::cout << t << " ";
    std::cout << "\n";  // Output: 1 3 6 10 15 21 28 36 45 55

    return 0;
}
