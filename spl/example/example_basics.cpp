// SPL Basics Example
// Demonstrates core SPL operations: apply, transform, filter, and accumulation

#include <spl/spl.h>
#include <iostream>
#include <vector>

int main() {
    std::vector<int> numbers = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};

    // Basic transform and filter
    std::cout << "=== Transform and Filter ===\n";
    auto doubled_evens = spl::apply(numbers,
        spl::filter([](int x) { return x % 2 == 0; }),
        spl::transform([](int x) { return x * 2; }),
        spl::to_vector()
    );
    std::cout << "Doubled evens: ";
    for (int x : doubled_evens) std::cout << x << " ";
    std::cout << "\n";  // Output: 4 8 12 16 20

    // Sum
    std::cout << "\n=== Accumulation ===\n";
    auto sum = spl::apply(numbers, spl::sum());
    std::cout << "Sum: " << sum << "\n";  // Output: 55

    // Count
    auto count = spl::apply(numbers, spl::count());
    std::cout << "Count: " << count << "\n";  // Output: 10

    // First and Last
    auto first = spl::apply(numbers, spl::first());
    auto last = spl::apply(numbers, spl::last());
    std::cout << "First: " << first.value_or(-1) << "\n";  // Output: 1
    std::cout << "Last: " << last.value_or(-1) << "\n";    // Output: 10

    // Min and Max
    auto min_val = spl::apply(numbers, spl::min());
    auto max_val = spl::apply(numbers, spl::max());
    std::cout << "Min: " << min_val.value_or(-1) << "\n";  // Output: 1
    std::cout << "Max: " << max_val.value_or(-1) << "\n";  // Output: 10

    // Take and Skip
    std::cout << "\n=== Take and Skip ===\n";
    auto first_three = spl::apply(numbers, spl::take(3), spl::to_vector());
    std::cout << "First 3: ";
    for (int x : first_three) std::cout << x << " ";
    std::cout << "\n";  // Output: 1 2 3

    auto skip_five = spl::apply(numbers, spl::skip(5), spl::to_vector());
    std::cout << "Skip 5: ";
    for (int x : skip_five) std::cout << x << " ";
    std::cout << "\n";  // Output: 6 7 8 9 10

    // Predicates
    std::cout << "\n=== Predicates ===\n";
    auto has_even = spl::apply(numbers, spl::any_of([](int x) { return x % 2 == 0; }));
    auto all_positive = spl::apply(numbers, spl::all_of([](int x) { return x > 0; }));
    auto none_negative = spl::apply(numbers, spl::none_of([](int x) { return x < 0; }));
    std::cout << "Has even: " << std::boolalpha << has_even << "\n";      // Output: true
    std::cout << "All positive: " << all_positive << "\n";                 // Output: true
    std::cout << "None negative: " << none_negative << "\n";               // Output: true

    return 0;
}
