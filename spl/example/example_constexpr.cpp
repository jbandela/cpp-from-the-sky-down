// SPL Constexpr Example
// Demonstrates compile-time pipeline evaluation

#include <spl/spl.h>
#include <iostream>
#include <array>

// Compile-time sum of squares
constexpr auto sum_of_squares() {
    constexpr std::array nums = {1, 2, 3, 4, 5};
    return spl::apply(nums,
        spl::transform([](int x) { return x * x; }),
        spl::sum()
    );
}

// Compile-time filtering
constexpr auto count_evens() {
    constexpr std::array nums = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
    return spl::apply(nums,
        spl::filter([](int x) { return x % 2 == 0; }),
        spl::count()
    );
}

// Compile-time first N elements sum
constexpr auto sum_first_three() {
    constexpr std::array nums = {10, 20, 30, 40, 50};
    return spl::apply(nums,
        spl::take(3),
        spl::sum()
    );
}

// Compile-time composed pipeline
constexpr auto complex_calculation() {
    constexpr std::array data = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};

    auto pipeline = spl::compose(
        spl::filter([](int x) { return x % 2 == 1; }),  // Keep odds
        spl::transform([](int x) { return x * x; }),    // Square
        spl::sum()                                       // Sum
    );

    return spl::apply(data, pipeline);
}

// Compile-time predicate check
constexpr auto all_positive() {
    constexpr std::array nums = {1, 2, 3, 4, 5};
    return spl::apply(nums, spl::all_of([](int x) { return x > 0; }));
}

// Compile-time min/max
constexpr auto find_min() {
    constexpr std::array nums = {5, 2, 8, 1, 9, 3};
    return spl::apply(nums, spl::min());
}

int main() {
    // All calculations happen at compile-time
    static_assert(sum_of_squares() == 55, "1+4+9+16+25 = 55");
    static_assert(count_evens() == 5, "5 even numbers in 1-10");
    static_assert(sum_first_three() == 60, "10+20+30 = 60");
    static_assert(complex_calculation() == 165, "1+9+25+49+81 = 165");
    static_assert(all_positive() == true, "All numbers are positive");
    static_assert(find_min().value() == 1, "Minimum is 1");

    std::cout << "=== Compile-time Calculations ===\n";
    std::cout << "Sum of squares [1..5]: " << sum_of_squares() << "\n";
    std::cout << "Count of evens [1..10]: " << count_evens() << "\n";
    std::cout << "Sum of first 3 [10,20,30,40,50]: " << sum_first_three() << "\n";
    std::cout << "Sum of squared odds [1..10]: " << complex_calculation() << "\n";
    std::cout << "All positive [1..5]: " << std::boolalpha << all_positive() << "\n";
    std::cout << "Min of [5,2,8,1,9,3]: " << find_min().value() << "\n";

    std::cout << "\nAll static_assert checks passed - calculations verified at compile-time!\n";

    return 0;
}
