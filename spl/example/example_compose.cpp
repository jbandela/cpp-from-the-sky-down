// SPL Compose Example
// Demonstrates creating reusable pipeline stages with compose()

#include <spl/spl.h>
#include <iostream>
#include <vector>
#include <string>

int main() {
    // Create reusable composed stages
    std::cout << "=== Reusable Composed Stages ===\n";

    // A reusable "sum of squares" pipeline
    auto sum_of_squares = spl::compose(
        spl::transform([](int x) { return x * x; }),
        spl::sum()
    );

    std::vector<int> v1 = {1, 2, 3, 4, 5};
    std::vector<int> v2 = {10, 20, 30};

    std::cout << "Sum of squares [1,2,3,4,5]: "
              << spl::apply(v1, sum_of_squares) << "\n";  // 1+4+9+16+25 = 55
    std::cout << "Sum of squares [10,20,30]: "
              << spl::apply(v2, sum_of_squares) << "\n";  // 100+400+900 = 1400

    // Compose multiple stages for data processing
    std::cout << "\n=== Complex Composed Pipeline ===\n";

    auto process_data = spl::compose(
        spl::filter([](int x) { return x > 0; }),      // Keep positive
        spl::transform([](int x) { return x * 2; }),   // Double
        spl::take(5),                                   // Take first 5
        spl::to_vector()
    );

    std::vector<int> data = {-1, 3, -2, 5, 1, 8, -4, 2, 9, 0, 7};
    auto result = spl::apply(data, process_data);

    std::cout << "Processed data: ";
    for (int x : result) std::cout << x << " ";
    std::cout << "\n";  // Output: 6 10 2 16 4

    // Nested composition
    std::cout << "\n=== Nested Composition ===\n";

    auto double_it = spl::compose(spl::transform([](int x) { return x * 2; }));
    auto filter_small = spl::compose(spl::filter([](int x) { return x < 20; }));

    auto combined = spl::compose(
        double_it,
        filter_small,
        spl::to_vector()
    );

    std::vector<int> nums = {1, 5, 10, 15, 20};
    auto filtered = spl::apply(nums, combined);

    std::cout << "Doubled then filtered (<20): ";
    for (int x : filtered) std::cout << x << " ";
    std::cout << "\n";  // Output: 2 10

    // Using compose for statistics
    std::cout << "\n=== Statistics with Tee and Compose ===\n";

    std::vector<double> values = {1.0, 2.0, 3.0, 4.0, 5.0};

    auto stats = spl::apply(values,
        spl::tee(
            spl::sum(),
            spl::count(),
            spl::min(),
            spl::max()
        ),
        spl::transform_complete([](double sum, size_t count,
                                   std::optional<double> min,
                                   std::optional<double> max) {
            std::cout << "Sum: " << sum << "\n";
            std::cout << "Count: " << count << "\n";
            std::cout << "Min: " << min.value_or(0) << "\n";
            std::cout << "Max: " << max.value_or(0) << "\n";
            std::cout << "Average: " << sum / count << "\n";
            return sum / count;
        })
    );

    return 0;
}
