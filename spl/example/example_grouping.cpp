// SPL Grouping Example
// Demonstrates group_by, chunk, chunk_by, and tee

#include <spl/spl.h>
#include <iostream>
#include <vector>
#include <string>

int main() {
    // group_by - group elements by a key
    std::cout << "=== Group By ===\n";
    std::vector<int> numbers = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};

    spl::apply(numbers,
        spl::group_by(
            [](int x) { return x % 2 == 0 ? "even" : "odd"; },
            spl::to_vector()
        ),
        spl::for_each([](const std::string& key, const std::vector<int>& group) {
            std::cout << key << ": ";
            for (int x : group) std::cout << x << " ";
            std::cout << "\n";
        })
    );

    // group_by with aggregation
    std::cout << "\n=== Group By with Sum ===\n";
    spl::apply(numbers,
        spl::group_by(
            [](int x) { return x % 3; },  // Group by remainder when divided by 3
            spl::sum()
        ),
        spl::for_each([](int key, int sum) {
            std::cout << "Remainder " << key << " sum: " << sum << "\n";
        })
    );

    // chunk - split into fixed-size groups
    std::cout << "\n=== Chunk ===\n";
    std::vector<int> data = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};

    spl::apply(data,
        spl::chunk(3, spl::to_vector()),
        spl::for_each([](const std::vector<int>& chunk) {
            std::cout << "Chunk: ";
            for (int x : chunk) std::cout << x << " ";
            std::cout << "\n";
        })
    );
    // Output: Chunk: 1 2 3 / Chunk: 4 5 6 / Chunk: 7 8 9 / Chunk: 10

    // chunk with sum
    std::cout << "\n=== Chunk with Sum ===\n";
    spl::apply(data,
        spl::chunk(4, spl::sum()),
        spl::for_each([](int chunk_sum) {
            std::cout << "Chunk sum: " << chunk_sum << "\n";
        })
    );

    // chunk_by - split when predicate fails between adjacent elements
    std::cout << "\n=== Chunk By (Consecutive Runs) ===\n";
    std::vector<int> runs = {1, 1, 2, 2, 2, 3, 1, 1, 1, 1};

    spl::apply(runs,
        spl::chunk_by(
            [](int a, int b) { return a == b; },  // Same value
            spl::to_vector()
        ),
        spl::for_each([](const std::vector<int>& run) {
            std::cout << "Run of " << run[0] << " (length " << run.size() << ")\n";
        })
    );

    // tee - process same stream through multiple pipelines
    std::cout << "\n=== Tee (Parallel Processing) ===\n";
    std::vector<int> values = {1, 2, 3, 4, 5};

    spl::apply(values,
        spl::tee(
            spl::sum(),
            spl::compose(spl::transform([](int x) { return x * x; }), spl::sum()),
            spl::count()
        ),
        spl::transform_complete([](int sum, int sum_sq, size_t n) {
            double mean = static_cast<double>(sum) / n;
            double mean_sq = static_cast<double>(sum_sq) / n;
            double variance = mean_sq - mean * mean;
            std::cout << "Sum: " << sum << "\n";
            std::cout << "Sum of squares: " << sum_sq << "\n";
            std::cout << "Count: " << n << "\n";
            std::cout << "Mean: " << mean << "\n";
            std::cout << "Variance: " << variance << "\n";
            return variance;
        })
    );

    return 0;
}
