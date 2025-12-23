// SPL Flatten Example
// Demonstrates flatten, flat_map, and nested data processing

#include <spl/spl.h>
#include <iostream>
#include <vector>
#include <string>

int main() {
    // Basic flatten - nested vectors
    std::cout << "=== Flatten Nested Vectors ===\n";
    std::vector<std::vector<int>> nested = {
        {1, 2, 3},
        {4, 5},
        {6, 7, 8, 9}
    };

    auto flat = spl::apply(nested,
        spl::flatten(),
        spl::to_vector()
    );

    std::cout << "Flattened: ";
    for (int x : flat) std::cout << x << " ";
    std::cout << "\n";  // Output: 1 2 3 4 5 6 7 8 9

    // flat_map - map then flatten
    std::cout << "\n=== Flat Map ===\n";
    std::vector<int> numbers = {1, 2, 3};

    auto repeated = spl::apply(numbers,
        spl::flat_map([]<typename Out>(Out&& out, int x) {
            if constexpr (spl::impl::calculate_type_v<Out>) {
                // Type calculation mode - just return what we'd output
                return out(x);
            } else {
                // Output x copies of x
                for (int i = 0; i < x; ++i) {
                    out(x);
                }
                return true;
            }
        }),
        spl::to_vector()
    );

    std::cout << "Repeated: ";
    for (int x : repeated) std::cout << x << " ";
    std::cout << "\n";  // Output: 1 2 2 3 3 3

    // Flatten strings into characters
    std::cout << "\n=== Flatten Strings ===\n";
    std::vector<std::string> words = {"hello", "world"};

    spl::apply(words,
        spl::flatten(),
        spl::for_each([](char c) {
            std::cout << c << " ";
        })
    );
    std::cout << "\n";  // Output: h e l l o w o r l d

    // Processing matrix data
    std::cout << "\n=== Matrix Processing ===\n";
    std::vector<std::vector<int>> matrix = {
        {1, 2, 3},
        {4, 5, 6},
        {7, 8, 9}
    };

    // Sum all elements in the matrix
    auto matrix_sum = spl::apply(matrix,
        spl::flatten(),
        spl::sum()
    );
    std::cout << "Matrix sum: " << matrix_sum << "\n";  // Output: 45

    // Count elements greater than 5
    auto count_gt5 = spl::apply(matrix,
        spl::flatten(),
        spl::filter([](int x) { return x > 5; }),
        spl::count()
    );
    std::cout << "Elements > 5: " << count_gt5 << "\n";  // Output: 4

    // Flatten with transform - get all squared values
    std::cout << "\n=== Flatten with Transform ===\n";
    auto all_squared = spl::apply(matrix,
        spl::flatten(),
        spl::transform([](int x) { return x * x; }),
        spl::to_vector()
    );

    std::cout << "All squared: ";
    for (int x : all_squared) std::cout << x << " ";
    std::cout << "\n";  // Output: 1 4 9 16 25 36 49 64 81

    // Deep flattening simulation
    std::cout << "\n=== Processing Nested Ranges ===\n";
    std::vector<std::vector<std::vector<int>>> deep = {
        {{1, 2}, {3}},
        {{4, 5, 6}},
        {{7}, {8, 9}}
    };

    // Flatten twice for 3D to 1D
    auto deep_flat = spl::apply(deep,
        spl::flatten(),  // 3D -> 2D
        spl::flatten(),  // 2D -> 1D
        spl::to_vector()
    );

    std::cout << "Deep flattened: ";
    for (int x : deep_flat) std::cout << x << " ";
    std::cout << "\n";  // Output: 1 2 3 4 5 6 7 8 9

    return 0;
}
