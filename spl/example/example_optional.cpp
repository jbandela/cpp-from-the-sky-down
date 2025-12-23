// SPL Optional Handling Example
// Demonstrates unwrap_optional (monadic) and flatten_optional (filtering)

#include <spl/spl.h>
#include <iostream>
#include <vector>
#include <optional>
#include <string>

// Simulate a function that might fail
std::optional<int> safe_divide(int a, int b) {
    if (b == 0) return std::nullopt;
    return a / b;
}

std::optional<int> safe_sqrt(int x) {
    if (x < 0) return std::nullopt;
    return static_cast<int>(std::sqrt(x));
}

int main() {
    // === flatten_optional: Filter out nullopt values ===
    std::cout << "=== flatten_optional (Filter out nullopt) ===\n";

    std::vector<std::optional<int>> maybe_values = {1, std::nullopt, 3, std::nullopt, 5};

    auto filtered = spl::apply(maybe_values,
        spl::flatten_optional(),
        spl::to_vector()
    );

    std::cout << "After flatten_optional: ";
    for (int x : filtered) std::cout << x << " ";
    std::cout << "\n";  // Output: 1 3 5

    // Sum only the present values
    auto sum = spl::apply(maybe_values,
        spl::flatten_optional(),
        spl::sum()
    );
    std::cout << "Sum of present values: " << sum << "\n";  // Output: 9

    // === unwrap_optional: Monadic short-circuit ===
    std::cout << "\n=== unwrap_optional (Monadic - short-circuits) ===\n";

    // All values present - succeeds
    std::vector<std::optional<int>> all_present = {1, 2, 3};
    auto result1 = spl::apply(all_present,
        spl::unwrap_optional(),
        spl::sum()
    );
    std::cout << "All present sum: "
              << (result1.has_value() ? std::to_string(result1.value()) : "nullopt")
              << "\n";  // Output: 6

    // Some missing - short-circuits
    std::vector<std::optional<int>> some_missing = {1, std::nullopt, 3};
    auto result2 = spl::apply(some_missing,
        spl::unwrap_optional(),
        spl::sum()
    );
    std::cout << "Some missing sum: "
              << (result2.has_value() ? std::to_string(result2.value()) : "nullopt")
              << "\n";  // Output: nullopt

    // === Practical example: Chained operations that might fail ===
    std::cout << "\n=== Chained Optional Operations ===\n";

    std::vector<std::pair<int, int>> divisions = {
        {10, 2},   // 5
        {20, 4},   // 5
        {15, 3}    // 5
    };

    // All succeed
    auto div_results = spl::apply(divisions,
        spl::transform([](auto p) { return safe_divide(p.first, p.second); }),
        spl::unwrap_optional(),
        spl::sum()
    );
    std::cout << "Sum of divisions: "
              << (div_results.has_value() ? std::to_string(div_results.value()) : "nullopt")
              << "\n";  // Output: 15

    // One fails
    std::vector<std::pair<int, int>> bad_divisions = {
        {10, 2},   // 5
        {20, 0},   // nullopt (divide by zero)
        {15, 3}    // would be 5
    };

    auto bad_results = spl::apply(bad_divisions,
        spl::transform([](auto p) { return safe_divide(p.first, p.second); }),
        spl::unwrap_optional(),
        spl::sum()
    );
    std::cout << "Sum with bad division: "
              << (bad_results.has_value() ? std::to_string(bad_results.value()) : "nullopt")
              << "\n";  // Output: nullopt

    // === Alternative: Use flatten_optional to skip failures ===
    std::cout << "\n=== Using flatten_optional to Skip Failures ===\n";

    auto skip_bad = spl::apply(bad_divisions,
        spl::transform([](auto p) { return safe_divide(p.first, p.second); }),
        spl::flatten_optional(),  // Skip the nullopt, continue with others
        spl::sum()
    );
    std::cout << "Sum skipping failures: " << skip_bad << "\n";  // Output: 10 (5 + 5)

    // === Multi-argument with flatten_optional_arg ===
    std::cout << "\n=== flatten_optional_arg for Multi-Argument Streams ===\n";

    std::vector<std::string> names = {"Alice", "Bob", "Charlie"};
    std::vector<std::optional<int>> ages = {30, std::nullopt, 25};

    spl::apply(names,
        spl::zip(ages),
        spl::flatten_optional_arg<1>(),  // Filter based on second arg being present
        spl::for_each([](const std::string& name, int age) {
            std::cout << name << " is " << age << " years old\n";
        })
    );
    // Output: Alice is 30 years old / Charlie is 25 years old (Bob filtered out)

    return 0;
}
