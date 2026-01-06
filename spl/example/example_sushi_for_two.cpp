// SPL Sushi for Two Example
// Codeforces Round 519, Problem A
//
// Problem: Given a sequence of sushi types (1 or 2), find the maximum number
// of contiguous pieces where exactly half are type 1 and half are type 2,
// with all pieces of one type coming before the other (two adjacent runs).
//
// Solution: Convert to run lengths, then for each pair of adjacent runs,
// compute 2 * min(run1, run2) and find the maximum.

#include <spl/spl.h>
#include <iostream>
#include <vector>
#include <array>
#include <algorithm>

template <std::ranges::range R>
size_t max_sushi(R&& sushi) {
    return spl::apply(std::forward<R>(sushi),
        spl::chunk_by(std::equal_to<>{}, spl::count()),
        spl::partial_accumulate_in_place(
            std::pair<size_t, size_t>{0, 0},  // {min(prev,cur), cur}
            [](std::pair<size_t, size_t>& state, size_t curr) {
                state.first = std::min(state.second, curr);
                state.second = curr;
            }
        ),
        spl::transform([](std::pair<size_t, size_t> p) { return 2 * p.first; }),
        spl::max()
    ).value_or(0);
}

int main() {
    // Example: 2 2 2 1 1 2 2
    // Runs: [3, 2, 2] (three 2s, two 1s, two 2s)
    // Adjacent pairs: min(3,2)*2=4, min(2,2)*2=4
    // Answer: 4
    std::vector<int> sushi1 = {2, 2, 2, 1, 1, 2, 2};

    std::cout << "Sushi sequence: ";
    for (int s : sushi1) std::cout << s << " ";
    std::cout << "\n";
    std::cout << "Maximum sushi for two: " << max_sushi(sushi1) << "\n";

    // Test case 2: 1 2 1 2 1 2
    // Runs: [1, 1, 1, 1, 1, 1]
    // All adjacent pairs give 2*min(1,1)=2
    // Answer: 2
    std::cout << "\n--- Test case 2 ---\n";
    std::vector<int> sushi2 = {1, 2, 1, 2, 1, 2};

    std::cout << "Sushi sequence: ";
    for (int s : sushi2) std::cout << s << " ";
    std::cout << "\n";
    std::cout << "Maximum sushi for two: " << max_sushi(sushi2) << "\n";

    // Test case 3: 1 1 1 1 1 2 2 2 2 2
    // Runs: [5, 5]
    // Answer: 10
    std::cout << "\n--- Test case 3 ---\n";
    std::array<int, 10> sushi3 = {1, 1, 1, 1, 1, 2, 2, 2, 2, 2};

    std::cout << "Sushi sequence: ";
    for (int s : sushi3) std::cout << s << " ";
    std::cout << "\n";
    std::cout << "Maximum sushi for two: " << max_sushi(sushi3) << "\n";

    return 0;
}
