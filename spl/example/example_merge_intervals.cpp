// SPL Merge Intervals Example
// Demonstrates using chunk_by to merge overlapping intervals

#include <spl/spl.h>
#include <iostream>
#include <vector>
#include <algorithm>
#include <utility>

int main() {
    // 10 overlapping intervals in range [0, 20]
    std::vector<std::pair<int, int>> intervals = {
        {1, 3}, {2, 6}, {8, 10}, {15, 18},
        {17, 20}, {5, 7}, {12, 14}, {0, 1},
        {9, 11}, {14, 16}
    };

    // Merge overlapping intervals using chunk_by
    auto merged = spl::apply(std::move(intervals),
        spl::sort(),
        spl::chunk_by(
            // Two intervals overlap if current.first <= previous.second
            [](auto& prev, auto& curr) {
                return curr.first <= prev.second;
            },
            // Accumulate each chunk into a single merged interval
            spl::accumulate_in_place(
                std::pair<int,int>{},
                [](std::pair<int,int>& acc, const std::pair<int,int>& interval) {
                    if (acc.first == 0 && acc.second == 0) {
                        acc = interval;  // First interval in chunk
                    } else {
                        acc.first = std::min(acc.first, interval.first);
                        acc.second = std::max(acc.second, interval.second);
                    }
                }
            )
        ),
        spl::to_vector()
    );


    std::cout << "\n\nMerged intervals:\n";
    for (const auto& [start, end] : merged) {
        std::cout << "[" << start << ", " << end << "] ";
    }
    std::cout << "\n";

    return 0;
}
