// SPL Benchmarks
// Based on Rappel benchmarks comparing:
// 1. Handwritten loop (optimized)
// 2. std::ranges (chain of filters)
// 3. SPL (chain of filters)

#include <benchmark/benchmark.h>
#include <spl/spl.h>
#include <vector>
#include <ranges>
#include <numeric>

namespace {

// Individual predicates for ranges and SPL
constexpr auto not_divisible_by_2 = [](int i) { return i % 2 != 0; };
constexpr auto not_divisible_by_3 = [](int i) { return i % 3 != 0; };
constexpr auto not_divisible_by_5 = [](int i) { return i % 5 != 0; };
constexpr auto not_divisible_by_7 = [](int i) { return i % 7 != 0; };
constexpr auto not_divisible_by_11 = [](int i) { return i % 11 != 0; };

// 1. Handwritten loop - explicitly optimized with inline test
static void BM_HandwrittenLoop(benchmark::State& state) {
    auto end = state.range(0);
    for (auto _ : state) {
        int result = 0;
        for (int i = 0; i < end; ++i) {
            if (i % 2 != 0 && i % 3 != 0 && i % 5 != 0
                && i % 7 != 0 && i % 11 != 0) {
                result += i;
            }
        }
        benchmark::DoNotOptimize(result);
    }
}
BENCHMARK(BM_HandwrittenLoop)->Range(1000, 1000000);

// 2. std::ranges - chain of separate filters
static void BM_StdRanges(benchmark::State& state) {
    auto end = state.range(0);
    for (auto _ : state) {
        using std::ranges::views::filter;
        using std::ranges::views::transform;
        int result = std::ranges::fold_left(std::ranges::iota_view(0,end)
            | filter(not_divisible_by_2)
            | filter(not_divisible_by_3)
            | filter(not_divisible_by_5)
            | filter(not_divisible_by_7)
            | filter(not_divisible_by_11), 0, std::plus<>{});
        benchmark::DoNotOptimize(result);
    }
}
BENCHMARK(BM_StdRanges)->Range(1000, 1000000);

// 3. SPL - chain of separate filters
static void BM_Spl(benchmark::State& state) {
    auto end = state.range(0);
    for (auto _ : state) {
        int result = spl::apply(spl::iota(0,end),
            spl::filter(not_divisible_by_2),
            spl::filter(not_divisible_by_3),
            spl::filter(not_divisible_by_5),
            spl::filter(not_divisible_by_7),
            spl::filter(not_divisible_by_11),
            spl::sum()
        );
        benchmark::DoNotOptimize(result);
    }
}
BENCHMARK(BM_Spl)->Range(1000, 1000000);

}  // namespace
