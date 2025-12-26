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

// Transform functions (using primes not in filter set: 13, 17, 19, 23, 29)
constexpr auto times_13 = [](int i) { return i * 13; };
constexpr auto plus_17 = [](int i) { return i + 17; };
constexpr auto times_19 = [](int i) { return i * 19; };
constexpr auto minus_23 = [](int i) { return i - 23; };
constexpr auto times_29 = [](int i) { return i * 29; };

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

// ============================================================================
// Transform Chain Benchmarks
// ============================================================================

// Handwritten transform chain
static void BM_HandwrittenTransform(benchmark::State& state) {
    auto end = state.range(0);
    for (auto _ : state) {
        long long result = 0;
        for (int i = 0; i < end; ++i) {
            result += ((((i * 13) + 17) * 19) - 23) * 29;
        }
        benchmark::DoNotOptimize(result);
    }
}
BENCHMARK(BM_HandwrittenTransform)->Range(1000, 1000000);

// std::ranges transform chain
static void BM_StdRangesTransform(benchmark::State& state) {
    auto end = state.range(0);
    for (auto _ : state) {
        using std::ranges::views::transform;
        long long result = std::ranges::fold_left(std::ranges::iota_view(0, end)
            | transform(times_13)
            | transform(plus_17)
            | transform(times_19)
            | transform(minus_23)
            | transform(times_29), 0LL, std::plus<>{});
        benchmark::DoNotOptimize(result);
    }
}
BENCHMARK(BM_StdRangesTransform)->Range(1000, 1000000);

// SPL transform chain
static void BM_SplTransform(benchmark::State& state) {
    auto end = state.range(0);
    for (auto _ : state) {
        long long result = spl::apply(spl::iota(0, end),
            spl::transform(times_13),
            spl::transform(plus_17),
            spl::transform(times_19),
            spl::transform(minus_23),
            spl::transform(times_29),
            spl::sum()
        );
        benchmark::DoNotOptimize(result);
    }
}
BENCHMARK(BM_SplTransform)->Range(1000, 1000000);

// ============================================================================
// Mixed Transform + Filter Benchmarks
// ============================================================================

// Handwritten mixed transform/filter
static void BM_HandwrittenMixed(benchmark::State& state) {
    auto end = state.range(0);
    for (auto _ : state) {
        long long result = 0;
        for (int i = 0; i < end; ++i) {
            long long x = i * 13;
            if (x % 3 != 0) {
                x = x + 17;
                if (x % 5 != 0) {
                    x = x * 19;
                    if (x % 7 != 0) {
                        x = x - 23;
                        if (x % 11 != 0) {
                            result += x * 29;
                        }
                    }
                }
            }
        }
        benchmark::DoNotOptimize(result);
    }
}
BENCHMARK(BM_HandwrittenMixed)->Range(1000, 1000000);

// std::ranges mixed transform/filter
static void BM_StdRangesMixed(benchmark::State& state) {
    auto end = state.range(0);
    for (auto _ : state) {
        using std::ranges::views::transform;
        using std::ranges::views::filter;
        long long result = std::ranges::fold_left(std::ranges::iota_view(0, end)
            | transform(times_13)
            | filter(not_divisible_by_3)
            | transform(plus_17)
            | filter(not_divisible_by_5)
            | transform(times_19)
            | filter(not_divisible_by_7)
            | transform(minus_23)
            | filter(not_divisible_by_11)
            | transform(times_29), 0LL, std::plus<>{});
        benchmark::DoNotOptimize(result);
    }
}
BENCHMARK(BM_StdRangesMixed)->Range(1000, 1000000);

// SPL mixed transform/filter
static void BM_SplMixed(benchmark::State& state) {
    auto end = state.range(0);
    for (auto _ : state) {
        long long result = spl::apply(spl::iota(0, end),
            spl::transform(times_13),
            spl::filter(not_divisible_by_3),
            spl::transform(plus_17),
            spl::filter(not_divisible_by_5),
            spl::transform(times_19),
            spl::filter(not_divisible_by_7),
            spl::transform(minus_23),
            spl::filter(not_divisible_by_11),
            spl::transform(times_29),
            spl::sum()
        );
        benchmark::DoNotOptimize(result);
    }
}
BENCHMARK(BM_SplMixed)->Range(1000, 1000000);

}  // namespace
