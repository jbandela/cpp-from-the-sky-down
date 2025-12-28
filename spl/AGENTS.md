# SPL (Skydown Pipeline Library) - AI Agent Guide

## Overview

SPL is a C++23 header-only library for composable, constexpr-friendly data processing pipelines. It provides a push-based alternative to `std::ranges` with better debug performance and compile-time evaluation.

## Project Structure

```
spl/
├── library/include/spl/spl.h    # Main header (single-file library)
├── tests/tests.cpp              # GTest-based test suite (419+ tests)
├── benchmarks/benchmarks.cpp    # Google Benchmark comparisons
├── example/                     # Example programs
└── AGENTS.md                    # This file
```

## Key Concepts

### Pipeline Architecture

SPL uses a **push-based** model where data flows through a chain of stages:

```cpp
spl::apply(source,
    spl::transform(f),    // Stage 1: transform each element
    spl::filter(pred),    // Stage 2: filter elements
    spl::sum()            // Stage 3: accumulate result
);
```

### Stage Types

1. **Incremental stages** - Process elements one at a time (`filter`, `transform`, `take`)
2. **Complete stages** - Process entire collections (`sort`, `transform_complete`)
3. **Accumulating stages** - Collect results (`sum`, `to_vector`, `accumulate`)

## Development Guidelines

### Building and Testing

```bash
# Build and run tests
cd spl/tests
cmake -B build -DCMAKE_BUILD_TYPE=Debug
cmake --build build
./build/run_tests

# Build and run benchmarks
cd spl/benchmarks
cmake -B build -DCMAKE_BUILD_TYPE=Release  # Use Release for accurate benchmarks
cmake --build build
./build/run_benchmarks
```

### Code Style

- Use `constexpr` wherever possible
- Use `std::invoke` to call functors (supports pointer-to-member functions like `&std::pair<int,int>::first`)
- Use `[[no_unique_address]]` for stateless functors

### Adding New Stages

The recommended approach is to compose new stages from existing building blocks:

- `flat_map` - For stages that may produce zero or more outputs per input
- `transform` - For 1:1 transformations
- `transform_cps` - For transformations with continuation-passing style
- `filter` - For filtering elements
- `accumulate_in_place` - For accumulating results

Example of composing a new stage:

```cpp
// Custom stage built on existing primitives
template<typename F>
constexpr auto my_custom_stage(F f) {
  return spl::transform([f](auto&& x) {
    return some_operation(f, x);
  });
}
```

For more complex stages, use `flat_map` with a lambda:

```cpp
template<typename F>
constexpr auto my_stage(F f) {
  return flat_map([f](auto&& out, auto&&... inputs) {
    if constexpr (impl::calculate_type_v<decltype(out)>) {
      return out(/* output types */);
    } else {
      // Process inputs and call out() for each output
      out(/* outputs */);
      return true;  // Continue processing
    }
  });
}
```

### Testing

- All tests are in `spl/tests/tests.cpp` using Google Test
- Include both runtime and constexpr tests where applicable
- Test edge cases: empty ranges, single elements, early termination

## Common Patterns

### Generator (lazy infinite sequences)
```cpp
spl::apply(spl::iota(0),           // 0, 1, 2, ...
    spl::filter(is_prime),
    spl::take(100),
    spl::to_vector()
);
```

### Nested iteration with flatten
```cpp
spl::apply(outer_range,
    spl::zip_result([](auto x) { return inner_range(x); }),
    spl::flatten(),
    ...
);
```

### Group by with sub-aggregation
```cpp
spl::apply(data,
    spl::group_by(&Item::category, spl::sum()),
    spl::to_vector()
);
```

### Using pointer-to-member
```cpp
spl::apply(pairs,
    spl::transform(&std::pair<int,int>::first),  // Extract first element
    spl::sum()
);
```

## File References

- **README.md** - Comprehensive API documentation with examples for all stages
- Main implementation: `library/include/spl/spl.h`
- Tests: `tests/tests.cpp`
- Benchmarks: `benchmarks/benchmarks.cpp`
- Examples: `example/*.cpp`

## Dependencies

- C++23 compiler (clang 16+, GCC 13+)
- Google Test (fetched automatically for tests)
- Google Benchmark (fetched automatically for benchmarks)
