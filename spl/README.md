# SPL - Skydown Pipeline Library

A modern C++23 library for composable, constexpr-compatible stream processing pipelines. By the author of Rappel ([slides](https://schedule.cppnow.org/wp-content/uploads/2024/02/Rappel.pdf), [video](https://www.youtube.com/watch?v=itnyR9j8y6E) - C++Now 2024).

## Overview

SPL provides a functional approach to data transformation using composable pipeline stages. Unlike traditional range libraries, SPL pipelines:

- Are fully **constexpr-compatible** - use at compile-time or runtime
- Support **multi-argument streams** - process tuples of values, not just single elements
- Handle **processing style transitions** - automatically convert between incremental and complete processing
- Enable **early termination** - stages like `take()` and `filter()` can stop processing early

## Quick Start

```cpp
#include <spl/spl.h>
#include <vector>

int main() {
    std::vector<int> numbers = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};

    // Sum of squares of even numbers
    auto result = spl::apply(numbers,
        spl::filter([](int x) { return x % 2 == 0; }),
        spl::transform([](int x) { return x * x; }),
        spl::sum()
    );
    // result = 4 + 16 + 36 + 64 + 100 = 220
}
```

## Namespace Organization

SPL uses a three-tier namespace structure:

| Namespace | Purpose | Audience |
|-----------|---------|----------|
| `spl::` | High-level API | All users |
| `spl::impl::` | Stage implementation API | Advanced users creating custom stages |
| `spl::detail::` | Private implementation | Internal use only |

---

# Public API (`spl::`)

## Core Functions

### `apply(range, stages...)`

Applies a pipeline of stages to a range and returns the result.

```cpp
std::vector<int> v = {1, 2, 3, 4, 5};

// Get sum of doubled values
auto result = spl::apply(v,
    spl::transform([](int x) { return x * 2; }),
    spl::sum()
);
// result = 30
```

### `compose(stages...)`

Composes multiple stages into a single reusable stage.

```cpp
// Create a reusable pipeline
auto double_and_square = spl::compose(
    spl::transform([](int x) { return x * 2; }),
    spl::transform([](int x) { return x * x; })
);

auto result = spl::apply(std::vector{1, 2, 3},
    double_and_square,
    spl::to_vector()
);
// result = {4, 16, 36}
```

---

## Transformation Stages

### `transform(f)`

Applies a function to each element.

```cpp
spl::apply(std::vector{1, 2, 3},
    spl::transform([](int x) { return x * x; }),
    spl::to_vector()
);
// {1, 4, 9}
```

### `transform_cps(f)`

Transform using continuation-passing style. The function receives an output callback.

```cpp
spl::apply(std::vector{1, 2, 3},
    spl::transform_cps([](auto&& out, int x) {
        return out(x, x * x);  // Output two values per input
    }),
    spl::to_vector()  // Won't work - use with stages that accept 2 args
);
```

### `transform_complete(f)`

Transform applied to complete (buffered) data.

```cpp
spl::apply(std::vector{3, 1, 2},
    spl::to_vector(),
    spl::transform_complete([](auto v) {
        std::sort(v.begin(), v.end());
        return v;
    })
);
// {1, 2, 3}
```

### `transform_arg<I>(f)`

Transform only the I-th argument in a multi-argument stream.

```cpp
spl::apply(std::vector{1, 2, 3},
    spl::zip(std::vector{10, 20, 30}),  // Stream of (int, int)
    spl::transform_arg<1>([](int x) { return x * 2; }),  // Double second arg
    spl::to_vector()  // Still won't work - need to reduce to 1 arg
);
```

---

## Filtering Stages

### `filter(predicate)`

Keep only elements matching the predicate.

```cpp
spl::apply(std::vector{1, 2, 3, 4, 5},
    spl::filter([](int x) { return x % 2 == 0; }),
    spl::to_vector()
);
// {2, 4}
```

### `filter()`

Filter using implicit boolean conversion (removes falsy values).

```cpp
spl::apply(std::vector{0, 1, 0, 2, 0, 3},
    spl::filter(),
    spl::to_vector()
);
// {1, 2, 3}
```

### `filter_arg<I>(predicate)`

Filter based on the I-th argument only.

### `take(n)`

Take only the first n elements.

```cpp
spl::apply(std::vector{1, 2, 3, 4, 5},
    spl::take(3),
    spl::to_vector()
);
// {1, 2, 3}
```

### `take_while(predicate)`

Take elements while predicate is true.

```cpp
spl::apply(std::vector{1, 2, 3, 4, 5},
    spl::take_while([](int x) { return x < 4; }),
    spl::to_vector()
);
// {1, 2, 3}
```

### `skip(n)`

Skip the first n elements.

```cpp
spl::apply(std::vector{1, 2, 3, 4, 5},
    spl::skip(2),
    spl::to_vector()
);
// {3, 4, 5}
```

### `skip_while(predicate)`

Skip elements while predicate is true.

```cpp
spl::apply(std::vector{1, 2, 3, 4, 5},
    spl::skip_while([](int x) { return x < 3; }),
    spl::to_vector()
);
// {3, 4, 5}
```

### `filter_duplicates()`

Remove consecutive duplicate elements.

```cpp
spl::apply(std::vector{1, 1, 2, 2, 2, 3, 1, 1},
    spl::filter_duplicates(),
    spl::to_vector()
);
// {1, 2, 3, 1}
```

---

## Accumulation Stages

### `sum()`

Sum all elements.

```cpp
spl::apply(std::vector{1, 2, 3, 4, 5}, spl::sum());
// 15
```

### `count()`

Count all elements.

```cpp
spl::apply(std::vector{1, 2, 3, 4, 5}, spl::count());
// 5
```

### `first()`

Get the first element as `std::optional`.

```cpp
spl::apply(std::vector{1, 2, 3}, spl::first());
// std::optional<int>(1)

spl::apply(std::vector<int>{}, spl::first());
// std::nullopt
```

### `last()`

Get the last element as `std::optional`.

```cpp
spl::apply(std::vector{1, 2, 3}, spl::last());
// std::optional<int>(3)
```

### `min()` / `min(comp)`

Get the minimum element.

```cpp
spl::apply(std::vector{3, 1, 4, 1, 5}, spl::min());
// std::optional<int>(1)
```

### `max()` / `max(comp)`

Get the maximum element.

```cpp
spl::apply(std::vector{3, 1, 4, 1, 5}, spl::max());
// std::optional<int>(5)
```

### `minmax()` / `minmax(comp)`

Get both min and max as `std::optional<std::pair<T, T>>`.

```cpp
spl::apply(std::vector{3, 1, 4, 1, 5}, spl::minmax());
// std::optional<std::pair<int, int>>({1, 5})
```

### `accumulate(f)`

Reduce using a binary function, starting with a default-constructed accumulator.

```cpp
spl::apply(std::vector{1, 2, 3, 4}, spl::accumulate(std::plus<>{}));
// 10
```

### `accumulate(init, f)`

Reduce with an explicit initial value.

```cpp
spl::apply(std::vector{1, 2, 3, 4},
    spl::accumulate(100, std::plus<>{})
);
// 110
```

### `accumulate_in_place(init, f)`

Accumulate by mutating the accumulator in place.

```cpp
spl::apply(std::vector{1, 2, 3},
    spl::accumulate_in_place(
        std::vector<int>{},
        [](auto& v, int x) { v.push_back(x * 2); }
    )
);
// {2, 4, 6}
```

### `for_each(f)`

Execute a function for each element (for side effects).

```cpp
int total = 0;
spl::apply(std::vector{1, 2, 3},
    spl::for_each([&](int x) { total += x; })
);
// total = 6
```

---

## Predicate Stages

### `any_of(predicate)`

Check if any element matches.

```cpp
spl::apply(std::vector{1, 2, 3, 4, 5},
    spl::any_of([](int x) { return x > 3; })
);
// true
```

### `all_of(predicate)`

Check if all elements match.

```cpp
spl::apply(std::vector{2, 4, 6, 8},
    spl::all_of([](int x) { return x % 2 == 0; })
);
// true
```

### `none_of(predicate)`

Check if no elements match.

```cpp
spl::apply(std::vector{1, 3, 5, 7},
    spl::none_of([](int x) { return x % 2 == 0; })
);
// true
```

---

## Collection Builders

### `to_vector()`

Collect elements into a `std::vector`.

```cpp
spl::apply(std::array{1, 2, 3},
    spl::transform([](int x) { return x * 2; }),
    spl::to_vector()
);
// std::vector{2, 4, 6}
```

### `push_back_to<Container>()`

Collect into a specific container type.

```cpp
spl::apply(std::vector{1, 2, 3},
    spl::push_back_to<std::deque>()
);
// std::deque{1, 2, 3}
```

### `push_back_into(container)`

Append elements into an existing container.

```cpp
std::vector<int> v = {1, 2, 3};
spl::apply(std::vector{4, 5, 6},
    spl::push_back_into(v)
);
// v = {1, 2, 3, 4, 5, 6}
```

### `to_map<MapType>()`

Collect pairs into a map.

```cpp
spl::apply(std::vector{1, 2, 3},
    spl::transform([](int x) { return x; }),
    spl::zip_result([](int x) { return x * x; }),
    spl::to_map<std::map>()
);
// std::map{{1, 1}, {2, 4}, {3, 9}}
```

---

## Sorting Stages

### `sort()` / `sort(comparator)`

Sort the collection.

```cpp
spl::apply(std::vector{3, 1, 4, 1, 5},
    spl::to_vector(),
    spl::sort()
);
// {1, 1, 3, 4, 5}

spl::apply(std::vector{3, 1, 4, 1, 5},
    spl::to_vector(),
    spl::sort(std::greater<>{})
);
// {5, 4, 3, 1, 1}
```

### `stable_sort()` / `stable_sort(comparator)`

Stable sort preserving relative order of equal elements.

### `partial_sort(n)` / `partial_sort(n, comparator)`

Sort only the first n elements.

### `nth_element(n)` / `nth_element(n, comparator)`

Partially sort so the n-th element is in its sorted position.

### `unique()` / `unique(predicate)`

Remove consecutive duplicates (use after sort for global uniqueness).

```cpp
spl::apply(std::vector{3, 1, 4, 1, 5, 1},
    spl::to_vector(),
    spl::sort(),
    spl::unique()
);
// {1, 3, 4, 5}
```

---

## Flattening Stages

### `flatten()`

Flatten nested ranges.

```cpp
spl::apply(std::vector<std::vector<int>>{{1, 2}, {3, 4}, {5}},
    spl::flatten(),
    spl::to_vector()
);
// {1, 2, 3, 4, 5}
```

### `flatten_arg<I>()`

Flatten only the I-th argument.

### `flat_map(f)`

Map each element to a range and flatten the results.

```cpp
spl::apply(std::vector{1, 2, 3},
    spl::flat_map([](auto&& out, int x) {
        for (int i = 0; i < x; ++i) out(x);
        return true;
    }),
    spl::to_vector()
);
// {1, 2, 2, 3, 3, 3}
```

### `flatten_optional()` / `flatten_optional_arg<I>()`

Unwrap optionals, filtering out `std::nullopt` values. This is equivalent to `compose(filter_arg<I>(), deref_arg<I>())` - it filters out empty optionals and dereferences the remaining ones.

Unlike `unwrap_optional()` which short-circuits the entire pipeline on the first `nullopt`, `flatten_optional()` simply skips `nullopt` values and continues processing.

```cpp
// Basic usage - filter out nullopt and unwrap
spl::apply(std::vector<std::optional<int>>{{1}, std::nullopt, {3}},
    spl::flatten_optional(),
    spl::to_vector()
);
// {1, 3}

// With transform - process only valid values
spl::apply(std::vector<std::optional<int>>{{1}, std::nullopt, {2}, std::nullopt, {3}},
    spl::flatten_optional(),
    spl::transform([](int x) { return x * x; }),
    spl::to_vector()
);
// {1, 4, 9}

// flatten_optional_arg<I>() for multi-argument streams
spl::apply(std::vector{1, 2, 3},
    spl::zip(std::vector<std::optional<int>>{{10}, std::nullopt, {30}}),
    spl::flatten_optional_arg<1>(),  // Flatten the second argument (index 1)
    spl::transform([](int a, int b) { return a + b; }),
    spl::to_vector()
);
// {11, 33} - pair (2, nullopt) is filtered out
```

---

## Zipping and Combining

### `zip(range)` / `zip(ranges...)`

Combine streams element-wise.

```cpp
spl::apply(std::vector{1, 2, 3},
    spl::zip(std::vector{'a', 'b', 'c'}),
    spl::transform([](int n, char c) {
        return std::string(n, c);
    }),
    spl::to_vector()
);
// {"a", "bb", "ccc"}
```

### `zip_result(f)`

Add a computed value to the stream.

```cpp
spl::apply(std::vector{1, 2, 3},
    spl::zip_result([](int x) { return x * x; }),
    // Stream now has (int, int) pairs
    spl::transform([](int x, int sq) { return sq; }),
    spl::to_vector()
);
// {1, 4, 9}
```

### `chain_before(range)`

Prepend a range before processing.

```cpp
spl::apply(std::vector{4, 5, 6},
    spl::chain_before(std::vector{1, 2, 3}),
    spl::to_vector()
);
// {1, 2, 3, 4, 5, 6}
```

### `chain_after(range)`

Append a range after processing.

```cpp
spl::apply(std::vector{1, 2, 3},
    spl::chain_after(std::vector{4, 5, 6}),
    spl::to_vector()
);
// {1, 2, 3, 4, 5, 6}
```

---

## Grouping Stages

### `group_by(selector, stages...)`

Group elements by a key and process each group with the given stages.

```cpp
spl::apply(std::vector{1, 2, 3, 4, 5, 6},
    spl::group_by(
        [](int x) { return x % 2; },  // Key selector
        spl::to_vector()               // Process each group
    ),
    spl::to_vector()
);
// Vector of (key, group_result) pairs
```

### `chunk(n, stages...)`

Split into fixed-size chunks, processing each chunk.

```cpp
spl::apply(std::vector{1, 2, 3, 4, 5, 6, 7},
    spl::chunk(3, spl::to_vector()),
    spl::to_vector()
);
// {{1, 2, 3}, {4, 5, 6}, {7}}
```

### `chunk_by(predicate, stages...)`

Split into chunks where adjacent elements satisfy the predicate.

---

## Parallel Processing

### `tee(stages...)`

Process the same stream through multiple pipelines in parallel.

```cpp
spl::apply(std::vector{1, 2, 3, 4, 5},
    spl::tee(spl::sum(), spl::count()),
    spl::transform_complete([](int sum, size_t count) {
        return static_cast<double>(sum) / count;
    })
);
// 3.0 (average)
```

---

## Tuple/Argument Manipulation

### `swizzle<Indices...>()`

Reorder or duplicate stream arguments.

```cpp
spl::apply(std::vector{1},
    spl::zip(std::vector{2}),
    spl::zip(std::vector{3}),
    // Stream has (int, int, int)
    spl::swizzle<2, 0, 1>(),  // Reorder to (third, first, second)
    // ...
);
```

### `make_tuple()` / `make_pair()`

Combine multiple stream arguments into a tuple/pair.

### `expand_tuple()`

Expand a tuple into multiple stream arguments.

### `enumerate(start = 0)`

Add an index to each element.

```cpp
spl::apply(std::vector{'a', 'b', 'c'},
    spl::enumerate(),
    // Stream has (size_t, char)
    spl::transform([](size_t i, char c) {
        return std::to_string(i) + ": " + c;
    }),
    spl::to_vector()
);
// {"0: a", "1: b", "2: c"}
```

---

## Value Manipulation

### `ref()` / `ref_arg<I>()`

Convert value to `std::reference_wrapper`.

### `deref()` / `deref_arg<I>()`

Dereference a pointer or reference wrapper.

### `move()` / `move_arg<I>()`

Force move semantics.

### `addressof()` / `addressof_arg<I>()`

Get address of value.

### `cast<T>()` / `cast_arg<I, T>()`

Cast to a different type.

### `construct<T>()`

Construct T from stream arguments.

---

## Generators

### `iota(start)` / `iota(start, end)`

Generate a sequence of integers.

```cpp
spl::apply(spl::iota(1, 6),
    spl::transform([](size_t x) { return x * x; }),
    spl::to_vector()
);
// {1, 4, 9, 16, 25}
```

### `generator<F>`

Create custom generators by wrapping a callable.

---

## Optional Handling

### `unwrap_optional()` / `unwrap_optional_arg<I>()`

Monadic bind for `std::optional`. Unwraps optionals and short-circuits the entire pipeline on the first `nullopt`, propagating the empty state to the final result.

This behaves like the monadic `>>=` (flatMap) operation:
- `Some(a) >>= f` → `f(a)` (continue processing)
- `None >>= f` → `None` (short-circuit, skip remaining computation)

The result is always wrapped in `std::optional` - either containing the computed value or `nullopt` if any input was empty.

```cpp
// All values present - pipeline completes successfully
spl::apply(std::vector<std::optional<int>>{{1}, {2}, {3}},
    spl::unwrap_optional(),
    spl::sum()
);
// std::optional<int>(6)

// A nullopt encountered - entire pipeline short-circuits
spl::apply(std::vector<std::optional<int>>{{1}, std::nullopt, {3}},
    spl::unwrap_optional(),
    spl::sum()
);
// std::nullopt (short-circuits on first nullopt, sum() never sees {3})

// unwrap_optional_arg<I>() for multi-argument streams
spl::apply(std::vector{1, 2, 3},
    spl::zip(std::vector<std::optional<int>>{{10}, {20}, std::nullopt}),
    spl::unwrap_optional_arg<1>(),  // Unwrap the second argument
    spl::transform([](int a, int b) { return a + b; }),
    spl::sum()
);
// std::nullopt (short-circuits when third pair has nullopt)
```

**Contrast with `flatten_optional()`**: Use `flatten_optional()` when you want to skip `nullopt` values and continue processing the rest. Use `unwrap_optional()` when any `nullopt` should invalidate the entire computation.

---

## Processing Style Conversion

### `as_incremental()`

Convert a complete-processing stage to incremental output.

---

## Repeat Stages

### `repeat(n)`

Repeat each element n times.

```cpp
spl::apply(std::vector{1, 2, 3},
    spl::repeat(2),
    spl::to_vector()
);
// {1, 1, 2, 2, 3, 3}
```

### `repeat_while(predicate)`

Repeat each element while predicate returns true.

---

# Stage Implementation API (`spl::impl::`)

The `spl::impl::` namespace provides tools for creating custom pipeline stages.

## Core Types

### `types<Ts...>`

A compile-time type list representing stream element types.

```cpp
using my_types = spl::impl::types<int, std::string>;
```

### `processing_style`

Enum indicating how a stage processes data:

```cpp
enum class processing_style {
    incremental,  // Process one element at a time
    complete      // Process entire collection at once
};
```

## Creating Custom Stages

### `stage<Impl, InputStyle, OutputStyle, Params...>(args...)`

Factory function to create stage instantiators.

```cpp
template<typename StageProperties, typename InputTypes, typename F>
struct my_stage_impl;

// Partial specialization
template<typename StageProperties, typename... InputTypes, typename F>
struct my_stage_impl<StageProperties, spl::impl::types<InputTypes...>, F>
    : spl::impl::stage_impl<my_stage_impl<StageProperties,
                                          spl::impl::types<InputTypes...>, F>> {
    using base = typename my_stage_impl::base;
    using output_types = spl::impl::types</* output types */>;

    F f;

    constexpr void process_incremental(InputTypes... inputs) {
        // Process and forward to next stage
        this->next.process_incremental(/* transformed output */);
    }

    constexpr decltype(auto) finish() {
        return this->next.finish();
    }
};

template<typename F>
constexpr auto my_stage(F f) {
    return spl::impl::stage<my_stage_impl,
                           spl::impl::processing_style::incremental,
                           spl::impl::processing_style::incremental,
                           F>(std::move(f));
}
```

### `stage_impl<Derived>`

CRTP base class providing common stage functionality:

- `next` - reference to the next stage in the pipeline
- `finish()` - default implementation forwards to next stage
- `done()` - check if pipeline should terminate early

### `make_pipeline<InputTypes, StartingStyle>(stages...)`

Low-level function to construct a pipeline from stages.

```cpp
auto pipeline = spl::impl::make_pipeline<
    spl::impl::types<int>,
    spl::impl::processing_style::incremental
>(my_stage1, my_stage2);

pipeline.process_incremental(42);
auto result = pipeline.finish();
```

## Outputters

### `incremental_outputter<Impl>`

Wraps a stage for incremental output. Call with `()` to output elements.

```cpp
spl::impl::incremental_outputter out{this->next};
out(value1, value2);  // Output to next stage
if (!out) { /* pipeline wants to stop */ }
```

### `type_calculating_outputter`

Special outputter for compile-time type deduction.

```cpp
using output_types = decltype(my_function(
    spl::impl::type_calculating_outputter(),
    std::declval<InputTypes>()...
));
```

### `as_outputter<Outputter, F>`

Wrap an outputter with a transformation function.

### `calculate_type_v<T>`

Compile-time constant: `true` if T is a type-calculating outputter.

```cpp
if constexpr (spl::impl::calculate_type_v<Output>) {
    return output(/* for type deduction */);
} else {
    output(/* actual processing */);
    return true;
}
```

## Concepts

### `incremental_input<Stage>`

True if stage expects incremental input.

### `complete_input<Stage>`

True if stage expects complete input.

### `incremental_output<Stage>`

True if stage produces incremental output.

### `complete_output<Stage>`

True if stage produces complete output.

---

## Example: Custom Stage

Here's a complete example of a custom stage that doubles every value:

```cpp
#include <spl/spl.h>

namespace my {

// Forward declaration
template<typename StageProperties, typename InputTypes>
struct double_impl;

// Partial specialization
template<typename StageProperties, typename... InputTypes>
struct double_impl<StageProperties, spl::impl::types<InputTypes...>>
    : spl::impl::stage_impl<double_impl<StageProperties, spl::impl::types<InputTypes...>>> {

    using base = typename double_impl::base;
    using output_types = spl::impl::types<InputTypes...>;

    constexpr void process_incremental(InputTypes... inputs) {
        // Output each value doubled
        ((this->next.process_incremental(inputs * 2)), ...);
    }
};

constexpr auto double_values() {
    return spl::impl::stage<double_impl,
                           spl::impl::processing_style::incremental,
                           spl::impl::processing_style::incremental>();
}

} // namespace my

// Usage
auto result = spl::apply(std::vector{1, 2, 3},
    my::double_values(),
    spl::to_vector()
);
// {2, 4, 6}
```

---

## Requirements

- C++23 compatible compiler
- Header-only library

## License

See repository LICENSE file.
