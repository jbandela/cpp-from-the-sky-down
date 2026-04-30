class: title-slide
background-image: url(spl_title_card.png)
background-size: contain
background-position: center
background-repeat: no-repeat

---
class: center, middle
# HISTORY

---
# 1986

---
# CHALLENGE

Given a text file and an integer k, print the k most common words in
the file (and the number of their occurrences) in decreasing frequency.

---
# DON KNUTH

Produces an absolute virtuoso work of art using literate programming and WEB.

* Elegant Pascal code
* Tries
* Beautifully typeset with cross-references

---
# DOUG MCILROY

```bash
tr -cs A-Za-z '\n' |
tr A-Z a-z |
sort |
uniq -c |
sort -rn |
sed ${1}q
```

---
# PIPELINE STYLE

Writing a series of transformations as a single expression without
nested parentheses, rather than the more standard style of using
separate invocations.

* Common in other languages
* Bash uses `|` to "pipe" data through a sequence of programs.
* F# uses `|>`
* The primary syntactic feature is the lack of unbounded nesting.

---
# C++ PSEUDOCODE

```cpp
// Standard C++, non-pipelined
auto result1 = f1(input);
auto result2 = f2(result1);
auto result3 = f3(result2);

// Pipeline style
auto result = input | f1 | f2 | f3;
```

---
# TERMINOLOGY

**Pipeline**

The entire series: `input | f1 | f2 | f3;`

**Stage**

Each individual component of the pipeline: `f1`, `f2`, `f3`

Also have seen *combinator* used for this, but this talk will use *stage*

---
# STD::RANGES

```cpp
auto const ints = {0, 1, 2, 3, 4, 5};
auto even = [](int i) { return 0 == i % 2; };
auto square = [](int i) { return i * i; };

for (int i : ints
    | std::views::filter(even)
    | std::views::transform(square))
  std::cout << i << ' ';
```

https://en.cppreference.com/w/cpp/ranges#Range_ada

---
# A MORE COMPLICATED EXAMPLE

```cpp
using IntAndString = std::pair<int, std::string>;
auto make_int_and_string = [](int i) -> IntAndString {
  return {i*i*i, std::to_string(i)};
};
auto result = std::views::iota(1,1000001)
  | std::views::transform(make_int_and_string)
  | std::views::filter([](const auto& p) {
      return p.first >= std::hash<std::string>()(p.second);
    })
  | std::views::transform(&IntAndString::second)
  | std::views::take(4);
for (auto s : result)
  std::cout << s << "\n";
```

---
# CRASH

---
# WHY?

```cpp
using IntAndString = std::pair<int, std::string>;
auto make_int_and_string = [](int i) -> IntAndString {
  return {i*i*i, std::to_string(i)};
};
auto result = std::views::iota(1,1000001)
  | std::views::transform(make_int_and_string)
  | std::views::filter([](const auto& p) {
      return p.first >= std::hash<std::string>()(p.second);
    })
* | std::views::transform(&IntAndString::second)
  | std::views::take(4);
for (auto s : result)
  std::cout << s << "\n";
```

---
# REDUCED

```cpp
using IntAndString = std::pair<int, std::string>;
auto make_int_and_string = [](int i) -> IntAndString {
  return {i*i*i, std::to_string(i)};
};
auto result = std::views::iota(1,1000001)
  | std::views::transform(make_int_and_string)
  | std::views::transform(&IntAndString::second)
for (auto s : result)
  std::cout << s << "\n";
```

---
# TRANSFORM ITERATORS

```cpp
struct MakeIntAndStringIterator{
  IotaIterator iota_iter;
  IntAndString operator*(){
    return make_int_and_string(*iota_iter);
  }
};

struct SecondIterator{
  MakeIntAndStringIterator int_and_string_iter;
  std::string& operator*(){
    return (*int_and_string_iter).second;  // dangling!
  }
}
```

---
# TRANSFORM ITERATORS INLINED

```cpp
struct SecondIteratorInlined{
  IotaIterator iota_iter;
  std::string& operator*(){
    int i = *iota_iterator;
    return std::pair<int,string>{i*i*i,std::to_string(i)}.second;
    //     ^^^ temporary destroyed, reference dangles
  }
}
```

---
# TPOIASI

```cpp
int times2(int n){
  return n * 2;
}
bool isMultipleOf4(int n){return n % 4 == 0;}

int main() {
  std::vector<int> numbers = {1, 2, 3, 4, 5};
  auto results = numbers
    | std::ranges::views::transform(times2)
    | std::ranges::views::filter(isMultipleOf4);
  for (auto result : results)
    std::cout << result << '\n';
}
```

https://www.fluentcpp.com/2019/02/12/the-terrible-problem-of-incrementing-a-smart-iterator/

---
# OUTPUT

```
4
8
```

---
# TPOIASI (with logging)

```cpp
int times2(int n){
  std::cout << "transforming " << n << "\n";
  return n * 2;
}
```

---
# OUTPUT

```
transforming 1
transforming 2
transforming 2     <-- duplicate!
4
transforming 3
transforming 4
transforming 4     <-- duplicate!
8
transforming 5
```

The filter iterator has an embedded loop.

---
# STACK SIZE

```cpp
__attribute__((noinline)) void Main7() {
  auto even = [](auto i) { return i % 2 == 0; };
  std::vector<int> r0 = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
  auto result = r0 | std::views::filter(even)
    | std::views::filter(even)
    | std::views::filter(even)
    | std::views::filter(even)
    | std::views::filter(even)
    | std::views::filter(even)
    | std::views::filter(even);
  PrintStack("Main7");
}
```

https://godbolt.org/z/6zhGaf588

---
# STACK SIZE OUTPUT

```
Main0 192 144 32 0
Main1 192 192 32 0
Main2 192 240 32 0
Main3 192 320 32 0
Main4 192 448 32 0
Main5 192 608 32 0
Main6 192 832 32 0
Main7 192 1120 32 0
```

---
# CUBIC GROWTH

`(144 + (N+1)(N+2)(N+3)/6 + 2N) * 8 bytes`

h/t Richard Smith

---
class: center, middle
# RETHINKING

---
# `<iterator>`

* Lazy
* Pull
* Interactive
* return

---
# `<algorithm>`

```cpp
template< class InputIt, class OutputIt, class UnaryOp >
OutputIt transform( InputIt first1, InputIt last1,
                    OutputIt d_first, UnaryOp unary_op );
```

* Eager
* Push
* Batch
* Continuation passing style — `d_first`

---
# IMPORTANT ANNOUNCEMENT

Do you or your friends mainly use ranges to:
--

* Iterate with range for
--

* Pass to an algorithm
--

* Construct a collection

--

**YOU MAY BE PAYING TOO MUCH FOR FLEXIBILITY**

---
# PREMISE

Trade flexibility of `<iterator>` for safety and efficiency of `<algorithm>`

---
# RETURN VS CONTINUATION PASSING STYLE

```cpp
void Output(const std::string& s){
  std::cout << s << "\n";
}
std::string& Iterator(){
  return std::pair{1, std::string("Hello")}.second;
  //     ^^^ dangling reference!
}
void Unsafe(){
  Output(Iterator());
}
```

---
# RETURN VS CONTINUATION PASSING STYLE (safe)

```cpp
void Output(const std::string& s){
  std::cout << s << "\n";
}
template<typename F>
void Algorithm(F f){
  f(std::pair<int, std::string>(1, "Hello").second);
  // temporary lives through the call
}
void Safe(){
  Algorithm(&Output);
}
```

---
# LOST LUGGAGE IS A DANGLING REFERENCE

---
# LANDINGS (LIKE RETURNS) ARE AN OPPORTUNITY TO DANGLE REFERENCES

---
class: center, middle
# SPL

---
# WHAT IS SPL?

The **Skydown Pipeline Library** — a C++23 header-only library for
algorithm composition

* Based on the eager model of `<algorithm>`
* Passes each value through the series of transformations using
  continuation passing style
* `constexpr`-compatible: works at compile-time *or* runtime

---
# QUICK EXAMPLES

We will have an example that we look at more in depth coming up. This
is mainly just to get a feel of the shape.

---
# CRASH EXAMPLE (SPL)

```cpp
using IntAndString = std::pair<int, std::string>;
auto make_int_and_string = [](int i) -> IntAndString {
  return {i*i*i, std::to_string(i)};
};

spl::apply(spl::iota(1, 10000001),
  spl::transform(make_int_and_string),
  spl::filter([](const auto& p) {
    return p.first >= std::hash<std::string>()(p.second);
  }),
  spl::transform(&IntAndString::second),
  spl::take(4),
  spl::for_each([](const auto& s){
    std::cout << s << "\n";
  })
);
```

---
# OUTPUT

```
1291
1292
1293
1294
```

---
# TPOIASI (SPL)

```cpp
int times2(int n){
  std::cout << "transforming: " << n << "\n";
  return n * 2;
}
bool isMultipleOf4(int n){return n % 4 == 0;}

int main() {
  std::vector<int> numbers = {1, 2, 3, 4, 5};
  spl::apply(numbers,
    spl::transform(times2),
    spl::filter(isMultipleOf4),
    spl::for_each([](auto result){
      std::cout << result << '\n';
    }));
}
```

---
# OUTPUT

```
transforming: 1
transforming: 2
4
transforming: 3
transforming: 4
8
transforming: 5
```

---
# SIMPLER EVALUATION MODEL

* Transforms and filters are not repeatedly evaluated
* Stateful function objects for `transform`
* Stateful predicates for `filter`

---
# STACK SIZE (SPL)

```cpp
__attribute__((noinline)) void SplMain7() {
  auto even = [](auto i) { return i % 2 == 0; };
  std::vector<int> r0 = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
  [[maybe_unused]] auto result = spl::apply(r0,
    spl::filter(even),
    spl::filter(even),
    spl::filter(even),
    spl::filter(even),
    spl::filter(even),
    spl::filter(even),
    spl::filter(even),
    spl::to_vector());
  PrintStack("SplMain7");
}
```

---
# STACK SIZE OUTPUT (SPL)

```
SplMain0  224 112 32 192 0
SplMain1  224 112 32 192 0
SplMain2  224 128 32 192 0
SplMain3  224 128 32 192 0
SplMain4  224 128 32 192 0
SplMain5  224 144 32 192 0
SplMain6  224 160 32 192 0
SplMain7  224 176 32 192 0
SplMain14 224 320 32 192 0
```

---
class: center, middle
# BENCHMARKS

---
# HANDWRITTEN LOOP

```cpp
std::vector<int> result;
for (int i : v) {
  if (i % 2 != 0 && i % 3 != 0 && i % 5 != 0
      && i % 7 != 0 && i % 11 != 0) {
    result.push_back(i);
  }
}
```

---
# STD::RANGES

```cpp
auto to_vector = [](auto&& r){
  return std::vector<int>(r.begin(), r.end());
};
using std::ranges::views::filter;
std::vector<int> result = to_vector(v
  | filter(not_divisible_by_2)
  | filter(not_divisible_by_3)
  | filter(not_divisible_by_5)
  | filter(not_divisible_by_7)
  | filter(not_divisible_by_11));
```

---
# SPL

```cpp
std::vector<int> result = spl::apply(v,
  spl::filter(not_divisible_by_2),
  spl::filter(not_divisible_by_3),
  spl::filter(not_divisible_by_5),
  spl::filter(not_divisible_by_7),
  spl::filter(not_divisible_by_11),
  spl::to_vector()
);
```

---
# OPTIMIZED

<!-- Benchmark chart: optimized -->

---
# UNOPTIMIZED

<!-- Benchmark chart: unoptimized -->

---
class: center, middle
# INTRODUCING SPL

---
# IMITATION IS THE SINCEREST FORM OF FLATTERY

---
# STD::RANGES IMPLEMENTATION

---
# HELPER RANGE

```cpp
template<Semiregular T>
struct maybe_view : view_interface<maybe_view<T>> {
  maybe_view() = default;
  maybe_view(T t) : data_(std::move(t)) {
  }
  T const *begin() const noexcept {
    return data_ ? &*data_ : nullptr;
  }
  T const *end() const noexcept {
    return data_ ? &*data_ + 1 : nullptr;
  }
  private:
  optional<T> data_{};
};
```

---
# HELPER LAMBDAS

```cpp
inline constexpr auto for_each =
  []<Range R,
     Iterator I = iterator_t<R>,
     IndirectUnaryInvocable<I> Fun>(R&& r, Fun fun)
    requires Range<indirect_result_t<Fun, I>> {
  return std::forward<R>(r)
    | view::transform(std::move(fun))
    | view::join;
};

inline constexpr auto yield_if =
  []<Semiregular T>(bool b, T x) {
  return b ? maybe_view{std::move(x)}
           : maybe_view<T>{};
};
```

---
# LAZY TRIPLES (std::ranges)

```cpp
using view::iota;
auto triples =
  for_each(iota(1), [](int z) {
    return for_each(iota(1, z+1), [=](int x) {
      return for_each(iota(x, z+1), [=](int y) {
        return yield_if(x*x + y*y == z*z,
          make_tuple(x, y, z));
      });
    });
  });
```

---
# OUTPUT

```cpp
for(auto triple : triples | view::take(10)) {
  cout << '('
       << get<0>(triple) << ','
       << get<1>(triple) << ','
       << get<2>(triple) << ')' << '\n';
}
```

---
class: center, middle
# SPL VERSION

---
# PYTHAGOREAN TRIPLES

```cpp
void OutputPythagoreanTriples() {
  spl::apply(
    spl::iota(1),
    spl::zip_result([](size_t c) {return spl::iota(1, c+1);}),
    spl::flatten(),
    spl::zip_result([](size_t c, size_t a){return spl::iota(a, c+1);}),
    spl::flatten(),
    spl::filter([](size_t c, size_t a, size_t b){
      return a*a + b*b == c*c;}),
    spl::swizzle<1, 2, 0>(),
    spl::take(10),
    spl::for_each([](size_t a, size_t b, size_t c){
      std::cout << a << " " << b << " " << c << "\n";
    })
  );
}
```

---
# REPETITION

The `zip_result` + `flatten` pattern repeats...

---
# COMPOSE

```cpp
void OutputPythagoreanTriples() {
  auto zip_flat = [](auto f){
    return spl::compose(spl::zip_result(f), spl::flatten());};

  spl::apply(
    spl::iota(1),
    zip_flat([](size_t c) {return spl::iota(1, c+1);}),
    zip_flat([](size_t c, size_t a) {return spl::iota(a, c+1);}),
    spl::filter([](size_t c, size_t a, size_t b){
      return a*a + b*b == c*c;}),
    spl::swizzle<1, 2, 0>(),
    spl::take(10),
    spl::for_each([](size_t a, size_t b, size_t c){
      std::cout << a << " " << b << " " << c << "\n";
    })
  );
}
```

---
# FLEXIBILITY

Replace `apply` with `compose` to get a reusable stage:

```cpp
auto PythagoreanTriples() {
  auto zip_flat = [](auto f){
    return spl::compose(spl::zip_result(f), spl::flatten());};

  return spl::compose(
    spl::iota(1),
    zip_flat([](size_t c) {return spl::iota(1, c+1);}),
    zip_flat([](size_t c, size_t a) {return spl::iota(a, c+1);}),
    spl::filter([](size_t c, size_t a, size_t b){
      return a*a + b*b == c*c;}),
    spl::swizzle<1, 2, 0>()
  );
}
```

---
# PYTHAGOREAN TRIPLES (reusable)

```cpp
vector<tuple<size_t,size_t,size_t>> triples =
  spl::apply(
    PythagoreanTriples(),
    spl::take(100),
    spl::make_tuple(),
    spl::to_vector()
  );
```

---
class: center, middle
# SPL BACKGROUND

---
# TERMINOLOGY

* Pipeline
* Stage
* Processing style

---
# PROCESSING STYLE

How a stage processes elements. Two kinds:

* **Incremental** — Processes elements one at a time
* **Complete** — Processes a single element as a whole

---
# INCREMENTAL

Processes elements one at a time.

`filter`, `transform` are all examples of incremental. We can evaluate
an element and output the result immediately without waiting for other
elements.

---
# COMPLETE

Process a single element as a whole.

* `sort` is an example of complete — you need all the elements to sort
* Surprising: **`max`** — you take all the elements and reduce them to
  a single value. You are treating that value as a whole, not part of
  something else.

---
# INPUT AND OUTPUT

The input and output processing style don't have to be the same.

| Input       | Output      |
|-------------|-------------|
| Incremental | Incremental |
| Incremental | Complete    |
| Complete    | Complete    |
| Complete    | Incremental |

---
# INCREMENTAL → INCREMENTAL

`transform`, `filter`

Take a single element, and output an element as part of something larger

---
# INCREMENTAL → COMPLETE

`to_vector`, `max`, `any_of`, `all_of`, `none_of`

Take values that are parts of a whole and collect them into new whole.
`accumulate` is the generalization of transformation.

---
# COMPLETE → COMPLETE

`sort` (based on `std::sort`)

Transforms a collection, sorts it, and outputs that collection as a
whole. Others include `partial_sort`, `stable_sort`, `nth_element`,
`unique`.

---
# COMPLETE → INCREMENTAL

Iterating a range. Implicit in SPL.

---
# RULES FOR INCREMENTAL AND COMPLETE

* The input processing style of a stage has to match the output
  processing style of the previous stage
* We can implicitly convert complete to incremental
* The final stage passed to `apply` has to be complete — `apply` needs
  to return a single value that stands on its own.

---
# APPLY

```cpp
template <typename Range, typename... Stages>
[[nodiscard]] constexpr auto apply(Range&& range,
                                   Stages&&... stages);
```

* The first parameter is a range
* The stages is one or more of the above stages.
* Note that the return type is `auto`. It will return a value and not a
  reference. This is for safety by default.

---
# FUME HOODS (LIKE APPLY) ISOLATE DANGEROUS INTERMEDIATES (LIKE REFERENCES TO TEMPORARIES)

---
# TEST

```cpp
std::vector<int> v{...};
// Assume we capture the return value
apply(v, max());                                          // ?
apply(v, transform(triple));                              // ?
apply(v, transform(triple), to_vector());                 // ?
apply(v, transform(triple), sort());                      // ?
apply(v, transform(triple), to_vector(), sort());         // ?
apply(v, transform(triple), to_vector(),
      sort(), filter_duplicates(), accumulate());         // ?
```

---
# COMPOSE

Creates new stages by composition.

* It is just a thin wrapper over `std::tuple`.
* `apply` treats passing in `compose` as if you passed what is in the
  elements directly.

---
class: center, middle
# STAGES

---
# AIM FOR DEFAULT SAFETY

* `max` returns `std::optional<T>`
* `partial_sort(n)` is safe even if `n` > number of elements
* `nth_element(n)` is safe even if `n` > number of elements

---
# COMPLETE TRANSFORMS

`transform_complete`, `sort`, `stable_sort`, `partial_sort`,
`nth_element`, `unique`

---
# INCREMENTAL TO COMPLETE ACCUMULATORS

`accumulate`, `accumulate_in_place`, `for_each`, `to_vector`,
`push_back_to`, `push_back_into`, `to_map`

--

`first`, `last`, `count`, `min`, `max`, `minmax`, `any_of`, `all_of`,
`none_of`, `sum`

---
# FILTERS

`filter`, `take`, `take_while`, `skip`, `skip_while`,
`filter_duplicates`

---
# INCREMENTAL TRANSFORMS

`transform`, `transform_arg`, `transform_cps`, `ref`, `move`, `deref`,
`addressof`, `cast`, `construct`

---
# INCREMENTAL ARGUMENT MANIPULATION

`expand_tuple`, `enumerate`, `zip`, `zip_result`, `swizzle`,
`make_tuple`, `make_pair`, `flatten`

---
# FLAT MAP

`flat_map` — Can output zero to many results

---
# HIGHER ORDER

`group_by`, `chunk`, `chunk_by`, `tee`

---
# SEQUENCE EXTENSION

`repeat`, `repeat_while`, `chain_before`, `chain_after`

---
# ERROR HANDLING

`unwrap_optional`, `unwrap_optional_arg`, `flatten_optional`,
`flatten_optional_arg`

---
class: center, middle
# IMPLEMENTING STAGES

---
# Warning: Internals ahead

Simplifications and hand-waving ahead

---
# STAGE IMPL

```cpp
template<typename StageProperties,
         typename InputTypes, typename... Params>
struct foo_stage_impl;

template<typename StageProperties,
         typename... InputTypes, typename... Params>
struct foo_stage_impl<StageProperties,
                      spl::impl::types<InputTypes...>, Params...>
  : spl::impl::stage_impl<foo_stage_impl<...>> {
  using output_types = spl::impl::types</* ... */>;

  // Required for incremental
  constexpr void process_incremental(InputTypes... inputs);

  // Optional
  constexpr decltype(auto) finish();
  constexpr bool done() const;

  // Required for complete
  constexpr decltype(auto) process_complete(InputTypes... inputs);
};
```

---
# COMPLETE TRANSFORM

```cpp
template <typename StageProperties,
          typename... InputTypes, typename F>
struct transform_complete_impl<StageProperties,
                               spl::impl::types<InputTypes...>, F>
    : spl::impl::stage_impl<transform_complete_impl<...>> {
  F f;
  using output_types = spl::impl::types<
    std::invoke_result_t<F, InputTypes...>>;

  constexpr decltype(auto)
  process_complete(InputTypes... inputs) {
    return this->next.process_complete(
      std::invoke(f, std::forward<InputTypes>(inputs)...));
  }
};
```

---
# INCREMENTAL TRANSFORM

```cpp
template <typename StageProperties,
          typename... InputTypes, typename F>
struct transform_impl<StageProperties,
                      spl::impl::types<InputTypes...>, F>
    : spl::impl::stage_impl<transform_impl<...>> {
  F f;
  using output_types = spl::impl::types<
    std::invoke_result_t<F, InputTypes...>>;

  constexpr void
  process_incremental(InputTypes... inputs) {
    this->next.process_incremental(
      std::invoke(f, std::forward<InputTypes>(inputs)...));
  }
};
```

---
# FILTER

```cpp
template <typename StageProperties,
          typename... InputTypes, typename Predicate>
struct filter_impl<StageProperties,
                   spl::impl::types<InputTypes...>, Predicate>
    : spl::impl::stage_impl<filter_impl<...>> {
  using output_types = spl::impl::types<InputTypes...>;
  Predicate f;

  constexpr void
  process_incremental(InputTypes... inputs) {
    if (std::invoke(f, std::as_const(inputs)...)) {
      this->next.process_incremental(
        std::forward<InputTypes>(inputs)...);
    }
  }
};
```

---
# STAGE_IMPL CRTP BASE

```cpp
template<typename Derived>
struct stage_impl {
  // Reference to the next stage in the pipeline
  /* next-stage reference */ next;

  // Default: forward finish to next stage
  constexpr decltype(auto) finish() {
    return next.finish();
  }

  // Default: not done unless overridden
  constexpr bool done() const { return false; }
};
```

---
# STAGE_IMPL CRTP BASE

```cpp
// Pipeline asks each stage:
constexpr decltype(auto) finish() {
  if constexpr (has_finish<Derived>) {
    return derived().finish();
  } else {
    return next.finish();
  }
}

constexpr bool done() const {
  if constexpr (has_done<Derived>) {
    return derived().done();
  } else {
    return false;
  }
}
```

---
# STAGE_IMPL CRTP BASE

```cpp
constexpr void process_incremental(InputTypes... t) {
  derived().process_incremental(
    std::forward<InputTypes>(t)...);
}

constexpr decltype(auto) process_complete(auto&& t) {
  if constexpr (is_incremental) {
    for (auto&& v : t)  // Handwavy
      derived().process_incremental(v);
    return finish();
  } else {
    return derived().process_complete(
      std::forward<decltype(t)>(t));
  }
}
```

---
# MAKE_PIPELINE

```cpp
auto pipeline = spl::impl::make_pipeline<
  spl::impl::types<int>,
  spl::impl::processing_style::incremental
>(stage1, stage2, stage3);

pipeline.process_incremental(42);
auto result = pipeline.finish();
```

---
# APPLY

```cpp
template<typename Range, typename... Stages>
constexpr auto apply(Range&& range, Stages&&... stages) {
  auto pipeline = spl::impl::make_pipeline<
    /* deduced input types */,
    /* starting style */
  >(std::forward<Stages>(stages)...);
  return pipeline.process_complete(std::forward<Range>(range));
}
```

---
class: center, middle
# USING SPL

---
class: center, middle
# COMPOSITION

---
# compose is a first-class stage

```cpp
auto flatten_optional() {
  return spl::compose(spl::filter(), spl::deref());
}

spl::apply(
  std::vector<std::optional<int>>{1, std::nullopt, 3},
  flatten_optional(),
  spl::to_vector());
```

---
# compose generators

```cpp
auto perfect_squares() {
  return spl::compose(
    spl::iota(1),
    spl::transform([](size_t i) { return i*i; }));
}

spl::apply(
  perfect_squares(),
  spl::filter([](size_t i) { return i >= 100; }),
  spl::take(5),
  spl::to_vector());
```

---
# compose ranges

```cpp
struct Transactions {
  std::vector<double> amounts;
  auto credits() const {
    return spl::compose(
      std::cref(amounts),
      spl::filter([](double d) { return d > 0; }));
  }
};
```

---
# GENERATORS

Input iterators can be difficult to write. SPL provides an easy way to
make generators. Wrap a callable in `spl::generator{...}` (CTAD); the
element type is deduced from what the callable passes to `output(...)`.

```cpp
template <typename F>
struct generator : F { using F::operator(); };

template <typename F>
generator(F) -> generator<F>;
```

---
# IOTA

```cpp
inline constexpr auto iota(size_t start) {
  return iota(start, std::numeric_limits<size_t>::max());
}
```

---
# IOTA WITH END

```cpp
inline constexpr auto iota(size_t start, size_t end) {
  return spl::generator([start, end](auto&& output) mutable {
    if (start < end) {
      output(size_t(start));
      ++start;
      return true;        // more to come
    }
    return false;         // done
  });
}
```

---
class: center, middle
# MULTI-ARGUMENT STREAMS

---
# Multi-argument streams are passed as separate arguments

```cpp
std::map<int, std::vector<int>> values = {...};
spl::apply(values,
  // pair<int, vector>
  spl::expand_tuple(),      // int, vector
  spl::flatten(),           // int, int
  spl::for_each([](int k, int v) { ... }));
```

---
# First class support

```cpp
std::map<std::string, int> name2id = {...};
auto id2name = spl::apply(
  std::move(name2id),
  spl::expand_tuple(),
  spl::swizzle<1, 0>(),
  spl::to_map<std::map>());
```

---
# transform_arg

```cpp
std::map<std::string, Person> name2person = {...};
auto id2name = spl::apply(
  std::move(name2person),
  spl::expand_tuple(),
  spl::transform_arg<1>(&Person::id),
  spl::swizzle<1, 0>(),
  spl::to_map<std::map>());
```

---
# Preserve Reference Semantics

```cpp
std::vector<std::unique_ptr<int>> values = {...};
spl::apply(
  values,
  spl::zip_result([](const auto& ptr) {
    return std::make_unique<int>(*ptr); }),
  spl::for_each([](auto&& first, auto&& second) {
    static_assert(std::is_same_v<
      decltype(first),
      const std::unique_ptr<int>&>);
    static_assert(std::is_same_v<
      decltype(second),
      std::unique_ptr<int>&&>);
  }));
```

---
class: center, middle
# ERROR HANDLING

---
# Short-circuit on error and wrap results

```cpp
std::optional<int> ParseInt(std::string_view);
std::vector<std::optional<std::string>> values = {...};

std::optional<int> sum = spl::apply(
  values,
  // optional<string>
  spl::unwrap_optional(),       // string
  spl::transform(&ParseInt),    // optional<int>
  spl::unwrap_optional(),       // int
  spl::accumulate(std::plus<>{})// int
);
// optional<int>
```

---
# flatten_optional vs unwrap_optional

```cpp
// flatten_optional: skip nullopts, keep going
spl::apply(std::vector<std::optional<int>>{{1}, std::nullopt, {3}},
  spl::flatten_optional(),
  spl::to_vector());
// {1, 3}

// unwrap_optional: short-circuit the whole pipeline
spl::apply(std::vector<std::optional<int>>{{1}, std::nullopt, {3}},
  spl::unwrap_optional(),
  spl::sum());
// std::nullopt — sum() never sees {3}
```

---
# Monadic style

```cpp
std::optional<int> ParseInt(std::string_view);
int Squared(int i);
std::optional<std::string> value = ...;

// C++23
std::optional<int> cpp23 = value
  .and_then(&ParseInt)
  .transform(&Squared);

// SPL
std::optional<int> result = spl::apply(
  value,
  spl::unwrap_optional(),
  spl::transform_complete(&ParseInt),
  spl::transform_complete(&Squared));
```

---
class: center, middle
# HIGHER-ORDER STAGES

---
# tee splits outputs to multiple stages

`tee` runs each sub-pipeline over the same input. Its results flow into
the next stage as separate arguments (not a tuple).

```cpp
std::vector<int> values = {...};
auto summary = spl::apply(
  values,
  spl::tee(spl::min(), spl::max(), spl::count()),
  spl::transform_complete([](std::optional<int> min,
                             std::optional<int> max,
                             size_t count) {
    return std::tuple{min, max, count};
  }));
```

---
# Computing an average

```cpp
double avg = spl::apply(std::vector{1, 2, 3, 4, 5},
  spl::tee(spl::sum(), spl::count()),
  spl::transform_complete([](int sum, size_t count) {
    return static_cast<double>(sum) / count;
  })
);
// 3.0
```

---
# Group elements

```cpp
struct Employee {
  int id;
  bool is_fulltime;
  std::string org;
};
std::vector<Employee> employees = {...};

auto counts = spl::apply(
  employees,
  spl::filter(&Employee::is_fulltime),
  spl::group_by(&Employee::org, spl::count()),
  spl::to_vector());
```

---
class: center, middle
# REFERENCE INPUTS

---
# Const reference iteration by default

```cpp
std::vector<std::unique_ptr<int>> values = {...};
spl::apply(
  values, // values not copied!
  spl::for_each(
    [](auto&& ptr) {
      static_assert(std::is_same_v<
        decltype(ptr),
        const std::unique_ptr<int>&>);
    }));
```

---
# Mutable references with std::ref

```cpp
spl::apply(
  std::ref(values),
  spl::for_each(
    [](auto&& ptr) {
      static_assert(std::is_same_v<
        decltype(ptr),
        std::unique_ptr<int>&>);
    }));
```

---
# R-Values when moved

```cpp
spl::apply(
  std::move(values),
  spl::for_each(
    [](auto&& ptr) {
      static_assert(std::is_same_v<
        decltype(ptr),
        std::unique_ptr<int>&&>);
    }));
```

---
class: center, middle
# CONVENIENCES

---
# Initializing std::vector with unique_ptr

```cpp
// Doesn't work
std::vector v{make_unique<int>(0), make_unique<int>(1)};

// Works
auto v = spl::apply(
  {std::make_unique<int>(0), std::make_unique<int>(1)},
  spl::to_vector());
```

---
# Moving keys out of map or set

```cpp
// Doesn't work
std::vector<std::string> v(
  std::move(s).begin(), std::move(s).end());

// Doesn't work
std::vector<std::string> v2(
  std::make_move_iterator(s.begin()),
  std::make_move_iterator(m.end()));

// Works
auto v = spl::apply(std::move(s), spl::to_vector());
```

---
# Bonus: constexpr-friendly

```cpp
// Compile-time pipeline evaluation
constexpr auto squares = spl::apply(
  spl::iota(1, 6),
  spl::transform([](size_t x) { return x * x; }),
  spl::to_vector()
);
// {1, 4, 9, 16, 25} computed at compile time
```

---
class: center, middle
# CHALLENGE REVISITED

---
# CHALLENGE

Given a text file and an integer k, print the k most common words in
the file (and the number of their occurrences) in decreasing frequency.

---
class: center, middle
# STAGES

---
# READ_LINES

Generator lambdas short-circuit when the pipeline is computing types:
emit one value, then return — don't run the real work.

```cpp
auto read_lines() {
  return spl::transform_complete([](std::istream &is) {
    return spl::generator(
      [&is, line = std::string()](auto&& output) mutable {
        auto emit = [&] { return output(std::string_view(line)); };
        if constexpr (spl::impl::calculate_type_v<decltype(output)>) {
          return emit();           // type-deduction pass
        } else {
          if (std::getline(is, line)) { emit(); return true; }
          return false;
        }
      });
  });
}
```

---
# SPLIT_STRING

```cpp
auto split_string() {
  return spl::compose(
    spl::transform(
      [](std::string_view s) {
        return absl::StrSplit(
          s,
          absl::ByAsciiWhitespace(),
          absl::SkipEmpty()
        );
      }),
    spl::flatten()
  );
}
```

---
# LOWER_CASE_STRING

```cpp
auto lower_case_string() {
  return spl::transform([](std::string_view s) {
    return spl::apply(
      s,
      spl::transform([](char c) {
        return std::tolower(c); }),
      spl::to_vector()  // assemble into std::string-like
    );
  });
}
```

---
# UNIQUE_COUNTS

```cpp
auto unique_counts() {
  return spl::compose(
    spl::group_by(std::identity{}, spl::count()),
    spl::swizzle<1, 0>(),
    spl::make_pair()
  );
}
```

---
# WORD COUNTS PIPELINE

```cpp
spl::apply(
  std::ref(std::cin),
  read_lines(),
  split_string(),
  lower_case_string(),
  unique_counts(),
  spl::to_vector(),
  spl::partial_sort(k, std::greater<>()),
  spl::take(k),
  spl::expand_tuple(),
  spl::for_each([](int n, const auto &s) {
    std::cout << n << " " << s << "\n";
  })
);
```

---
class: center, middle
# QUESTIONS
