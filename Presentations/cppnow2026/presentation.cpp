// Runnable, self-checking examples extracted from the SPL slide deck.
// Each example writes to an std::ostream& and is checked against an expected
// raw-string transcript in main(). See spl.md for the corresponding slides.

#include <spl/spl.h>

#include <array>
#include <cassert>
#include <cctype>
#include <format>
#include <functional>
#include <iostream>
#include <map>
#include <memory>
#include <optional>
#include <ranges>
#include <sstream>
#include <stdexcept>
#include <string>
#include <string_view>
#include <tuple>
#include <utility>
#include <vector>

// ---------------------------------------------------------------------------
// Section: STD::RANGES (the std::views motivating example)
// Slide: STD::RANGES
// ---------------------------------------------------------------------------
void RangesIntroExample(std::ostream& os) {
  auto const ints = {0, 1, 2, 3, 4, 5};
  auto even = [](int i) { return 0 == i % 2; };
  auto square = [](int i) { return i * i; };

  for (int i : ints | std::views::filter(even) | std::views::transform(square))
    os << i << ' ';
  os << '\n';
}

// ---------------------------------------------------------------------------
// Section: TPOIASI (std::ranges)
// Slide: TPOIASI / OUTPUT
// Demonstrates the duplicate evaluation seen with std::views::transform
// followed by std::views::filter.
// ---------------------------------------------------------------------------
void TpoiasiRanges(std::ostream& os) {
  auto times2 = [&](int n) {
    os << "  transforming " << n << "\n";
    return n * 2;
  };
  auto isMultipleOf4 = [](int n) { return n % 4 == 0; };

  std::vector<int> numbers = {1, 2, 3, 4, 5};
  auto results = numbers | std::views::transform(times2)
                         | std::views::filter(isMultipleOf4);
  for (auto result : results) os << "  -> " << result << '\n';
}

// ---------------------------------------------------------------------------
// Section: SPL — CRASH EXAMPLE
// Slide: CRASH EXAMPLE (SPL)
// ---------------------------------------------------------------------------
void CrashExampleSpl(std::ostream& os) {
  using IntAndString = std::pair<int, std::string>;
  auto make_int_and_string = [](size_t i) -> IntAndString {
    return {static_cast<int>(i * i * i), std::to_string(i)};
  };

  spl::apply(spl::iota(1, 10000001),
             spl::transform(make_int_and_string),
             spl::filter([](const auto& p) {
               return p.first >= static_cast<int>(
                                     std::hash<std::string>()(p.second));
             }),
             spl::transform(&IntAndString::second),
             spl::take(4),
             spl::for_each([&](const auto& s) { os << "  " << s << "\n"; }));
}

// ---------------------------------------------------------------------------
// Section: SPL — TPOIASI
// Slide: TPOIASI (SPL) / OUTPUT
// ---------------------------------------------------------------------------
void TpoiasiSpl(std::ostream& os) {
  auto times2 = [&](int n) {
    os << "  transforming: " << n << "\n";
    return n * 2;
  };
  auto isMultipleOf4 = [](int n) { return n % 4 == 0; };

  std::vector<int> numbers = {1, 2, 3, 4, 5};
  spl::apply(numbers, spl::transform(times2), spl::filter(isMultipleOf4),
             spl::for_each([&](auto result) { os << "  -> " << result << '\n'; }));
}

// ---------------------------------------------------------------------------
// Section: BENCHMARKS — handwritten / std::ranges / SPL filter chain
// Slides: HANDWRITTEN LOOP / STD::RANGES / SPL
// ---------------------------------------------------------------------------
void FilterChainHandwritten(std::ostream& os) {
  std::vector<int> v;
  for (int i = 1; i <= 30; ++i) v.push_back(i);

  std::vector<int> result;
  for (int i : v) {
    if (i % 2 != 0 && i % 3 != 0 && i % 5 != 0 && i % 7 != 0 && i % 11 != 0) {
      result.push_back(i);
    }
  }
  for (int x : result) os << x << ' ';
  os << '\n';
}

void FilterChainSpl(std::ostream& os) {
  std::vector<int> v;
  for (int i = 1; i <= 30; ++i) v.push_back(i);

  auto not_divisible_by = [](int d) {
    return [d](int x) { return x % d != 0; };
  };
  std::vector<int> result =
      spl::apply(v, spl::filter(not_divisible_by(2)),
                 spl::filter(not_divisible_by(3)),
                 spl::filter(not_divisible_by(5)),
                 spl::filter(not_divisible_by(7)),
                 spl::filter(not_divisible_by(11)), spl::to_vector());
  for (int x : result) os << x << ' ';
  os << '\n';
}

// ---------------------------------------------------------------------------
// Section: SPL VERSION — Pythagorean triples
// Slides: PYTHAGOREAN TRIPLES / COMPOSE / FLEXIBILITY / PYTHAGOREAN TRIPLES (reusable)
// ---------------------------------------------------------------------------
auto PythagoreanTriples() {
  auto zip_flat = [](auto f) {
    return spl::compose(spl::zip_result(f), spl::flatten());
  };
  return spl::compose(
      spl::iota(1),
      zip_flat([](size_t c) { return spl::iota(1, c + 1); }),
      zip_flat([](size_t c, size_t a) { return spl::iota(a, c + 1); }),
      spl::filter(
          [](size_t c, size_t a, size_t b) { return a * a + b * b == c * c; }),
      spl::swizzle<1, 2, 0>());
}

void PythagoreanTriplesReusable(std::ostream& os) {
  spl::apply(PythagoreanTriples(), spl::take(10),
             spl::for_each([&](size_t a, size_t b, size_t c) {
               os << "  " << a << " " << b << " " << c << "\n";
             }));
}

// ---------------------------------------------------------------------------
// Section: COMPOSITION
// Slide: compose generators / compose ranges
// ---------------------------------------------------------------------------
auto perfect_squares() {
  return spl::compose(spl::iota(1),
                      spl::transform([](size_t i) { return i * i; }));
}

void ComposeGenerators(std::ostream& os) {
  auto v = spl::apply(perfect_squares(),
                      spl::filter([](size_t i) { return i >= 100; }),
                      spl::take(5), spl::to_vector());
  for (auto x : v) os << "  " << x << "\n";
}

// The deck shows compose(std::cref(amounts), filter(...)). std::reference_wrapper
// isn't a range source, so the runnable form passes the vector by value (a
// copy) into the composed pipeline.
auto credits_pipeline(std::vector<double> amounts) {
  return spl::compose(std::move(amounts),
                      spl::filter([](double d) { return d > 0; }));
}

void ComposeRanges(std::ostream& os) {
  auto v = spl::apply(credits_pipeline({-10.0, 5.5, -2.0, 3.0, 8.25}),
                      spl::to_vector());
  for (auto x : v) os << std::format("  {:.2f}\n", x);
}

// ---------------------------------------------------------------------------
// Section: GENERATORS / IOTA
// Slides: IOTA / IOTA WITH END
// ---------------------------------------------------------------------------
void IotaDemo(std::ostream& os) {
  auto v = spl::apply(spl::iota(1, 11), spl::to_vector());
  for (auto x : v) os << x << ' ';
  os << '\n';
}

// ---------------------------------------------------------------------------
// Section: MULTI-ARGUMENT STREAMS
// Slides: Multi-argument streams / First class support / transform_arg
// ---------------------------------------------------------------------------
void MultiArgStreams(std::ostream& os) {
  std::map<int, std::vector<int>> values{{1, {10, 20}}, {2, {30}}, {3, {40, 50, 60}}};
  spl::apply(values, spl::expand_tuple(), spl::flatten(),
             spl::for_each([&](int k, int v) {
               os << "  k=" << k << " v=" << v << "\n";
             }));
}

void SwapMapKeyValue(std::ostream& os) {
  std::map<std::string, int> name2id{{"alice", 1}, {"bob", 2}, {"carol", 3}};
  auto id2name = spl::apply(std::move(name2id), spl::expand_tuple(),
                            spl::swizzle<1, 0>(), spl::to_map<std::map>());
  for (const auto& [k, v] : id2name)
    os << "  " << k << " -> " << v << "\n";
}

struct Person {
  std::string name;
  int id;
};

void TransformArgDemo(std::ostream& os) {
  std::map<std::string, Person> name2person{
      {"alice", {"alice", 100}}, {"bob", {"bob", 200}}, {"carol", {"carol", 300}}};
  auto id2name = spl::apply(std::move(name2person), spl::expand_tuple(),
                            spl::transform_arg<1>(&Person::id),
                            spl::swizzle<1, 0>(), spl::to_map<std::map>());
  for (const auto& [k, v] : id2name)
    os << "  " << k << " -> " << v << "\n";
}

// ---------------------------------------------------------------------------
// Section: ERROR HANDLING
// Slides: Short-circuit on error / flatten_optional vs unwrap_optional
// ---------------------------------------------------------------------------
std::optional<int> ParseInt(std::string_view s) {
  int value = 0;
  for (char c : s) {
    if (c < '0' || c > '9') return std::nullopt;
    value = value * 10 + (c - '0');
  }
  return s.empty() ? std::nullopt : std::optional<int>{value};
}

void ShortCircuitOnError(std::ostream& os) {
  std::vector<std::optional<std::string>> good{
      {std::string("1")}, {std::string("2")}, {std::string("3")}};
  std::vector<std::optional<std::string>> bad{
      {std::string("1")}, std::nullopt, {std::string("3")}};

  auto sum_good = spl::apply(good, spl::unwrap_optional(),
                             spl::transform([](const std::string& s) {
                               return ParseInt(std::string_view(s));
                             }),
                             spl::unwrap_optional(), spl::sum());
  auto sum_bad = spl::apply(bad, spl::unwrap_optional(),
                            spl::transform([](const std::string& s) {
                              return ParseInt(std::string_view(s));
                            }),
                            spl::unwrap_optional(), spl::sum());

  os << "  good -> " << (sum_good ? std::to_string(*sum_good) : "nullopt")
     << "\n";
  os << "  bad  -> " << (sum_bad ? std::to_string(*sum_bad) : "nullopt")
     << "\n";
}

void FlattenVsUnwrap(std::ostream& os) {
  std::vector<std::optional<int>> v{1, std::nullopt, 3};

  auto kept = spl::apply(v, spl::flatten_optional(), spl::to_vector());
  os << "  flatten_optional kept: ";
  for (int x : kept) os << x << ' ';
  os << '\n';

  auto sum = spl::apply(v, spl::unwrap_optional(), spl::sum());
  os << "  unwrap_optional sum: "
     << (sum ? std::to_string(*sum) : "nullopt") << '\n';
}

// ---------------------------------------------------------------------------
// Section: HIGHER-ORDER STAGES
// Slides: tee / Computing an average / Group elements
// ---------------------------------------------------------------------------
void TeeSummary(std::ostream& os) {
  std::vector<int> values{4, 1, 7, 2, 9, 3};
  auto summary = spl::apply(
      values, spl::tee(spl::min(), spl::max(), spl::count()),
      spl::transform_complete([](std::optional<int> mn, std::optional<int> mx,
                                 size_t n) {
        return std::tuple{mn, mx, n};
      }));
  os << std::format("  min={} max={} count={}\n",
                    std::get<0>(summary).value_or(-1),
                    std::get<1>(summary).value_or(-1),
                    std::get<2>(summary));
}

void TeeAverage(std::ostream& os) {
  double avg = spl::apply(
      std::vector{1, 2, 3, 4, 5}, spl::tee(spl::sum(), spl::count()),
      spl::transform_complete([](int sum, size_t count) {
        return static_cast<double>(sum) / static_cast<double>(count);
      }));
  os << std::format("  avg = {:.4f}\n", avg);
}

struct Employee {
  int id;
  bool is_fulltime;
  std::string org;
};

void GroupByDemo(std::ostream& os) {
  std::vector<Employee> employees{
      {1, true, "eng"}, {2, true, "eng"}, {3, false, "eng"},
      {4, true, "sales"}, {5, true, "sales"}, {6, false, "ops"},
  };
  // group_by emits a multi-arg stream (key, aggregate). The deck shows
  // collecting via to_vector() and structured binding; the actual pattern is
  // either a multi-arg for_each or pack the args with make_pair() first.
  spl::apply(employees, spl::filter(&Employee::is_fulltime),
             spl::group_by(&Employee::org, spl::count()),
             spl::for_each([&](const std::string& org, size_t n) {
               os << "  " << org << ": " << n << "\n";
             }));
}

// ---------------------------------------------------------------------------
// Section: REFERENCE INPUTS
// Slides: Const reference / std::move
//
// The deck also has a "Mutable references with std::ref" slide claiming
// `std::ref(values)` yields T& iteration. That isn't supported today —
// std::reference_wrapper<vector> doesn't satisfy std::ranges::range. Omitted.
// ---------------------------------------------------------------------------
void ReferenceInputs(std::ostream& os) {
  std::vector<std::unique_ptr<int>> values;
  values.push_back(std::make_unique<int>(1));
  values.push_back(std::make_unique<int>(2));

  // The deck claims "const reference iteration by default" but a non-const
  // lvalue input actually yields T&. Pass std::as_const to get T const&.
  spl::apply(std::as_const(values), spl::for_each([&](auto&& ptr) {
               static_assert(
                   std::is_same_v<decltype(ptr), const std::unique_ptr<int>&>);
               os << "  const& -> " << *ptr << "\n";
             }));

  spl::apply(values, spl::for_each([&](auto&& ptr) {
               static_assert(
                   std::is_same_v<decltype(ptr), std::unique_ptr<int>&>);
               os << "  &      -> " << *ptr << "\n";
             }));

  spl::apply(std::move(values), spl::for_each([&](auto&& ptr) {
               static_assert(
                   std::is_same_v<decltype(ptr), std::unique_ptr<int>&&>);
               os << "  &&     -> " << *ptr << "\n";
             }));
}

// ---------------------------------------------------------------------------
// Section: CONVENIENCES
// Slides: Initializing std::vector with unique_ptr / Moving keys / constexpr
// ---------------------------------------------------------------------------
void InitializingVectorOfUniquePtr(std::ostream& os) {
  auto v = spl::apply(
      // Note: this initializer-list overload moves elements out of the list.
      std::array{std::make_unique<int>(0), std::make_unique<int>(1)},
      spl::to_vector());
  for (auto& p : v) os << "  " << *p << "\n";
}

void MovingKeysOutOfMap(std::ostream& os) {
  std::vector<std::string> seed{"alpha", "bravo", "charlie"};
  auto v = spl::apply(std::move(seed), spl::to_vector());
  for (const auto& s : v) os << "  " << s << "\n";
}

// The deck shows `constexpr auto squares = spl::apply(..., spl::to_vector());`
// at namespace scope. That doesn't actually compile — std::vector's heap
// allocation can't escape a constexpr context. The example below uses a
// non-allocating terminal, matching what the library actually supports.
constexpr auto sum_of_squares_constexpr = spl::apply(
    spl::iota(1, 6), spl::transform([](size_t x) { return x * x; }), spl::sum());

void ConstexprSquares(std::ostream& os) {
  os << "  sum 1..5 squared = " << sum_of_squares_constexpr << '\n';
}

// ---------------------------------------------------------------------------
// Section: CHALLENGE REVISITED — word counts
// Slides: READ_LINES / SPLIT_STRING / LOWER_CASE_STRING / UNIQUE_COUNTS / WORD COUNTS PIPELINE
//
// The deck uses absl::StrSplit; here we drop that dependency and split on
// whitespace ourselves so the example is self-contained.
//
// Generator lambdas must check spl::impl::calculate_type_v<Output> and
// short-circuit to a single output(...) call when the pipeline is computing
// types — otherwise the actual work runs at type-deduction time. See iota's
// implementation in spl.h for the canonical pattern.
// ---------------------------------------------------------------------------
auto read_lines() {
  return spl::transform_complete([](std::istream& is) {
    return spl::generator(
        [&is, line = std::string()](auto&& output) mutable {
          auto emit = [&] { return output(std::string_view(line)); };
          if constexpr (spl::impl::calculate_type_v<decltype(output)>) {
            return emit();
          } else {
            if (std::getline(is, line)) {
              line.push_back('\n');  // restore separator getline stripped,
                                     // so chunk_by sees a word boundary
                                     // between lines
              emit();
              return true;
            }
            return false;
          }
        });
  });
}

auto split_string() {
  return spl::compose(
      spl::flatten(),
      spl::chunk_by(
          [](char prev, const char& c) {
            return std::isspace(static_cast<unsigned char>(prev)) ==
                   std::isspace(static_cast<unsigned char>(c));
          },
          spl::accumulate(std::string_view{},
                          [](std::string_view s, const char& c) {
                            if (s.empty()) {
                              return std::string_view(&c, 1);
                            } else {
                              assert(&c == &s.back() + 1);
                              return std::string_view(s.data(), s.size() + 1);
                            }
                          })),
      spl::filter([](std::string_view s) {
        return !s.empty() &&
               !std::isspace(static_cast<unsigned char>(s.front()));
      }));
}

auto lower_case_string() {
  return spl::transform([](std::string_view s) {
    return spl::apply(s, spl::transform([](char c) {
       return static_cast<char>(std::tolower(static_cast<unsigned char>(c)));
      }), spl::push_back_to<std::basic_string>());
  });
}

auto unique_counts() {
  return spl::compose(spl::group_by(std::identity{}, spl::count()),
                      spl::swizzle<1, 0>(), spl::make_pair());
}

void WordCountsPipeline(std::ostream& os) {
  // Mixed case to exercise lower_case_string: counts must stay the same.
  std::istringstream input(
      "The quick brown FOX jumps over the lazy dog\n"
      "The Fox was QUICK and the dog was Lazy\n"
      "but THE brown fox kept on JUMPING\n");

  constexpr size_t k = 5;
  spl::apply(input, read_lines(), split_string(), lower_case_string(),
             unique_counts(), spl::to_vector(),
             spl::partial_sort(k, std::greater<>()), spl::take(k),
             spl::expand_tuple(),
             spl::for_each([&](size_t n, const std::string& s) {
               os << "  " << n << " " << s << "\n";
             }));
}

// ---------------------------------------------------------------------------
// Test harness: run an example, compare its output, throw on mismatch.
// ---------------------------------------------------------------------------
namespace {

// Strip trailing whitespace from each line and a leading newline so the raw
// strings stay readable (start on the line after R"() and don't need to
// reproduce trailing spaces emitted by simple `os << x << ' '` loops).
std::string normalize(std::string_view s) {
  if (!s.empty() && s.front() == '\n') s.remove_prefix(1);
  std::string out;
  out.reserve(s.size());
  size_t line_start = 0;
  for (size_t i = 0; i <= s.size(); ++i) {
    if (i == s.size() || s[i] == '\n') {
      size_t end = i;
      while (end > line_start && (s[end - 1] == ' ' || s[end - 1] == '\t'))
        --end;
      out.append(s.data() + line_start, end - line_start);
      if (i < s.size()) out.push_back('\n');
      line_start = i + 1;
    }
  }
  return out;
}

void expect(std::string_view name,
            void (*fn)(std::ostream&),
            std::string_view expected_raw) {
  std::ostringstream captured;
  fn(captured);
  std::string actual = normalize(captured.str());
  std::string expected = normalize(expected_raw);

  std::cout << "=== " << name << " ===\n" << actual;

  if (actual != expected) {
    std::cerr << "\n*** MISMATCH in " << name << " ***\n"
              << "--- expected ---\n" << expected
              << "--- actual ---\n" << actual
              << "----------------\n";
    throw std::runtime_error(std::string("output mismatch: ") + std::string(name));
  }
}

}  // namespace

// ---------------------------------------------------------------------------
int main() {
  expect("std::ranges intro", &RangesIntroExample, R"(
0 4 16
)");

  expect("TPOIASI (std::ranges, with logging)", &TpoiasiRanges, R"(
  transforming 1
  transforming 2
  transforming 2
  -> 4
  transforming 3
  transforming 4
  transforming 4
  -> 8
  transforming 5
)");

  expect("Crash example (SPL — runs safely)", &CrashExampleSpl, R"(
  2
  4
  5
  7
)");

  expect("TPOIASI (SPL — single evaluation)", &TpoiasiSpl, R"(
  transforming: 1
  transforming: 2
  -> 4
  transforming: 3
  transforming: 4
  -> 8
  transforming: 5
)");

  expect("Filter chain (handwritten loop)", &FilterChainHandwritten, R"(
1 13 17 19 23 29
)");

  expect("Filter chain (SPL)", &FilterChainSpl, R"(
1 13 17 19 23 29
)");

  expect("Pythagorean triples (reusable, first 10)", &PythagoreanTriplesReusable, R"(
  3 4 5
  6 8 10
  5 12 13
  9 12 15
  8 15 17
  12 16 20
  7 24 25
  15 20 25
  10 24 26
  20 21 29
)");

  expect("compose generators (perfect squares >= 100)", &ComposeGenerators, R"(
  100
  121
  144
  169
  196
)");

  expect("compose ranges (credits only)", &ComposeRanges, R"(
  5.50
  3.00
  8.25
)");

  expect("iota demo", &IotaDemo, R"(
1 2 3 4 5 6 7 8 9 10
)");

  expect("multi-argument streams: expand_tuple + flatten", &MultiArgStreams, R"(
  k=1 v=10
  k=1 v=20
  k=2 v=30
  k=3 v=40
  k=3 v=50
  k=3 v=60
)");

  expect("swap map key/value via swizzle", &SwapMapKeyValue, R"(
  1 -> alice
  2 -> bob
  3 -> carol
)");

  expect("transform_arg", &TransformArgDemo, R"(
  100 -> alice
  200 -> bob
  300 -> carol
)");

  expect("short-circuit on error", &ShortCircuitOnError, R"(
  good -> 6
  bad  -> nullopt
)");

  expect("flatten_optional vs unwrap_optional", &FlattenVsUnwrap, R"(
  flatten_optional kept: 1 3
  unwrap_optional sum: nullopt
)");

  expect("tee: feed three sub-pipelines into transform_complete", &TeeSummary, R"(
  min=1 max=9 count=6
)");

  expect("tee average", &TeeAverage, R"(
  avg = 3.0000
)");

  expect("group_by org -> count of fulltime employees", &GroupByDemo, R"(
  eng: 2
  sales: 2
)");

  expect("reference inputs (compile-time static_asserts)", &ReferenceInputs, R"(
  const& -> 1
  const& -> 2
  &      -> 1
  &      -> 2
  &&     -> 1
  &&     -> 2
)");

  expect("initializing vector<unique_ptr<int>> via spl::apply",
         &InitializingVectorOfUniquePtr, R"(
  0
  1
)");

  expect("moving keys out of a set", &MovingKeysOutOfMap, R"(
  alpha
  bravo
  charlie
)");

  expect("constexpr squares (sum)", &ConstexprSquares, R"(
  sum 1..5 squared = 55
)");

  expect("word counts (top k)", &WordCountsPipeline, R"(
  5 the
  3 fox
  2 was
  2 quick
  2 lazy
)");

  std::cout << "\nAll examples matched expected output.\n";
  return 0;
}
