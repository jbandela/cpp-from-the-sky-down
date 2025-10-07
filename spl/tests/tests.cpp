#include <gmock/gmock.h>
#include <spl/spl.h> // Include your library's header

namespace {

using ::testing::Pair;
using ::testing::ElementsAre;

// Constexpr-compatible map implementation using std::array with std::optional
template<typename K, typename V, std::size_t MaxSize = 10>
struct constexpr_map {
  std::array<std::optional<std::pair<K, V>>, MaxSize> data{};
  std::size_t count = 0;

  struct iterator {
    std::optional<std::pair<K, V>>* ptr;

    constexpr std::pair<K, V>& operator*() const { return **ptr; }
    constexpr std::pair<K, V>* operator->() const { return &**ptr; }
    constexpr iterator& operator++() { ++ptr; return *this; }
    constexpr bool operator==(const iterator& other) const { return ptr == other.ptr; }
    constexpr bool operator!=(const iterator& other) const { return ptr != other.ptr; }
  };

  struct const_iterator {
    const std::optional<std::pair<K, V>>* ptr;

    constexpr const std::pair<K, V>& operator*() const { return **ptr; }
    constexpr const std::pair<K, V>* operator->() const { return &**ptr; }
    constexpr const_iterator& operator++() { ++ptr; return *this; }
    constexpr bool operator==(const const_iterator& other) const { return ptr == other.ptr; }
    constexpr bool operator!=(const const_iterator& other) const { return ptr != other.ptr; }
  };

  constexpr iterator begin() { return iterator{data.data()}; }
  constexpr iterator end() { return iterator{data.data() + count}; }
  constexpr const_iterator begin() const { return const_iterator{data.data()}; }
  constexpr const_iterator end() const { return const_iterator{data.data() + count}; }

  constexpr iterator find(const K& key) {
    for (std::size_t i = 0; i < count; ++i) {
      if (data[i]->first == key) {
        return iterator{data.data() + i};
      }
    }
    return end();
  }

  constexpr const_iterator find(const K& key) const {
    for (std::size_t i = 0; i < count; ++i) {
      if (data[i]->first == key) {
        return const_iterator{data.data() + i};
      }
    }
    return end();
  }

  template<typename... Args>
  constexpr std::pair<iterator, bool> emplace(K key, Args&&... args) {
    auto it = find(key);
    if (it != end()) {
      return {it, false};
    }
    data[count].emplace(std::move(key), V(std::forward<Args>(args)...));
    ++count;
    return {iterator{data.data() + (count - 1)}, true};
  }
};

// Wrapper struct for constexpr_map to use with group_by
template<std::size_t MaxSize = 10>
struct constexpr_map_factory {
  template<typename K, typename V>
  using type = constexpr_map<K, V, MaxSize>;
};

constexpr auto calculate() {
  constexpr std::array v{1, 2, 3, 4};
  auto t = spl::compose(
      spl::filter([](int i) { return i != 2; }),
      spl::sum());
  return spl::apply(v,
                    spl::transform([](int i) { return i * 2; }),
                    std::move(t));

}

constexpr auto calculate_take() {
  constexpr std::array v{1, 2, 3, 4};
  auto t = spl::compose(
      spl::filter([](int i) { return i != 2; }),
      spl::sum());
  return spl::apply(v,
                    spl::take(3),
                    spl::transform([](int i) { return i * 2; }),
                    std::move(t));

}

constexpr auto calculate_take_lvalue_stage() {
  constexpr std::array v{1, 2, 3, 4};
  auto t = spl::compose(
      spl::filter([](int i) { return i != 2; }),
      spl::sum());
  auto double_i = spl::transform([](int i) { return i * 2; });
  return spl::apply(v,
                    spl::take(3),
                    double_i,
                    t);

}

TEST(SplTest, ConstexprCalculate) {
  static_assert(calculate() == 18);
  EXPECT_THAT(calculate(), 18);

}

TEST(SplTest, ConstexprCalculateTake) {
  static_assert(calculate_take() == 10);
  EXPECT_THAT(calculate_take(), 10);

}

TEST(SplTest, ConstexprCalculateTakeLValueStage) {
  static_assert(calculate_take_lvalue_stage() == 10);
  EXPECT_THAT(calculate_take_lvalue_stage(), 10);

}

constexpr auto constexpr_group_by_test() {
  using pair = std::pair<int, int>;
  constexpr std::array v{pair{1, 2}, pair{2, 4}, pair{1, 1}};

  return spl::apply(v,
                    spl::group_by<constexpr_map_factory<>>(&pair::first,
                                                           spl::transform(&pair::second),
                                                           spl::sum()),
                    spl::transform([](auto&& p) { return p.second; }),
                    spl::sum());
}

TEST(SplTest, GroupBy) {
  static_assert(constexpr_group_by_test() == 7); // group 1: 2+1=3, group 2: 4, sum: 3+4=7
  EXPECT_THAT(constexpr_group_by_test(), 7);

  using pair = std::pair<std::string, int>;
  std::array v{pair{"a", 2}, pair{"b", 4}, pair{"a", 1}};

  auto result = spl::apply(v,
                           spl::group_by(&pair::first,
                                         spl::transform(&pair::second),
                                         spl::sum()),
                           spl::to_vector());

  EXPECT_THAT(result, ElementsAre(Pair("a", 3), Pair("b", 4)));

}

constexpr auto constexpr_iota_test() {

  return spl::apply(spl::iota(0, 10), spl::sum());

}



TEST(SplTest, Iota) {
  auto result = constexpr_iota_test();
  static_assert(constexpr_iota_test() == 45);
  EXPECT_THAT(result, 45);

}

constexpr auto constexpr_bounded_iota_test() {

  return spl::apply(spl::iota(2, 5), spl::sum());

}

TEST(SplTest, BoundedIota) {
  auto result = constexpr_bounded_iota_test();
  static_assert(constexpr_bounded_iota_test() == 9); // 2 + 3 + 4 = 9
  EXPECT_THAT(result, 9);

}

constexpr auto constexpr_unbounded_iota_test() {
  return spl::apply(spl::iota(5), spl::take(3), spl::sum());
}

TEST(SplTest, UnboundedIota) {
  auto result = constexpr_unbounded_iota_test();
  static_assert(constexpr_unbounded_iota_test() == 18); // 5 + 6 + 7 = 18
  EXPECT_THAT(result, 18);
}


constexpr auto constexpr_flatten_test() {
  constexpr std::array<std::array<int, 3>, 2> v{{{1, 2, 3}, {4, 5, 6}}};
  return spl::apply(v, spl::flatten(), spl::sum());
}

TEST(SplTest, Flatten) {
  auto result = constexpr_flatten_test();
  static_assert(constexpr_flatten_test() == 21);
  EXPECT_THAT(result, 21);
}

TEST(SplTest, FlattenToVector) {
  std::array<std::vector<int>, 3> v{{
                                        {1, 2},
                                        {3, 4, 5},
                                        {6}
                                    }};

  auto result = spl::apply(v, spl::flatten(), spl::to_vector());

  EXPECT_THAT(result, ElementsAre(1, 2, 3, 4, 5, 6));
}

constexpr auto constexpr_flatten_with_transform_test() {
  constexpr std::array<std::array<int, 2>, 3> v{{{1, 2}, {3, 4}, {5, 6}}};

  return spl::apply(v,
                    spl::flatten(),
                    spl::transform([](int i) { return i * 2; }),
                    spl::sum());
}

TEST(SplTest, FlattenWithTransform) {
  static_assert(constexpr_flatten_with_transform_test() == 42); // (1+2+3+4+5+6)*2 = 21*2 = 42
  EXPECT_THAT(constexpr_flatten_with_transform_test(), 42);

  constexpr std::array<std::array<int, 2>, 3> v{{{1, 2}, {3, 4}, {5, 6}}};

  auto result = spl::apply(v,
                           spl::flatten(),
                           spl::transform([](int i) { return i * 2; }),
                           spl::to_vector());

  EXPECT_THAT(result, ElementsAre(2, 4, 6, 8, 10, 12));
}

constexpr auto constexpr_flatten_with_take_test() {
  constexpr std::array<std::array<int, 3>, 3>
      v{{{1, 2, 3}, {4, 5, 6}, {7, 8, 9}}};

  return spl::apply(v,
                    spl::flatten(),
                    spl::take(5),
                    spl::sum());
}

TEST(SplTest, FlattenWithTake) {
  static_assert(constexpr_flatten_with_take_test() == 15); // 1+2+3+4+5 = 15
  EXPECT_THAT(constexpr_flatten_with_take_test(), 15);

  constexpr std::array<std::array<int, 3>, 3>
      v{{{1, 2, 3}, {4, 5, 6}, {7, 8, 9}}};

  auto result = spl::apply(v,
                           spl::flatten(),
                           spl::take(5),
                           spl::to_vector());

  EXPECT_THAT(result, ElementsAre(1, 2, 3, 4, 5));
}
constexpr auto constexpr_flatten_multiple_args_test() {
  constexpr std::array<std::pair<int, std::array<int, 3>>, 2> v{{
      {10, {1, 2, 3}},
      {20, {4, 5, 6}}
  }};

  return spl::apply(v,
                    spl::expand_tuple(),
                    spl::flatten(),
                    spl::transform([](int key, int val) { return key + val; }),
                    spl::sum());
}

#if 1
TEST(SplTest, FlattenMultipleArgs) {
  static_assert(constexpr_flatten_multiple_args_test() == 111); // (10+1)+(10+2)+(10+3)+(20+4)+(20+5)+(20+6) = 11+12+13+24+25+26 = 111
  EXPECT_THAT(constexpr_flatten_multiple_args_test(), 111);

  // Test that flatten() now works with multiple arguments by flattening the last one
  std::array<std::pair<std::string, std::vector<int>>, 2> v{{
                                                                {"a",
                                                                 {1, 2, 3}},
                                                                {"b", {4, 5}}
                                                            }};

  auto result = spl::apply(v,
                           spl::expand_tuple(),
                           spl::flatten(),
                           spl::make_pair(),
                           spl::to_vector());

  using pair_type = std::pair<std::string, int>;
  EXPECT_THAT(result, ElementsAre(
      pair_type{"a", 1}, pair_type{"a", 2}, pair_type{"a", 3},
      pair_type{"b", 4}, pair_type{"b", 5}
  ));
}
#endif

constexpr auto constexpr_zip_result_test() {
  constexpr std::array v{1, 2, 3, 4};
  return spl::apply(v,
                    spl::zip_result([](int i) { return i * 2; }),
                    spl::transform([](int orig, int doubled) { return orig + doubled; }),
                    spl::sum());
}

TEST(SplTest, ZipResult) {
  auto result = constexpr_zip_result_test();
  static_assert(constexpr_zip_result_test() == 30); // 1+2 + 2+4 + 3+6 + 4+8 = 3+6+9+12 = 30
  EXPECT_THAT(result, 30);
}

constexpr auto constexpr_zip_result_to_vector_test() {
  constexpr std::array v{1, 2, 3};
  return spl::apply(v,
                    spl::zip_result([](int i) { return i * i; }),
                    spl::transform([](int orig, int squared) { return orig + squared; }),
                    spl::sum());
}

TEST(SplTest, ZipResultToVector) {
  static_assert(constexpr_zip_result_to_vector_test() == 20); // (1+1)+(2+4)+(3+9) = 2+6+12 = 20
  EXPECT_THAT(constexpr_zip_result_to_vector_test(), 20);

  std::array v{1, 2, 3};
  auto result = spl::apply(v,
                           spl::zip_result([](int i) { return i * i; }),
                           spl::make_pair(),
                           spl::to_vector());

  EXPECT_THAT(result, ElementsAre(Pair(1, 1), Pair(2, 4), Pair(3, 9)));
}

constexpr auto constexpr_zip_result_multiple_args_test() {
  constexpr std::array<std::pair<int, int>, 3> v{{
      {1, 2},
      {3, 4},
      {5, 6}
  }};

  return spl::apply(v,
                    spl::expand_tuple(),
                    spl::zip_result([](int a, int b) { return a + b; }),
                    spl::transform([](int a, int b, int sum) { return a + b + sum; }),
                    spl::sum());
}

TEST(SplTest, ZipResultMultipleArgs) {
  static_assert(constexpr_zip_result_multiple_args_test() == 42); // (1+2+3)+(3+4+7)+(5+6+11) = 6+14+22 = 42
  EXPECT_THAT(constexpr_zip_result_multiple_args_test(), 42);

  std::array<std::pair<int, int>, 3> v{{
      {1, 2},
      {3, 4},
      {5, 6}
  }};

  auto result = spl::apply(v,
                           spl::expand_tuple(),
                           spl::zip_result([](int a, int b) { return a + b; }),
                           spl::make_tuple(),
                           spl::to_vector());

  using triple = std::tuple<int, int, int>;
  EXPECT_THAT(result, ElementsAre(triple{1, 2, 3}, triple{3, 4, 7}, triple{5, 6, 11}));
}

constexpr auto constexpr_swizzle_reorder_test() {
  constexpr std::array<std::tuple<int, int, int>, 2> v{{
      {1, 2, 3},
      {4, 5, 6}
  }};

  return spl::apply(v,
                    spl::expand_tuple(),
                    spl::swizzle<2, 0, 1>(),
                    spl::transform([](int a, int b, int c) { return a + b + c; }),
                    spl::sum());
}

TEST(SplTest, SwizzleReorder) {
  static_assert(constexpr_swizzle_reorder_test() == 21); // (3+1+2)+(6+4+5) = 6+15 = 21
  EXPECT_THAT(constexpr_swizzle_reorder_test(), 21);

  std::array<std::tuple<int, int, int>, 2> v{{
      {1, 2, 3},
      {4, 5, 6}
  }};

  auto result = spl::apply(v,
                           spl::expand_tuple(),
                           spl::swizzle<2, 0, 1>(),
                           spl::make_tuple(),
                           spl::to_vector());

  using triple = std::tuple<int, int, int>;
  EXPECT_THAT(result, ElementsAre(triple{3, 1, 2}, triple{6, 4, 5}));
}

constexpr auto constexpr_swizzle_filter_test() {
  constexpr std::array<std::tuple<int, int, int>, 2> v{{
      {1, 2, 3},
      {4, 5, 6}
  }};

  return spl::apply(v,
                    spl::expand_tuple(),
                    spl::swizzle<2, 0>(),
                    spl::transform([](int a, int b) { return a + b; }),
                    spl::sum());
}

TEST(SplTest, SwizzleFilter) {
  static_assert(constexpr_swizzle_filter_test() == 14); // (3+1)+(6+4) = 4+10 = 14
  EXPECT_THAT(constexpr_swizzle_filter_test(), 14);

  std::array<std::tuple<int, int, int>, 2> v{{
      {1, 2, 3},
      {4, 5, 6}
  }};

  auto result = spl::apply(v,
                           spl::expand_tuple(),
                           spl::swizzle<2, 0>(),
                           spl::make_pair(),
                           spl::to_vector());

  EXPECT_THAT(result, ElementsAre(Pair(3, 1), Pair(6, 4)));
}

constexpr auto constexpr_swizzle_duplicate_test() {
  constexpr std::array<std::pair<int, int>, 2> v{{
      {1, 2},
      {3, 4}
  }};

  return spl::apply(v,
                    spl::expand_tuple(),
                    spl::swizzle<0, 0, 1>(),
                    spl::transform([](int a, int b, int c) { return a + b + c; }),
                    spl::sum());
}

TEST(SplTest, SwizzleDuplicate) {
  static_assert(constexpr_swizzle_duplicate_test() == 14); // (1+1+2)+(3+3+4) = 4+10 = 14
  EXPECT_THAT(constexpr_swizzle_duplicate_test(), 14);

  std::array<std::pair<int, int>, 2> v{{
      {1, 2},
      {3, 4}
  }};

  auto result = spl::apply(v,
                           spl::expand_tuple(),
                           spl::swizzle<0, 0, 1>(),
                           spl::make_tuple(),
                           spl::to_vector());

  using triple = std::tuple<int, int, int>;
  EXPECT_THAT(result, ElementsAre(triple{1, 1, 2}, triple{3, 3, 4}));
}

constexpr auto constexpr_swizzle_single_test() {
  constexpr std::array<std::tuple<int, int, int>, 3> v{{
      {1, 2, 3},
      {4, 5, 6},
      {7, 8, 9}
  }};

  return spl::apply(v,
                    spl::expand_tuple(),
                    spl::swizzle<1>(),
                    spl::sum());
}

TEST(SplTest, SwizzleSingle) {
  static_assert(constexpr_swizzle_single_test() == 15); // 2 + 5 + 8 = 15
  EXPECT_THAT(constexpr_swizzle_single_test(), 15);
}

auto PythagoreanTriples() {
  auto zip_flat = [](auto f) {
    return spl::compose(spl::zip_result(f), spl::flatten());
  };
  return spl::compose(
      spl::iota(1),
      zip_flat([](int c) { return spl::iota(1, c + 1); }),
      zip_flat([](int c, int a) { return spl::iota(a, c + 1); }),
      spl::filter([](int c, int a, int b) { return a * a + b * b == c * c; }),
      spl::swizzle<1, 2, 0>()
  );

}


TEST(SplTest, PythagoreanTriples){

  auto zip_flat = [](auto f) {
    return spl::compose(spl::zip_result(f), spl::flatten());
  };
  auto triples = spl::apply(
      spl::iota(1),
      zip_flat([](int c) { return spl::iota(1, c + 1); }),
      zip_flat([](int c, int a) { return spl::iota(a, c + 1); }),
      spl::filter([](int c, int a, int b) { return a * a + b * b == c * c; }),
      spl::swizzle<1, 2, 0>(),
      spl::take(10),
      spl::make_tuple(),
      spl::to_vector()
  );

  using triple = std::tuple<int, int, int>;
  EXPECT_THAT(triples, ElementsAre(
      triple{3, 4, 5},
      triple{6, 8, 10},
      triple{5, 12, 13},
      triple{9, 12, 15},
      triple{8, 15, 17},
      triple{12, 16, 20},
      triple{7, 24, 25},
      triple{15, 20, 25},
      triple{10, 24, 26},
      triple{20, 21, 29}
  ));

}


}

