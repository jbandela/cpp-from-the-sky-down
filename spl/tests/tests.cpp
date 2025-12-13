#include <gmock/gmock.h>
#include <spl/spl.h> // Include your library's header

namespace {

using ::testing::Pair;
using ::testing::ElementsAre;
using ::testing::UnorderedElementsAre;

// Constexpr-compatible map implementation using std::array with std::optional
template<typename K, typename V, std::size_t MaxSize = 10>
struct constexpr_map {
  std::array<std::optional<std::pair<K, V>>, MaxSize> data{};
  std::size_t count = 0;

  struct iterator {
    std::optional<std::pair<K, V>> *ptr;

    constexpr std::pair<K, V> &operator*() const { return **ptr; }
    constexpr std::pair<K, V> *operator->() const { return &**ptr; }
    constexpr iterator &operator++() {
      ++ptr;
      return *this;
    }
    constexpr bool operator==(const iterator &other) const {
      return ptr == other.ptr;
    }
    constexpr bool operator!=(const iterator &other) const {
      return ptr != other.ptr;
    }
  };

  struct const_iterator {
    const std::optional<std::pair<K, V>> *ptr;

    constexpr const std::pair<K, V> &operator*() const { return **ptr; }
    constexpr const std::pair<K, V> *operator->() const { return &**ptr; }
    constexpr const_iterator &operator++() {
      ++ptr;
      return *this;
    }
    constexpr bool operator==(const const_iterator &other) const {
      return ptr == other.ptr;
    }
    constexpr bool operator!=(const const_iterator &other) const {
      return ptr != other.ptr;
    }
  };

  constexpr iterator begin() { return iterator{data.data()}; }
  constexpr iterator end() { return iterator{data.data() + count}; }
  constexpr const_iterator begin() const { return const_iterator{data.data()}; }
  constexpr const_iterator end() const {
    return const_iterator{data.data() + count};
  }

  constexpr iterator find(const K &key) {
    for (std::size_t i = 0; i < count; ++i) {
      if (data[i]->first == key) {
        return iterator{data.data() + i};
      }
    }
    return end();
  }

  constexpr const_iterator find(const K &key) const {
    for (std::size_t i = 0; i < count; ++i) {
      if (data[i]->first == key) {
        return const_iterator{data.data() + i};
      }
    }
    return end();
  }

  template<typename... Args>
  constexpr std::pair<iterator, bool> emplace(K key, Args &&... args) {
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
                    spl::make_pair(),
                    spl::transform([](auto &&p) { return p.second; }),
                    spl::sum());
}

TEST(SplTest, GroupBy) {
  static_assert(
      constexpr_group_by_test() == 7); // group 1: 2+1=3, group 2: 4, sum: 3+4=7
  EXPECT_THAT(constexpr_group_by_test(), 7);

  using pair = std::pair<std::string, int>;
  std::array v{pair{"a", 2}, pair{"b", 4}, pair{"a", 1}};

  auto result = spl::apply(v,
                           spl::group_by(&pair::first,
                                         spl::transform(&pair::second),
                                         spl::sum()),
                           spl::make_pair(),
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
  static_assert(constexpr_flatten_with_transform_test()
                    == 42); // (1+2+3+4+5+6)*2 = 21*2 = 42
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
                                                                    {10,
                                                                     {1, 2, 3}},
                                                                    {20,
                                                                     {4, 5, 6}}
                                                                }};

  return spl::apply(v,
                    spl::expand_tuple(),
                    spl::flatten(),
                    spl::transform([](int key, int val) { return key + val; }),
                    spl::sum());
}

#if 1
TEST(SplTest, FlattenMultipleArgs) {
  static_assert(constexpr_flatten_multiple_args_test()
                    == 111); // (10+1)+(10+2)+(10+3)+(20+4)+(20+5)+(20+6) = 11+12+13+24+25+26 = 111
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
                    spl::transform([](int orig, int doubled) {
                      return orig + doubled;
                    }),
                    spl::sum());
}

TEST(SplTest, ZipResult) {
  auto result = constexpr_zip_result_test();
  static_assert(constexpr_zip_result_test()
                    == 30); // 1+2 + 2+4 + 3+6 + 4+8 = 3+6+9+12 = 30
  EXPECT_THAT(result, 30);
}

constexpr auto constexpr_zip_result_to_vector_test() {
  constexpr std::array v{1, 2, 3};
  return spl::apply(v,
                    spl::zip_result([](int i) { return i * i; }),
                    spl::transform([](int orig, int squared) {
                      return orig + squared;
                    }),
                    spl::sum());
}

TEST(SplTest, ZipResultToVector) {
  static_assert(constexpr_zip_result_to_vector_test()
                    == 20); // (1+1)+(2+4)+(3+9) = 2+6+12 = 20
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
                    spl::transform([](int a, int b, int sum) {
                      return a + b + sum;
                    }),
                    spl::sum());
}

TEST(SplTest, ZipResultMultipleArgs) {
  static_assert(constexpr_zip_result_multiple_args_test()
                    == 42); // (1+2+3)+(3+4+7)+(5+6+11) = 6+14+22 = 42
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
  EXPECT_THAT(result,
              ElementsAre(triple{1, 2, 3}, triple{3, 4, 7}, triple{5, 6, 11}));
}

constexpr auto constexpr_swizzle_reorder_test() {
  constexpr std::array<std::tuple<int, int, int>, 2> v{{
                                                           {1, 2, 3},
                                                           {4, 5, 6}
                                                       }};

  return spl::apply(v,
                    spl::expand_tuple(),
                    spl::swizzle<2, 0, 1>(),
                    spl::transform([](int a, int b, int c) {
                      return a + b + c;
                    }),
                    spl::sum());
}

TEST(SplTest, SwizzleReorder) {
  static_assert(
      constexpr_swizzle_reorder_test() == 21); // (3+1+2)+(6+4+5) = 6+15 = 21
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
  static_assert(
      constexpr_swizzle_filter_test() == 14); // (3+1)+(6+4) = 4+10 = 14
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
                    spl::transform([](int a, int b, int c) {
                      return a + b + c;
                    }),
                    spl::sum());
}

TEST(SplTest, SwizzleDuplicate) {
  static_assert(
      constexpr_swizzle_duplicate_test() == 14); // (1+1+2)+(3+3+4) = 4+10 = 14
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

TEST(SplTest, PythagoreanTriples) {

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

TEST(SplTest, Tee) {
  constexpr auto result = spl::apply(
      std::array{1, 2, 3, 4, 5},
      spl::tee(
          spl::sum(),
          spl::compose(spl::transform([](int x) { return x * x; }), spl::sum())
      )
  );

  static_assert(std::get<0>(result) == 15);  // sum
  static_assert(std::get<1>(result) == 55);  // sum of squares
  EXPECT_EQ(std::get<0>(result), 15);  // sum
  EXPECT_EQ(std::get<1>(result), 55);  // sum of squares
}

TEST(SplTest, TeeConstexpr) {
  constexpr auto result = []() {
    return spl::apply(
        std::array{2, 4, 6, 8},
        spl::tee(
            spl::sum(),
            spl::compose(spl::transform([](int x) { return x / 2; }),
                         spl::sum())
        )
    );
  }();

  static_assert(std::get<0>(result) == 20);  // sum
  static_assert(std::get<1>(result) == 10);  // sum of halves
}

TEST(SplTest, TeeSinglePipeline) {
  auto result = spl::apply(
      std::vector{1, 2, 3},
      spl::tee(spl::sum())
  );

  EXPECT_EQ(std::get<0>(result), 6);
}

TEST(SplTest, TeeThreePipelines) {
  constexpr auto result = spl::apply(
      std::array{2, 3, 4, 5},
      spl::tee(
          spl::sum(),
          spl::compose(spl::transform([](int x) { return x * x; }), spl::sum()),
          spl::compose(spl::transform([](int x) { return x * 2; }), spl::sum())
      )
  );

  static_assert(std::get<0>(result) == 14);   // sum: 2+3+4+5
  static_assert(std::get<1>(result) == 54);   // sum of squares: 4+9+16+25
  static_assert(std::get<2>(result) == 28);   // sum of doubles: 4+6+8+10
  EXPECT_EQ(std::get<0>(result), 14);   // sum: 2+3+4+5
  EXPECT_EQ(std::get<1>(result), 54);   // sum of squares: 4+9+16+25
  EXPECT_EQ(std::get<2>(result), 28);   // sum of doubles: 4+6+8+10
}

TEST(SplTest, TeeWithVector) {
  auto result = spl::apply(
      std::vector{10, 20, 30},
      spl::tee(
          spl::sum(),
          spl::to_vector(),
          spl::compose(spl::filter([](int x) { return x > 15; }), spl::sum())
      )
  );

  EXPECT_EQ(std::get<0>(result), 60);
  EXPECT_EQ(std::get<1>(result), (std::vector{10, 20, 30}));
  EXPECT_EQ(std::get<2>(result), 50);  // 20 + 30
}

TEST(SplTest, TeeMultiArgument) {
  constexpr auto result = spl::apply(
      std::array{1, 2, 3},
      spl::zip_result([](int x) { return x * 10; }),
      spl::tee(
          spl::compose(spl::transform([](int x, int y) { return x + y; }),
                       spl::sum()),
          spl::compose(spl::transform([](int x, int y) { return x * y; }),
                       spl::sum())
      )
  );

  static_assert(
      std::get<0>(result) == 66);   // (1+10) + (2+20) + (3+30) = 11+22+33
  static_assert(
      std::get<1>(result) == 140);  // (1*10) + (2*20) + (3*30) = 10+40+90

  EXPECT_EQ(std::get<0>(result), 66);   // (1+10) + (2+20) + (3+30) = 11+22+33
  EXPECT_EQ(std::get<1>(result), 140);  // (1*10) + (2*20) + (3*30) = 10+40+90
}

constexpr auto constexpr_for_each_test() {
  constexpr std::array v{1, 2, 3, 4, 5};
  int sum = 0;
  spl::apply(v, spl::for_each([&sum](int x) { sum += x; }));
  return sum;
}

TEST(SplTest, ForEach) {
  static_assert(constexpr_for_each_test() == 15);  // 1+2+3+4+5 = 15
  EXPECT_EQ(constexpr_for_each_test(), 15);
}

constexpr auto constexpr_for_each_with_filter_test() {
  constexpr std::array v{1, 2, 3, 4, 5, 6};
  int sum = 0;
  spl::apply(v,
             spl::filter([](int x) { return x % 2 == 0; }),
             spl::for_each([&sum](int x) { sum += x; }));
  return sum;
}

TEST(SplTest, ForEachWithFilter) {
  static_assert(constexpr_for_each_with_filter_test() == 12);  // 2+4+6 = 12
  EXPECT_EQ(constexpr_for_each_with_filter_test(), 12);
}

constexpr auto constexpr_for_each_with_transform_test() {
  constexpr std::array v{1, 2, 3};
  int product = 1;
  spl::apply(v,
             spl::transform([](int x) { return x * 2; }),
             spl::for_each([&product](int x) { product *= x; }));
  return product;
}

TEST(SplTest, ForEachWithTransform) {
  static_assert(constexpr_for_each_with_transform_test() == 48);  // 2*4*6 = 48
  EXPECT_EQ(constexpr_for_each_with_transform_test(), 48);
}

TEST(SplTest, ForEachWithVector) {
  std::vector<int> results;
  spl::apply(std::vector{10, 20, 30},
             spl::for_each([&results](int x) { results.push_back(x * 2); }));

  EXPECT_THAT(results, ElementsAre(20, 40, 60));
}

constexpr auto constexpr_for_each_multi_arg_test() {
  constexpr std::array<std::pair<int, int>, 3> v{{
                                                     {1, 2},
                                                     {3, 4},
                                                     {5, 6}
                                                 }};

  int sum = 0;
  spl::apply(v,
             spl::expand_tuple(),
             spl::for_each([&sum](int a, int b) { sum += a + b; }));
  return sum;
}

TEST(SplTest, ForEachMultiArgument) {
  static_assert(constexpr_for_each_multi_arg_test()
                    == 21);  // (1+2)+(3+4)+(5+6) = 3+7+11 = 21
  EXPECT_EQ(constexpr_for_each_multi_arg_test(), 21);

  std::vector<int> results;
  std::array<std::pair<int, int>, 2> v{{
                                           {10, 20},
                                           {30, 40}
                                       }};

  spl::apply(v,
             spl::expand_tuple(),
             spl::for_each([&results](int a, int b) {
               results.push_back(a * b);
             }));

  EXPECT_THAT(results, ElementsAre(200, 1200));
}

constexpr auto constexpr_for_each_count_test() {
  constexpr std::array v{1, 2, 3, 4, 5};
  int count = 0;
  spl::apply(v,
             spl::filter([](int x) { return x > 2; }),
             spl::for_each([&count](int) { ++count; }));
  return count;
}

TEST(SplTest, ForEachCount) {
  static_assert(
      constexpr_for_each_count_test() == 3);  // 3, 4, 5 pass the filter
  EXPECT_EQ(constexpr_for_each_count_test(), 3);
}

constexpr auto constexpr_transform_complete_test() {
  constexpr std::array v{1, 2, 3, 4, 5};
  return spl::apply(v,
                    spl::sum(),
                    spl::transform_complete([](int x) { return x * 2; }));
}

TEST(SplTest, TransformComplete) {
  static_assert(
      constexpr_transform_complete_test() == 30);  // (1+2+3+4+5)*2 = 15*2 = 30
  EXPECT_EQ(constexpr_transform_complete_test(), 30);
}

constexpr auto constexpr_transform_complete_multiple_test() {
  constexpr std::array v{2, 4, 6};
  return spl::apply(v,
                    spl::tee(
                        spl::sum(),
                        spl::compose(spl::transform([](int x) {
                          return x * x;
                        }), spl::sum())
                    ),
                    spl::transform_complete([](auto tuple) {
                      return std::get<0>(tuple) + std::get<1>(tuple);
                    }));
}

TEST(SplTest, TransformCompleteMultiple) {
  static_assert(constexpr_transform_complete_multiple_test()
                    == 68);  // sum=12, sum_squares=56, total=68
  EXPECT_EQ(constexpr_transform_complete_multiple_test(), 68);
}

constexpr auto constexpr_transform_complete_cps_test() {
  constexpr std::array v{10, 20, 30};
  return spl::apply(v,
                    spl::sum(),
                    spl::transform_complete_cps([](auto &&out, int x) {
                      return out(x / 10, x % 10);
                    }),
                    spl::transform_complete([](int a, int b) {
                      return a + b;
                    }));
}

TEST(SplTest, TransformCompleteCps) {
  static_assert(constexpr_transform_complete_cps_test()
                    == 6);  // 60/10 + 60%10 = 6 + 0 = 6
  EXPECT_EQ(constexpr_transform_complete_cps_test(), 6);
}

TEST(SplTest, TransformCompleteWithVector) {
  auto result = spl::apply(std::vector{1, 2, 3, 4},
                           spl::sum(),
                           spl::transform_complete([](int x) {
                             return x * 3;
                           }));

  EXPECT_EQ(result, 30);  // (1+2+3+4)*3 = 10*3 = 30
}

constexpr auto constexpr_transform_complete_chain_test() {
  constexpr std::array v{5, 10, 15};
  return spl::apply(v,
                    spl::sum(),
                    spl::transform_complete([](int x) { return x * 2; }),
                    spl::transform_complete([](int x) { return x + 10; }),
                    spl::transform_complete([](int x) { return x / 5; }));
}

TEST(SplTest, TransformCompleteChain) {
  static_assert(constexpr_transform_complete_chain_test()
                    == 14);  // ((30*2)+10)/5 = 70/5 = 14
  EXPECT_EQ(constexpr_transform_complete_chain_test(), 14);
}

constexpr auto constexpr_accumulate_in_place_with_type_calculator_test() {
  constexpr std::array v{1, 2, 3, 4, 5};
  return spl::apply(v,
                    spl::accumulate_in_place_with_type_calculator(
                        []<typename... Types>(spl::types<Types...>) {
                          return spl::types<int>();
                        },
                        [](int &acc, int x) { acc += x * x; }));
}

TEST(SplTest, AccumulateInPlaceWithTypeCalculator) {
  static_assert(constexpr_accumulate_in_place_with_type_calculator_test()
                    == 55);  // 1+4+9+16+25 = 55
  EXPECT_EQ(constexpr_accumulate_in_place_with_type_calculator_test(), 55);
}

constexpr auto constexpr_accumulate_in_place_with_type_calculator_product_test() {
  constexpr std::array v{2, 3, 4};
  return spl::apply(v,
                    spl::accumulate_in_place_with_type_calculator(
                        []<typename... Types>(spl::types<Types...>) {
                          return spl::types<int>();
                        },
                        [](int &acc, int x) {
                          if (acc == 0) acc = 1;
                          acc *= x;
                        }));
}

TEST(SplTest, AccumulateInPlaceWithTypeCalculatorProduct) {
  static_assert(
      constexpr_accumulate_in_place_with_type_calculator_product_test()
          == 24);  // 2*3*4 = 24
  EXPECT_EQ(constexpr_accumulate_in_place_with_type_calculator_product_test(),
            24);
}

constexpr auto constexpr_accumulate_in_place_with_type_calculator_pair_test() {
  constexpr std::array<std::pair<int, int>, 3> v{{
                                                     {1, 10},
                                                     {2, 20},
                                                     {3, 30}
                                                 }};

  struct Acc {
    int sum_first = 0;
    int sum_second = 0;
  };

  auto result = spl::apply(v,
                           spl::expand_tuple(),
                           spl::accumulate_in_place_with_type_calculator(
                               []<typename... Types>(spl::types<Types...>) {
                                 return spl::types<Acc>();
                               },
                               [](Acc &acc, int a, int b) {
                                 acc.sum_first += a;
                                 acc.sum_second += b;
                               }));
  return result.sum_first + result.sum_second;
}

TEST(SplTest, AccumulateInPlaceWithTypeCalculatorPair) {
  static_assert(constexpr_accumulate_in_place_with_type_calculator_pair_test()
                    == 66);  // (1+2+3)+(10+20+30) = 6+60 = 66
  EXPECT_EQ(constexpr_accumulate_in_place_with_type_calculator_pair_test(), 66);
}

TEST(SplTest, AccumulateInPlaceWithTypeCalculatorVector) {
  std::vector<int> v{5, 10, 15, 20};
  auto result = spl::apply(v,
                           spl::accumulate_in_place_with_type_calculator(
                               []<typename... Types>(spl::types<Types...>) {
                                 return spl::types<int>();
                               },
                               [](int &acc, int x) { acc += x; }));

  EXPECT_EQ(result, 50);  // 5+10+15+20 = 50
}

constexpr auto constexpr_accumulate_in_place_with_type_calculator_with_filter_test() {
  constexpr std::array v{1, 2, 3, 4, 5, 6, 7, 8};
  return spl::apply(v,
                    spl::filter([](int x) { return x % 2 == 0; }),
                    spl::accumulate_in_place_with_type_calculator(
                        []<typename... Types>(spl::types<Types...>) {
                          return spl::types<int>();
                        },
                        [](int &acc, int x) { acc += x; }));
}

TEST(SplTest, AccumulateInPlaceWithTypeCalculatorWithFilter) {
  static_assert(
      constexpr_accumulate_in_place_with_type_calculator_with_filter_test()
          == 20);  // 2+4+6+8 = 20
  EXPECT_EQ(constexpr_accumulate_in_place_with_type_calculator_with_filter_test(),
            20);
}

}

// to_map tests
constexpr auto constexpr_to_map_test() {
  constexpr std::array<std::pair<int, int>, 3> v{{
                                                     {1, 10},
                                                     {2, 20},
                                                     {3, 30}
                                                 }};

  auto m = spl::apply(v, spl::expand_tuple(), spl::to_map<constexpr_map>());

  int sum = 0;
  for (auto &[k, v] : m) {
    sum += k + v;
  }
  return sum;
}

TEST(SplTest, ToMap) {
  static_assert(
      constexpr_to_map_test() == 66);  // (1+10)+(2+20)+(3+30) = 11+22+33 = 66
  EXPECT_EQ(constexpr_to_map_test(), 66);
}

TEST(SplTest, ToMapStdMap) {
  std::array<std::pair<int, std::string>, 3> v{{
                                                   {1, "one"},
                                                   {2, "two"},
                                                   {3, "three"}
                                               }};

  auto result = spl::apply(v, spl::expand_tuple(), spl::to_map<std::map>());

  EXPECT_EQ(result.size(), 3);
  EXPECT_EQ(result[1], "one");
  EXPECT_EQ(result[2], "two");
  EXPECT_EQ(result[3], "three");
}

TEST(SplTest, ToMapWithTransform) {
  std::array<int, 4> v{1, 2, 3, 4};

  auto result = spl::apply(v,
                           spl::transform([](int x) {
                             return std::make_pair(x, x * x);
                           }),
                           spl::expand_tuple(),
                           spl::to_map<std::map>());

  EXPECT_EQ(result.size(), 4);
  EXPECT_EQ(result[1], 1);
  EXPECT_EQ(result[2], 4);
  EXPECT_EQ(result[3], 9);
  EXPECT_EQ(result[4], 16);
}

TEST(SplTest, ToMapWithFilter) {
  std::array<std::pair<int, std::string>, 5> v{{
                                                   {1, "a"},
                                                   {2, "b"},
                                                   {3, "c"},
                                                   {4, "d"},
                                                   {5, "e"}
                                               }};

  auto result = spl::apply(v,
                           spl::filter([](const auto &p) {
                             return p.first % 2 == 0;
                           }),
                           spl::expand_tuple(),
                           spl::to_map<std::map>());

  EXPECT_EQ(result.size(), 2);
  EXPECT_EQ(result[2], "b");
  EXPECT_EQ(result[4], "d");
}

constexpr auto constexpr_to_map_with_zip_test() {
  constexpr std::array v{5, 10, 15};

  auto m = spl::apply(v,
                      spl::zip_result([](int x) { return x * 2; }),
                      spl::to_map<constexpr_map>());

  int sum = 0;
  for (auto &[k, v] : m) {
    sum += v;
  }
  return sum;
}

TEST(SplTest, ToMapWithZipResult) {
  static_assert(constexpr_to_map_with_zip_test() == 60);  // 10+20+30 = 60
  EXPECT_EQ(constexpr_to_map_with_zip_test(), 60);

  std::array v{1, 2, 3};
  auto result = spl::apply(v,
                           spl::zip_result([](int x) { return x * x; }),
                           spl::to_map<std::map>());

  EXPECT_EQ(result.size(), 3);
  EXPECT_EQ(result[1], 1);
  EXPECT_EQ(result[2], 4);
  EXPECT_EQ(result[3], 9);
}

TEST(SplTest, ToMapDuplicateKeys) {
  // When duplicate keys are inserted, the map keeps the first one
  std::array<std::pair<int, std::string>, 4> v{{
                                                   {1, "first"},
                                                   {2, "two"},
                                                   {1, "second"},
                                                   {3, "three"}
                                               }};

  auto result = spl::apply(v, spl::expand_tuple(), spl::to_map<std::map>());

  EXPECT_EQ(result.size(), 3);
  EXPECT_EQ(result[1], "first");  // First insertion wins
  EXPECT_EQ(result[2], "two");
  EXPECT_EQ(result[3], "three");
}

constexpr auto constexpr_to_map_from_iota_test() {
  auto m = spl::apply(spl::iota(1, 5),
                      spl::transform_cps([](auto &&out, int x) {
                        return out(x, x * 10);
                      }),
                      spl::to_map<constexpr_map>());

  int sum = 0;
  for (auto &[k, v] : m) {
    sum += k + v;
  }
  return sum;
}

TEST(SplTest, ToMapFromIota) {
  static_assert(constexpr_to_map_from_iota_test()
                    == 110);  // (1+10)+(2+20)+(3+30)+(4+40) = 11+22+33+44 = 110
  EXPECT_EQ(constexpr_to_map_from_iota_test(), 110);
}

TEST(SplTest, ToMapStringKeys) {
  std::vector<std::pair<std::string, int>> v{
      {"apple", 5},
      {"banana", 3},
      {"cherry", 8}
  };

  auto result = spl::apply(v, spl::expand_tuple(), spl::to_map<std::map>());

  EXPECT_EQ(result.size(), 3);
  EXPECT_EQ(result["apple"], 5);
  EXPECT_EQ(result["banana"], 3);
  EXPECT_EQ(result["cherry"], 8);
}

// Generator tests
TEST(SplTest, GeneratorBasic) {
  // Create a simple generator that yields values
  auto gen = spl::generator([i = 0](auto &&out) mutable {
    if constexpr (spl::calculate_type_v<decltype(out)>) {
      return out(int(i));
    } else {
      if (i >= 5) return false;
      out(i++);
      return true;
    }
  });

  auto result = spl::apply(std::move(gen), spl::to_vector());

  EXPECT_THAT(result, ElementsAre(0, 1, 2, 3, 4));
}

TEST(SplTest, GeneratorWithTransform) {
  auto gen = spl::generator([i = 1](auto &&out) mutable {
    if constexpr (spl::calculate_type_v<decltype(out)>) {
      return out(int(i));
    } else {
      if (i > 4) return false;
      out(i++);
      return true;
    }
  });

  auto result = spl::apply(std::move(gen),
                           spl::transform([](int x) { return x * x; }),
                           spl::to_vector());

  EXPECT_THAT(result, ElementsAre(1, 4, 9, 16));
}

TEST(SplTest, GeneratorWithFilter) {
  auto gen = spl::generator([i = 0](auto &&out) mutable {
    if constexpr (spl::calculate_type_v<decltype(out)>) {
      return out(int(i));
    } else {
      if (i >= 10) return false;
      out(i++);
      return true;
    }
  });

  auto result = spl::apply(std::move(gen),
                           spl::filter([](int x) { return x % 2 == 0; }),
                           spl::to_vector());

  EXPECT_THAT(result, ElementsAre(0, 2, 4, 6, 8));
}

TEST(SplTest, GeneratorWithTake) {
  auto gen = spl::generator([i = 0](auto &&out) mutable {
    if constexpr (spl::calculate_type_v<decltype(out)>) {
      return out(int(i));
    } else {
      out(i++);
      return true;  // Infinite generator
    }
  });

  auto result = spl::apply(std::move(gen),
                           spl::take(5),
                           spl::to_vector());

  EXPECT_THAT(result, ElementsAre(0, 1, 2, 3, 4));
}

TEST(SplTest, GeneratorWithSum) {
  auto gen = spl::generator([i = 1](auto &&out) mutable {
    if constexpr (spl::calculate_type_v<decltype(out)>) {
      return out(int(i));
    } else {
      if (i > 5) return false;
      out(i++);
      return true;
    }
  });

  auto result = spl::apply(std::move(gen), spl::sum());

  EXPECT_EQ(result, 15);  // 1+2+3+4+5 = 15
}

TEST(SplTest, GeneratorFibonacci) {
  // Fibonacci generator
  auto gen = spl::generator([a = 0, b = 1, count = 0](auto &&out) mutable {
    if constexpr (spl::calculate_type_v<decltype(out)>) {
      return out(int(a));
    } else {
      if (count >= 10) return false;
      int current = a;
      out(std::move(current));
      int next = a + b;
      a = b;
      b = next;
      count++;
      return true;
    }
  });

  auto result = spl::apply(std::move(gen), spl::to_vector());

  EXPECT_THAT(result, ElementsAre(0, 1, 1, 2, 3, 5, 8, 13, 21, 34));
}

TEST(SplTest, GeneratorWithGroupBy) {
  auto gen = spl::generator([i = 0](auto &&out) mutable {
    if constexpr (spl::calculate_type_v<decltype(out)>) {
      return out(int(i));
    } else {
      if (i >= 10) return false;
      out(i++);
      return true;
    }
  });

  auto result = spl::apply(std::move(gen),
                           spl::transform([](int x) {
                             return std::make_pair(x % 3, x);
                           }),
                           spl::group_by(&std::pair<int, int>::first,
                                         spl::transform(&std::pair<int,
                                                                   int>::second),
                                         spl::sum()),
                           spl::make_pair(),
                           spl::to_vector());

  using pair_type = std::pair<int, int>;
  EXPECT_THAT(result, ElementsAre(
      Pair(0, 18),  // 0+3+6+9 = 18
      Pair(1, 12),  // 1+4+7 = 12
      Pair(2, 15)   // 2+5+8 = 15
  ));
}

constexpr auto constexpr_generator_test() {
  auto gen = spl::generator([i = 1](auto &&out) mutable {
    if constexpr (spl::calculate_type_v<decltype(out)>) {
      return out(int(i));
    } else {
      if (i > 5) return false;
      out(i++);
      return true;
    }
  });

  return spl::apply(std::move(gen), spl::sum());
}

TEST(SplTest, GeneratorConstexpr) {
  static_assert(constexpr_generator_test() == 15);  // 1+2+3+4+5 = 15
  EXPECT_EQ(constexpr_generator_test(), 15);
}

TEST(SplTest, GeneratorMultipleIterations) {
  // Test that generator can be used to produce pairs
  auto gen = spl::generator([i = 0](auto &&out) mutable {
    if constexpr (spl::calculate_type_v<decltype(out)>) {
      return out(std::pair<int, int>(i, i * 10));
    } else {
      if (i >= 5) return false;
      out(std::make_pair(i, i * 10));
      i++;
      return true;
    }
  });

  auto result = spl::apply(std::move(gen),
                           spl::expand_tuple(),
                           spl::transform([](int k, int v) { return k + v; }),
                           spl::to_vector());

  EXPECT_THAT(result,
              ElementsAre(0,
                          11,
                          22,
                          33,
                          44));  // (0+0), (1+10), (2+20), (3+30), (4+40)
}

constexpr auto calculate_generator() {
  constexpr std::array v{1, 2, 3, 4};
  auto t = spl::compose(
      spl::filter([](int i) { return i != 2; }),
      spl::sum());
  using spl::SkydownSplMakeGenerator;
  return spl::apply(SkydownSplMakeGenerator(v),
                    spl::transform([](int i) { return i * 2; }),
                    std::move(t));

}

TEST(SplTest, ConstexprCalculateGenerator) {
  static_assert(calculate_generator() == 18);
  EXPECT_THAT(calculate_generator(), 18);

}

constexpr auto constexpr_zip_to_map_test() {
  constexpr std::array<int, 3> v1{{
                                      1,
                                      2,
                                      3
                                  }};

  constexpr std::array<int, 3> v2{{
                                      10,
                                      20,
                                      30
                                  }};

  auto m = spl::apply(v1, spl::zip(v2), spl::to_map<constexpr_map>());

  int sum = 0;
  for (auto &[k, v] : m) {
    sum += k + v;
  }
  return sum;
}

TEST(SplTest, ZipToMap) {
  static_assert(
      constexpr_to_map_test() == 66);  // (1+10)+(2+20)+(3+30) = 11+22+33 = 66
  EXPECT_EQ(constexpr_to_map_test(), 66);
}
// Additional Zip tests
TEST(SplTest, ZipBasic) {
  std::array<int, 4> v1{1, 2, 3, 4};
  std::array<int, 4> v2{10, 20, 30, 40};

  auto result = spl::apply(v1,
                          spl::zip(v2),
                          spl::make_pair(),
                          spl::to_vector());

  EXPECT_THAT(result, ElementsAre(
      Pair(1, 10),
      Pair(2, 20),
      Pair(3, 30),
      Pair(4, 40)
  ));
}

constexpr auto constexpr_zip_basic_test() {
  constexpr std::array<int, 3> v1{1, 2, 3};
  constexpr std::array<int, 3> v2{4, 5, 6};

  return spl::apply(v1,
                   spl::zip(v2),
                   spl::transform([](int a, int b) { return a + b; }),
                   spl::sum());
}

TEST(SplTest, ZipBasicConstexpr) {
  static_assert(constexpr_zip_basic_test() == 21);  // (1+4)+(2+5)+(3+6) = 5+7+9 = 21
  EXPECT_EQ(constexpr_zip_basic_test(), 21);
}

TEST(SplTest, ZipWithTransform) {
  std::array<int, 4> v1{1, 2, 3, 4};
  std::array<int, 4> v2{5, 6, 7, 8};

  auto result = spl::apply(v1,
                          spl::zip(v2),
                          spl::transform([](int a, int b) { return a * b; }),
                          spl::to_vector());

  EXPECT_THAT(result, ElementsAre(5, 12, 21, 32));  // 1*5, 2*6, 3*7, 4*8
}

TEST(SplTest, ZipWithFilter) {
  std::array<int, 5> v1{1, 2, 3, 4, 5};
  std::array<int, 5> v2{10, 20, 30, 40, 50};

  auto result = spl::apply(v1,
                          spl::zip(v2),
                          spl::filter([](int a, int b) { return (a + b) % 3 == 0; }),
                          spl::make_pair(),
                          spl::to_vector());

  EXPECT_THAT(result, ElementsAre(
      Pair(3, 30)  // 3+30=33, divisible by 3
  ));
}

TEST(SplTest, ZipMultipleRanges) {
  std::array<int, 3> v1{1, 2, 3};
  std::array<int, 3> v2{10, 20, 30};
  std::array<int, 3> v3{100, 200, 300};

  auto result = spl::apply(v1,
                          spl::zip(v2, v3),
                          spl::transform([](int a, int b, int c) { return a + b + c; }),
                          spl::to_vector());

  EXPECT_THAT(result, ElementsAre(111, 222, 333));  // 1+10+100, 2+20+200, 3+30+300
}

constexpr auto constexpr_zip_multiple_test() {
  constexpr std::array<int, 4> v1{1, 2, 3, 4};
  constexpr std::array<int, 4> v2{10, 10, 10, 10};
  constexpr std::array<int, 4> v3{100, 100, 100, 100};

  return spl::apply(v1,
                   spl::zip(v2, v3),
                   spl::transform([](int a, int b, int c) { return a + b + c; }),
                   spl::sum());
}

TEST(SplTest, ZipMultipleConstexpr) {
  static_assert(constexpr_zip_multiple_test() == 450);  // (1+10+100)*4 + (1+2+3) = 111*4 + 6 = 444 + 6 = 450
  EXPECT_EQ(constexpr_zip_multiple_test(), 450);
}

TEST(SplTest, ZipDifferentTypes) {
  std::array<int, 3> v1{1, 2, 3};
  std::array<std::string, 3> v2{"a", "b", "c"};

  auto result = spl::apply(v1,
                          spl::zip(v2),
                          spl::make_pair(),
                          spl::to_vector());

  using pair_type = std::pair<int, std::string>;
  EXPECT_THAT(result, ElementsAre(
      pair_type{1, "a"},
      pair_type{2, "b"},
      pair_type{3, "c"}
  ));
}

TEST(SplTest, ZipToTuple) {
  std::array<int, 3> v1{1, 2, 3};
  std::array<int, 3> v2{4, 5, 6};
  std::array<int, 3> v3{7, 8, 9};

  auto result = spl::apply(v1,
                          spl::zip(v2, v3),
                          spl::make_tuple(),
                          spl::to_vector());

  using tuple_type = std::tuple<int, int, int>;
  EXPECT_THAT(result, ElementsAre(
      tuple_type{1, 4, 7},
      tuple_type{2, 5, 8},
      tuple_type{3, 6, 9}
  ));
}

TEST(SplTest, ZipWithSum) {
  std::array<int, 5> v1{1, 2, 3, 4, 5};
  std::array<int, 5> v2{5, 4, 3, 2, 1};

  auto result = spl::apply(v1,
                          spl::zip(v2),
                          spl::transform([](int a, int b) { return a * b; }),
                          spl::sum());

  EXPECT_EQ(result, 35);  // 1*5 + 2*4 + 3*3 + 4*2 + 5*1 = 5+8+9+8+5 = 35
}

TEST(SplTest, ZipWithTake) {
  std::array<int, 10> v1{0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
  std::array<int, 10> v2{10, 11, 12, 13, 14, 15, 16, 17, 18, 19};

  auto result = spl::apply(v1,
                          spl::zip(v2),
                          spl::take(5),
                          spl::make_pair(),
                          spl::to_vector());

  EXPECT_THAT(result, ElementsAre(
      Pair(0, 10),
      Pair(1, 11),
      Pair(2, 12),
      Pair(3, 13),
      Pair(4, 14)
  ));
}

TEST(SplTest, ZipWithGroupBy) {
  std::array<int, 6> v1{1, 2, 3, 4, 5, 6};
  std::array<int, 6> v2{10, 20, 30, 40, 50, 60};

  auto result = spl::apply(v1,
                          spl::zip(v2),
                          spl::transform([](int a, int b) {
                            return std::make_pair(a % 2, b);  // Group by odd/even
                          }),
                          spl::group_by(&std::pair<int, int>::first,
                                       spl::transform(&std::pair<int, int>::second),
                                       spl::sum()),
                          spl::make_pair(),
                          spl::to_vector());

  EXPECT_THAT(result, UnorderedElementsAre(
      Pair(1, 90),   // Odd: 10+30+50=90
      Pair(0, 120)   // Even: 20+40+60=120
  ));
}


TEST(SplTest, ZipNested) {
  std::array<int, 3> v1{1, 2, 3};
  std::array<int, 3> v2{10, 20, 30};
  std::array<int, 3> v3{100, 200, 300};
  std::array<int, 3> v4{1000, 2000, 3000};

  auto result = spl::apply(v1,
                          spl::zip(v2),
                          spl::zip(v3),
                          spl::zip(v4),
                          spl::transform([](int a, int b, int c, int d) {
                            return a + b + c + d;
                          }),
                          spl::to_vector());

  EXPECT_THAT(result, ElementsAre(1111, 2222, 3333));
}

constexpr auto constexpr_zip_with_iota_test() {
  constexpr std::array<int, 5> v{10, 20, 30, 40, 50};

  return spl::apply(spl::iota(0, 5),
                   spl::zip(v),
                   spl::transform([](int idx, int val) { return idx * val; }),
                   spl::sum());
}

TEST(SplTest, ZipWithIota) {
  static_assert(constexpr_zip_with_iota_test() == 400);  // 0*10 + 1*20 + 2*30 + 3*40 + 4*50 = 0+20+60+120+200 = 400
  EXPECT_EQ(constexpr_zip_with_iota_test(), 400);
}

TEST(SplTest, ZipStringValues) {
  std::array<std::string, 3> v1{"hello", "world", "test"};
  std::array<int, 3> v2{1, 2, 3};

  auto result = spl::apply(v1,
                          spl::zip(v2),
                          spl::transform([](const std::string& s, int n) {
                            return s + std::to_string(n);
                          }),
                          spl::to_vector());

  EXPECT_THAT(result, ElementsAre("hello1", "world2", "test3"));
}

// chain_before and chain_after tests
TEST(SplTest, ChainBeforeBasic) {
  std::array<int, 3> v1{1, 2, 3};
  std::array<int, 2> v2{-1, 0};

  auto result = spl::apply(v1,
                          spl::chain_before(v2),
                          spl::to_vector());

  EXPECT_THAT(result, ElementsAre(-1, 0, 1, 2, 3));
}

TEST(SplTest, ChainAfterBasic) {
  std::array<int, 3> v1{1, 2, 3};
  std::array<int, 2> v2{4, 5};

  auto result = spl::apply(v1,
                          spl::chain_after(v2),
                          spl::to_vector());

  EXPECT_THAT(result, ElementsAre(1, 2, 3, 4, 5));
}

constexpr auto constexpr_chain_before_test() {
  constexpr std::array<int, 3> v1{10, 20, 30};
  constexpr std::array<int, 2> v2{1, 2};

  return spl::apply(v1,
                   spl::chain_before(v2),
                   spl::sum());
}

TEST(SplTest, ChainBeforeConstexpr) {
  static_assert(constexpr_chain_before_test() == 63);  // 1+2+10+20+30 = 63
  EXPECT_EQ(constexpr_chain_before_test(), 63);
}

constexpr auto constexpr_chain_after_test() {
  constexpr std::array<int, 3> v1{10, 20, 30};
  constexpr std::array<int, 2> v2{1, 2};

  return spl::apply(v1,
                   spl::chain_after(v2),
                   spl::sum());
}

TEST(SplTest, ChainAfterConstexpr) {
  static_assert(constexpr_chain_after_test() == 63);  // 10+20+30+1+2 = 63
  EXPECT_EQ(constexpr_chain_after_test(), 63);
}

TEST(SplTest, ChainBeforeWithTransform) {
  std::array<int, 3> v1{1, 2, 3};
  std::array<int, 2> v2{10, 20};

  auto result = spl::apply(v1,
                          spl::chain_before(v2),
                          spl::transform([](int x) { return x * 2; }),
                          spl::to_vector());

  EXPECT_THAT(result, ElementsAre(20, 40, 2, 4, 6));  // [10, 20, 1, 2, 3] * 2
}

TEST(SplTest, ChainAfterWithTransform) {
  std::array<int, 3> v1{1, 2, 3};
  std::array<int, 2> v2{10, 20};

  auto result = spl::apply(v1,
                          spl::chain_after(v2),
                          spl::transform([](int x) { return x * 2; }),
                          spl::to_vector());

  EXPECT_THAT(result, ElementsAre(2, 4, 6, 20, 40));  // [1, 2, 3, 10, 20] * 2
}

TEST(SplTest, ChainBeforeWithFilter) {
  std::array<int, 5> v1{1, 2, 3, 4, 5};
  std::array<int, 3> v2{0, 10, 20};

  auto result = spl::apply(v1,
                          spl::chain_before(v2),
                          spl::filter([](int x) { return x % 2 == 0; }),
                          spl::to_vector());

  EXPECT_THAT(result, ElementsAre(0, 10, 20, 2, 4));  // Even numbers from [0, 10, 20, 1, 2, 3, 4, 5]
}

TEST(SplTest, ChainAfterWithFilter) {
  std::array<int, 5> v1{1, 2, 3, 4, 5};
  std::array<int, 3> v2{6, 7, 8};

  auto result = spl::apply(v1,
                          spl::chain_after(v2),
                          spl::filter([](int x) { return x > 3; }),
                          spl::to_vector());

  EXPECT_THAT(result, ElementsAre(4, 5, 6, 7, 8));  // >3 from [1, 2, 3, 4, 5, 6, 7, 8]
}

TEST(SplTest, ChainBeforeEmpty) {
  std::array<int, 3> v1{1, 2, 3};
  std::array<int, 0> v2{};

  auto result = spl::apply(v1,
                          spl::chain_before(v2),
                          spl::to_vector());

  EXPECT_THAT(result, ElementsAre(1, 2, 3));  // Empty chain has no effect
}

TEST(SplTest, ChainAfterEmpty) {
  std::array<int, 3> v1{1, 2, 3};
  std::array<int, 0> v2{};

  auto result = spl::apply(v1,
                          spl::chain_after(v2),
                          spl::to_vector());

  EXPECT_THAT(result, ElementsAre(1, 2, 3));  // Empty chain has no effect
}

TEST(SplTest, ChainBeforeEmptySource) {
  std::array<int, 0> v1{};
  std::array<int, 3> v2{1, 2, 3};

  auto result = spl::apply(v1,
                          spl::chain_before(v2),
                          spl::to_vector());

  EXPECT_THAT(result, ElementsAre(1, 2, 3));  // Only chained items
}

TEST(SplTest, ChainAfterEmptySource) {
  std::array<int, 0> v1{};
  std::array<int, 3> v2{1, 2, 3};

  auto result = spl::apply(v1,
                          spl::chain_after(v2),
                          spl::to_vector());

  EXPECT_THAT(result, ElementsAre(1, 2, 3));  // Only chained items
}

TEST(SplTest, ChainBeforeAndAfter) {
  std::array<int, 3> v1{10, 20, 30};
  std::array<int, 2> v2{1, 2};
  std::array<int, 2> v3{100, 200};

  auto result = spl::apply(v1,
                          spl::chain_before(v2),
                          spl::chain_after(v3),
                          spl::to_vector());

  EXPECT_THAT(result, ElementsAre(1, 2, 10, 20, 30, 100, 200));
}

TEST(SplTest, ChainMultipleBefore) {
  std::array<int, 2> v1{10, 20};
  std::array<int, 2> v2{5, 6};
  std::array<int, 2> v3{1, 2};

  auto result = spl::apply(v1,
                          spl::chain_before(v2),
                          spl::chain_before(v3),
                          spl::to_vector());

  EXPECT_THAT(result, ElementsAre(1, 2, 5, 6, 10, 20));
}

TEST(SplTest, ChainMultipleAfter) {
  std::array<int, 2> v1{1, 2};
  std::array<int, 2> v2{10, 20};
  std::array<int, 2> v3{100, 200};

  auto result = spl::apply(v1,
                          spl::chain_after(v2),
                          spl::chain_after(v3),
                          spl::to_vector());

  EXPECT_THAT(result, ElementsAre(1, 2, 10, 20, 100, 200));
}

TEST(SplTest, ChainBeforeWithIota) {
  std::array<int, 2> v{100, 200};

  auto result = spl::apply(spl::iota(0, 5),
                          spl::chain_before(v),
                          spl::to_vector());

  EXPECT_THAT(result, ElementsAre(100, 200, 0, 1, 2, 3, 4));
}

TEST(SplTest, ChainAfterWithIota) {
  std::array<int, 2> v{100, 200};

  auto result = spl::apply(spl::iota(0, 5),
                          spl::chain_after(v),
                          spl::to_vector());

  EXPECT_THAT(result, ElementsAre(0, 1, 2, 3, 4, 100, 200));
}

TEST(SplTest, ChainBeforeWithTake) {
  std::array v1{1, 2, 3, 4, 5};
  std::array<int, 2> v2{-1, 0};

  auto result = spl::apply(v1,
                          spl::chain_before(v2),
                          spl::take(4),
                          spl::to_vector());

  EXPECT_THAT(result, ElementsAre(-1, 0, 1, 2));  // Takes first 4 from [-1, 0, 1, 2, 3, 4, 5]
}

TEST(SplTest, ChainAfterWithSum) {
  std::array<int, 3> v1{1, 2, 3};
  std::array<int, 2> v2{10, 20};

  auto result = spl::apply(v1,
                          spl::chain_after(v2),
                          spl::sum());

  EXPECT_EQ(result, 36);  // 1+2+3+10+20 = 36
}

TEST(SplTest, ChainBeforeVectors) {
  std::vector<int> v1{5, 6, 7};
  std::vector<int> v2{1, 2, 3};

  auto result = spl::apply(v1,
                          spl::chain_before(v2),
                          spl::to_vector());

  EXPECT_THAT(result, ElementsAre(1, 2, 3, 5, 6, 7));
}

TEST(SplTest, ChainAfterVectors) {
  std::vector<int> v1{1, 2, 3};
  std::vector<int> v2{10, 20, 30};

  auto result = spl::apply(v1,
                          spl::chain_after(v2),
                          spl::to_vector());

  EXPECT_THAT(result, ElementsAre(1, 2, 3, 10, 20, 30));
}

TEST(SplTest, ChainBeforeDifferentTypes) {
  std::array<std::string, 2> v1{"world", "!"};
  std::array<std::string, 2> v2{"Hello", " "};

  auto result = spl::apply(v1,
                          spl::chain_before(v2),
                          spl::to_vector());

  EXPECT_THAT(result, ElementsAre("Hello", " ", "world", "!"));
}

TEST(SplTest, ChainAfterWithGroupBy) {
  std::array<int, 4> v1{1, 2, 3, 4};
  std::array<int, 2> v2{5, 6};

  auto result = spl::apply(v1,
                          spl::chain_after(v2),
                          spl::transform([](int x) { return std::make_pair(x % 2, x); }),
                          spl::group_by(&std::pair<int, int>::first,
                                       spl::transform(&std::pair<int, int>::second),
                                       spl::sum()),
                          spl::make_pair(),
                          spl::to_vector());

  EXPECT_THAT(result, ElementsAre(
      Pair(0, 12),   // Even: 2+4+6 = 12
      Pair(1, 9)   // Odd: 1+3+5 = 9
  ));
}

constexpr auto constexpr_chain_combined_test() {
  constexpr std::array<int, 3> v1{10, 20, 30};
  constexpr std::array<int, 2> v2{1, 2};
  constexpr std::array<int, 2> v3{100, 200};

  return spl::apply(v1,
                   spl::chain_before(v2),
                   spl::chain_after(v3),
                   spl::transform([](int x) { return x / 10; }),
                   spl::sum());
}

TEST(SplTest, ChainBeforeAfterConstexpr) {
  static_assert(constexpr_chain_combined_test() == 36);  // (1+2+10+20+30+100+200)/10 = 363/10 = 36.3 -> 36
  EXPECT_EQ(constexpr_chain_combined_test(), 36);
}

TEST(SplTest, ChainBeforeWithZip) {
  std::array<int, 3> v1{1, 2, 3};
  std::array<int, 2> v2{0, -1};
  std::array<int, 5> v3{10, 20, 30, 40, 50};

  auto result = spl::apply(v1,
                          spl::chain_before(v2),
                          spl::zip(v3),
                          spl::transform([](int a, int b) { return a + b; }),
                          spl::to_vector());

  EXPECT_THAT(result, ElementsAre(10, 19, 31, 42, 53));  // [0, -1, 1, 2, 3] + [10, 20, 30, 40, 50]
}

TEST(SplTest, ChainAfterSingleElement) {
  std::array<int, 1> v1{42};
  std::array<int, 3> v2{1, 2, 3};

  auto result = spl::apply(v1,
                          spl::chain_after(v2),
                          spl::to_vector());

  EXPECT_THAT(result, ElementsAre(42, 1, 2, 3));
}
