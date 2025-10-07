#include <gmock/gmock.h>
#include <spl/spl.h> // Include your library's header

namespace {

using ::testing::Pair;
using ::testing::ElementsAre;

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

TEST(SplTest, GroupBy) {
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

TEST(SplTest, FlattenWithTransform) {
  constexpr std::array<std::array<int, 2>, 3> v{{{1, 2}, {3, 4}, {5, 6}}};

  auto result = spl::apply(v,
                           spl::flatten(),
                           spl::transform([](int i) { return i * 2; }),
                           spl::to_vector());

  EXPECT_THAT(result, ElementsAre(2, 4, 6, 8, 10, 12));
}

TEST(SplTest, FlattenWithTake) {
  constexpr std::array<std::array<int, 3>, 3>
      v{{{1, 2, 3}, {4, 5, 6}, {7, 8, 9}}};

  auto result = spl::apply(v,
                           spl::flatten(),
                           spl::take(5),
                           spl::to_vector());

  EXPECT_THAT(result, ElementsAre(1, 2, 3, 4, 5));
}
#if 1
TEST(SplTest, FlattenMultipleArgs) {
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

TEST(SplTest, ZipResultToVector) {
  std::array v{1, 2, 3};
  auto result = spl::apply(v,
                           spl::zip_result([](int i) { return i * i; }),
                           spl::make_pair(),
                           spl::to_vector());

  EXPECT_THAT(result, ElementsAre(Pair(1, 1), Pair(2, 4), Pair(3, 9)));
}

TEST(SplTest, ZipResultMultipleArgs) {
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

TEST(SplTest, SwizzleReorder) {
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

TEST(SplTest, SwizzleFilter) {
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

TEST(SplTest, SwizzleDuplicate) {
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

TEST(SplTest, SwizzleSingle) {
  std::array<std::tuple<int, int, int>, 3> v{{
      {1, 2, 3},
      {4, 5, 6},
      {7, 8, 9}
  }};

  auto result = spl::apply(v,
                           spl::expand_tuple(),
                           spl::swizzle<1>(),
                           spl::sum());

  EXPECT_THAT(result, 15); // 2 + 5 + 8
}

}
