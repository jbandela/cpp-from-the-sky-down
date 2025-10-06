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
  auto double_i =   spl::transform([](int i) { return i * 2; });
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


constexpr auto constexpr_iota_test(){

  return spl::apply(spl::iota(10), spl::sum());

}

TEST(SplTest, Iota) {
  auto result = constexpr_iota_test();
  static_assert(constexpr_iota_test() == 45);
  EXPECT_THAT(result, 45);

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
  constexpr std::array<std::array<int, 3>, 3> v{{{1, 2, 3}, {4, 5, 6}, {7, 8, 9}}};

  auto result = spl::apply(v,
                           spl::flatten(),
                           spl::take(5),
                           spl::to_vector());

  EXPECT_THAT(result, ElementsAre(1, 2, 3, 4, 5));
}


}
