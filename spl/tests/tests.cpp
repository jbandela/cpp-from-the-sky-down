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

TEST(SplTest, ConstexprCalculate) {
  static_assert(calculate() == 18);
  EXPECT_THAT(calculate(), 18);

}

TEST(SplTest, ConstexprCalculateTake) {
  static_assert(calculate_take() == 10);
  EXPECT_THAT(calculate_take(), 10);

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

TEST(SplTest, Iota) {
  auto result = spl::apply(spl::iota(3), spl::to_vector());
  EXPECT_THAT(result, ElementsAre(0, 1, 2));

}

}
