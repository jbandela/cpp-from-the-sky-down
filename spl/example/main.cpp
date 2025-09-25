#include <iostream>
#include <vector>

#include <spl/spl.h>


constexpr auto calculate() {
  constexpr std::array v{1, 2, 3, 4};
  auto t = spl::compose(
      spl::transform([](int i) { return i * 2; }),
      spl::filter([](int i) { return i != 2; }),
      spl::sum());
  return spl::apply(v,
                    spl::filter([](auto &&) { return true; }),
                    std::move(t));

}

int main() {
  std::vector<int> v{1, 2, 3, 4};
  spl::apply(v,
             spl::filter([](auto &&i) {
               return i != 2;
             }),
             spl::to_vector(),
             spl::for_each([](int i) {
               std::cout << i << "\n";
             }));
  static_assert(calculate() == 18);
  std::cout << "calculate:" << calculate() << std::endl;
  static_assert(std::same_as<decltype(calculate()), int>);

  auto v2 = spl::apply(v,
                       spl::filter([](auto &&i) {
                         return i != 2;
                       }),
                       spl::to_vector());

  std::cout << "Hello, World!" << std::endl;
  return 0;
}
