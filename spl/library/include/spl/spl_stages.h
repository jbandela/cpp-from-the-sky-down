#ifndef SKYDOWN_SPL_STAGES_H_
#define SKYDOWN_SPL_STAGES_H_

#include "spl_core.h"
#include <map>

namespace spl {
// Forward declarations
template<typename StageProperties, typename InputTypes, typename F>
struct for_each_impl;

// Partial specialization to extract types from types<...>
template<typename StageProperties, typename... InputTypes, typename F>
struct for_each_impl<StageProperties, types<InputTypes...>, F>
    : stage_impl<for_each_impl<StageProperties, types<InputTypes...>, F>> {
  using base = typename for_each_impl::base;
  using output_types = types<std::monostate&&>;

  F f;

  constexpr void process_incremental(InputTypes... inputs) {
    f(std::forward<InputTypes>(inputs)...);
  }

  constexpr decltype(auto) finish() {
    return this->next.process_complete(std::monostate{});
  }
};

template<typename F>
constexpr auto for_each(F f) {
  return stage<for_each_impl,
               processing_style::incremental,
               processing_style::complete,
               F>(std::move(f));
}

template<template<typename...> typename T>
struct template_to_typename {
};

template<typename T, typename>
struct apply_input_to_typename {
  using type = T;
};

template<template<typename...> typename T, typename Input>
struct apply_input_to_typename<template_to_typename<T>, Input> {
  using type = T<Input>;
};

template<typename T, typename Input>
using apply_input_to_typename_t = typename apply_input_to_typename<T,
                                                                   Input>::type;

// Forward declaration
template<typename StageProperties, typename InputTypes, typename T, typename F>
struct accumulate_in_place_impl;

// Partial specialization to extract types from types<...>
template<typename StageProperties, typename... InputTypes, typename T, typename F>
struct accumulate_in_place_impl<StageProperties, types<InputTypes...>, T, F>
    : stage_impl<accumulate_in_place_impl<StageProperties, types<InputTypes...>, T, F>> {
  using base = typename accumulate_in_place_impl::base;
  // For single-argument case, use the first input type
  using first_input = std::tuple_element_t<0, std::tuple<InputTypes...>>;
  using accumulated_type = std::remove_cvref_t<apply_input_to_typename_t<T, first_input>>;
  static_assert(!std::is_same_v<accumulated_type,void>);
  using output_types = types<accumulated_type&&>;
  [[no_unique_address]] F f{};
  [[no_unique_address]] accumulated_type accumulated{};

  constexpr decltype(auto) process_incremental(InputTypes... inputs) {
    std::invoke(f, accumulated, std::forward<InputTypes>(inputs)...);
  }

  constexpr decltype(auto) finish() {
    return this->next.process_complete(std::move(accumulated));
  }

};

template<typename T, typename F>
constexpr auto accumulate_in_place(T t, F f) {
  return stage<accumulate_in_place_impl,
               processing_style::incremental,
               processing_style::complete,
               T,
               F>(
      std::move(f), std::move(t));
}

template<template<typename...> typename T, typename F>
constexpr auto accumulate_in_place(F f) {
  return stage<accumulate_in_place_impl,
               processing_style::incremental,
               processing_style::complete,
               template_to_typename<T>,
               F>(std::move(f));
}

template<typename T, typename F>
constexpr auto accumulate(T t, F f) {
  return accumulate_in_place(std::forward<T>(t),
                             [f = std::move(f)](auto &accumulated,
                                                auto &&v) {
                               accumulated =
                                   std::invoke(f,
                                               std::forward<decltype(v)>(
                                                   v));
                             });
}

template<template<typename> typename T, typename F>
constexpr auto accumulate(F f) {
  return accumulate_in_place<T>([f = std::move(f)](auto &accumulated,
                                                   auto &&v) {
    accumulated = std::invoke(f, accumulated,
                              std::forward<decltype(v)>(v));
  });
}

constexpr auto sum() {
  return accumulate<std::type_identity_t>(std::plus<>{});
}

// Forward declaration
template<typename StageProperties, typename InputTypes, typename F, typename OutputTypeCalculator>
struct flat_map_impl;

// Partial specialization to extract types from types<...>
template<typename StageProperties, typename... InputTypes, typename F, typename OutputTypeCalculator>
struct flat_map_impl<StageProperties, types<InputTypes...>, F, OutputTypeCalculator>
    : stage_impl<flat_map_impl<StageProperties, types<InputTypes...>, F, OutputTypeCalculator>> {
  using base = typename flat_map_impl::base;
  using output_types = typename OutputTypeCalculator::template output_types<InputTypes...>;
  [[no_unique_address]] F f{};
  bool done_ = false;

  constexpr decltype(auto) process_incremental(InputTypes... inputs) {
    done_ = !std::invoke(f,
                         incremental_outputter{this->next},
                         static_cast<InputTypes>(inputs)...);
  }

  constexpr bool done() const {
    return done_;
  }

};

struct IdentityOutputCalculator {
  template<typename... Ts>
  using output_types = types<Ts...>;
};

template<typename OutputCalculator = IdentityOutputCalculator, typename F>
constexpr auto flat_map(F f) {
  return stage<flat_map_impl,
               processing_style::incremental,
               processing_style::incremental,
               F,
               OutputCalculator>(std::move(f));

}

template<typename Predicate>
constexpr auto filter(Predicate predicate) {
  return flat_map([predicate = std::move(predicate)](auto &&output,
                                                     auto &&input) {
    if (std::invoke(predicate, std::as_const(input))) {
      output(std::forward<decltype(input)>(input));
    }
    return true;
  });
}

constexpr auto take(size_t
n) {
return flat_map([
i = size_t(0), n
](
auto &&output,
auto &&input
)mutable {
if (i++ >= n) return false;
else {
output(std::forward<decltype(input)>(input)
);
return true;

}
});
}

struct iota_struct {
  size_t n;
  size_t i = 0;
  using output_types = types<size_t&&>;
};

template<typename Output>
constexpr bool SkydownSplOutput(iota_struct &is, Output &&output) {
  for (; is.i < is.n; ++is.i) {
    if (!output) return false;
    output(size_t(is.i));
  }
  return true;
}

template<typename Output>
constexpr bool SkydownSplOutput(iota_struct &&is, Output &&output) {
  return SkydownSplOutput(is, output);
}

inline constexpr auto iota(size_t n) {
  return iota_struct{n};
}

template<typename F>
struct TransformOutputCalculator {
  template<typename... Input>
  using output_types = types<std::invoke_result_t<F, Input...>&&>;
};

template<typename F>
constexpr auto transform(F f) {
  return flat_map<TransformOutputCalculator<F>>([f = std::move(f)](auto &&out,
                                                                   auto &&input) {
    out(std::invoke(f, std::forward<decltype(input)>(input)));
    return true;
  });
}

template<typename T>
struct FlattenOutputCalculator {
  template<typename Input>
  using output_types = adapt_input_t<types<Input>, processing_style::complete, processing_style::incremental>;
};

template<typename OutputCalculator = FlattenOutputCalculator<void>>
constexpr auto flatten() {
  return flat_map<OutputCalculator>([](auto &&out, auto &&input) {
     SkydownSplOutput(std::forward<decltype(input)>(input), out);
     return true;
  });
}

template<typename T>
using vector_impl = std::vector<std::remove_cvref_t<T>>;

constexpr auto to_vector() {

  return accumulate_in_place<vector_impl>([](auto &c, auto &&v) {
    c.push_back(std::forward<decltype(v)>(v));
  });
}

namespace detail {

struct std_map {
  template<typename K, typename V>
  using type = std::map<K, V>;
};
}

// Forward declaration
template<typename StageProperties, typename InputTypes, typename SelectorF, typename Composed, typename MapType = detail::std_map>
struct group_by_impl;

// Partial specialization to extract types from types<...>
template<typename StageProperties, typename... InputTypes, typename SelectorF, typename Composed, typename MapType>
struct group_by_impl<StageProperties, types<InputTypes...>, SelectorF, Composed, MapType>
    : stage_impl<group_by_impl<StageProperties, types<InputTypes...>, SelectorF, Composed, MapType>> {
  using base = typename group_by_impl::base;
  // For single-argument case, use the first input type
  using first_input = std::tuple_element_t<0, std::tuple<InputTypes...>>;
  using key = std::remove_cvref_t<std::invoke_result_t<SelectorF, first_input>>;

  SelectorF selector_f;
  Composed composed;

  using pipeline_type = decltype(spl::make_pipeline<types<InputTypes...>,
                                                    processing_style::incremental>(
      std::declval<Composed>()));

  using output_types = types<std::pair<key,
                                std::remove_cvref_t<decltype(std::declval<
                                    pipeline_type>().finish())>>&&>;

  typename MapType::template type<key, pipeline_type> map;

  constexpr decltype(auto) process_incremental(InputTypes... inputs) {
    auto copy = [](auto &&t) { return t; };
    auto k = std::invoke(selector_f, std::forward<InputTypes>(inputs)...);
    auto iter = map.find(k);
    if (iter == map.end()) {
      bool b;
      std::tie(iter, b) = map.emplace(std::move(k),
                                      spl::make_pipeline<types<InputTypes...>,
                                                         processing_style::incremental>(
                                          copy(composed)));
    }
    iter->second.process_incremental(std::forward<InputTypes>(inputs)...);
  }

  constexpr decltype(auto) finish() {
    for (auto &&[k, v] : map) {
      this->next.process_incremental(std::make_pair(k, v.finish()));
    }
    return this->next.finish();
  }

};

template<typename MapType = detail::std_map, typename SelectorF, typename... Stages>
auto group_by(SelectorF selector_f, Stages... stages) {
  using C = decltype(compose(std::move(stages)...));
  return stage<group_by_impl,
               processing_style::incremental,
               processing_style::incremental,
               SelectorF,
               C,
               MapType>(std::move(selector_f), compose(std::move(stages)...));
}

}

#endif //SKYDOWN_SPL_STAGES_H_
