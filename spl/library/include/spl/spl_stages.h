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
  using output_types = types<std::monostate &&>;

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
    : stage_impl<accumulate_in_place_impl<StageProperties,
                                          types<InputTypes...>,
                                          T,
                                          F>> {
  using base = typename accumulate_in_place_impl::base;
  // For single-argument case, use the first input type
  using first_input = std::tuple_element_t<0, std::tuple<InputTypes...>>;
  using accumulated_type = std::remove_cvref_t<apply_input_to_typename_t<T,
                                                                         first_input>>;
  static_assert(!std::is_same_v<accumulated_type, void>);
  using output_types = types<accumulated_type &&>;
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
struct flat_map_impl<StageProperties,
                     types<InputTypes...>,
                     F,
                     OutputTypeCalculator>
    : stage_impl<flat_map_impl<StageProperties,
                               types<InputTypes...>,
                               F,
                               OutputTypeCalculator>> {
  using base = typename flat_map_impl::base;
  using output_types = decltype(std::invoke(std::declval<F>(),
                                            type_calculating_outputter(),
                                            std::declval<InputTypes>()...));
  static_assert(is_types<output_types>::value);
  [[no_unique_address]] F f{};
  bool done_ = false;

  constexpr decltype(auto) process_incremental(InputTypes... inputs) {
    done_ = !std::invoke(f,
                         incremental_outputter{this->next},
                         static_cast<InputTypes>(inputs)...);
  }

  constexpr bool done() const {
    return done_ || this->next.done();
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
  return flat_map([predicate =
  std::move(predicate)]<typename Output, typename... Inputs>(Output &&output,
                                                             Inputs &&... inputs) {
    if constexpr (calculate_type_v<Output>) {

      return output(std::forward<Inputs>(inputs)...);

    } else {
      if (std::invoke(predicate, std::as_const(inputs)...)) {
        output(std::forward<Inputs>(inputs)...);
      }
      return true;
    }
  });
}

constexpr auto take(size_t
                    n) {
  return flat_map([
                      i = size_t(0), n
                  ]<typename Out>(
      Out &&output,
      auto &&...inputs
  )mutable {

    if constexpr (calculate_type_v<Out>) {
      return output(std::forward<decltype(inputs)>(inputs)...
      );

    } else {
      if (i++ >= n) return false;
      else {
        output(std::forward<decltype(inputs)>(inputs)...
        );
        return true;

      }
    }
  });
}

struct iota_struct_base {};
template<typename T>
struct iota_struct : iota_struct_base {
  T end;
  T current;
};

template<typename Output, typename T>
requires(std::is_base_of_v<iota_struct_base, std::decay_t<T>>)
constexpr auto SkydownSplOutput(Output &&output, T &&is) {
  if constexpr (calculate_type_v<Output>) {
    return output(size_t(is.current));
  } else {
    for (; is.current < is.end; ++is.current) {
      if (!output) return false;
      output(size_t(is.current));
    }
    return true;
  }
}

inline constexpr auto iota(size_t start) {
  return iota_struct<size_t>{{}, std::numeric_limits<size_t>::max(), start};
}

inline constexpr auto iota(size_t start, size_t end) {
  return iota_struct<size_t>{{}, end, start};
}

template<typename F>
constexpr auto transform_cps(F f) {
  return flat_map([f = std::move(f)]<typename Out>(Out &&out,
                                                   auto &&...inputs) {

    auto invoke = [&] {
      return std::invoke(f,
                         std::forward<decltype(out)>(out),
                         std::forward<decltype(inputs)>(inputs)...);
    };
    if constexpr (calculate_type_v<Out>) {

      return invoke();

    } else {
      invoke();
      return true;
    }
  });
}

template<typename F>
constexpr auto transform(F f) {
  return transform_cps([f = std::move(f)](auto &&out, auto &&... inputs) {
    return out(std::invoke(f, std::forward<decltype(inputs)>(inputs)...));
  });
}

inline constexpr auto expand_tuple() {
  return transform_cps([&](auto &&out, auto &&tuple) {
    return std::apply(out, std::forward<decltype(tuple)>(tuple));
  });
}

inline constexpr auto make_tuple() {
  return transform_cps([&](auto &&out, auto &&... ts) {
    return out(std::make_tuple(std::forward<decltype(ts)>(ts)...));
  });
}
inline constexpr auto make_pair() {
  return transform_cps([&](auto &&out, auto &&... ts) {
    return out(std::make_pair(std::forward<decltype(ts)>(ts)...));
  });
}

template<typename F>
constexpr auto zip_result(F f) {
  return transform_cps([f = std::move(f)](auto &&out, auto &&... inputs) {
    return out(std::forward<decltype(inputs)>(inputs)...,
               std::invoke(f, std::as_const(inputs)...));
  });
}

template<std::size_t... Indices>
constexpr auto swizzle() {
  return transform_cps([](auto &&out, auto &&... inputs) {
    auto tuple =
        std::forward_as_tuple(std::forward<decltype(inputs)>(inputs)...);
    return out(std::get<Indices>(std::move(tuple))...);
  });
}

constexpr auto flatten() {
  return flat_map([]<typename Out>(Out &&out, auto &&...inputs) {
    return invoke_with_last_first(
        [&](auto &&last, auto &&... inputs) {
          return SkydownSplOutput(as_outputter(out,
                                               [&](auto &&out, auto &&item) {
                                                 return out(std::forward<
                                                                decltype(inputs)>(inputs)...,
                                                            std::forward<
                                                                decltype(item)>(
                                                                item));
                                               }),
                                  std::forward<decltype(last)>(last));
        },
        std::forward<decltype(inputs)>(inputs)...);
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
struct group_by_impl<StageProperties,
                     types<InputTypes...>,
                     SelectorF,
                     Composed,
                     MapType>
    : stage_impl<group_by_impl<StageProperties,
                               types<InputTypes...>,
                               SelectorF,
                               Composed,
                               MapType>> {
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
                                           pipeline_type>().finish())>> &&>;

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
constexpr auto group_by(SelectorF selector_f, Stages... stages) {
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
