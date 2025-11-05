#ifndef SKYDOWN_SPL_STAGES_H_
#define SKYDOWN_SPL_STAGES_H_

#include "spl_core.h"
#include <map>

namespace spl {

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
  using accumulated_type = std::remove_cvref_t<first_type_t<std::invoke_result_t<T,types<InputTypes...>>>>;
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
  constexpr auto type_calculator = [](auto){return types<T>();};
  return stage<accumulate_in_place_impl,
               processing_style::incremental,
               processing_style::complete,
               decltype(type_calculator),
               F>(
      std::move(f), std::move(t));
}

template<typename TypeCalculator, typename F>
constexpr auto accumulate_in_place_with_type_calculator(TypeCalculator t, F f) {
  return stage<accumulate_in_place_impl,
               processing_style::incremental,
               processing_style::complete,
               TypeCalculator,
               F>(std::move(f));
}

template<typename T, typename F>
requires(std::is_invocable_v<T,types<int&&>>)
constexpr auto accumulate_in_place(T t, F f) {
  return accumulate_in_place_with_type_calculator(std::move(t), std::move(f));
}

template<typename T, typename F>
constexpr auto accumulate(T t, F f) {
  return accumulate_in_place(std::forward<T>(t),
                             [f = std::move(f)](auto &accumulated,
                                                auto &&v) {
                               accumulated =
                                   std::invoke(f,
                                               accumulated, std::forward<decltype(v)>(
                                                   v));
                             });
}

template<typename F>
constexpr auto accumulate(F f) {
  return accumulate_in_place([]<typename... Types>(types<Types...>){
    static_assert(sizeof...(Types) == 1, "Accumulate needs to only have one input");
    return types<std::remove_cvref_t<Types>...>();},
                             [f = std::move(f)](auto &accumulated,
                                                auto &&v) {
                               accumulated =
                                   std::invoke(f,
                                               accumulated, std::forward<decltype(v)>(
                                                   v));
                             });
}


template<typename F>
constexpr auto for_each(F f) {
  return accumulate_in_place(std::monostate(),[f=std::move(f)](auto&, auto&&... ts){
    std::invoke(f,std::forward<decltype(ts)>(ts)...);
  });
}
constexpr auto sum() {
  return accumulate(std::plus<>{});
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

// Forward declaration
template<typename StageProperties, typename InputTypes, typename F>
struct transform_complete_cps_impl;

// Partial specialization to extract types from types<...>
template<typename StageProperties, typename... InputTypes, typename F>
struct transform_complete_cps_impl<StageProperties, types<InputTypes...>, F>
    : stage_impl<transform_complete_cps_impl<StageProperties, types<InputTypes...>, F>> {
  using base = typename transform_complete_cps_impl::base;

  using output_types = decltype(std::invoke(
      std::declval<F>(),
      type_calculating_outputter(),
      std::declval<InputTypes>()...));

  static_assert(is_types<output_types>::value);

  [[no_unique_address]] F f{};

  constexpr decltype(auto) process_complete(InputTypes... inputs) {
    return std::invoke(f,
                      [this](auto&&... outputs) -> decltype(auto) {
                        return this->next.process_complete(std::forward<decltype(outputs)>(outputs)...);
                      },
                      std::forward<InputTypes>(inputs)...);
  }
};

template<typename F>
constexpr auto transform_complete_cps(F f) {
  return stage<transform_complete_cps_impl,
               processing_style::complete,
               processing_style::complete,
               F>(std::move(f));
}

template<typename F>
constexpr auto transform_complete(F f) {
  return transform_complete_cps([f = std::move(f)](auto &&out, auto &&... inputs) {
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

template<template<typename...> typename C>
constexpr auto push_back_to(){
  return accumulate_in_place([]<typename... Inputs>(types<Inputs...>){
    static_assert(sizeof...(Inputs) == 1,"push_back_to can only take 1 input argument");
    return types<C<std::remove_cvref_t<first_type_t<types<Inputs...>>>>>();
  },[](auto &c, auto &&v) {
    c.push_back(std::forward<decltype(v)>(v));
  });

}

constexpr auto to_vector() {
  return push_back_to<std::vector>();
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

// Forward declaration
template<typename StageProperties, typename InputTypes, typename... Composed>
struct tee_impl;

// Partial specialization to extract types from types<...>
template<typename StageProperties, typename... InputTypes, typename... Composed>
struct tee_impl<StageProperties, types<InputTypes...>, Composed...>
    : stage_impl<tee_impl<StageProperties, types<InputTypes...>, Composed...>> {
  using base = typename tee_impl::base;

  template<typename C>
  using pipeline_type = decltype(spl::make_pipeline<types<InputTypes...>,
                                                    processing_style::incremental>(
      std::declval<C>()));

  using output_types = types<std::tuple<std::remove_cvref_t<decltype(std::declval<
      pipeline_type<Composed>>().finish())>...> &&>;

  std::tuple<pipeline_type<Composed>...> pipelines;

  constexpr tee_impl(typename base::next_type&& next, Composed... composed_stages)
      : base(std::move(next)),
        pipelines(make_pipeline_helper(composed_stages)...) {
  }

  constexpr void process_incremental(InputTypes... inputs) {
    std::apply([&](auto&... pipes) {
      (pipes.process_incremental(static_cast<InputTypes>(inputs)...), ...);
    }, pipelines);
  }

  constexpr decltype(auto) finish() {
    return this->next.process_complete(std::apply([](auto&&... pipes)  {
      return std::make_tuple(pipes.finish()...);
    }, pipelines));
  }

private:
  template<typename C>
  static constexpr auto make_pipeline_helper(C c) {
    return spl::make_pipeline<types<InputTypes...>, processing_style::incremental>(
        c);
  }
};

template<typename... Stages>
constexpr auto tee_helper(Stages... stages) {
  return stage<tee_impl,
               processing_style::incremental,
               processing_style::complete,
               std::remove_cvref_t<Stages>...>(std::move(stages)...);
}

template<typename... Stages>
constexpr auto tee(Stages&&... stages) {
  return tee_helper((std::forward<Stages>(stages))...);
}

}

#endif //SKYDOWN_SPL_STAGES_H_
