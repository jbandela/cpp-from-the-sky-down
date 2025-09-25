#ifndef SKYDOWN_SPL_STAGES_H_
#define SKYDOWN_SPL_STAGES_H_

#include "spl_core.h"

namespace spl{
template<typename StageProperties, typename F>
struct for_each_impl
    : stage_impl<for_each_impl<StageProperties, F>> {
  using base = typename for_each_impl::base;
  using typename base::input_type;
  using output_type = std::monostate &&;

  F f;

  constexpr void process_incremental(input_type input) {
    f(std::forward<input_type>(input));
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

template<typename StageProperties, typename T, typename F>
struct accumulate_in_place_impl
    : stage_impl<accumulate_in_place_impl<StageProperties, T, F>
    > {
  using base = typename accumulate_in_place_impl::base;
  using typename base::input_type;
  using accumulated_type = std::remove_cvref_t<apply_input_to_typename_t<T,
                                                                         input_type>>;
  using output_type = accumulated_type &&;
  [[no_unique_address]] F f{};
  [[no_unique_address]] accumulated_type accumulated{};

  constexpr decltype(auto) process_incremental(input_type input) {
    std::invoke(f, accumulated, this->forward(input));
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

template<typename StageProperties, typename F, typename OutputTypeCalculator>
struct flat_map_impl
    : stage_impl<flat_map_impl<StageProperties, F, OutputTypeCalculator>
    > {
  using base = typename flat_map_impl::base;
  using typename base::input_type;
  using output_type = typename OutputTypeCalculator::template type<input_type> &&;
  [[no_unique_address]] F f{};
  bool done_ = false;

  constexpr decltype(auto) process_incremental(input_type input) {
    auto out = [this](auto &&output) {
      this->next.process_incremental(std::forward<decltype(output)>(output));
    };
    done_ = !std::invoke(f, out, this->forward(input));
  }

  constexpr bool done() const {
    return done_;
  }

};

struct IdentityOutputCalculator {
  template<typename T>
  using type = T;
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
  return flat_map([predicate = std::move(predicate)](auto &output,
                                                     auto &&input) {
    if (std::invoke(predicate, std::as_const(input))) {
      output(std::forward<decltype(input)>(input));
    }
    return true;
  });
}

template<typename F>
struct TransformOutputCalculator {
  template<typename Input>
  using type = std::invoke_result_t<F, Input>;
};

template<typename F>
constexpr auto transform(F f) {
  return flat_map<TransformOutputCalculator<F>>([f = std::move(f)](auto &out,
                                                                   auto &&input) {
    out(std::invoke(f, std::forward<decltype(input)>(input)));
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

}

#endif //SKYDOWN_SPL_STAGES_H_
