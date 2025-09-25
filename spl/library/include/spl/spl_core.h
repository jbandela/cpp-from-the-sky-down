#ifndef SKYDOWN_SPL_CORE_H_
#define SKYDOWN_SPL_CORE_H_

#include <iostream>
#include <type_traits>
#include <functional>
#include <tuple>
#include <utility>

namespace spl {

enum class processing_style {
  incremental,
  complete
};

template<typename Range, typename... Stages>
[[nodiscard]] constexpr auto apply(Range &&range, Stages &&... stages);

template<typename Child>
struct stage_impl;

template<template<typename...> typename Derived, typename StageProperties, typename... Parameters>
struct stage_impl<Derived<StageProperties, Parameters...>>;

template<typename Stage>
concept incremental_input = Stage::input_processing_style
    == processing_style::incremental;

template<typename Stage>
concept complete_input = Stage::input_processing_style
    == processing_style::complete;

template<typename Stage>
concept incremental_output = Stage::output_processing_style
    == processing_style::incremental;

template<typename Stage>
concept complete_output = Stage::output_processing_style
    == processing_style::complete;

template<typename Input, typename Next, processing_style PreviousOutputProcessingStyle,
    processing_style InputProcessingStyle, processing_style OutputProcessingStyle>
struct stage_properties {
  using input_type = Input;
  using next_type = Next;
  static constexpr auto
      previous_output_processing_style = PreviousOutputProcessingStyle;
  static constexpr auto input_processing_style = InputProcessingStyle;
  static constexpr auto output_processing_style = OutputProcessingStyle;
};

// Handle complete -> incremental conversions
template<typename Input, processing_style PreviousOutputProcessingStyle, processing_style InputProcessingStyle>
struct adapt_input;

template<typename Input, processing_style PreviousOutputProcessingStyle, processing_style InputProcessingStyle> requires (
PreviousOutputProcessingStyle == InputProcessingStyle)
struct adapt_input<Input, PreviousOutputProcessingStyle, InputProcessingStyle> {
  using type = Input;
};

template<typename Input>
struct adapt_input<Input,
                   processing_style::complete,
                   processing_style::incremental> {
  using type = std::ranges::range_reference_t<std::remove_reference_t<Input>>;
};

template<typename Input, processing_style PreviousOutputProcessingStyle, processing_style InputProcessingStyle>
using adapt_input_t = typename adapt_input<Input,
                                           PreviousOutputProcessingStyle,
                                           InputProcessingStyle>::type;

template<template<typename...> typename Derived, typename StageProperties, typename... Parameters>
struct stage_impl<Derived<StageProperties, Parameters...>> {

  using child_type = Derived<StageProperties, Parameters...>;
  using next_type = typename StageProperties::next_type;
  [[no_unique_address]] next_type next;

  using raw_input_type = typename StageProperties::input_type;
  using base = stage_impl;

  constexpr static auto previous_output_processing_style =
      StageProperties::previous_output_processing_style;
  constexpr static auto
      input_processing_style = StageProperties::input_processing_style;
  constexpr static auto
      output_processing_style = StageProperties::output_processing_style;

  using input_type = adapt_input_t<raw_input_type,
                                   previous_output_processing_style,
                                   input_processing_style>;

  constexpr stage_impl(next_type &&next)
      : next(std::move(next)) {}

  constexpr decltype(auto) finish()requires(incremental_input<
      child_type>) {
    return next.finish();
  }

  constexpr bool done() const requires(incremental_input<
      child_type>) {
    return next.done();
  }

  constexpr void
  process_incremental(input_type input)requires(incremental_input<
      child_type>) {
    return next.process_incremental(static_cast<input_type>(input));
  }

  constexpr decltype(auto)
  process_complete(input_type input)requires(complete_input<
      child_type>) {
    return next.process_complete(static_cast<input_type>(input));

  }

  constexpr decltype(auto)
  process_complete(raw_input_type input)requires (
  previous_output_processing_style == processing_style::complete
      && incremental_input<
          child_type>) {
    for (auto &&v : static_cast<raw_input_type>(input)) {
      static_assert(std::is_same_v<decltype(v), input_type>);
      auto &child = static_cast<child_type &>(*this);
      if (child.done()) break;
      child.process_incremental(
          std::forward<decltype(v)>(v));
    }

    return static_cast<child_type &>(*this).finish();

  }

  // Utilities
  constexpr input_type forward(auto &&input) {
    return static_cast<input_type>(input);
  }

};

template<typename T>
struct instantiable {
  using type = T;
};

template<>
struct instantiable<void> {
  using type = std::monostate;
};

template<typename T>
using instantiable_t = typename instantiable<T>::type;

template<template<typename, typename...> typename StageImpl, processing_style InputProcessingStyle,
    processing_style OutputProcessingStyle,
    typename Parameters = std::tuple<>, typename... TypeParameters>
class stage_instantiator {
  [[no_unique_address]] instantiable_t<Parameters> parameters_;

 public:
  static constexpr auto input_processing_style = InputProcessingStyle;
  static constexpr auto output_processing_style = OutputProcessingStyle;
  template<processing_style PreviousOutputProcessingStyle, typename Input>
  using output_type = typename StageImpl<stage_properties<Input,
                                                          std::monostate,
                                                          PreviousOutputProcessingStyle,
                                                          input_processing_style,
                                                          output_processing_style>,
                                         TypeParameters...>::output_type;

  template<typename... Ts>
  constexpr explicit stage_instantiator(Ts &&... ts)
      :parameters_{std::forward<Ts>(ts)...} {}

  template<typename StageProperties, typename Next>
  constexpr auto make(Next &&next) {
    return std::apply([&](auto &&... ts) {
      return StageImpl<StageProperties, TypeParameters...>{
          std::forward<Next>(next),
          std::forward<decltype(ts)>(ts)...};
    }, std::move(parameters_));
  }
};

template<template<typename, typename...> typename StageImpl, processing_style InputProcessingStyle,
    processing_style OutputProcessingStyle,
    typename... TypeParameters, typename... Ts>
constexpr auto stage(Ts &&... ts) {
  return stage_instantiator<StageImpl,
                            InputProcessingStyle,
                            OutputProcessingStyle,
                            std::tuple<std::remove_cvref_t<Ts>...>,
                            TypeParameters...>(std::forward<
      decltype(ts)>(ts)...);
}

template<typename F>
struct composed {
  [[no_unique_address]] F f;

  template<typename... Ts>
  constexpr auto operator()(Ts &&... ts) {
    return f(std::forward<Ts>(ts)...);
  }

};

template<typename F>
composed(F) -> composed<F>;

template<typename... Ts>
constexpr auto compose(Ts &&... ts) {
  return composed{[...args = std::forward<Ts>(ts)](auto&& prev, auto&&... next)constexpr mutable{
    if constexpr (sizeof...(next) == 1) {
      return (std::forward<decltype(prev)>(prev) + ... + std::move(
          args)).make(
          std::forward<decltype(next)>(next)...);
    } else {
      return (std::forward<decltype(prev)>(prev) + ... + std::move(
          args));
    }
  }};
}

namespace detail {

struct end_stage {
  template<typename T>
  constexpr decltype(auto) process_complete(T &&t) {
    return std::forward<T>(t);
  }

  constexpr bool done() const { return false; }

  constexpr void process_complete(std::monostate) {}
};

template<typename Previous>
struct end_factory {
  Previous &previous;

  template<typename Input, typename Next>
  constexpr auto make(Next &&next) {
    return previous.make(std::forward<Next>(next));
  }
};

struct end_factory_tag {
};

struct starting_previous {
  template<typename Next>
  constexpr auto make(Next &&next) {
    return std::forward<Next>(next);
  }
};

template<typename Input>
struct starting_factory {
  template<auto, typename>
  using output_type = Input;
  static constexpr auto input_processing_style = processing_style::complete;
  static constexpr auto output_processing_style = processing_style::complete;

  template<typename, typename Next>
  constexpr auto make(Next &&next) {
    return std::forward<Next>(next);
  }
};

template<typename Factory, typename Composed>
struct composed_factory {
  template<processing_style, typename>
  using output_type = typename Factory::output_type;
  static constexpr auto
      output_processing_style = Factory::output_processing_style;
  static constexpr auto
      input_processing_style = Factory::input_processing_style;
  Composed &composed;

  template<typename... Ts>
  constexpr auto operator()(Ts &&... ts) {
    return composed(std::forward<Ts>(ts)...);
  }

};

template<typename F>
struct factory_holder_type {
  using type = F &;
};

template<typename Factory, typename Composed>
struct factory_holder_type<composed_factory<Factory, Composed>> {
  using type = composed_factory<Factory, Composed>;
};

template<typename T>
using factory_holder_t = typename factory_holder_type<T>::type;

template<typename Input, processing_style PreviousOutputProcessingStyle, typename Factory = starting_factory<
    Input>, typename Previous = starting_previous>
struct input_factory {
  static constexpr auto
      previous_output_processing_style = PreviousOutputProcessingStyle;
  using output_type = typename Factory::template output_type<
      previous_output_processing_style,
      Input>;
  static constexpr auto
      output_processing_style = Factory::output_processing_style;
  static constexpr auto
      input_processing_style = Factory::input_processing_style;
  factory_holder_t<Factory> factory;
  Previous &previous;

  template<typename NewFactory>
  constexpr auto operator+(NewFactory &&new_factory) {
    return input_factory<output_type, output_processing_style,
                         NewFactory,
                         input_factory>
        {new_factory, *this};
  }

  template<typename F>
  constexpr auto operator+(composed<F> &&c) {
    using ComposedFactory = composed_factory<decltype(c(
        *this)), composed<F>>;
    return input_factory<output_type, output_processing_style,
                         ComposedFactory,
                         input_factory>
        {ComposedFactory(c), *this};
  }

  constexpr auto operator+(end_factory_tag) {
    return end_factory<input_factory>{*this};
  }

  template<typename Next>
  constexpr auto make(Next &&next) {
    if constexpr (std::is_invocable_v<Factory,
                                      decltype(previous),
                                      decltype(next)>) {
      return factory(std::forward<decltype(previous)>(previous),
                     std::forward<decltype(next)>(next));

    } else {
      return previous.make(
          factory.template make<stage_properties<Input,
                                                 Next,
                                                 previous_output_processing_style,
                                                 input_processing_style,
                                                 output_processing_style>>(
              std::forward<Next>(next)));
    }
  }

};

}

template<typename StartingInput, processing_style starting_processing_style, typename... Stages>
[[nodiscard]] constexpr auto make_pipeline(Stages &&... stages) {
  detail::starting_previous empty;
  detail::starting_factory<StartingInput> starting_factory;
  return
      (detail::input_factory<StartingInput, starting_processing_style>{
          starting_factory, empty} + ... + std::forward<
          Stages>(stages)).make(detail::end_stage{});

}

template<typename Range, typename... Stages>
[[nodiscard]] constexpr auto apply(Range &&range, Stages &&... stages) {

  auto chain = make_pipeline<decltype(range), processing_style::complete>(
      std::forward<Stages>(stages)...);
  return chain.process_complete(std::forward<Range>(range));
}

}

#endif //SKYDOWN_SPL_CORE_H_
