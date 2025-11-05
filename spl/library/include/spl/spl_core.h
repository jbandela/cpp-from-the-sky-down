#ifndef SKYDOWN_SPL_CORE_H_
#define SKYDOWN_SPL_CORE_H_

#include <iostream>
#include <type_traits>
#include <functional>
#include <tuple>
#include <utility>

namespace spl {

// Template to hold multiple types - all must be reference types
template<typename... Ts>
struct types {
};

template<typename T>
struct is_types:std::false_type {};

template<typename... Ts>
struct is_types<types<Ts...>>:std::true_type {} ;

template<typename Types>
struct first_type;

template<typename First, typename... Types>
struct first_type<types<First, Types...>>{
  using type = First;
};

template<typename Types>
using first_type_t = typename first_type<Types>::type;


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

template<typename InputTypes, typename Next, processing_style PreviousOutputProcessingStyle,
    processing_style InputProcessingStyle, processing_style OutputProcessingStyle>
struct stage_properties {
  using input_types = InputTypes;
  using next_type = Next;
  static constexpr auto
      previous_output_processing_style = PreviousOutputProcessingStyle;
  static constexpr auto input_processing_style = InputProcessingStyle;
  static constexpr auto output_processing_style = OutputProcessingStyle;
};

template<typename Impl>
struct incremental_outputter {
  Impl &impl;

  constexpr decltype(auto) operator()(auto &&...output) {
    return impl.process_incremental(std::forward<decltype(output)>(output)...);
  }

  constexpr operator bool() const {
    return !impl.done();
  }

  static constexpr bool calculate_type = false;

};

struct type_calculating_outputter {
  constexpr decltype(auto) operator()(auto &&...output) {
    return types<decltype(output)...>();
  }

  static constexpr bool calculate_type = true;

};

template<typename Outputter, typename F>
struct as_outputter:Outputter{
  F f;
  constexpr decltype(auto) operator()(auto &&...ts) {
    return std::invoke(f,static_cast<Outputter&>(*this), std::forward<decltype(ts)>(ts)...);
  }

};

template<typename Outputter, typename F>
as_outputter(Outputter&, F) -> as_outputter<Outputter,F>;

template<typename T>
constexpr bool calculate_type_v = std::remove_cvref_t<T>::calculate_type;

// Helper to invoke a callable with the last argument first
template<typename F, typename... Args>
constexpr decltype(auto) invoke_with_last_first(F&& f, Args&&... args) {
  if constexpr (sizeof...(Args) == 0) {
    return std::invoke(std::forward<F>(f));
  } else if constexpr (sizeof...(Args) == 1) {
    return std::invoke(std::forward<F>(f), std::forward<Args>(args)...);
  } else {
    return []<std::size_t... Is>(F&& func, auto&& tuple, std::index_sequence<Is...>) {
      constexpr std::size_t last = sizeof...(Is);
      return std::invoke(
        std::forward<F>(func),
        std::get<last>(std::forward<decltype(tuple)>(tuple)),
        std::get<Is>(std::forward<decltype(tuple)>(tuple))...
      );
    }(std::forward<F>(f),
      std::forward_as_tuple(std::forward<Args>(args)...),
      std::make_index_sequence<sizeof...(Args) - 1>{});
  }
}


template<typename Impl>
incremental_outputter(Impl &) -> incremental_outputter<Impl>;

template<typename R>
concept movable_range = std::ranges::range<std::remove_cvref_t<R>>
    && !std::same_as<decltype(std::ranges::begin(std::declval<R>())),
                     decltype(std::ranges::begin(std::as_const(std::declval<R>())))>
    && std::is_rvalue_reference_v<R>;

template<typename R, typename T>
constexpr decltype(auto) move_if_movable_range(T &&t) requires(std::ranges::range<
    std::remove_cvref_t<R>>) {
  return (t);
}

template<movable_range R, typename T>
constexpr decltype(auto) move_if_movable_range(T &&t) {
  return std::move(t);
}

template<typename Output, typename R>
constexpr auto SkydownSplOutput(Output &&output,
                                R &&r)requires(std::ranges::range<std::remove_cvref_t<
    R>>) {
  if constexpr (calculate_type_v<Output>) {
    auto &&v = *r.begin();
    return output(
        move_if_movable_range<std::remove_cvref_t<R>>(std::forward<decltype(v)>(
            v)));

  } else {
    for (auto &&v : std::forward<R>(r)) {
      if (!output) return false;
      output(
          move_if_movable_range<std::remove_cvref_t<
              R>>(std::forward<decltype(v)>(
              v)));
    }
    return true;
  }
}

// Partial specialization extracting types<...> as second parameter
template<template<typename...> typename Derived, typename StageProperties, typename... InputTypes, typename... Parameters>
struct stage_impl<Derived<StageProperties,
                          types<InputTypes...>,
                          Parameters...>> {

  using child_type = Derived<StageProperties,
                             types<InputTypes...>,
                             Parameters...>;
  using next_type = typename StageProperties::next_type;
  [[no_unique_address]] next_type next;

  using input_types = typename StageProperties::input_types;
  using base = stage_impl;

  constexpr static auto previous_output_processing_style =
      StageProperties::previous_output_processing_style;
  constexpr static auto
      input_processing_style = StageProperties::input_processing_style;
  constexpr static auto
      output_processing_style = StageProperties::output_processing_style;

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

  constexpr bool done() const requires(complete_input<
      child_type>) {
    return false;
  }


  constexpr void
  process_incremental(InputTypes... inputs)requires(incremental_input<
      child_type>) {
    return next.process_incremental(std::forward<InputTypes>(inputs)...);
  }

  constexpr decltype(auto)
  process_complete(InputTypes... inputs)requires(complete_input<
      child_type>) {
    return next.process_complete(static_cast<InputTypes>(inputs)...);

  }

  // Utilities
  template<typename T>
  constexpr decltype(auto) forward(T &&input) {
    return std::forward<T>(input);
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
  template<processing_style PreviousOutputProcessingStyle, typename InputTypes>
  using output_types = typename StageImpl<stage_properties<InputTypes,
                                                           std::monostate,
                                                           PreviousOutputProcessingStyle,
                                                           input_processing_style,
                                                           output_processing_style>,
                                          InputTypes,
                                          TypeParameters...>::output_types;

  template<typename... Ts>
  constexpr explicit stage_instantiator(Ts &&... ts)
      :parameters_{std::forward<Ts>(ts)...} {}

  template<typename StageProperties, typename Next>
  constexpr auto make(Next &&next) {
    return std::apply([&](auto &&... ts) {
      return StageImpl<StageProperties,
                       typename StageProperties::input_types,
                       TypeParameters...>{
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



// Forward declaration
template<typename StageProperties, typename InputTypes>
struct values_impl;

// Partial specialization to extract types from types<...>
template<typename StageProperties, typename InputType>
struct values_impl<StageProperties, types<InputType>>
    : stage_impl<values_impl<StageProperties, types<InputType>>> {
  using base = typename values_impl::base;

using output_types =  decltype( SkydownSplOutput(type_calculating_outputter(), std::declval<InputType>()));

static_assert(is_types<output_types>::value);

  constexpr decltype(auto) process_complete(InputType inputs) {
    std::invoke([](auto &&output, InputType input) {
                  SkydownSplOutput(output, static_cast<InputType>(input));

                },
                incremental_outputter{this->next},
                std::forward<InputType>(inputs));
    return this->next.finish();
  }

};

inline constexpr auto values() {
  return stage<values_impl,
               processing_style::complete,
               processing_style::incremental>();
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
    if constexpr (std::is_lvalue_reference_v<decltype(t)>){
      return t;
    } else{
      return T{std::move(t)};
    }
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

template<typename InputTypes, processing_style InputProcesingStyle>
struct starting_factory {
  template<auto, typename>
  using output_types = InputTypes;

  static constexpr auto input_processing_style = InputProcesingStyle;
  static constexpr auto output_processing_style = InputProcesingStyle;

  template<typename, typename Next>
  constexpr auto make(Next &&next) {
    return std::forward<Next>(next);
  }
};

template<typename Composed, bool owning>
struct composed_type {
  using type = Composed &;
};

template<typename Composed>
struct composed_type<Composed, true> {
  using type = Composed;
};

template<typename Factory, typename Composed, bool owning = false>
struct composed_factory {
  template<processing_style, typename>
  using output_types = typename Factory::output_types;
  static constexpr auto
      output_processing_style = Factory::output_processing_style;
  static constexpr auto
      input_processing_style = Factory::input_processing_style;
  typename composed_type<Composed, owning>::type composed;

  template<typename... Ts>
  constexpr auto operator()(Ts &&... ts) {
    return composed(std::forward<Ts>(ts)...);
  }

};

template<typename F>
struct factory_holder_type {
  using type = F &;
};

template<typename Factory, typename Composed, bool owning>
struct factory_holder_type<composed_factory<Factory, Composed, owning>> {
  using type = composed_factory<Factory, Composed, owning>;
};

template<typename T>
using factory_holder_t = typename factory_holder_type<T>::type;

template<typename InputTypes, processing_style PreviousOutputProcessingStyle, typename Factory = starting_factory<
    InputTypes,
    PreviousOutputProcessingStyle>, typename Previous = starting_previous>
struct input_factory {
  static constexpr auto
      previous_output_processing_style = PreviousOutputProcessingStyle;
  using output_types = typename Factory::template output_types<
      previous_output_processing_style,
      InputTypes>;

  static_assert(is_types<output_types>::value);
  static constexpr auto
      output_processing_style = Factory::output_processing_style;
  static constexpr auto
      input_processing_style = Factory::input_processing_style;
  factory_holder_t<Factory> factory;
  Previous &previous;

  template<typename NewFactory>
  constexpr auto operator+(NewFactory &&new_factory) {
    if constexpr (output_processing_style
        == std::remove_cvref_t<NewFactory>::input_processing_style) {
      return input_factory<output_types, output_processing_style,
                           std::remove_cvref_t<NewFactory>,
                           input_factory>
          {new_factory, *this};
    } else {
      auto c = compose(values(), std::forward<NewFactory>(new_factory));
      using ComposedFactory = composed_factory<decltype(c(
          *this)), decltype(c), true>;
      return input_factory<output_types, output_processing_style,
                           ComposedFactory,
                           input_factory>
          {ComposedFactory(std::move(c)), *this};
    }
  }

  template<typename F>
  constexpr auto operator+(composed<F> &&c) {
    using ComposedFactory = composed_factory<decltype(c(
         *this)), composed<F>>;
    return input_factory<output_types, output_processing_style,
                         ComposedFactory,
                         input_factory>
        {ComposedFactory(c), *this};
  }

  template<typename F>
  constexpr auto operator+(composed<F> &c) {
    using ComposedFactory = composed_factory<decltype(c(
        *this)), composed<F>>;
    return input_factory<output_types, output_processing_style,
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
          factory.template make<stage_properties<InputTypes,
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
  detail::starting_factory<StartingInput, starting_processing_style>
      starting_factory;
  return
      (detail::input_factory<StartingInput, starting_processing_style,
                             decltype(starting_factory)>{
          starting_factory, empty} + ... + std::forward<
          Stages>(stages)).make(detail::end_stage{});

}

template<typename Range, typename... Stages>
[[nodiscard]] constexpr auto apply(Range &&range, Stages &&... stages) {

  auto
      chain = make_pipeline<types<decltype(range)>, processing_style::complete>(
      std::forward<Stages>(stages)...);
  return chain.process_complete(std::forward<Range>(range));
}

}

#endif //SKYDOWN_SPL_CORE_H_
