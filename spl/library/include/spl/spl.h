#ifndef SKYDOWN_SPL_H_
#define SKYDOWN_SPL_H_

// SPL - Skydown Pipeline Library
// A modern C++23 library for composable, constexpr-compatible stream processing pipelines.
// Single-header version combining core infrastructure and stages.

#include <algorithm>
#include <functional>
#include <iostream>
#include <map>
#include <optional>
#include <tuple>
#include <type_traits>
#include <utility>

namespace spl {

// ============================================================================
// CORE INFRASTRUCTURE
// ============================================================================

namespace impl {

// Template to hold multiple types - all must be reference types
template<typename... Ts>
struct types {
};

} // namespace impl

namespace detail {

template<typename T>
struct is_types:std::false_type {};

template<typename... Ts>
struct is_types<impl::types<Ts...>>:std::true_type {} ;

template<typename Types>
struct first_type;

template<typename First, typename... Types>
struct first_type<impl::types<First, Types...>>{
  using type = First;
};

template<typename Types>
using first_type_t = typename first_type<Types>::type;

} // namespace detail

namespace impl {

enum class processing_style {
  incremental,
  complete
};

} // namespace impl

template<typename Range, typename... Stages>
[[nodiscard]] constexpr auto apply(Range &&range, Stages &&... stages);

namespace impl {

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

} // namespace impl

namespace detail {

template<typename InputTypes, typename Next, impl::processing_style PreviousOutputProcessingStyle,
    impl::processing_style InputProcessingStyle, impl::processing_style OutputProcessingStyle>
struct stage_properties {
  using input_types = InputTypes;
  using next_type = Next;
  static constexpr auto
      previous_output_processing_style = PreviousOutputProcessingStyle;
  static constexpr auto input_processing_style = InputProcessingStyle;
  static constexpr auto output_processing_style = OutputProcessingStyle;
};

} // namespace detail

namespace impl {

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

template<typename Impl>
incremental_outputter(Impl &) -> incremental_outputter<Impl>;

} // namespace impl

namespace detail {

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

} // namespace detail

// ADL customization point for outputting ranges
template<typename Output, typename R>
constexpr auto SkydownSplOutput(Output &&output,
                                R &&r)requires(std::ranges::range<std::remove_cvref_t<
    R>>) {
  if constexpr (spl::impl::calculate_type_v<Output>) {
    auto &&v = *r.begin();
    return output(
        detail::move_if_movable_range<std::remove_cvref_t<R>>(std::forward<decltype(v)>(
            v)));

  } else {
    for (auto &&v : std::forward<R>(r)) {
      if (!output) return false;
      output(
          detail::move_if_movable_range<std::remove_cvref_t<
              R>>(std::forward<decltype(v)>(
              v)));
    }
    return true;
  }
}

namespace impl {

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

} // namespace impl

namespace detail {

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

template<template<typename, typename...> typename StageImpl, impl::processing_style InputProcessingStyle,
    impl::processing_style OutputProcessingStyle,
    typename Parameters = std::tuple<>, typename... TypeParameters>
class stage_instantiator {
  [[no_unique_address]] instantiable_t<Parameters> parameters_;

 public:
  static constexpr auto input_processing_style = InputProcessingStyle;
  static constexpr auto output_processing_style = OutputProcessingStyle;
  template<impl::processing_style PreviousOutputProcessingStyle, typename InputTypes>
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

template<typename T>
struct is_stage_instantiator:std::false_type {};

template<template<typename, typename...> typename StageImpl, impl::processing_style InputProcessingStyle,
    impl::processing_style OutputProcessingStyle,
    typename Parameters, typename... TypeParameters>
struct is_stage_instantiator<stage_instantiator<StageImpl,
                                                 InputProcessingStyle,
                                                 OutputProcessingStyle,
                                                 Parameters,
                                                 TypeParameters...>>:std::true_type {};

} // namespace detail

namespace impl {



template<template<typename, typename...> typename StageImpl, processing_style InputProcessingStyle,
    processing_style OutputProcessingStyle,
    typename... TypeParameters, typename... Ts>
constexpr auto make_stage(Ts &&... ts) {
  return detail::stage_instantiator<StageImpl,
                            InputProcessingStyle,
                            OutputProcessingStyle,
                            std::tuple<Ts...>,
                            TypeParameters...>(std::forward<
      decltype(ts)>(ts)...);
}

} // namespace impl

namespace detail {

// Forward declaration
template<typename StageProperties, typename InputTypes>
struct values_impl;

// Partial specialization to extract types from types<...>
template<typename StageProperties, typename InputType>
struct values_impl<StageProperties, impl::types<InputType>>
    : impl::stage_impl<values_impl<StageProperties, impl::types<InputType>>> {
  using base = typename values_impl::base;

using output_types =  decltype( SkydownSplOutput(impl::type_calculating_outputter(), std::declval<InputType>()));

static_assert(is_types<output_types>::value);

  constexpr decltype(auto) process_complete(InputType inputs) {
    std::invoke([](auto &&output, InputType input) {
                  SkydownSplOutput(output, static_cast<InputType>(input));

                },
                impl::incremental_outputter{this->next},
                std::forward<InputType>(inputs));
    return this->next.finish();
  }

};

inline constexpr auto values() {
  return spl::impl::make_stage<values_impl,
               spl::impl::processing_style::complete,
               spl::impl::processing_style::incremental>();
}

} // namespace detail

namespace detail {

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

template<typename T>
struct is_composed:std::false_type{};


template<typename F>
struct is_composed<composed<F>>:std::true_type{};



} // namespace detail

namespace impl{
  template<typename T>
  concept composed = detail::is_composed<std::remove_cvref_t<T>>::value;

  template<typename T>
  concept stage_instantiator = detail::is_stage_instantiator<std::remove_cvref_t<T>>::value;

  template<typename T>
  concept stage = stage_instantiator<T> || composed<T>;
}

namespace detail {
  template<typename T, spl::impl::composed Composed>
  struct t_and_composed{
    [[no_unique_address]] T t;
    [[no_unique_address]] Composed composed;
  };

  template<typename T>
  struct is_t_and_composed:std::false_type{};

  template<typename T, spl::impl::composed Composed>
  struct is_t_and_composed<t_and_composed<T,Composed>>:std::true_type{};

  template<typename T>
  concept t_composed = is_t_and_composed<std::remove_cvref_t<
      T>>::value;

} // namespace detail

template<impl::stage... Ts>
constexpr auto compose(Ts &&... ts) {
  return detail::composed{[...args = std::forward<Ts>(ts)](auto&& prev, auto&&... next)constexpr mutable{
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

template<typename T, spl::impl::stage... Ts>
requires(!impl::stage<T>)
constexpr auto compose(T&& t, Ts&&... ts) {
  auto c = compose(std::forward<Ts>(ts)...);
  return detail::t_and_composed<std::remove_cvref_t<T>, decltype(c)>{
      std::forward<T>(t),
      std::move(c)
  };

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

template<typename InputTypes, impl::processing_style InputProcesingStyle>
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
  template<impl::processing_style, typename>
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

template<typename InputTypes, impl::processing_style PreviousOutputProcessingStyle, typename Factory = starting_factory<
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

namespace impl {

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

} // namespace impl

template<typename Range, typename... Stages>
[[nodiscard]] constexpr auto apply(Range &&range, Stages &&... stages) {

  auto
      chain = impl::make_pipeline<impl::types<decltype(range)>, impl::processing_style::complete>(
      std::forward<Stages>(stages)...);
  return chain.process_complete(std::forward<Range>(range));
}

template<detail::t_composed TAndComposed, impl::stage... Stages>
[[nodiscard]] constexpr auto apply(TAndComposed &&t_and_composed,
                                   Stages &&... stages) {
  return apply(std::forward<TAndComposed>(t_and_composed).t,
               std::forward<TAndComposed>(t_and_composed).composed,
               std::forward<Stages>(stages)...);
}


// ============================================================================
// STAGES
// ============================================================================

#if 0 // API Documentation - these forward declarations are for reference only
// ============================================================================
// Forward Declarations - User-facing Stage Functions
// ============================================================================

// Accumulation stages
template<typename T, typename F>
constexpr auto accumulate_in_place(T t, F f);


template<typename T, typename F>
constexpr auto accumulate(T t, F f);

template<typename F>
constexpr auto accumulate(F f);

template<typename F>
constexpr auto for_each(F f);

constexpr auto sum();

constexpr auto count();

inline constexpr auto first();
inline constexpr auto last();

// Transformation stages
template<typename F>
constexpr auto transform(F f);

template<typename F>
constexpr auto transform_cps(F f);

template<typename F>
constexpr auto transform_complete(F f);

template<typename F>
constexpr auto transform_complete_cps(F f);

template<size_t I, typename F>
constexpr auto transform_arg_cps(F f);

template<size_t I, typename F>
constexpr auto transform_arg(F f);

// Enumerate (prepend index to each element)
inline constexpr auto enumerate(size_t start = 0);

// Sorting stages
template<typename Comparator>
constexpr auto sort(Comparator comp);

constexpr auto sort();

template<typename Comparator>
constexpr auto stable_sort(Comparator comp);

constexpr auto stable_sort();

template<typename Comparator>
constexpr auto partial_sort(size_t n, Comparator comp);

constexpr auto partial_sort(size_t n);

template<typename Comparator>
constexpr auto nth_element(size_t n, Comparator comp);

constexpr auto nth_element(size_t n);

template<typename BinaryPredicate>
constexpr auto unique(BinaryPredicate pred);

constexpr auto unique();

// Filtering and selection stages
template<typename Predicate>
constexpr auto filter(Predicate predicate);

template<size_t I, typename Predicate>
constexpr auto filter_arg(Predicate predicate);

template<size_t I>
constexpr auto filter_arg();

constexpr auto take(size_t n);

template<typename Predicate>
constexpr auto take_while(Predicate predicate);

constexpr auto skip(size_t n);

template<typename Predicate>
constexpr auto skip_while(Predicate predicate);

constexpr auto filter_duplicates();

constexpr auto repeat(size_t n);

template<typename Predicate>
constexpr auto repeat_while(Predicate predicate);

// Flattening and mapping stages
template<typename F>
constexpr auto flat_map(F f);

template<size_t I, typename F>
constexpr auto flat_map_arg(F f);

constexpr auto flatten();

template<size_t I>
constexpr auto flatten_arg();

inline constexpr auto flatten_optional();

template<size_t I>
constexpr auto flatten_optional_arg();

// Tuple/pair manipulation
inline constexpr auto expand_tuple();
inline constexpr auto make_tuple();
inline constexpr auto make_pair();

// Value manipulation stages
template<size_t I>
constexpr auto ref_arg();
inline constexpr auto ref();

template<size_t I>
constexpr auto move_arg();
inline constexpr auto move();

template<size_t I>
constexpr auto lref_arg();
inline constexpr auto lref();

template<size_t I>
constexpr auto deref_arg();
inline constexpr auto deref();

template<size_t I>
constexpr auto addressof_arg();
inline constexpr auto addressof();

template<size_t I, typename T>
constexpr auto cast_arg();
template<typename T>
constexpr auto cast();

template<typename T>
constexpr auto construct();

// Zipping stages
template<typename F>
constexpr auto zip_result(F f);

template<typename R>
constexpr auto zip(R&& r);

template<typename... Rs>
constexpr auto zip(Rs&&... rs);

template<typename R>
constexpr auto zip_exact(R&& r);

template<typename... Rs>
constexpr auto zip_exact(Rs&&... rs);

// Chaining stages
template<typename R>
constexpr auto chain_before(R&& r);

template<typename R>
constexpr auto chain_after(R&& r);

// Swizzle (reorder/select tuple elements)
template<std::size_t... Indices>
constexpr auto swizzle();

// Grouping stages
template<typename MapType, typename SelectorF, typename... Stages>
constexpr auto group_by(SelectorF selector_f, Stages... stages);

template<typename Comparer, typename... Stages>
constexpr auto chunk_by(Comparer comparer, Stages... stages);

template<typename... Stages>
constexpr auto chunk(size_t n, Stages... stages);

// Tee (parallel processing)
template<typename... Stages>
constexpr auto tee(Stages&&... stages);

// Collection builders
template<template<typename...> typename C>
constexpr auto push_back_to();

constexpr auto to_vector();

template<template<typename, typename> typename MapType>
constexpr auto to_map();

// Generators and sources
inline constexpr auto iota(size_t start);
inline constexpr auto iota(size_t start, size_t end);

template<typename F>
struct generator;

template<typename R>
constexpr auto SkydownSplMakeGenerator(R&& r);

// Complete-to-incremental conversion
inline constexpr auto as_incremental();
#endif // API Documentation

// ============================================================================
// Implementation Details
// ============================================================================

// Category-preserving as_const: lvalue T& -> const T&, rvalue T&& -> const T&&
template<typename T>
constexpr std::add_const_t<std::remove_reference_t<T>>& forward_as_const(std::remove_reference_t<T>& t) noexcept {
  return t;
}

template<typename T>
constexpr std::add_const_t<std::remove_reference_t<T>>&& forward_as_const(std::remove_reference_t<T>&& t) noexcept {
  return static_cast<std::add_const_t<std::remove_reference_t<T>>&&>(t);
}

// Storage that preserves memory model: lvalues stored as references, rvalues stored by value
template<typename T>
struct value_storage {
  // For lvalues: store as pointer (reference semantics)
  std::remove_reference_t<T>* ptr_;

  constexpr value_storage(T& value) : ptr_(&value) {}

  constexpr decltype(auto) operator*() const { return *ptr_; }

  constexpr bool operator==(const std::remove_reference_t<T>& other) const {
    return *ptr_ == other;
  }
};

template<typename T>
struct value_storage<T&&> {
  // For rvalues: store by value, dropping const since we're making a copy
  std::remove_cvref_t<T> value_;

  constexpr value_storage(T&& value) : value_(std::move(value)) {}

  constexpr std::remove_cvref_t<T>&& operator*() { return std::move(value_); }

  constexpr bool operator==(const std::remove_cvref_t<T>& other) const {
    return value_ == other;
  }
};

// Forward declaration
template<typename StageProperties, typename InputTypes, typename Init, typename F>
struct accumulate_in_place_impl;

// Partial specialization to extract types from impl::types<...>
template<typename StageProperties, typename... InputTypes, typename Init, typename F>
struct accumulate_in_place_impl<StageProperties, impl::types<InputTypes...>, Init, F>
    : impl::stage_impl<accumulate_in_place_impl<StageProperties,
                                          impl::types<InputTypes...>,
                                          Init,
                                          F>> {
  using base = typename accumulate_in_place_impl::base;
  using accumulated_type = std::invoke_result_t<
      Init,
      impl::types<InputTypes...>>;
  static_assert(!std::is_same_v<accumulated_type, void>);
  using output_types = impl::types<accumulated_type &&>;
  [[no_unique_address]] Init init;
  [[no_unique_address]] F f{};
  [[no_unique_address]] accumulated_type accumulated = std::invoke(init,impl::types<InputTypes...>());

  constexpr decltype(auto) process_incremental(InputTypes... inputs) {
    std::invoke(f, accumulated, std::forward<InputTypes>(inputs)...);
  }

  constexpr decltype(auto) finish() {
    return this->next.process_complete(std::move(accumulated));
  }

};


template<typename Init, typename F>
constexpr auto accumulate_in_place_with_init(Init init, F f) {
  return impl::make_stage<accumulate_in_place_impl,
              impl::processing_style::incremental,
              impl::processing_style::complete,
              decltype(init),
              F>(
     std::move(init), std::move(f));
}

template<typename T, typename F>
constexpr auto accumulate_in_place(T t, F f) {
  auto init = [t = std::move(t)](auto) mutable { return std::move(t); };
  return accumulate_in_place_with_init(std::move(init), std::move(f));
}

template<typename T, typename F>
constexpr auto accumulate(T t, F f) {
  return accumulate_in_place(std::forward<T>(t),
                             [f = std::move(f)](auto &accumulated,
                                                auto &&v) {
                               accumulated =
                                   std::invoke(f,
                                               accumulated,
                                               std::forward<decltype(v)>(
                                                   v));
                             });
}

template<typename F>
constexpr auto accumulate(F f) {
  return accumulate_in_place_with_init([]<typename T, typename... Types>(impl::types<T,Types...>) {
                               static_assert(sizeof...(Types) == 0,
                                             "Accumulate without an explicit init needs to only have one input");
                               return std::remove_cvref_t<T>();
                             },
                             [f = std::move(f)](auto &accumulated,
                                                auto &&v) {
                               accumulated =
                                   std::invoke(f,
                                               accumulated,
                                               std::forward<decltype(v)>(
                                                   v));
                             });
}

template<typename F>
constexpr auto for_each(F f) {
  return accumulate_in_place(std::monostate(),
                             [f = std::move(f)](auto &, auto &&... ts) {
                               std::invoke(f,
                                           std::forward<decltype(ts)>(ts)...);
                             });
}
constexpr auto sum() {
  return accumulate(std::plus<>{});
}

constexpr auto count() {
  return accumulate_in_place(size_t{0}, [](size_t& c, auto&&...) { ++c; });
}

// Forward declaration
template<typename StageProperties, typename InputTypes, typename F>
struct flat_map_impl;

// Partial specialization to extract types from impl::types<...>
template<typename StageProperties, typename... InputTypes, typename F>
struct flat_map_impl<StageProperties,
                     impl::types<InputTypes...>,
                     F>
    : impl::stage_impl<flat_map_impl<StageProperties,
                               impl::types<InputTypes...>,
                               F>> {
  using base = typename flat_map_impl::base;
  using output_types = decltype(std::invoke(std::declval<F>(),
                                            impl::type_calculating_outputter(),
                                            std::declval<InputTypes>()...));
  static_assert(detail::is_types<output_types>::value);
  [[no_unique_address]] F f{};
  bool done_ = false;

  constexpr decltype(auto) process_incremental(InputTypes... inputs) {
    done_ = !std::invoke(f,
                         impl::incremental_outputter{this->next},
                         static_cast<InputTypes>(inputs)...);
  }

  constexpr bool done() const {
    return done_ || this->next.done();
  }

};

struct IdentityOutputCalculator {
  template<typename... Ts>
  using output_types = impl::types<Ts...>;
};

template<typename F>
constexpr auto flat_map(F f) {
  return impl::make_stage<flat_map_impl,
               impl::processing_style::incremental,
               impl::processing_style::incremental,
               F>(std::move(f));

}

template<size_t I, typename F>
constexpr auto flat_map_arg(F f) {
  return flat_map([f = std::move(f)]<typename Out, typename... Inputs>(Out &&out, Inputs&&... inputs) {
    auto tuple = std::forward_as_tuple(std::forward<Inputs>(inputs)...);

    auto invoke = [&]<size_t... Before, size_t... After>(std::index_sequence<Before...>, std::index_sequence<After...>) {
      return std::invoke(f,
                         std::forward<Out>(out),
                         std::forward_as_tuple(std::get<Before>(std::move(tuple))...),
                         std::get<I>(std::move(tuple)),
                         std::forward_as_tuple(std::get<I + 1 + After>(std::move(tuple))...));
    };

    if constexpr (impl::calculate_type_v<Out>) {
      return invoke(std::make_index_sequence<I>{}, std::make_index_sequence<sizeof...(Inputs) - I - 1>{});
    } else {
      invoke(std::make_index_sequence<I>{}, std::make_index_sequence<sizeof...(Inputs) - I - 1>{});
      return true;
    }
  });
}

// Forward declaration
template<typename StageProperties, typename InputTypes, typename Predicate>
struct filter_impl;

// Partial specialization to extract types from impl::types<...>
template<typename StageProperties, typename... InputTypes, typename Predicate>
struct filter_impl<StageProperties,
                   impl::types<InputTypes...>,
                   Predicate>
    : impl::stage_impl<filter_impl<StageProperties,
                             impl::types<InputTypes...>,
                             Predicate>> {
  using base = typename filter_impl::base;
  using output_types = impl::types<InputTypes...>;
  [[no_unique_address]] Predicate predicate{};

  constexpr void process_incremental(InputTypes... inputs) {
    if (std::invoke(predicate,inputs...)) {
      this->next.process_incremental(static_cast<InputTypes>(inputs)...);
    }
  }

  constexpr bool done() const {
    return this->next.done();
  }
};

template<typename Predicate>
constexpr auto filter(Predicate predicate) {
  return impl::make_stage<filter_impl,
               impl::processing_style::incremental,
               impl::processing_style::incremental,
               Predicate>(std::move(predicate));
}

constexpr inline auto filter(){
  return filter(std::identity{});
}

template<size_t I, typename Predicate>
constexpr auto filter_arg(Predicate predicate) {
  return flat_map_arg<I>([predicate = std::move(predicate)](auto&& out, auto&& before, auto&& arg, auto&& after) {
    if constexpr (impl::calculate_type_v<decltype(out)>) {
      return std::apply([&](auto&&... before_args) {
        return std::apply([&](auto&&... after_args) {
          return out(std::forward<decltype(before_args)>(before_args)...,
                     std::forward<decltype(arg)>(arg),
                     std::forward<decltype(after_args)>(after_args)...);
        }, std::forward<decltype(after)>(after));
      }, std::forward<decltype(before)>(before));
    } else {
      if (std::invoke(predicate, std::as_const(arg))) {
        std::apply([&](auto&&... before_args) {
          std::apply([&](auto&&... after_args) {
            out(std::forward<decltype(before_args)>(before_args)...,
                std::forward<decltype(arg)>(arg),
                std::forward<decltype(after_args)>(after_args)...);
          }, std::forward<decltype(after)>(after));
        }, std::forward<decltype(before)>(before));
      }
      return true;
    }
  });
}

template<size_t I>
constexpr auto filter_arg() {
  return filter_arg<I>(std::identity{});
}

constexpr auto take(size_t
                    n) {
  return flat_map([
                      i = size_t(0), n
                  ]<typename Out>(
      Out &&output,
      auto &&...inputs
  )mutable {

    if constexpr (impl::calculate_type_v<Out>) {
      return output(std::forward<decltype(inputs)>(inputs)...
      );

    } else {
      ++i;
      if (i > n) return false;
      else {
        output(std::forward<decltype(inputs)>(inputs)...
        );
        return i < n;

      }
    }
  });
}

template<typename Predicate>
constexpr auto take_while(Predicate predicate) {
  return flat_map([predicate = std::move(predicate),
                   done = false]<typename Out>(
      Out&& output,
      auto&&... inputs
  ) mutable {
    if constexpr (impl::calculate_type_v<Out>) {
      return output(std::forward<decltype(inputs)>(inputs)...);
    } else {
      if (done) return false;
      if (done = !std::invoke(predicate, std::as_const(inputs)...); !done) {
        output(std::forward<decltype(inputs)>(inputs)...);
      }
      return !done;
    }
  });
}

constexpr auto skip(size_t n) {
  return flat_map([i = size_t(0), n]<typename Out>(
      Out&& output,
      auto&&... inputs
  ) mutable {
    if constexpr (impl::calculate_type_v<Out>) {
      return output(std::forward<decltype(inputs)>(inputs)...);
    } else {
      if (i < n) {
        ++i;
        return true;
      }
      output(std::forward<decltype(inputs)>(inputs)...);
      return true;
    }
  });
}

template<typename Predicate>
constexpr auto skip_while(Predicate predicate) {
  return flat_map([predicate = std::move(predicate),
                   skipping = true]<typename Out>(
      Out&& output,
      auto&&... inputs
  ) mutable {
    if constexpr (impl::calculate_type_v<Out>) {
      return output(std::forward<decltype(inputs)>(inputs)...);
    } else {
      if (skipping && std::invoke(predicate, std::as_const(inputs)...)) {
        return true;
      }
      skipping = false;
      output(std::forward<decltype(inputs)>(inputs)...);
      return true;
    }
  });
}

constexpr auto repeat(size_t n) {
  return flat_map([n]<typename Out>(
      Out&& output,
      auto&&... inputs
  ) {
    if constexpr (impl::calculate_type_v<Out>) {
      return output(forward_as_const<decltype(inputs)>(inputs)...);
    } else {
      for (size_t i = 0; i < n && output; ++i) {
        output(forward_as_const<decltype(inputs)>(inputs)...);
      }
      return bool(output);
    }
  });
}

template<typename Predicate>
constexpr auto repeat_while(Predicate predicate) {
  return flat_map([predicate = std::move(predicate)]<typename Out>(
      Out&& output,
      auto&&... inputs
  ) {
    if constexpr (impl::calculate_type_v<Out>) {
      return output(forward_as_const<decltype(inputs)>(inputs)...);
    } else {
      while (output && std::invoke(predicate, std::as_const(inputs)...)) {
        output(forward_as_const<decltype(inputs)>(inputs)...);
      }
      return bool(output);
    }
  });
}

// Forward declaration
template<typename StageProperties, typename InputTypes>
struct filter_duplicates_impl;

// Partial specialization to extract types from impl::types<...>
template<typename StageProperties, typename... InputTypes>
struct filter_duplicates_impl<StageProperties, impl::types<InputTypes...>>
    : impl::stage_impl<filter_duplicates_impl<StageProperties, impl::types<InputTypes...>>> {
  using base = typename filter_duplicates_impl::base;
  using output_types = impl::types<InputTypes...>;
  using storage_type = std::tuple<value_storage<InputTypes>...>;

  std::optional<storage_type> stored_;

  constexpr void process_incremental(InputTypes... inputs) {
    if (stored_.has_value()) {
      // Compare stored values with incoming values using std::tie
      if (stored_.value() == std::tie(inputs...)) {
        // Same as previous, skip
        return;
      }
      // Different, output the previously stored value
      std::apply([&](auto&... stored) {
        this->next.process_incremental(*stored...);
      }, stored_.value());
    }
    // Store the new value
    stored_.emplace(value_storage<InputTypes>(std::forward<InputTypes>(inputs))...);
  }

  constexpr decltype(auto) finish() {
    if (stored_.has_value()) {
      std::apply([&](auto&... stored) {
        this->next.process_incremental(*stored...);
      }, stored_.value());
    }
    return this->next.finish();
  }
};

constexpr auto filter_duplicates() {
  return impl::make_stage<filter_duplicates_impl,
               impl::processing_style::incremental,
               impl::processing_style::incremental>();
}

// Forward declaration
template<typename StageProperties, typename InputTypes, typename F>
struct transform_cps_impl;

// Partial specialization to extract types from impl::types<...>
template<typename StageProperties, typename... InputTypes, typename F>
struct transform_cps_impl<StageProperties, impl::types<InputTypes...>, F>
    : impl::stage_impl<transform_cps_impl<StageProperties, impl::types<InputTypes...>, F>> {
  using base = typename transform_cps_impl::base;

  using output_types = decltype(std::invoke(
      std::declval<F>(),
      impl::type_calculating_outputter(),
      std::declval<InputTypes>()...));

  static_assert(detail::is_types<output_types>::value);

  [[no_unique_address]] F f{};

  constexpr void process_incremental(InputTypes... inputs)
      requires(impl::incremental_input<transform_cps_impl>) {
    std::invoke(f,
                [this](auto&&... outputs) {
                  this->next.process_incremental(std::forward<decltype(outputs)>(outputs)...);
                },
                std::forward<InputTypes>(inputs)...);
  }

  constexpr decltype(auto) process_complete(InputTypes... inputs)
      requires(impl::complete_input<transform_cps_impl>) {
    return std::invoke(f,
                       [this](auto&&... outputs) -> decltype(auto) {
                         return this->next.process_complete(std::forward<decltype(outputs)>(outputs)...);
                       },
                       std::forward<InputTypes>(inputs)...);
  }
};

template<typename F>
constexpr auto transform_cps(F f) {
  return impl::make_stage<transform_cps_impl,
               impl::processing_style::incremental,
               impl::processing_style::incremental,
               F>(std::move(f));
}

// Forward declaration for transform_impl
template<typename StageProperties, typename InputTypes, typename F>
struct transform_impl;

// Partial specialization to extract types from impl::types<...>
template<typename StageProperties, typename... InputTypes, typename F>
struct transform_impl<StageProperties,
                      impl::types<InputTypes...>,
                      F>
    : impl::stage_impl<transform_impl<StageProperties,
                                      impl::types<InputTypes...>,
                                      F>> {
  using base = typename transform_impl::base;
  using output_types = impl::types<std::invoke_result_t<F, InputTypes...>>;
  [[no_unique_address]] F f{};

  constexpr void process_incremental(InputTypes... inputs)
      requires(impl::incremental_input<transform_impl>) {
    this->next.process_incremental(std::invoke(f, static_cast<InputTypes>(inputs)...));
  }

  constexpr decltype(auto) process_complete(InputTypes... inputs)
      requires(impl::complete_input<transform_impl>) {
    return this->next.process_complete(std::invoke(f, static_cast<InputTypes>(inputs)...));
  }

  constexpr bool done() const {
    return this->next.done();
  }
};

template<typename F>
constexpr auto transform(F f) {
  return impl::make_stage<transform_impl,
               impl::processing_style::incremental,
               impl::processing_style::incremental,
               F>(std::move(f));
}

template<size_t I, typename F>
constexpr auto transform_arg_cps(F f) {
  return flat_map_arg<I>([f = std::move(f)](auto&& out, auto&& before, auto&& arg, auto&& after) {
    auto invoke = [&] {
      return std::invoke(f,
                         std::forward<decltype(out)>(out),
                         std::forward<decltype(before)>(before),
                         std::forward<decltype(arg)>(arg),
                         std::forward<decltype(after)>(after));
    };
    if constexpr (impl::calculate_type_v<decltype(out)>) {
      return invoke();
    } else {
      invoke();
      return true;
    }
  });
}

template<size_t I, typename F>
constexpr auto transform_arg(F f) {
  return transform_arg_cps<I>([f = std::move(f)](auto&& out, auto&& before, auto&& arg, auto&& after) {
    return std::apply([&](auto&&... before_args) {
      return std::apply([&](auto&&... after_args) {
        return out(std::forward<decltype(before_args)>(before_args)...,
                   std::invoke(f, std::forward<decltype(arg)>(arg)),
                   std::forward<decltype(after_args)>(after_args)...);
      }, std::forward<decltype(after)>(after));
    }, std::forward<decltype(before)>(before));
  });
}

inline constexpr auto enumerate(size_t start = 0) {
  return transform_cps([i = start](auto&& out, auto&&... inputs) mutable {
    return out(i++, std::forward<decltype(inputs)>(inputs)...);
  });
}

// Value manipulation stages
template<size_t I>
constexpr auto ref_arg() {
  return transform_arg<I>([](auto&& arg) { return std::ref(arg); });
}

inline constexpr auto ref() {
  return ref_arg<0>();
}

template<size_t I>
constexpr auto move_arg() {
  return transform_arg<I>([](auto&& arg) -> decltype(auto) { return std::move(arg); });
}

inline constexpr auto move() {
  return move_arg<0>();
}

template<size_t I>
constexpr auto lref_arg() {
  return transform_arg<I>([](auto&& arg) -> decltype(auto) { return static_cast<std::remove_reference_t<decltype(arg)>&>(arg); });
}

inline constexpr auto lref() {
  return lref_arg<0>();
}

template<size_t I>
constexpr auto deref_arg() {
  return transform_arg<I>([](auto&& arg) -> decltype(auto) { return *std::forward<decltype(arg)>(arg); });
}

inline constexpr auto deref() {
  return deref_arg<0>();
}

template<size_t I>
constexpr auto addressof_arg() {
  return transform_arg<I>([](auto&& arg) { return std::addressof(arg); });
}

inline constexpr auto addressof() {
  return addressof_arg<0>();
}

template<size_t I, typename T>
constexpr auto cast_arg() {
  return transform_arg<I>([](auto&& arg) -> T { return static_cast<T>(std::forward<decltype(arg)>(arg)); });
}

template<typename T>
constexpr auto cast() {
  return cast_arg<0, T>();
}

template<typename T>
constexpr auto construct() {
  return transform_cps([](auto&& out, auto&&... inputs) {
    return out(T(std::forward<decltype(inputs)>(inputs)...));
  });
}

template<typename F>
constexpr auto transform_complete_cps(F f) {
  return impl::make_stage<transform_cps_impl,
               impl::processing_style::complete,
               impl::processing_style::complete,
               F>(std::move(f));
}

template<typename F>
constexpr auto transform_complete(F f) {
  return impl::make_stage<transform_impl,
               impl::processing_style::complete,
               impl::processing_style::complete,
               F>(std::move(f));
}

template<typename Comparator>
constexpr auto sort(Comparator comp) {
  return transform_complete_cps([comp = std::move(comp)](auto &&out,
                                                         auto &&container) {
    auto sorted = std::forward<decltype(container)>(container);
    std::sort(sorted.begin(), sorted.end(), comp);
    return out(std::move(sorted));
  });
}

constexpr auto sort() {
  return sort(std::less<>{});
}

template<typename Comparator>
constexpr auto stable_sort(Comparator comp) {
  return transform_complete_cps([comp = std::move(comp)](auto &&out,
                                                         auto &&container) {
    auto sorted = std::forward<decltype(container)>(container);
    std::stable_sort(sorted.begin(), sorted.end(), comp);
    return out(std::move(sorted));
  });
}

constexpr auto stable_sort() {
  return stable_sort(std::less<>{});
}

template<typename Comparator>
constexpr auto partial_sort(size_t n, Comparator comp) {
  return transform_complete_cps([n, comp = std::move(comp)](auto &&out,
                                                             auto &&container) {
    auto result = std::forward<decltype(container)>(container);
    auto middle = n < result.size() ? result.begin() + n : result.end();
    std::partial_sort(result.begin(), middle, result.end(), comp);
    return out(std::move(result));
  });
}

constexpr auto partial_sort(size_t n) {
  return partial_sort(n, std::less<>{});
}

template<typename Comparator>
constexpr auto nth_element(size_t n, Comparator comp) {
  return transform_complete_cps([n, comp = std::move(comp)](auto &&out,
                                                             auto &&container) {
    auto result = std::forward<decltype(container)>(container);
    auto nth = n < result.size() ? result.begin() + n : result.end();
    std::nth_element(result.begin(), nth, result.end(), comp);
    return out(std::move(result));
  });
}

constexpr auto nth_element(size_t n) {
  return nth_element(n, std::less<>{});
}

template<typename BinaryPredicate>
constexpr auto unique(BinaryPredicate pred) {
  return transform_complete_cps([pred = std::move(pred)](auto &&out,
                                                          auto &&container) {
    auto result = std::forward<decltype(container)>(container);
    auto new_end = std::unique(result.begin(), result.end(), pred);
    result.erase(new_end, result.end());
    return out(std::move(result));
  });
}

constexpr auto unique() {
  return unique(std::equal_to<>{});
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

inline constexpr auto expand_tuple_complete() {
  return transform_complete_cps([&](auto &&out, auto &&tuple) {
    return std::apply(out, std::forward<decltype(tuple)>(tuple));
  });
}

inline constexpr auto make_tuple_complete() {
  return transform_complete_cps([&](auto &&out, auto &&... ts) {
    return out(std::make_tuple(std::forward<decltype(ts)>(ts)...));
  });
}
inline constexpr auto make_pair_complete() {
  return transform_complete_cps([&](auto &&out, auto &&... ts) {
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
    return detail::invoke_with_last_first(
        [&](auto &&last, auto &&... inputs) {
          return SkydownSplOutput(impl::as_outputter(out,
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

template<size_t I>
constexpr auto flatten_arg() {
  return flat_map_arg<I>([](auto&& out, auto&& before, auto&& arg, auto&& after) {
    return std::apply([&](auto&&... before_args) {
      return std::apply([&](auto&&... after_args) {
        return SkydownSplOutput(impl::as_outputter(out,
            [&](auto&& out, auto&& item) {
              return out(forward_as_const<decltype(before_args)>(before_args)...,
                         std::forward<decltype(item)>(item),
                         forward_as_const<decltype(after_args)>(after_args)...);
            }),
            std::forward<decltype(arg)>(arg));
      }, std::forward<decltype(after)>(after));
    }, std::forward<decltype(before)>(before));
  });
}

template<size_t I>
constexpr auto flatten_optional_arg() {
  return compose(filter_arg<I>(), deref_arg<I>());
}

inline constexpr auto flatten_optional() {
  return flatten_optional_arg<0>();
}

template<template<typename...> typename C>
constexpr auto push_back_to() {
  return accumulate_in_place_with_init([]<typename... Inputs>(impl::types<Inputs...>) {
    static_assert(sizeof...(Inputs) == 1,
                  "push_back_to can only take 1 input argument");
    return C<std::remove_cvref_t<detail::first_type_t<impl::types<Inputs...>>>>{};
  }, [](auto &c, auto &&v) {
    c.push_back(std::forward<decltype(v)>(v));
  });
}

constexpr auto to_vector() {
  return push_back_to<std::vector>();
}

template<template<typename, typename> typename MapType>
constexpr auto to_map() {
  return accumulate_in_place_with_init([]<typename... Inputs>(impl::types<Inputs...>) {
    static_assert(sizeof...(Inputs) == 2, "to_map needs 2 input arguments");
    return MapType<std::remove_cvref_t<Inputs>...>{};
  }, [](auto &m, auto &&... args) {
    m.emplace(std::forward<decltype(args)>(args)...);
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

// Partial specialization to extract types from impl::types<...>
template<typename StageProperties, typename... InputTypes, typename SelectorF, typename Composed, typename MapType>
struct group_by_impl<StageProperties,
                     impl::types<InputTypes...>,
                     SelectorF,
                     Composed,
                     MapType>
    : impl::stage_impl<group_by_impl<StageProperties,
                               impl::types<InputTypes...>,
                               SelectorF,
                               Composed,
                               MapType>> {
  using base = typename group_by_impl::base;
  // For single-argument case, use the first input type
  using first_input = std::tuple_element_t<0, std::tuple<InputTypes...>>;
  using key = std::remove_cvref_t<std::invoke_result_t<SelectorF, first_input>>;

  SelectorF selector_f;
  Composed composed;

  using pipeline_type = decltype(impl::make_pipeline<impl::types<InputTypes...>,
                                                    impl::processing_style::incremental>(
      std::declval<Composed>()));

  using output_types = impl::types<key,
                                       decltype(std::declval<
                                           pipeline_type>().finish()) >;

  typename MapType::template type<key, pipeline_type> map;

  constexpr decltype(auto) process_incremental(InputTypes... inputs) {
    auto copy = [](auto &&t) { return t; };
    auto k = std::invoke(selector_f, std::forward<InputTypes>(inputs)...);
    auto iter = map.find(k);
    if (iter == map.end()) {
      bool b;
      std::tie(iter, b) = map.emplace(std::move(k),
                                      impl::make_pipeline<impl::types<InputTypes...>,
                                                         impl::processing_style::incremental>(
                                          copy(composed)));
    }

    if(!iter->second.done()){
      iter->second.process_incremental(std::forward<InputTypes>(inputs)...);
    }
  }

  constexpr decltype(auto) finish() {
    for (auto &&[k, v] : map) {
      this->next.process_incremental(k, v.finish());
    }
    return this->next.finish();
  }

};

template<typename MapType = detail::std_map, typename SelectorF, typename... Stages>
constexpr auto group_by(SelectorF selector_f, Stages... stages) {
  using C = decltype(compose(std::move(stages)...));
  return impl::make_stage<group_by_impl,
               impl::processing_style::incremental,
               impl::processing_style::incremental,
               SelectorF,
               C,
               MapType>(std::move(selector_f), compose(std::move(stages)...));
}

// Forward declaration
template<typename StageProperties, typename InputTypes, typename Comparer, typename Composed>
struct chunk_by_impl;

// Partial specialization to extract types from impl::types<...>
template<typename StageProperties, typename... InputTypes, typename Comparer, typename Composed>
struct chunk_by_impl<StageProperties, impl::types<InputTypes...>, Comparer, Composed>
    : impl::stage_impl<chunk_by_impl<StageProperties, impl::types<InputTypes...>, Comparer, Composed>> {
  using base = typename chunk_by_impl::base;
  using storage_type = std::tuple<value_storage<InputTypes>...>;

  using pipeline_type = decltype(impl::make_pipeline<impl::types<InputTypes...>,
                                                    impl::processing_style::incremental>(
      std::declval<Composed>()));

  using output_types = impl::types<decltype(std::declval<pipeline_type>().finish())>;

  Comparer comparer;
  Composed composed;
  std::optional<storage_type> stored_;
  std::optional<pipeline_type> pipeline_;

  template<typename... Stored, typename... Current>
  constexpr bool compare_items(std::tuple<Stored...>& stored_tuple, Current&&... current) {
    if constexpr (sizeof...(InputTypes) == 1) {
      // Single argument - pass directly
      return std::apply([&](auto&... stored) {
        return std::invoke(comparer, std::as_const(*stored)..., std::as_const(current)...);
      }, stored_tuple);
    } else {
      // Multiple arguments - wrap in forward_as_tuple
      return std::apply([&](auto&... stored) {
        return std::invoke(comparer,
                           std::forward_as_tuple(std::as_const(*stored)...),
                           std::forward_as_tuple(std::as_const(current)...));
      }, stored_tuple);
    }
  }

  constexpr void process_incremental(InputTypes... inputs) {
    auto copy = [](auto&& t) { return t; };

    if (!stored_.has_value()) {
      // First item - store it and create pipeline
      stored_.emplace(value_storage<InputTypes>(std::forward<InputTypes>(inputs))...);
      pipeline_.emplace(impl::make_pipeline<impl::types<InputTypes...>,
                                           impl::processing_style::incremental>(copy(composed)));
    } else {
      // Compare current item with stored item
      bool same_chunk = compare_items(stored_.value(), inputs...);

      if (same_chunk) {
        // Same chunk - send old item to pipeline, store new item
        std::apply([&](auto&&... stored) {
          pipeline_->process_incremental(*std::forward<decltype(stored)>(stored)...);
        }, std::move(stored_.value()));
        stored_.emplace(value_storage<InputTypes>(std::forward<InputTypes>(inputs))...);
      } else {
        // Different chunk - send old item to pipeline, finish pipeline, send result to next
        std::apply([&](auto&&... stored) {
          pipeline_->process_incremental(*std::forward<decltype(stored)>(stored)...);
        }, std::move(stored_.value()));
        this->next.process_incremental(pipeline_->finish());
        // Reset: store new item and create new pipeline
        stored_.emplace(value_storage<InputTypes>(std::forward<InputTypes>(inputs))...);
        pipeline_.emplace(impl::make_pipeline<impl::types<InputTypes...>,
                                             impl::processing_style::incremental>(copy(composed)));
      }
    }
  }

  constexpr decltype(auto) finish() {
    if (stored_.has_value() && pipeline_.has_value()) {
      // Send final stored item to pipeline and finish
      std::apply([&](auto&&... stored) {
        pipeline_->process_incremental(*std::forward<decltype(stored)>(stored)...);
      }, std::move(stored_.value()));
      this->next.process_incremental(pipeline_->finish());
    }
    return this->next.finish();
  }
};

template<typename Comparer, typename... Stages>
constexpr auto chunk_by(Comparer comparer, Stages... stages) {
  using C = decltype(compose(std::move(stages)...));
  return impl::make_stage<chunk_by_impl,
               impl::processing_style::incremental,
               impl::processing_style::incremental,
               Comparer,
               C>(std::move(comparer), compose(std::move(stages)...));
}

// Forward declaration
template<typename StageProperties, typename InputTypes, typename Composed>
struct chunk_impl;

// Partial specialization to extract types from impl::types<...>
template<typename StageProperties, typename... InputTypes, typename Composed>
struct chunk_impl<StageProperties, impl::types<InputTypes...>, Composed>
    : impl::stage_impl<chunk_impl<StageProperties, impl::types<InputTypes...>, Composed>> {
  using base = typename chunk_impl::base;

  using pipeline_type = decltype(impl::make_pipeline<impl::types<InputTypes...>,
                                                    impl::processing_style::incremental>(
      std::declval<Composed>()));

  using output_types = impl::types<decltype(std::declval<pipeline_type>().finish())>;

  size_t chunk_size;
  Composed composed;
  size_t current_count = 0;
  std::optional<pipeline_type> pipeline_;

  constexpr void process_incremental(InputTypes... inputs) {
    auto copy = [](auto&& t) { return t; };

    if (!pipeline_.has_value()) {
      // First item - create pipeline
      pipeline_.emplace(impl::make_pipeline<impl::types<InputTypes...>,
                                           impl::processing_style::incremental>(copy(composed)));
    }

    pipeline_->process_incremental(std::forward<InputTypes>(inputs)...);
    ++current_count;

    if (current_count >= chunk_size) {
      // Chunk complete - finish pipeline and send result to next
      this->next.process_incremental(pipeline_->finish());
      // Reset for next chunk
      current_count = 0;
      pipeline_.emplace(impl::make_pipeline<impl::types<InputTypes...>,
                                           impl::processing_style::incremental>(copy(composed)));
    }
  }

  constexpr decltype(auto) finish() {
    if (pipeline_.has_value() && current_count > 0) {
      // Send partial final chunk
      this->next.process_incremental(pipeline_->finish());
    }
    return this->next.finish();
  }
};

template<typename... Stages>
constexpr auto chunk(size_t n, Stages... stages) {
  using C = decltype(compose(std::move(stages)...));
  return impl::make_stage<chunk_impl,
               impl::processing_style::incremental,
               impl::processing_style::incremental,
               C>(std::size_t{n}, compose(std::move(stages)...));
}

// Forward declaration
template<typename StageProperties, typename InputTypes, typename... Composed>
struct tee_impl;

// Partial specialization to extract types from impl::types<...>
template<typename StageProperties, typename... InputTypes, typename... Composed>
struct tee_impl<StageProperties, impl::types<InputTypes...>, Composed...>
    : impl::stage_impl<tee_impl<StageProperties, impl::types<InputTypes...>, Composed...>> {
  using base = typename tee_impl::base;

  template<typename C>
  using pipeline_type = decltype(impl::make_pipeline<impl::types<InputTypes...>,
                                                    impl::processing_style::incremental>(
      std::declval<C>()));

  using output_types = impl::types<decltype(std::declval<
      pipeline_type<Composed>>().finish())&&...>;

  std::tuple<pipeline_type<Composed>...> pipelines;

  constexpr tee_impl(typename base::next_type &&next,
                     Composed... composed_stages)
      : base(std::move(next)),
        pipelines(make_pipeline_helper(composed_stages)...) {
  }

  constexpr void process_incremental(InputTypes... inputs) {
    std::apply([&](auto &... pipes) {
      (pipes.process_incremental(static_cast<InputTypes>(inputs)...), ...);
    }, pipelines);
  }

  constexpr decltype(auto) finish() {
    return std::apply([this](auto &&... pipes) {
      return this->next.process_complete(pipes.finish()...);
    }, pipelines);
  }

 private:
  template<typename C>
  static constexpr auto make_pipeline_helper(C c) {
    return impl::make_pipeline<impl::types<InputTypes...>,
                              impl::processing_style::incremental>(
        c);
  }
};

template<typename... Stages>
constexpr auto tee_helper(Stages... stages) {
  return impl::make_stage<tee_impl,
               impl::processing_style::incremental,
               impl::processing_style::complete,
               std::remove_cvref_t<Stages>...>(std::move(stages)...);
}

template<typename... Stages>
constexpr auto tee(Stages &&... stages) {
  return tee_helper((std::forward<Stages>(stages))...);
}

template<typename F>
struct generator:F {
    using F::operator();
};


template<typename F>
generator(F) -> generator<F>;

template<typename R>
constexpr auto SkydownSplMakeGenerator(
    R &&r)requires(std::ranges::range<std::remove_cvref_t<
    R>>) {
  return generator([begin = r.begin(), end = r.end()](auto &&out, auto&&... arg_stream)mutable {
    auto &&v = *begin;
    if constexpr (impl::calculate_type_v<decltype(out)>) {
      return out(std::forward<decltype(arg_stream)>(arg_stream)...,
          detail::move_if_movable_range<std::remove_cvref_t<R>>(std::forward<decltype(v)>(
              v)));

    } else {
      if (begin == end) {
        return false;
      }
      out(std::forward<decltype(arg_stream)>(arg_stream)...,
          detail::move_if_movable_range<std::remove_cvref_t<
              R>>(std::forward<decltype(v)>(
              v)));
      ++begin;
      return true;

    }
  });
}

template<typename Output, typename F>
constexpr auto SkydownSplOutput(Output &&output,
                                generator<F>&& g){
  if constexpr (impl::calculate_type_v<Output>) {
    return g(std::forward<Output>(output));
  } else {
    while(output && g(std::forward<Output>(output))){}
    return output;
  }
}


template<typename R>
constexpr auto zip(R&& r){
  return flat_map([g = SkydownSplMakeGenerator(std::forward<R>(r))](auto&& out, auto&&... args)mutable{
    return g(std::forward<decltype(out)>(out),std::forward<decltype(args)>(args)...);
  });
}


template<typename... Rs>
requires(sizeof...(Rs) > 1)
constexpr auto zip(Rs&&... rs){
  return compose(zip(std::forward<Rs>(rs))...);
}


inline constexpr auto iota(size_t start, size_t end) {
  return generator([start,end](auto&& output, auto&&... args)mutable{
    auto invoke = [&]{
      return output(std::forward<decltype(args)>(args)...,size_t(start));
    };

    if constexpr (impl::calculate_type_v<decltype(output)>) {
      return invoke();
    } else {
      if(start < end){
        invoke();
        ++start;
        return true;
      } else{
        return false;
      }
   }
  });
}
inline constexpr auto iota(size_t start) {
  return iota(start,std::numeric_limits<size_t>::max());
}




// Chain position enum
enum class chain_position {
  before,
  after
};

// Forward declaration
template<typename StageProperties, typename InputTypes, typename R, typename Position>
struct chain_impl;

// Partial specialization to extract types from impl::types<...>
template<typename StageProperties, typename... InputTypes, typename R, chain_position Position>
struct chain_impl<StageProperties, impl::types<InputTypes...>, R, std::integral_constant<chain_position, Position>>
    : impl::stage_impl<chain_impl<StageProperties, impl::types<InputTypes...>, R, std::integral_constant<chain_position,Position>>> {
  using base = typename chain_impl::base;
  using output_types = impl::types<InputTypes...>;

  [[no_unique_address]] R range;
  bool first_call = true;

  constexpr void process_incremental(InputTypes... inputs) {
    if constexpr (Position == chain_position::before) {
      if (first_call) {
        first_call = false;
        // Output all items from the chained sequence before the first element

        SkydownSplOutput(impl::incremental_outputter{this->next}, range);
      }
    }
    // Output the current input
    this->next.process_incremental(std::forward<InputTypes>(inputs)...);
  }

  constexpr decltype(auto) finish() {
    if (first_call || Position == chain_position::after) {
      // Output all items from the chained sequence after all inputs
      SkydownSplOutput(impl::incremental_outputter{this->next}, range);
    }
    return this->next.finish();
  }
};

template<typename R>
constexpr auto chain_before(R&& r) {
  return impl::make_stage<chain_impl,
               impl::processing_style::incremental,
               impl::processing_style::incremental,
               R,
               std::integral_constant<chain_position, chain_position::before>>(std::forward<R>(r));
}

template<typename R>
constexpr auto chain_after(R&& r) {
  return impl::make_stage<chain_impl,
               impl::processing_style::incremental,
               impl::processing_style::incremental,
               R,
               std::integral_constant<chain_position, chain_position::after>>(std::forward<R>(r));
}

template<typename... Types>
struct single_type_or_tuple{
  using type = std::tuple<Types...>;
};

template<typename Type>
struct single_type_or_tuple<Type>{
  using type = Type;
};

template<typename... Types>
using single_type_or_tuple_t = typename single_type_or_tuple<Types...>::type;

constexpr inline auto last(){
  return accumulate_in_place_with_init([]<typename... InputTypes>(impl::types<InputTypes...>) {
    return std::optional<single_type_or_tuple_t<std::remove_cvref_t<InputTypes>...>>{};
 }, [](auto &acc, auto &&... args) {
    acc.emplace(std::forward<decltype(args)>(args)...);
  });
}

constexpr inline auto first(){
  return compose(take(1),last());
}

template<typename Comp>
constexpr auto min(Comp comp){
  return accumulate_in_place_with_init([]<typename... InputTypes>(impl::types<InputTypes...>) {
    static_assert(sizeof...(InputTypes) == 1, "Cannot have max/min on multi-argument stream");
    return std::optional<std::remove_cvref_t<InputTypes>...>{};
 }, [comp = std::move(comp)](auto &acc, auto &&... args) {
    if(!acc.has_value() || comp(std::as_const(args)..., *acc)){
      acc.emplace(std::forward<decltype(args)>(args)...);
    }
  });
}

template<typename Comp>
constexpr auto max(Comp comp){
  return min([comp = std::move(comp)](auto&& a, auto&& b){return comp(b,a);});
}

constexpr inline auto min(){
  return min(std::less<>{});
}

constexpr inline auto max(){
 return max(std::less<>{});
}

template<typename Comp>
constexpr auto minmax(Comp comp){
  return spl::compose(spl::tee(min(comp),max(comp)),
  spl::transform_complete([]<typename T, typename U>(std::optional<T> a,std::optional<U> b){
    using type = std::optional<std::pair<T,U>>;
    if(a.has_value() && b.has_value()){
      return type(std::in_place,std::move(*a),std::move(*b));
    } else {
      return type();
    }
  }));
}

constexpr inline auto minmax(){
  return minmax(std::less<>());
}

template<typename Predicate>
constexpr inline auto any_of(Predicate pred){
  return spl::compose(spl::filter(pred),
  spl::transform([](auto&&...){return true;}),
  spl::first(),
  spl::transform_complete([](auto&& v){return v.value_or(false);})
);
}

template<typename Predicate>
constexpr inline auto none_of(Predicate pred){
  return spl::compose(spl::any_of(std::move(pred)),
  spl::transform_complete([](bool v){return !v;})
);
}

template<typename Predicate>
constexpr inline auto all_of(Predicate pred){
  return spl::none_of([pred = std::move(pred)](auto&&... args){
    return !pred(std::forward<decltype(args)>(args)...);
  });
}

template<typename C>
constexpr auto push_back_into(C& c){
  return spl::for_each([&c](auto&& arg){
    c.push_back(std::forward<decltype(arg)>(arg));
  });
}


// Forward declaration
template<typename StageProperties, typename InputTypes, typename Unwrapper>
struct unwrap_impl;

template<typename T>
struct is_optional_impl:std::false_type{};

template<typename T>
struct is_optional_impl<std::optional<T>>:std::true_type{};

template<typename T>
concept is_optional = is_optional_impl<std::remove_cvref_t<T>>::value;

template<typename T>
struct unwrapper_impl;

template<size_t TargetI, size_t I, typename T>
struct unwrap_optional_type_helper{
  using type = T;
 constexpr static decltype(auto) unwrap(T t){
   return static_cast<T>(t);
 }

constexpr static bool has_error(auto&&){
  return false;
 }

constexpr static void set_error(auto&&, bool&){
}
};

template<size_t I, typename T>
struct unwrap_optional_type_helper<I,I,T>{
 using type = decltype(*std::declval<T>());

 constexpr static decltype(auto) unwrap(T t){
   return *static_cast<T>(t);
 }

constexpr static bool has_error(auto&& t){
  return !t;
 }

constexpr static void set_error(auto&& t, bool& b){
  b = !t;
}
};


template<size_t I>
struct optional_unwrapper{

// We can't use nullopt_t as the error type since that will be stored in unwrap_impl as an optional.
 using error_type = bool;

  template<typename Out, typename... Ts>
  static constexpr decltype(auto) unwrap(Out&& out, Ts&&... ts ){

    return [&]<size_t... Is>(std::index_sequence<Is...>) {

    return out(unwrap_optional_type_helper<I, Is,Ts&&>::unwrap(std::forward<Ts>(ts))...);
    }(std::make_index_sequence<sizeof...(Ts)>{});
  }


  template<typename... Ts>
  static constexpr bool has_error(Ts&&... ts){
    return [&]<size_t... Is>(std::index_sequence<Is...>) {
      return (unwrap_optional_type_helper<I, Is,Ts&&>::has_error(std::as_const(ts)) || ...);
    }(std::make_index_sequence<sizeof...(Ts)>{});
  }

  template<typename... Ts>
  static constexpr error_type get_error(Ts&&... ts){
    bool b = false;
    return [&]<size_t... Is>(std::index_sequence<Is...>) {
      (unwrap_optional_type_helper<I, Is,Ts&&>::set_error(std::as_const(ts),b), ...);
      return b;
    }(std::make_index_sequence<sizeof...(Ts)>{});
  }

  template<is_optional U>
  static constexpr auto wrap_error(impl::types<U>, error_type){
    return std::remove_cvref_t<U>(std::nullopt);
  }

  template<typename U>
  static constexpr auto wrap_error(impl::types<U>, error_type){
    return std::optional<std::remove_cvref_t<U>>(std::nullopt);
  }


  template<is_optional U>
  static constexpr auto wrap_success(U&& v){
    return std::forward<U>(v);
  }

  template<typename U>
  static constexpr std::optional<std::remove_cvref_t<U>> wrap_success(U&& u){
    return std::optional<std::remove_cvref_t<U>>(std::forward<U>(u));
  }





};







// Partial specialization to extract types from impl::types<...>
template<typename StageProperties, typename... InputTypes, typename Unwrapper>
struct unwrap_impl<StageProperties,
                     impl::types<InputTypes...>, Unwrapper>
    : impl::stage_impl<unwrap_impl<StageProperties,
                               impl::types<InputTypes...>,Unwrapper>> {
  using base = typename unwrap_impl::base;
  using output_types = decltype(Unwrapper::unwrap(
      impl::type_calculating_outputter(),
      std::declval<InputTypes>()...));


  static_assert(detail::is_types<output_types>::value);
  using unwrapper = Unwrapper;

  std::optional<typename unwrapper::error_type> error;
  bool done_ = false;




  constexpr decltype(auto) process_incremental(InputTypes... inputs) {
    if(unwrapper::has_error(std::as_const(inputs)...)){
      done_ = true;
      error = unwrapper::get_error(std::as_const(inputs)...);
    } else{
      unwrapper::unwrap(impl::incremental_outputter{this->next}, static_cast<InputTypes>(inputs)...);
    }
  }

  constexpr decltype(auto) finish() {
    if(error.has_value()){
      return unwrapper::wrap_error(impl::types<decltype(this->next.finish())>(),*std::move(error));
    }else{
      return unwrapper::wrap_success(this->next.finish());
    }

  }

  constexpr bool done() const {
    return done_ || this->next.done();
  }

};

inline constexpr auto unwrap_optional(){
  return impl::make_stage<unwrap_impl,
               impl::processing_style::incremental,
               impl::processing_style::incremental, optional_unwrapper<0>>();
}

template<size_t I>
inline constexpr auto unwrap_optional_arg(){
  return impl::make_stage<unwrap_impl,
               impl::processing_style::incremental,
               impl::processing_style::incremental, optional_unwrapper<I>>();
}

// Forward declaration
template<typename StageProperties, typename InputTypes>
struct as_incremental_impl;

// Partial specialization to extract types from impl::types<...>
template<typename StageProperties, typename... InputTypes>
struct as_incremental_impl<StageProperties, impl::types<InputTypes...>>
    : impl::stage_impl<as_incremental_impl<StageProperties, impl::types<InputTypes...>>> {
  using base = typename as_incremental_impl::base;
  using output_types = impl::types<InputTypes...>;

  constexpr decltype(auto) process_complete(InputTypes... inputs) {
    this->next.process_incremental(std::forward<InputTypes>(inputs)...);
    return this->next.finish();
  }
};

inline constexpr auto as_incremental() {
  return impl::make_stage<as_incremental_impl,
               impl::processing_style::complete,
               impl::processing_style::incremental>();
}


}


#endif //SKYDOWN_SPL_H_
