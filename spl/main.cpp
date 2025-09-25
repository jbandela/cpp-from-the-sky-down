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
      auto& child = static_cast<child_type &>(*this);
      if(child.done()) break;
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
    typename Parameters = void, typename... TypeParameters>
class stage {
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
  constexpr explicit stage(Ts &&... ts)
      :parameters_{std::forward<Ts>(ts)...} {}

  template<typename StageProperties, typename Next>
  constexpr auto make(Next &&next) {
    if constexpr (std::is_same_v<Parameters, void>) {
      return StageImpl<StageProperties, TypeParameters...>{
          std::forward<Next>(next)};
    } else {
      return StageImpl<StageProperties, TypeParameters...>{
          std::forward<Next>(next),
          std::move(parameters_)};
    }
  }
};

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

  constexpr bool done() const {return false;}

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
               F,
               F>{std::move(f)};
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
  [[no_unique_address]] accumulated_type accumulated{};
  [[no_unique_address]] F f{};

  constexpr accumulate_in_place_impl(auto &&stage_properties,
                                     std::tuple<accumulated_type, F> &&tuple)
      : base(
      std::forward<decltype(stage_properties)>(stage_properties)),
        accumulated(std::get<0>(tuple)),
        f(std::get<1>(tuple)) {}

  constexpr accumulate_in_place_impl(auto &&stage_properties, F &&f)
      : base(
      std::forward<decltype(stage_properties)>(stage_properties)),
        accumulated{},
        f(std::move(f)) {}

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
               std::tuple<T, F>,
               T,
               F>(std::make_tuple(std::move(t),
                                  std::move(f)));
}

template<template<typename...> typename T, typename F>
constexpr auto accumulate_in_place(F f) {
  return stage<accumulate_in_place_impl,
               processing_style::incremental,
               processing_style::complete,
               F,
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
  bool done_ =  false;

  constexpr decltype(auto) process_incremental(input_type input) {
    auto out =  [this](auto&& output){
      this->next.process_incremental(std::forward<decltype(output)>(output));
    };
    done_ = !std::invoke(f,out,  this->forward(input));
  }

  constexpr bool done() const{
    return done_;
  }

};

struct IdentityOutputCalculator{
  template<typename T>
  using type = T;
};

template<typename OutputCalculator = IdentityOutputCalculator, typename F>
constexpr auto flat_map(F f){
  return stage<flat_map_impl,processing_style ::incremental, processing_style ::incremental,
  F, F, OutputCalculator>(std::move(f));

}


template<typename Predicate>
constexpr auto filter(Predicate predicate) {
  return flat_map([predicate = std::move(predicate)](auto& output,
      auto&& input){
    if(std::invoke(predicate,std::as_const(input))){
     output(std::forward<decltype(input)>(input));
    }
    return true;
  });
}

template<typename F>
struct TransformOutputCalculator{
    template<typename Input>
    using type = std::invoke_result_t<F,Input>;
};

template<typename F>
constexpr auto transform(F f){
  return flat_map<TransformOutputCalculator<F>>([f = std::move(f)](auto& out, auto&& input){
    out(std::invoke(f,std::forward<decltype(input)>(input)));
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

#include <vector>

constexpr auto calculate() {
  constexpr std::array v{1, 2, 3, 4};
  auto t = spl::compose(
      spl::transform([](int i){return i*2;}),
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
