#ifndef SKYDOWN_SPL_STAGES_H_
#define SKYDOWN_SPL_STAGES_H_

#include "spl_core.h"
#include <algorithm>
#include <map>
#include <optional>

namespace spl {

// ============================================================================
// Forward Declarations - User-facing Stage Functions
// ============================================================================

// Accumulation stages
template<typename T, typename F>
constexpr auto accumulate_in_place(T t, F f);

template<typename TypeCalculator, typename F>
constexpr auto accumulate_in_place_with_type_calculator(TypeCalculator t, F f);

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

constexpr auto take(size_t n);

template<typename Predicate>
constexpr auto take_while(Predicate predicate);

constexpr auto skip(size_t n);

template<typename Predicate>
constexpr auto skip_while(Predicate predicate);

constexpr auto filter_duplicates();

// Flattening and mapping stages
template<typename F>
constexpr auto flat_map(F f);

template<size_t I, typename F>
constexpr auto flat_map_arg(F f);

constexpr auto flatten();

template<size_t I>
constexpr auto flatten_arg();

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
  using accumulated_type = std::remove_cvref_t<first_type_t<std::invoke_result_t<
      T,
      types<InputTypes...>>>>;
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
  constexpr auto type_calculator = [](auto) { return types<T>(); };
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
requires(std::is_invocable_v<T, types<int &&>>)
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
                                               accumulated,
                                               std::forward<decltype(v)>(
                                                   v));
                             });
}

template<typename F>
constexpr auto accumulate(F f) {
  return accumulate_in_place([]<typename... Types>(types<Types...>) {
                               static_assert(sizeof...(Types) == 1,
                                             "Accumulate needs to only have one input");
                               return types<std::remove_cvref_t<Types>...>();
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

// Partial specialization to extract types from types<...>
template<typename StageProperties, typename... InputTypes, typename F>
struct flat_map_impl<StageProperties,
                     types<InputTypes...>,
                     F>
    : stage_impl<flat_map_impl<StageProperties,
                               types<InputTypes...>,
                               F>> {
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

template<typename F>
constexpr auto flat_map(F f) {
  return stage<flat_map_impl,
               processing_style::incremental,
               processing_style::incremental,
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

    if constexpr (calculate_type_v<Out>) {
      return invoke(std::make_index_sequence<I>{}, std::make_index_sequence<sizeof...(Inputs) - I - 1>{});
    } else {
      invoke(std::make_index_sequence<I>{}, std::make_index_sequence<sizeof...(Inputs) - I - 1>{});
      return true;
    }
  });
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

constexpr inline auto filter(){
  return filter(std::identity{});
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
    if constexpr (calculate_type_v<Out>) {
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
    if constexpr (calculate_type_v<Out>) {
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
    if constexpr (calculate_type_v<Out>) {
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

// Forward declaration
template<typename StageProperties, typename InputTypes>
struct filter_duplicates_impl;

// Partial specialization to extract types from types<...>
template<typename StageProperties, typename... InputTypes>
struct filter_duplicates_impl<StageProperties, types<InputTypes...>>
    : stage_impl<filter_duplicates_impl<StageProperties, types<InputTypes...>>> {
  using base = typename filter_duplicates_impl::base;
  using output_types = types<InputTypes...>;
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
  return stage<filter_duplicates_impl,
               processing_style::incremental,
               processing_style::incremental>();
}

// Forward declaration
template<typename StageProperties, typename InputTypes, typename F>
struct transform_cps_impl;

// Partial specialization to extract types from types<...>
template<typename StageProperties, typename... InputTypes, typename F>
struct transform_cps_impl<StageProperties, types<InputTypes...>, F>
    : stage_impl<transform_cps_impl<StageProperties, types<InputTypes...>, F>> {
  using base = typename transform_cps_impl::base;

  using output_types = decltype(std::invoke(
      std::declval<F>(),
      type_calculating_outputter(),
      std::declval<InputTypes>()...));

  static_assert(is_types<output_types>::value);

  [[no_unique_address]] F f{};

  constexpr void process_incremental(InputTypes... inputs)
      requires(incremental_input<transform_cps_impl>) {
    std::invoke(f,
                [this](auto&&... outputs) {
                  this->next.process_incremental(std::forward<decltype(outputs)>(outputs)...);
                },
                std::forward<InputTypes>(inputs)...);
  }

  constexpr decltype(auto) process_complete(InputTypes... inputs)
      requires(complete_input<transform_cps_impl>) {
    return std::invoke(f,
                       [this](auto&&... outputs) -> decltype(auto) {
                         return this->next.process_complete(std::forward<decltype(outputs)>(outputs)...);
                       },
                       std::forward<InputTypes>(inputs)...);
  }
};

template<typename F>
constexpr auto transform_cps(F f) {
  return stage<transform_cps_impl,
               processing_style::incremental,
               processing_style::incremental,
               F>(std::move(f));
}

template<typename F>
constexpr auto transform(F f) {
  return transform_cps([f = std::move(f)](auto &&out, auto &&... inputs) {
    return out(std::invoke(f, std::forward<decltype(inputs)>(inputs)...));
  });
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
    if constexpr (calculate_type_v<decltype(out)>) {
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
  return stage<transform_cps_impl,
               processing_style::complete,
               processing_style::complete,
               F>(std::move(f));
}

template<typename F>
constexpr auto transform_complete(F f) {
  return transform_complete_cps([f = std::move(f)](auto &&out,
                                                   auto &&... inputs) {
    return out(std::invoke(f, std::forward<decltype(inputs)>(inputs)...));
  });
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

template<size_t I>
constexpr auto flatten_arg() {
  return flat_map_arg<I>([](auto&& out, auto&& before, auto&& arg, auto&& after) {
    return std::apply([&](auto&&... before_args) {
      return std::apply([&](auto&&... after_args) {
        return SkydownSplOutput(as_outputter(out,
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

template<template<typename...> typename C>
constexpr auto push_back_to() {
  return accumulate_in_place([]<typename... Inputs>(types<Inputs...>) {
    static_assert(sizeof...(Inputs) == 1,
                  "push_back_to can only take 1 input argument");
    return types<C<std::remove_cvref_t<first_type_t<types<Inputs...>>>>>();
  }, [](auto &c, auto &&v) {
    c.push_back(std::forward<decltype(v)>(v));
  });

}

constexpr auto to_vector() {
  return push_back_to<std::vector>();
}

template<template<typename, typename> typename MapType>
constexpr auto to_map() {
  return accumulate_in_place_with_type_calculator([]<typename... Inputs>(types<
      Inputs...>) {
    static_assert(sizeof...(Inputs) == 2, "to_map needs 2 input arguments");
    return types<MapType<std::remove_cvref_t<Inputs>...>>();
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

  using output_types = types<key,
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
                                      spl::make_pipeline<types<InputTypes...>,
                                                         processing_style::incremental>(
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

  using output_types = types<decltype(std::declval<
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
    return spl::make_pipeline<types<InputTypes...>,
                              processing_style::incremental>(
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
    if constexpr (calculate_type_v<decltype(out)>) {
      return out(std::forward<decltype(arg_stream)>(arg_stream)...,
          move_if_movable_range<std::remove_cvref_t<R>>(std::forward<decltype(v)>(
              v)));

    } else {
      if (begin == end) {
        return false;
      }
      out(std::forward<decltype(arg_stream)>(arg_stream)...,
          move_if_movable_range<std::remove_cvref_t<
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
  if constexpr (calculate_type_v<Output>) {
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

    if constexpr (calculate_type_v<decltype(output)>) {
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

// Partial specialization to extract types from types<...>
template<typename StageProperties, typename... InputTypes, typename R, chain_position Position>
struct chain_impl<StageProperties, types<InputTypes...>, R, std::integral_constant<chain_position, Position>>
    : stage_impl<chain_impl<StageProperties, types<InputTypes...>, R, std::integral_constant<chain_position,Position>>> {
  using base = typename chain_impl::base;
  using output_types = types<InputTypes...>;
  
  [[no_unique_address]] R range;
  bool first_call = true;
  
  constexpr void process_incremental(InputTypes... inputs) {
    if constexpr (Position == chain_position::before) {
      if (first_call) {
        first_call = false;
        // Output all items from the chained sequence before the first element

        SkydownSplOutput(incremental_outputter{this->next}, range);
      }
    }
    // Output the current input
    this->next.process_incremental(std::forward<InputTypes>(inputs)...);
  }
  
  constexpr decltype(auto) finish() {
    if (first_call || Position == chain_position::after) {
      // Output all items from the chained sequence after all inputs
      SkydownSplOutput(incremental_outputter{this->next}, range);
    }
    return this->next.finish();
  }
};

template<typename R>
constexpr auto chain_before(R&& r) {
  return stage<chain_impl,
               processing_style::incremental,
               processing_style::incremental,
               R,
               std::integral_constant<chain_position, chain_position::before>>(std::forward<R>(r));
}

template<typename R>
constexpr auto chain_after(R&& r) {
  return stage<chain_impl,
               processing_style::incremental,
               processing_style::incremental,
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
  return accumulate_in_place([]<typename... InputTypes>(types<InputTypes...>) {
    return types<std::optional<single_type_or_tuple_t<std::remove_cvref_t<InputTypes>...>>>();
 }, [](auto &acc, auto &&... args) {
    acc.emplace(std::forward<decltype(args)>(args)...);
  });
}

constexpr inline auto first(){
  return compose(take(1),last());
}

template<typename Comp>
constexpr auto min(Comp comp){
  return accumulate_in_place([]<typename... InputTypes>(types<InputTypes...>) {
    static_assert(sizeof...(InputTypes) == 1, "Cannot have max/min on multi-argument stream");
    return types<std::optional<std::remove_cvref_t<InputTypes>...>>();
 }, [comp = std::move(comp)](auto &acc, auto &&... args) {
    if(!acc.has_value() || comp(std::as_const(args)...,acc)){
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
  static constexpr auto wrap_error(types<U>, error_type){
    return std::remove_cvref_t<U>(std::nullopt);
  }

  template<typename U>
  static constexpr auto wrap_error(types<U>, error_type){
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







// Partial specialization to extract types from types<...>
template<typename StageProperties, typename... InputTypes, typename Unwrapper>
struct unwrap_impl<StageProperties,
                     types<InputTypes...>, Unwrapper>
    : stage_impl<unwrap_impl<StageProperties,
                               types<InputTypes...>,Unwrapper>> {
  using base = typename unwrap_impl::base;
  using output_types = decltype(Unwrapper::unwrap(
      type_calculating_outputter(),
      std::declval<InputTypes>()...));


  static_assert(is_types<output_types>::value);
  using unwrapper = Unwrapper;

  std::optional<typename unwrapper::error_type> error;
  bool done_ = false;




  constexpr decltype(auto) process_incremental(InputTypes... inputs) {
    if(unwrapper::has_error(std::as_const(inputs)...)){
      done_ = true;
      error = unwrapper::get_error(std::as_const(inputs)...);
    } else{
      unwrapper::unwrap(incremental_outputter{this->next}, static_cast<InputTypes>(inputs)...);
    }
  }

  constexpr decltype(auto) finish() {
    if(error.has_value()){
      return unwrapper::wrap_error(types<decltype(this->next.finish())>(),*std::move(error));
    }else{
      return unwrapper::wrap_success(this->next.finish());
    }

  }

  constexpr bool done() const {
    return done_ || this->next.done();
  }

};

inline constexpr auto unwrap_optional(){
  return stage<unwrap_impl,
               processing_style::incremental,
               processing_style::incremental, optional_unwrapper<0>>();
}

template<size_t I>
inline constexpr auto unwrap_optional_arg(){
  return stage<unwrap_impl,
               processing_style::incremental,
               processing_style::incremental, optional_unwrapper<I>>();
}

// Forward declaration
template<typename StageProperties, typename InputTypes>
struct as_incremental_impl;

// Partial specialization to extract types from types<...>
template<typename StageProperties, typename... InputTypes>
struct as_incremental_impl<StageProperties, types<InputTypes...>>
    : stage_impl<as_incremental_impl<StageProperties, types<InputTypes...>>> {
  using base = typename as_incremental_impl::base;
  using output_types = types<InputTypes...>;

  constexpr decltype(auto) process_complete(InputTypes... inputs) {
    this->next.process_incremental(std::forward<InputTypes>(inputs)...);
    return this->next.finish();
  }
};

inline constexpr auto as_incremental() {
  return stage<as_incremental_impl,
               processing_style::complete,
               processing_style::incremental>();
}


}


#endif //SKYDOWN_SPL_STAGES_H_
