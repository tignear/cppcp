#pragma once
#include <iterator>
#include <functional>
#include <optional>
#include <tuple>
#include <variant>
namespace tig::cppcp {
	template<class... Ts> struct overloaded : Ts... { using Ts::operator()...; };
	template<class... Ts> overloaded(Ts...)->overloaded<Ts...>;
	template<class Target, class... Other>
	struct is_same_any {

	};
	template<class Target>
	struct is_same_any<Target> {
		constexpr static bool value = false;
	};
	template<class Target,class OtherH,class... OtherT>
	struct is_same_any<Target, OtherH,OtherT...> {
		constexpr static bool value = std::is_same_v<Target, OtherH>|| is_same_any<Target, OtherT...>::value;
	};
	template<class Target, class... Other>
	constexpr bool is_same_any_v = is_same_any<Target, Other...>::value;


	template<class Tuple,size_t index>
	using get_t = decltype(std::get<index>(std::declval<Tuple>()));

	template < class Target,class Tuple, size_t ...I >
	struct tupled_is_same_any_impl {
		constexpr tupled_is_same_any_impl(Target,Tuple,std::index_sequence<I...>) {

		}
		constexpr static bool value = is_same_any_v < std::decay_t<Target>, std::decay_t< get_t<std::decay_t<Tuple>, I>>... > ;
	};
	template <class Target>
	struct tupled_is_same_any_impl<std::tuple<>, Target> {
		constexpr tupled_is_same_any_impl(Target, std::tuple<>, std::index_sequence<>) {

		}
		constexpr static bool value =false;
	};
	template < class Target, class Tuple >
	struct tupled_is_same_any {
		constexpr static bool value = decltype(tupled_is_same_any_impl(std::declval<Target>(), std::declval<Tuple>(),std::make_index_sequence<std::tuple_size_v<std::decay_t<Tuple>>>{}))::value;
	};
	template < class Target, class Tuple >
	constexpr bool tupled_is_same_any_v = tupled_is_same_any<Target,Tuple>::value;

	template <class Tuple, size_t index, class... Processed>
	struct unique_type_impl;
	template <bool end, class Tuple, size_t index, class... Processed>
	struct unique_type_impl_if {

	};
	template <class Tuple, size_t index, class... Processed>
	struct unique_type_impl_if<true,Tuple,index, Processed...>{
		using type = std::tuple<Processed...>;
	};
	template <class Tuple, size_t index, class... Processed>
	struct unique_type_impl_if<false, Tuple, index, Processed...> {
		using type = std::conditional_t<
			tupled_is_same_any_v<std::decay_t<get_t<Tuple, index>>, std::tuple<Processed...>>,
			typename unique_type_impl<Tuple, index + 1, Processed...>::type,
			typename unique_type_impl<Tuple, index + 1, get_t<Tuple, index>, Processed...>::type
		>;
	};
	template <class Tuple, size_t index, class... Processed>
	struct unique_type_impl {
		using type = typename unique_type_impl_if<index == std::tuple_size_v<Tuple>, Tuple, index, std::decay_t<Processed>...>::type;
	};

	template <class... Targets>
	using unique_type_t =typename unique_type_impl<std::tuple<Targets...>, 0>::type;
	template < class From, size_t ...I >
	struct convart_wrapping_object_impl {
		constexpr convart_wrapping_object_impl(From, std::index_sequence<I...>){}
		template < template<class...> class To >
		struct value {
			using type=To<std::decay_t<get_t<From, I>>...>;
		};
	};
	template < class From, template<class...> class To >
	struct convart_wrapping_object {
		using type = typename decltype(decltype(convart_wrapping_object_impl(
			std::declval<From>(),std::make_index_sequence<std::tuple_size_v<std::decay_t<From>>>{}
		))::value<To>{})::type;
	};
	template < class From, template<class...> class To >
	using convart_wrapping_object_t = typename convart_wrapping_object<From, To>::type;

	template<class Src, class R>
	class ret;

	template<class Src, class R,class T>
	struct parser {
		using source_type = Src;
		using result_type = R;
		using self_type = T;
		constexpr ret<Src,R> operator()(Src&& src)const {
			return static_cast<const T&>(*this).parse(std::move(src));
		}
	};

	template<class Ph, class... Pt>
	struct parser_type_traits {
		using source_type = typename std::remove_reference_t<Ph>::source_type;
		using result_type = typename std::remove_reference_t<Ph>::result_type;
		using self_type = typename std::remove_reference_t<Ph>::self_type;
		constexpr static bool is_parser=std::is_base_of_v<parser<source_type, result_type, Ph>, Ph>;
	};
	template<class... T>
	using result_type_t = typename parser_type_traits<T...>::result_type;
	template<class... T>
	using source_type_t = typename parser_type_traits<T...>::source_type;
	template<class... T>
	using self_type_t = typename parser_type_traits<T...>::self_type;


	template<class Src, class Out>
	struct parser_type {
		template<class Fn>
		using type = parser<Src, Out, Fn>;
	};

	template<class Src, class R,class Fn>
	class parser_f:public parser<Src,R,parser_f<Src,R,Fn>> {
		Fn f_;
	public:
		using source_type = Src;
		using result_type = R;
		using self_type = parser_f<Src, R, Fn>;
		constexpr parser_f(Fn f) :f_(f) {}
		constexpr ret<Src, R> parse(Src&& src)const{
			return f_(std::move(src));
		}
	};

	template<class Fn>
	struct parser_builder{
		template<class Src,class Out>
		using To = parser_f<Src, Out, Fn>;
		Fn f_;
		constexpr parser_builder(Fn f):f_(f) {

		}
		template<class Src, class Out>
		constexpr To<Src,Out> build() {
			return parser_f<Src, Out, Fn>{f_};
		}
	};

	template<class Src,class R>
	class ret {	
		Src itr_;
		R ret_;
	public:
		using value_type = R;
		using source_type = Src;

		ret(Src itr, R ret) :itr_(itr), ret_(ret) {}
		R&& get()&& {
			return std::move(ret_);
		}
		R& get() & {
			return ret_;
		}
		R* operator->() {
			return &ret;
		}
		template<class NR>
		constexpr ret<Src, NR> map(std::function<NR(R&&)> fn)&& {
			return ret<Src,NR>(itr_,fn(std::move(ret_)));
		}
		template<class NR>
		constexpr ret<Src, R> map(std::function<NR(R)> fn) & {
			return ret(itr(), fn(get()));
		}
		template<class NSrc>
		constexpr ret<NSrc, R> mapI(std::function<NSrc(Src&&)> fn) && {
			return ret(fn(itr()), get());
		}
		template<class NSrc>
		constexpr ret<NSrc, R> mapI(std::function<NSrc(Src)> fn) & {
			return ret(fn(itr()), get());
		}

		template<class NSrc,class NR>
		constexpr ret<NSrc, NR> flatMap(std::function<ret<NSrc,NR>(Src&&,R&&)> p) && {
			return p(std::move(itr_),std::move(ret_));
		}
		template<class NSrc,class NR>
		constexpr ret<NSrc, NR> flatMap(std::function<ret<NSrc, NR>(Src, R)> p) & {
			return p(itr(), get());
		}
		constexpr Src&& itr()&&{
			return std::move(itr_);
		}
		constexpr Src itr() & {
			return itr_;
		}
	}; 

	template<class T>
	constexpr std::optional<T> typing_nullopt = std::nullopt;

	template<class T,class A>
	inline constexpr std::function<std::pair<bool,A>(A,std::optional<T>)> exit_if_nullopt(std::function<A(A,T)> fn) {
		return [=](auto&& a,auto&& e) {
			if (e) {
				return std::make_pair(true, fn(a,e.value()));
			}
			else
			{
				return std::make_pair(false, a);
			}
		};
	}

	template<class Src>
	constexpr auto any()
	{
		auto f= [](Src&& itr) {
			typename std::iterator_traits<Src>::value_type r = *itr;
			++itr;
			return ret<Src,typename std::iterator_traits<Src>::value_type>{ itr,r };
		};
		return parser_builder(f).build< Src, typename std::iterator_traits<Src>::value_type >();
	}
	template<class Src>
	struct any_unless_end:public parser<Src, std::optional<typename std::iterator_traits<Src>::value_type>, any_unless_end<Src>>
	{
	private:
		Src end_;
	public:
		using source_type = Src;
		using result_type = std::optional<typename std::iterator_traits<Src>::value_type>;
		using self_type = any_unless_end<Src>;
		constexpr any_unless_end(Src end):end_(end) {

		}
		constexpr ret<Src, std::optional<typename std::iterator_traits<Src>::value_type>> parse(Src&& src)const {
			if (src == end_) {
				return ret{ src,typing_nullopt<typename std::iterator_traits<Src>::value_type> };
			}
			return any<Src>()(std::move(src))
				.map<std::optional<typename std::iterator_traits<Src>::value_type>>(
					[](auto&& e) {
						return std::make_optional(e);
					}
				);
		}
	};
	template<class Src>
	constexpr auto satisfy(
		std::function<bool(const typename std::iterator_traits<Src>::reference)> fn
	)
	{
		auto f= [=](Src&& itr) {
			return any<Src>()(std::move(itr)).map<std::optional<typename std::iterator_traits<Src>::value_type>>([=](typename std::iterator_traits<Src>::value_type&& e) {
				if (fn(e)) {
					return std::make_optional<typename std::iterator_traits<Src>::value_type>(e);
				}
				return typing_nullopt<typename std::iterator_traits<Src>::value_type>;

			});
		};
		return parser_f<Src, std::optional<typename std::iterator_traits<Src>::value_type>,decltype(f)>{f };
	}
	template<class Src,class R,class RF>
	constexpr auto supLazy(
		RF supF
	)
	{
		auto f = [=](Src&& src) {
			return ret<Src, R>{src,supF()};
		};
		return parser<Src, R, decltype(f)>{
			f
		};
	}
	template<class Src, class R>
	constexpr auto supLazy(
		std::function<R()> supF
	)
	{
		auto f = [=](Src&& src) {
			return ret<Src, R>{src, supF()};
		};
		return parser<Src, R, decltype(f)>{
			f
		};
	}
	template<class Src, class RT>
	constexpr auto sup(RT v)
	{
		auto f = [=](Src&& src) {
			return ret<Src, RT>{src, v};
		};
		return parser_f<Src, RT, decltype(f)>{
			f
		};
	}
	template<class Src,class RT, RT v>
	constexpr auto sup()
	{
		auto f = [=](Src&& src) {
			return ret<Src,RT>{src, rv};
		};
		return parser_f<Src, R, decltype(f)>{
			f
		};
	}
	template<class P1,class P2,class Accumulator>
	class fold:public parser<typename P1::source_type,typename P2::result_type,fold<P1,P2, Accumulator>> {
		P1 p_;
		P2 init_;
		Accumulator accumulator_;
	public:
		constexpr fold(
			P1 p, P2/*std::function<R(void)>*/ init,
			Accumulator/*std::function<std::pair<bool,R>(R&&,Value&&)>*/ accumulator
		):parser(*this),p_(p),init_(init),accumulator_(accumulator) {

		}
		constexpr ret<typename parser::source_type, typename parser::result_type> parse(typename P1::source_type&& src)const{
			auto&& rs = init_(std::move(src));
			auto&& itr = rs.itr();
			auto&& current = rs.get();
			do {
				auto&& r = p_(std::move(itr));
				itr = r.itr();
				auto&& ar = accumulator_(std::move(current), std::move(r.get()));
				current = std::move(ar.second);
				if (!ar.first) {
					break;
				}
			} while (true);
			return ret{std::move(itr),std::move(current)};
		};
	};


	template<class Src, class Value,class S,class Accumulator>
	constexpr auto reduce(
		S p,
		Accumulator/*std::function<std::pair<bool, Value>(Value&&, Value&&)>*/ accumulator) {
		auto f= [=](Src&& src)
		{
			ret<Src,Value> r1 = p(std::move(src));
			auto&& itr = r1.itr();
			auto&& current = r1.get();
			do {
				auto&& r = p(std::move(itr));
				itr = r.itr();
				auto&& ar = accumulator(std::move(current), std::move(r.get()));
				current = std::move(ar.second);
				if (!ar.first) {
					break;
				}

			} while (true);
			return ret{ std::move(itr),std::move(current) };
		};
		return parser_f<Src,Value,decltype(f)>{ f };
	}
	/*template <class Src>
	constexpr auto join() {
		return parser_builder{ [=](Src&& src) {
			return ret<Src, std::tuple<>>{ src,{} };
		} }.build<Src, std::tuple<>>();
	}*/
	template <class T>
	struct is_skip_tag {
		static const constexpr bool value = false;
	};
	struct skip_tag {
	};
	template <>
	struct is_skip_tag<skip_tag> {
		static const constexpr bool value = true;
	};

	template<size_t v>
	struct not_zero {
		const static constexpr bool value = true;
	};
	template<>
	struct not_zero<size_t(0)> {
		const static constexpr bool value = false;
	};

	/*
	join start
	*/
	template<bool b, class P>
	struct join_result_type_single_supplier {
		using type = std::tuple<P>;
	};
	template<class P>
	struct join_result_type_single_supplier<true, P> {
		using type = std::tuple<>;
	};
	template<template<class Target> class SkipJudge, class... Ps>
	struct join_result_type_supplier {};
	template<template<class Target> class SkipJudge, class Ph, class... Pt>
	struct join_result_type_supplier<SkipJudge, Ph, Pt...> {
		using type = decltype(
			std::tuple_cat(
				std::declval<typename join_result_type_single_supplier<SkipJudge<result_type_t<Ph>>::value, result_type_t<Ph>>::type>(),
				std::declval<typename join_result_type_supplier<SkipJudge, Pt...>::type >()
			)
			);
	};
	template<template<class Target> class SkipJudge>
	struct join_result_type_supplier<SkipJudge> {
		using type = std::tuple<>;
	};

	template<class Src,class Tuple,template<class Target> class SkipJudge,size_t index,size_t size>
	constexpr auto join_impl(
		Src&& src,
		Tuple tuple,
		std::enable_if_t<index != size>* = 0,
		std::enable_if_t<
			!SkipJudge<
				typename std::remove_reference_t<decltype(std::get<index>(std::declval<Tuple>()))>::result_type>::value
		>* =0
	) {
		auto r = std::get<index>(tuple)(std::move(src));
		auto r2 = join_impl<Src, Tuple, SkipJudge, index + size_t(1), size>(std::move(r.itr()), std::move(tuple));
		return ret{ r2.itr(), std::tuple_cat(std::tuple{ r.get() }, r2.get()) };
	}
	template<class Src, class Tuple, template<class Target> class SkipJudge, size_t index, size_t size>
	constexpr auto join_impl(
		Src&& src,
		Tuple tuple,
		std::enable_if_t<index != size>* = 0,
		std::enable_if_t<
			SkipJudge<typename std::remove_reference_t<decltype(std::get<index>(std::declval<Tuple>()))>::result_type>::value
		>* = 0
	) {
		auto r = std::get<index>(tuple)(std::move(src));
		auto r2 = join_impl<Src, Tuple, SkipJudge, index + size_t(1), size>(std::move(r.itr()), std::move(tuple));
		return ret{ r2.itr(), r2.get() };
	}
	template<class Src, class Tuple, template<class Target> class SkipJudge, size_t index, size_t size>
	constexpr ret<Src,std::tuple<>> join_impl(Src&& src, Tuple&& tuple, std::enable_if_t<index == size>* = 0) {
		return ret{ src,std::make_tuple() };
	}

	template<class Src, template<class Target> class SkipJudge, class... Parsers>
	struct join :public parser<
		Src,
		typename join_result_type_supplier<SkipJudge, Parsers...>::type,
		join<Src, SkipJudge, Parsers...>
	> {
		join() = delete;
	};

	template<class Src, template<class Target> class SkipJudge,class P1,class... Parsers>
	class join<Src, SkipJudge, P1,Parsers...> :public parser<
		Src,
		typename join_result_type_supplier<SkipJudge,P1,Parsers...>::type,
		join<Src , SkipJudge,P1,Parsers...>
	> {
		std::tuple<P1,Parsers...> ps_;
	public:
		constexpr join(P1 p1,Parsers... ps):ps_(std::tuple{ p1,ps... }) {}
		constexpr join(std::tuple<P1,Parsers...> ps) : ps_(ps) {}

		constexpr decltype(
			join_impl<Src, std::tuple<P1,std::remove_reference_t<Parsers>...>, SkipJudge, 0, 1+sizeof...(Parsers)>(std::move(std::declval<Src>()), std::declval<std::tuple<P1,Parsers...>>())
		)parse(
			Src&& src
		)const {
			return join_impl<Src,std::tuple<P1, std::remove_reference_t<Parsers>...>, SkipJudge,0,1+sizeof...(Parsers)>(std::move(src),ps_);
		}
	};
	

	template<class Src, template<class Target> class SkipJudge>
	struct join<Src, SkipJudge> :public parser<Src, std::tuple<>, join<Src, SkipJudge>> {
		constexpr join() {}
		constexpr ret<Src, std::tuple<>> parse(Src&& src)const {
			return ret{ src,std::make_tuple() };
		}
	};
	template<class Src>
	struct join<Src,is_skip_tag> :public parser<Src, std::tuple<>, join<Src, is_skip_tag>> {
		constexpr join(){}
		constexpr ret<Src, std::tuple<>> parse(Src&& src)const {
			return ret{ src,std::make_tuple() };
		}
	};

	template<class... Parsers, std::enable_if_t<parser_type_traits<Parsers...>::is_parser, nullptr_t> = nullptr>
	join(
		Parsers... p
	)->join<typename parser_type_traits<Parsers...>::source_type,is_skip_tag,Parsers...>;

	template<template<class Target> class SkipJudge,class... Parsers, std::enable_if_t<parser_type_traits<Parsers...>::is_parser, nullptr_t> =nullptr>
	join(
		Parsers... p
	)->join<typename parser_type_traits<Parsers...>::source_type, SkipJudge, Parsers...>;
	template<class Src>
	join()->join<Src,is_skip_tag>;
	/*
		pure join end
		custom join start
	*/
	template <template<class Target> class SkipJudge>
	struct join_c {
		join_c() = delete;
		template <class... Parsers>
		constexpr static auto join(Parsers... ps) {
			return cppcp::join<typename parser_type_traits<Parsers...>::source_type, SkipJudge, Parsers...>{ps...};
		}
	};
	/*
	custom join end
	*/
	template<class Src,class V,class Fn>
	constexpr auto skip(parser<Src, V,Fn> p) {
		return parser_builder{ [=](Src&& src) {
			return ret<Src, skip_tag> { p(std::move(src)).itr(), skip_tag{} };
		} }.build<Src, skip_tag>();
	}
	template<class Src,class V,class Fn>
	constexpr auto skipN(parser<Src, V,Fn> p,typename std::iterator_traits<Src>::difference_type n) {
		return parser_builder{ [=](Src&& src) {
			auto&& itr = src;
			for (auto i = 0; i < n; ++i)
			{
				itr = p(std::move(itr)).itr();
			}
			return ret<Src, skip_tag>{itr, skip_tag{}};
		} }.build<Src, skip_tag>();
	}
	template<class Src>
	constexpr typename auto skipN(typename std::iterator_traits<Src>::difference_type n) {
		return parser_builder{ [=](Src&& src) {
			std::advance(src, n);
			return ret<Src, skip_tag>{ src,skip_tag{} };
		} }.build<Src, skip_tag>();
	}
	template<class Src,class Fn,class... Rs>
	constexpr parser<Src,std::tuple<Rs...>,Fn> empty_tuple_as_skip(
		parser<Src,std::tuple<Rs...>,Fn> p,
		std::enable_if_t<not_zero<sizeof...(Rs)>::value>* = 0
	) {
		return p;
	}
	template<class Src,class Fn, class... Rs>
	constexpr typename auto empty_tuple_as_skip(
		parser<Src, std::tuple<Rs...>,Fn> p,
		std::enable_if_t<!not_zero<sizeof...(Rs)>::value>* = 0
	) {
		return parser_builder{ [=](Src&& src) {
			return ret<Src, skip_tag>{p(std::move(src)).itr(), skip_tag{}};
		} }.build<Src, skip_tag>();
	}
	template<class P, class F>
	class map:public parser<typename parser_type_traits<P>::source_type,std::invoke_result_t<F,result_type_t<P>>,map<P,F>> {
		P p_;
		F mapping_;
	public:
		constexpr map(P p, F mapping):p_(p),mapping_(mapping) {}
		constexpr auto parse(source_type_t<P>&& src)const {
			return p_(std::move(src)).map<std::invoke_result_t<F, result_type_t<P>>>(mapping_);
		}
	};
	template<class Src, class F, class R, class Tuple, size_t index, size_t size>
	constexpr R trys_impl(Src&&, F,Tuple, std::enable_if_t<index == size>* = 0) {
		throw std::invalid_argument("");
	}

	template<class Src,class F,class R,class Tuple,size_t index,size_t size>
	constexpr R trys_impl(Src&& src,F f,Tuple tuple,std::enable_if_t<index!=size>* =0) {
		auto m = src;
		auto r = std::get<index>(tuple)(std::move(src));
		if (f(r.get())) {
			return { r.itr(),r.get() };
		}
		return trys_impl<Src, F, R,Tuple,index+1,size>(std::move(m),f,tuple);
	}

	template<class F, class... Parsers>
	class trys:public parser<
		typename parser_type_traits<Parsers...>::source_type,
		typename std::common_type<result_type_t<Parsers>...>::type,
		trys<F,Parsers...>
	> {
		F f_;
		std::tuple<Parsers...> parsers_;
	public:
		constexpr trys(F f, Parsers... parsers) :f_(f), parsers_{ parsers... } {}
		constexpr trys(F f) : f_(f), parsers{} {
			static_assert(false,"parsers are one or more args required.");
		}

		constexpr ret<
			typename parser_type_traits<Parsers...>::source_type,
			typename std::common_type<result_type_t<Parsers>...>::type
		> parse(typename parser_type_traits<Parsers...>::source_type&& src)const {
			return trys_impl<typename parser_type_traits<Parsers...>::source_type, F, ret<
				typename parser_type_traits<Parsers...>::source_type,
				typename std::common_type<result_type_t<Parsers>...>::type
			>, std::tuple<Parsers...> ,0,sizeof...(Parsers)>(
				std::move(src),f_,parsers_
				);
		}
	};
	template<class F,class... Parsers>
	class trys_variant:public parser<
		typename parser_type_traits<Parsers...>::source_type,
		convart_wrapping_object_t<unique_type_t<result_type_t<Parsers>...>, std::variant>,
		trys_variant<F,Parsers...>
	> {
		F f_;
		std::tuple<Parsers...> ps_;
	public:
		constexpr trys_variant(F f, Parsers... ps) :f_(f), ps_{ ps...}{}
		constexpr ret<
			source_type_t<Parsers...>,
			convart_wrapping_object_t<unique_type_t<result_type_t<Parsers>...>, std::variant>
		> parse(source_type_t<Parsers...>&& src)const {
			return trys_impl<
				source_type_t<Parsers...>,
				F,
				ret<source_type_t<Parsers...>, convart_wrapping_object_t<unique_type_t<result_type_t<Parsers>...>,std::variant>>,
				std::tuple<Parsers...>,
				0,
				sizeof...(Parsers)
			>(std::move(src), f_, ps_);
		}
	};
}