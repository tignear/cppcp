#pragma once
#include <iterator>
#include <functional>
#include <optional>
#include <tuple>
namespace tig::parser {

	template<class Src, class R>
	class ret;
	template<class Src, class R,class T>
	struct parser {
	private:
	public:
		using source_type = Src;
		using result_type = R;
		using self_type = T;
		constexpr ret<Src,R> operator()(Src&& src)const {
			return static_cast<const T&>(*this).parse(std::move(src));
		}
	};

	
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
		using to = parser_f<Src, Out, Fn>;
		Fn f_;
		constexpr parser_builder(Fn f):f_(f) {

		}
		template<class Src, class Out>
		constexpr to<Src,Out> build() {
			return parser_f<Src, Out, Fn>{f_};
		}
		/*template<class Src, class Out>
		constexpr to<Src, Out> operator()() {
			return parser_f<Src, Out, Fn>{f};
		}*/
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
	inline constexpr std::optional<T> typing_nullopt() {
		return std::nullopt;
	}

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
				return ret{ src,typing_nullopt<typename std::iterator_traits<Src>::value_type>() };
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
				return typing_nullopt<typename std::iterator_traits<Src>::value_type>();

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
	template <bool b,class R>
	struct join_type_single_supplier {
		using result_type = std::tuple<R>;
	};
	template<class R>
	struct join_type_single_supplier<true,R> {
		using result_type = std::tuple<>;
	};
	template<template <class Target> class SkipJudge, class... >
	struct join_type_supplier 
	{ 
		using result_type = std::tuple<>;
	};
	template<template <class Target> class SkipJudge,class R,class... Rs>
	struct join_type_supplier<SkipJudge,R,Rs...> {
		using result_type = decltype(
			std::tuple_cat
			(
				std::declval<typename join_type_single_supplier<SkipJudge<R>::value,R>::result_type>(), std::declval<typename join_type_supplier<SkipJudge,Rs...>::result_type>()
			)
		);
	};
	template<size_t v>
	struct not_zero {
		const static constexpr bool value = true;
	};
	template<>
	struct not_zero<size_t(0)> {
		const static constexpr bool value = false;
	};
	/*template <class Src, template <class Target> class SkipJudge, class R1,class Fn>
	constexpr auto join(
		parser<Src, R1,Fn> p1,
		typename std::enable_if<SkipJudge<R1>::value>::type* = 0
	) {
		return parser{ [=](Src&& src) {
			return p1(std::move(src)).flatMap<Src, std::tuple<>>([](auto&& itr,auto&& ignore) {
				return ret<Src, std::tuple<>>{itr, std::make_tuple()};
			});
		} };
	}
	template <class Src, template <class Target> class SkipJudge,class R1,class S>
	constexpr auto join(
		parser<Src, R1,S> p1,
		std::enable_if_t <!SkipJudge<R1>::value>* = 0
	) {
		return parser_builder{ [=](Src&& src) {
			return p1(std::move(src)).map< std::tuple<R1>>([](auto&& e) {return std::make_tuple(e); });
		} }.build<Src, std::tuple<R1>>();
	}
	template <class Src, class R1,class S>
	constexpr auto join(
		parser<Src, R1,S> p1,
		std::enable_if_t < !is_skip_tag<R1>::value>* = 0
	) {
		return join<Src, is_skip_tag, R1,S>(p1);
	}
	template <class Src, class R1,class Fn>
	constexpr auto join(
		parser<Src, R1,Fn> p1,
		std::enable_if_t < is_skip_tag<R1>::value>* = 0
	) {
		return join<Src, is_skip_tag, R1,Fn>(p1);
	}

	template <class Src, template <class Target> class SkipJudge,class R1,class Fn,class... RFs>
	constexpr auto join(
		parser<Src, R1,Fn> p1, parser<Src, decltype(std::get<0>(std::declval<RFs>())), decltype(std::get<1>(std::declval<RFs>()))>...  ps,
		std::enable_if_t<not_zero<sizeof...(RFs)>::value>* = 0
	) {
		return typename parser_type<Src, typename join_type_supplier<SkipJudge, R1, decltype(std::get<0>(std::declval<RFs>()))...>::result_type>::type{ [=](Src&& src) {
			auto&& r = join<Src, SkipJudge,R1>(p1)(std::move(src));
			auto&& r2 = join<Src, SkipJudge,Rs...>(ps...)(std::move(r.itr()));
			return ret<Src, typename join_type_supplier<SkipJudge,R1, Rs...>::result_type>{ r2.itr(),std::tuple_cat(std::tuple{std::move(r.get())},r2.get()) };
		} };
	}
	template <class Src,class R1,class Fn, class... RFs>
	constexpr auto join(
		parser<Src, R1,Fn> p1, parser<Src, decltype(std::get<0>(std::declval<RFs>())), decltype(std::get<1>(std::declval<RFs>()))>...  ps,
		std::enable_if_t<not_zero<sizeof...(RFs)>::value>* = 0
	) {
		return join<Src, is_skip_tag, R1, RFs...>(p1,ps...);
	}*/



	template<class Src,class Tuple,size_t index,size_t size>
	constexpr ret<Src,
		decltype(
			std::tuple_cat(
				std::declval<std::tuple<typename std::remove_reference_t<decltype(std::get<index>(std::declval<Tuple>()))>::result_type>>(),
				std::declval<decltype(join_impl<Src,Tuple,index+1,size>(std::declval<Src>(),std::declval<Tuple>()).get())>()
			)
		)
	>join_impl(Src&& src, Tuple tuple, std::enable_if_t<index != size>* = 0) {
		auto  r = std::get<index>(tuple)(std::move(src));
		auto r2 = join_impl<Src, Tuple, index + size_t(1), size>(std::move(r.itr()), std::move(tuple));
		return ret{r2.itr(), std::tuple_cat(std::tuple{ r.get() }, r2.get())};
	}
	template<class Src, class Tuple, size_t index, size_t size>
	constexpr ret<Src,std::tuple<>> join_impl(Src&& src, Tuple&& tuple, std::enable_if_t<index == size>* = 0) {
		return ret{ src,std::make_tuple() };
	}
	template<class T>
	using result_type_t = typename std::remove_reference_t<T>::result_type;
	template<class Src, class... Parsers>
	struct join :public parser<Src, std::tuple<result_type_t<Parsers>...>, join<Src ,Parsers...>> {
		std::tuple<Parsers...> ps_;
		constexpr join(Parsers... ps):ps_(std::tuple{ ps... }) {}
		constexpr join(std::tuple<Parsers...> ps) : ps_(ps) {}

		constexpr ret<Src, std::tuple<result_type_t<std::remove_reference_t<Parsers>>...>> parse(
			Src&& src
		)const {
			return join_impl<Src, std::tuple<std::remove_reference_t<Parsers>...>,0,sizeof...(Parsers)>(std::move(src),ps_);
		}
	};
	

	template<class Src>
	struct join<Src> :public parser<Src, std::tuple<>, join<Src>> {
		constexpr ret<Src, std::tuple<>> parse(Src&& src)const {
			return ret{ src,std::make_tuple() };
		}
	};

	template<class Ph ,class... Pt>
	struct parser_type_traits {
		using source_type =typename std::remove_reference_t<Ph>::source_type;
	};
	template<class... Parsers>
	join(
		Parsers... p
	)->join<typename parser_type_traits<Parsers...>::source_type,Parsers...>;



	template<class Src,class V,class Fn>
	constexpr auto skip(parser<Src, V,Fn> p) {
		return typename parser_type<Src, skip_tag>::type{ [=](Src&& src) {
			return ret<Src, skip_tag> { p(std::move(src)).itr(), skip_tag{} };
		} };
	}
	template<class Src,class V,class Fn>
	constexpr auto skipN(parser<Src, V,Fn> p,typename std::iterator_traits<Src>::difference_type n) {
		return typename parser_type<Src, skip_tag>::type{ [=](Src&& src) {
			auto&& itr = src;
			for (auto i = 0; i < n; ++i)
			{
				itr = p(std::move(itr)).itr();
			}
			return ret<Src, skip_tag>{itr, skip_tag{}};
		} };
	}
	template<class Src>
	constexpr typename parser_type<Src, skip_tag>::type skipN(typename std::iterator_traits<Src>::difference_type n) {
		return [=](Src&& src) {
			std::advance(src, n);
			return ret<Src, skip_tag>{ src,skip_tag{} };
		};
	}
	template<class Src,class Fn,class... Rs>
	constexpr parser<Src,std::tuple<Rs...>,Fn> empty_tuple_as_skip(
		parser<Src,std::tuple<Rs...>,Fn> p,
		std::enable_if_t<not_zero<sizeof...(Rs)>::value>* = 0
	) {
		return p;
	}
	template<class Src,class Fn, class... Rs>
	constexpr typename parser_type<Src, skip_tag>::type empty_tuple_as_skip(
		parser<Src, std::tuple<Rs...>,Fn> p,
		std::enable_if_t<!not_zero<sizeof...(Rs)>::value>* = 0
	) {
		return [=](Src&& src) {
			return ret<Src, skip_tag>{p(std::move(src)).itr(), skip_tag{}};
		};
	}
	
	template<class Src,class M, class Fn,class R=M>
	constexpr typename parser_type<Src, R>::type map(parser<Src,M,Fn> p,std::function<R(M)> mapping) {
		return [=](Src&& src) {
			return p(std::move(src)).map<R>(mapping);
		};
	}



}