#pragma once
#include <iterator>
#include <functional>
#include <optional>
#include <tuple>
#include <variant>
namespace tig::cppcp {
	constexpr auto always_true = [](const auto&) {return true; };
	constexpr auto always_false = [](const auto&) {return false; };

	template<class... Ts> struct overloaded : Ts... { using Ts::operator()...; };
	template<class... Ts> overloaded(Ts...)->overloaded<Ts...>;

	enum class either_tag {
		LEFT,
		RIGHT,
	};
	template<class Left, class Right>
	class either {

		const union
		{
			Left left_;
			Right right_;
		};
	public:
		const either_tag tag_;
		using left_type=Left;
		using right_type = Right;
		template<class T=std::enable_if_t<std::is_same_v<Left,Right>,nullptr_t>>
		constexpr either(Left const& v, either_tag tag) : tag_(tag) {
			switch (tag) {
			case either_tag::LEFT:
				left_ = v;
				break;
			case either_tag::RIGHT:
				right_ = v;
				break;
			}
		}
		template<class T=std::enable_if_t<std::is_same_v<Left, Right>, nullptr_t >>
		constexpr either(Right const& v, either_tag tag) : tag_(tag) {
			switch (tag) {
			case either_tag::LEFT:
				left_ = v;
				break;
			case either_tag::RIGHT:
				right_ = v;
				break;
			}
		}
		constexpr either(Left const& left) : tag_(either_tag::LEFT) {
			left_ = left;
		}

		constexpr either(Right const& right) : tag_(either_tag::RIGHT) {
			right_ = right;
		}

		~either() {
			switch (tag_) {
			case either_tag::LEFT:
				left_.~Left();
				break;
			case either_tag::RIGHT:
				right_.~Right();
				break;
			}
		}

		constexpr either(const either& r) : tag_(r.tag_) {
			switch (tag_) {
			case either_tag::LEFT:
				left_ = r.left_;
				break;
			case either_tag::RIGHT:
				right_ = r.right_;
				break;
			}
		}
		constexpr either(either&& r) : tag_(r.tag_) {
			switch (tag_) {
			case either_tag::LEFT:
				left_ = std::move(r.left_);
				break;
			case either_tag::RIGHT:
				right_ = std::move(r.right_);
				break;
			}
		}
		constexpr Left left()const {
			if (either_tag::LEFT != tag_) {
				throw "invalid get operation";
			}
			return left_;
		}
		constexpr Right right()const {
			if (either_tag::RIGHT != tag_) {
				throw "invalid get operation";
			}
			return right_;
		}
		template<class T=typename std::common_type<Left, Right>::type>
		constexpr T value()const{
			switch (tag_) {
			case either_tag::LEFT:
				return left_;
			case either_tag::RIGHT:
				return right_;
			}
		}
		constexpr either_tag tag()const {
			return tag_;
		}
		constexpr bool isLeft()const {
			return tag_ == either_tag::LEFT;
		}
		constexpr bool isRight()const {
			return tag_ == either_tag::RIGHT;
		}
		template<class F>
		constexpr void get(F f) {
			switch (tag_)
			{
			case either_tag::LEFT:
				f(left_);
				break;
			case either_tag::RIGHT:
				f(right_);
				break;
			default:
				break;
			}
		}
	};

	template <typename T>
	struct left_value {
		constexpr explicit left_value(T t) : t(t) {}

		template <typename Left, typename Right>
		constexpr operator either<Left, Right>() const {
			if constexpr (std::is_same_v<Left,Right>) {
				return either<Left, Right>(t, either_tag::LEFT);
			}
			else {
				return either<Left, Right>(t);
			}
		}

	private:
		T t;
	};

	template <typename T>
	constexpr left_value<T> left(T t) {
		return left_value<T>(t);
	}
	template <typename T>
	struct right_value {
		constexpr explicit right_value(T t) : t(t) {}

		template <typename Left, typename Right>
		constexpr operator either<Left, Right>() const {
			if constexpr (std::is_same_v<Left, Right>) {
				return either<Left, Right>(t, either_tag::RIGHT);
			}
			else {
				return either<Left, Right>(t);
			}
		}

	private:
		T t;
	};

	template <typename T>
	constexpr right_value<T> right(T t) {
		return right_value<T>(t);
	}

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

	struct parser_base {

	};
	template<class Src, class R,class T>
	struct parser :public parser_base{
	protected:
		constexpr parser() = default;
	public:
		using source_type = Src;
		using result_type = R;
		using self_type = T;
		constexpr ret<Src,R> operator()(Src&& src)const {
			return static_cast<const T&>(*this).parse(std::move(src));
		}
	};
	template<class Src, class R, class T>
	struct parser_def_helper:public parser<Src,R,T>
	{
	protected:
		using ret_type = ret<Src, R>;
	};
	template<class Ph, class... Pt>
	struct parser_type_traits {
		using source_type = typename std::remove_reference_t<Ph>::source_type;
		using result_type = typename std::remove_reference_t<Ph>::result_type;
		using self_type = typename std::remove_reference_t<Ph>::self_type;
	};


	template<class... T>
	using result_type_t = typename parser_type_traits<T...>::result_type;
	template<class... T>
	using source_type_t = typename parser_type_traits<T...>::source_type;
	template<class... T>
	using self_type_t = typename parser_type_traits<T...>::self_type;
	template<class Ph, class... Pt>
	constexpr static bool is_parser_v = std::is_base_of_v<parser_base,Ph>;

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
	class parser_exception_base:public std::exception {

	};
	class parser_exception:public parser_exception_base {
		
	};
	template<class E>
	class uncaught_parser_exception :public parser_exception_base {
		E ex_;
	public:
		uncaught_parser_exception(E ex):ex_(ex) {

		}
		E get_reason_exception()const {
			return ex_;
		}
	};
	class eof_exception :public parser_exception {

	};
	template<class Src,class R>
	class ret {	
		Src itr_;
		R ret_;
	public:
		using value_type = R;
		using source_type = Src;

		constexpr ret(Src itr, R ret) :itr_(itr), ret_(ret) {}
		constexpr R&& get()&& {
			return std::move(ret_);
		}
		constexpr R& get() & {
			return ret_;
		}
		constexpr R* operator->() {
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
				return cppcp::accm::contd(fn(a,e.value()));
			}
			else
			{
				return cppcp::accm::terminate(a);
			}
		};
	}
	namespace itr {
		template<class Src>
		struct any :public parser<Src, typename std::iterator_traits<Src>::value_type, any<Src>>
		{

			constexpr ret<Src, typename std::iterator_traits<Src>::value_type> parse(Src&& src)const {
				auto&& v = *src;
				src++;
				return ret<Src, typename std::iterator_traits<Src>::value_type>{src, v};
			}
		};
		template<class Src>
		struct any_unless_end :public parser<Src, std::optional<typename std::iterator_traits<Src>::value_type>, any_unless_end<Src>>
		{
		private:
			Src end_;
			any<Src> p_{};
		public:
			constexpr any_unless_end(Src end) :end_(end) {

			}
			constexpr ret<Src,typename parser::result_type> parse(Src&& src)const {
				if (src == end_) {
					return ret{ src,std::optional<typename std::iterator_traits<Src>::value_type>() };
				}
				auto&& r = p_(std::move(src));
				return ret{r.itr(), std::optional<typename std::iterator_traits<Src>::value_type>(r.get()) };
				
			}
		};
	}
	template<class P, class F, class RT = std::optional<result_type_t<P>>>
	class filter:public parser<source_type_t<P>, RT, filter<P,F, RT>>{
		F f_;
		P p_;
		RT fail_;
	public:
		constexpr filter(P p,F f,RT fail =typing_nullopt<result_type_t<P>>):p_(p),f_(f),fail_(fail) {}
		constexpr ret<source_type_t<P>, RT> parse(source_type_t<P>&& src)const {
			auto&& x=p_(std::move(src));
			if (f_(x.get())) {
				return ret<source_type_t<P>, RT>(x.itr(),x.get() );
			}
			return ret<source_type_t<P>, RT>(src,fail_);

		}
	};
	template<class P, class F, class RT = std::optional<result_type_t<P>>>
	class reject :public parser<source_type_t<P>, RT, reject<P, F, RT>> {
		F f_;
		P p_;
		RT fail_;
	public:
		constexpr reject(P p, F f, RT fail = typing_nullopt<result_type_t<P>>) :p_(p), f_(f), fail_(fail) {}
		constexpr ret<source_type_t<P>, RT> parse(source_type_t<P>&& src)const {
			auto&& x = p_(std::move(src));
			if (!f_(x.get())) {
				return ret<source_type_t<P>, RT>(x.itr(), x.get());
			}
			return ret<source_type_t<P>, RT>(src, fail_);

		}
	};
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
			return ret<Src,RT>{src, v};
		};
		return parser_f<Src, RT, decltype(f)>{
			f
		};
	}
	namespace accm {
		template<class T>
		constexpr std::pair<bool,T> contd(T t) {
			return std::make_pair(false, t);
		}
		template<class T>
		constexpr std::pair<bool, T> terminate(T t) {
			return std::make_pair(true, t);
		}
		template<class Pair>
		constexpr bool is_terminate(const Pair p) {
			return p.first;
		}
	}
	template<class PI, class PA, class Accumulator>
	class fold:public parser<typename PI::source_type,typename PI::result_type,fold<PI,PA, Accumulator>> {
		PA p_;
		PI init_;
		Accumulator accumulator_;
	public:
		constexpr fold(
			 PI/*std::function<R(void)>*/ init, PA p,
			Accumulator/*std::function<either<R,R>>(R&&,Value&&)>*/ accumulator
		):p_(p),init_(init),accumulator_(accumulator) {

		}
		constexpr ret<typename parser::source_type, typename parser::result_type> parse(typename PI::source_type&& src)const{
			auto&& rs = init_(std::move(src));
			auto&& itr = rs.itr();
			auto&& current = rs.get();
			do {
				try {
					auto&& r = p_(std::move(itr));
					itr = r.itr();
					auto&& ar = accumulator_(std::move(current), std::move(r.get()));
					current = std::move(ar.second);
					if (ar.first) {
						break;
					}
				}
				catch (parser_exception) {
					break;
				}
			} while (true);
			return ret{std::move(itr),std::move(current)};
		};
	};


	template<class Src, class Value,class S,class Accumulator>
	constexpr auto reduce(
		S p,
		Accumulator/*std::function<either<R,R>(Value&&, Value&&)>*/ accumulator) {
		auto f= [=](Src&& src)
		{
			ret<Src,Value> r1 = p(std::move(src));
			auto&& itr = r1.itr();
			auto&& current = r1.get();
			do {
				try {

					auto&& r = p(std::move(itr));
					itr = r.itr();
					auto&& ar = accumulator(std::move(current), std::move(r.get()));
					current = std::move(ar.second);
					if (ar.first) {
						break;
					}
				}
				catch (parser_exception) {
					break;
				}
			} while (true);
			return ret{ std::move(itr),std::move(current) };
		};
		return parser_f<Src,Value,decltype(f)>{ f };
	}

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
		return ret{ r2.itr(), std::tuple_cat(std::tuple{r.get() }, r2.get()) };
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

	template<class Src, class... Parsers>
	struct join :public parser<
		Src,
		typename join_result_type_supplier<is_skip_tag, Parsers...>::type,
		join<Src, Parsers...>
	> {
		std::tuple< std::remove_reference_t<Parsers>...> ps_;
		constexpr join( Parsers... ps) :ps_(std::tuple{ ps... }) {}

		constexpr decltype(
			join_impl<source_type_t<Parsers...>, std::tuple< std::remove_reference_t<Parsers>...>, is_skip_tag, 0, sizeof...(Parsers)>(std::move(std::declval<source_type_t<Parsers...>>()), std::declval<std::tuple< Parsers...>>())
			)parse(
				source_type_t<Parsers...>&& src
			)const {
			return join_impl<source_type_t<Parsers...>, std::tuple< std::remove_reference_t<Parsers>...>, is_skip_tag, 0, sizeof...(Parsers)>(std::move(src), ps_);
		}
	};
	template<class Src, template<class Target> class SkipJudge, class... Parsers>
	struct join_c_impl :public parser<
		Src,
		typename join_result_type_supplier<SkipJudge, Parsers...>::type,
		join_c_impl<Src, SkipJudge, Parsers...>
	> {
		join_c_impl() = delete;
	};
	template<template<class Target> class SkipJudge,class P1,class... Parsers>
	class join_c_impl<source_type_t<P1>, SkipJudge, P1,Parsers...> :public parser<
		source_type_t<P1>,
		typename join_result_type_supplier<SkipJudge,P1,Parsers...>::type,
		join_c_impl<source_type_t<P1>, SkipJudge,P1,Parsers...>
	> {
		std::tuple<P1,Parsers...> ps_;
	public:
		constexpr join_c_impl(P1 p1,Parsers... ps):ps_(std::tuple{ p1,ps... }) {}

		constexpr decltype(
			join_impl<source_type_t<P1>, std::tuple<P1,std::remove_reference_t<Parsers>...>, SkipJudge, 0, 1+sizeof...(Parsers)>(std::move(std::declval<source_type_t<P1>>()), std::declval<std::tuple<P1,Parsers...>>())
		)parse(
			source_type_t<P1>&& src
		)const {
			return join_impl<source_type_t<P1>,std::tuple<P1, std::remove_reference_t<Parsers>...>, SkipJudge,0,1+sizeof...(Parsers)>(std::move(src),ps_);
		}
	};
	template<  class P1, class... Parsers>
	class join<source_type_t<P1>, P1, Parsers...> :public parser<
		source_type_t<P1>,
		typename join_result_type_supplier<is_skip_tag, P1, Parsers...>::type,
		join<source_type_t<P1>, P1, Parsers...>
	> {
		std::tuple<P1, Parsers...> ps_;
	public:
		constexpr join(P1 p1, Parsers... ps) :ps_(std::tuple{ p1,ps... }) {}
		constexpr join(std::tuple<P1, Parsers...> ps) : ps_(ps) {}

		constexpr decltype(
			join_impl<source_type_t<P1>, std::tuple<P1, std::remove_reference_t<Parsers>...>, is_skip_tag, 0, 1 + sizeof...(Parsers)>(std::move(std::declval<source_type_t<P1>>()), std::declval<std::tuple<P1, Parsers...>>())
			)parse(
				source_type_t<P1>&& src
			)const {
			return join_impl<source_type_t<P1>, std::tuple<P1, std::remove_reference_t<Parsers>...>, is_skip_tag, 0, 1 + sizeof...(Parsers)>(std::move(src), ps_);
		}
	};

	template<class Src, template<class Target> class SkipJudge>
	struct join_c_impl<Src, SkipJudge> :public parser<Src, std::tuple<>, join_c_impl<Src, SkipJudge>> {
		constexpr join_c_impl() {}
		constexpr ret<Src, std::tuple<>> parse(Src&& src)const {
			return ret{ src,std::make_tuple() };
		}
	};
	template<class Src>
	struct join<Src> :public parser<Src, std::tuple<>, join<Src>> {
		constexpr join(){}
		constexpr ret<Src, std::tuple<>> parse(Src&& src)const {
			return ret{ src,std::make_tuple() };
		}
	};

	template<class... Parsers>
	join(
		Parsers... p
	)->join<typename parser_type_traits<Parsers...>::source_type,Parsers...>;

	template<template<class Target> class SkipJudge,class... Parsers, std::enable_if_t<is_parser_v<Parsers...>, nullptr_t> =nullptr>
	join_c_impl(
		Parsers... p
	)->join_c_impl<typename parser_type_traits<Parsers...>::source_type, SkipJudge, Parsers...>;
	template<class Src>
	join()->join<Src>;
	/*
		pure join end
		custom join start
	*/
	template <template<class Target> class SkipJudge>
	struct join_c {
		join_c() = delete;
		template <class... Parsers>
		constexpr static auto join(Parsers... ps) {
			return cppcp::join_c_impl<typename parser_type_traits<Parsers...>::source_type, SkipJudge, Parsers...>{ps...};
		}
	};
	/*
	custom join end
	*/
	template<class P>
	constexpr auto skip(P p) {
		return parser_builder{ [=](source_type_t<P>&& src) {
			return ret<source_type_t<P>, skip_tag> { p(std::move(src)).itr(), skip_tag{} };
		} }.build<source_type_t<P>, skip_tag>();
	}
	template<class P>
	constexpr auto skipN(P p,typename std::iterator_traits<source_type_t<P>>::difference_type n) {
		return parser_builder{ [=](source_type_t<P>&& src) {
			auto&& itr = src;
			for (auto i = 0; i < n; ++i)
			{
				itr = p(std::move(itr)).itr();
			}
			return ret<source_type_t<P>, skip_tag>{itr, skip_tag{}};
		} }.build<source_type_t<P>, skip_tag>();
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
	/*
	* trys start
	*/
	class all_of_parser_failed_exception :public parser_exception {

	};
	template<class Src, class F, class R, class Tuple, size_t index, size_t size>
	constexpr R trys_impl(Src&&, F,Tuple, std::enable_if_t<index == size>* = 0) {
		throw all_of_parser_failed_exception();
	}

	template<class Src,class F,class R,class Tuple,size_t index,size_t size>
	constexpr R trys_impl(Src&& src,F f,Tuple tuple,std::enable_if_t<index!=size>* =0) {
		auto m = src;
		try {
			auto r = std::get<index>(tuple)(std::move(src));
			if (f(r.get())) {
				return { r.itr(),r.get() };
			}
		}
		catch(const parser_exception&){
			//next
		}
		return trys_impl<Src, F, R,Tuple,index+1,size>(std::move(m),f,tuple);
	}

	template<class F, class... Parsers>
	class trys_internal :public parser<
		typename parser_type_traits<Parsers...>::source_type,
		typename std::common_type<result_type_t<Parsers>...>::type,
		trys_internal<F,Parsers...>
	> {
		F f_;
		std::tuple<Parsers...> parsers_;
	public:
		constexpr trys_internal(F f, Parsers... parsers) :f_(f), parsers_{ parsers... } {}


		constexpr trys_internal(F f) : f_(f), parsers{} {
			static_assert(false,"parsers are one or more args required.");
		}
		//constexpr trys(Parsers... parsers, std::enable_if_t<is_parser_v<Parsers>>* = nullptr) : f_(always_true), parsers_{ parsers... } {}

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
	template<class H,class... Parsers>
	class trys :public parser <
		source_type_t<std::conditional_t<is_parser_v<H>, trys_internal<decltype(always_true), H, Parsers...>, trys_internal<H, Parsers...>>>,
		result_type_t<std::conditional_t<is_parser_v<H>, trys_internal<decltype(always_true), H, Parsers...>, trys_internal<H, Parsers...>>>,
		trys<H, Parsers...>
	> {
		using T = std::conditional_t<is_parser_v<H>, trys_internal<decltype(always_true), H, Parsers...>, trys_internal<H, Parsers...>>;
		T internal_;
	public:
		template<class X = H, std::enable_if_t<is_parser_v<X>, nullptr_t> = nullptr>
		constexpr trys(X h,Parsers... parsers ):internal_{ always_true, h, parsers... } {
		}
		template<class X=H,std::enable_if_t<!is_parser_v<X>, nullptr_t> = nullptr>
		constexpr trys(X h, Parsers... parsers) :internal_{  h, parsers... } {
		}
		constexpr auto parse(source_type_t<T>&& src)const {
			return internal_(std::move(src));
		}
	};
	template<class H , class... Parsers>
	trys(H h, Parsers... ps)->trys<H, Parsers...>;


	template<class F,class... Parsers>
	class trys_variant_internal:public parser<
		typename parser_type_traits<Parsers...>::source_type,
		convart_wrapping_object_t<unique_type_t<result_type_t<Parsers>...>, std::variant>,
		trys_variant_internal<F,Parsers...>
	> {
		F f_;
		std::tuple<Parsers...> ps_;
	public:
		constexpr trys_variant_internal(F f, Parsers... ps) :f_(f), ps_{ ps...}{}
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
	template<class H, class... Parsers>
	class trys_variant :public parser <
		source_type_t<std::conditional_t<is_parser_v<H>, trys_variant_internal<decltype(always_true), H, Parsers...>, trys_variant_internal<H, Parsers...>>>,
		result_type_t<std::conditional_t<is_parser_v<H>, trys_variant_internal<decltype(always_true), H, Parsers...>, trys_variant_internal<H, Parsers...>>>,
		trys_variant<H, Parsers...>
	> {
		using T = std::conditional_t<is_parser_v<H>, trys_variant_internal<decltype(always_true), H, Parsers...>, trys_variant_internal<H, Parsers...>>;
		T internal_;
	public:
		template<class X = H, std::enable_if_t<is_parser_v<X>, nullptr_t> = nullptr>
		constexpr trys_variant(X h, Parsers... parsers) :internal_{ always_true, h, parsers... } {
		}
		template<class X = H, std::enable_if_t<!is_parser_v<X>, nullptr_t> = nullptr>
		constexpr trys_variant(X h, Parsers... parsers) : internal_{ h, parsers... } {
		}
		constexpr auto parse(source_type_t<T>&& src)const {
			return internal_(std::move(src));
		}
	};
	template<class H, class... Parsers>
	trys_variant(H h, Parsers... ps)->trys_variant<H, Parsers...>;

	/*
	* trys end
	*/


	template<class F>
	class lazy :public parser<
		source_type_t<std::invoke_result_t<F>>,
		result_type_t<std::invoke_result_t<F>>,
		lazy<F>
	> {
		F f_;
	public:
		constexpr lazy(F f) :f_(f) {}
		constexpr auto parse(source_type_t<std::invoke_result_t<F>>&& src)const {
			return f_()(std::move(src));
		}
	};
	template<class Src,class R>
	class throwing :public parser<Src, R, throwing<Src, R>> {
		parser_exception ex_;
	public:
		constexpr throwing(parser_exception ex):ex_(ex) {

		}
		constexpr ret<Src,R> parse(Src&& src)const {
			throw ex_;
		}
	};
	template<class P,class E,class F>
	class catching :public parser<source_type_t<P>, result_type_t<P>, catching<P, E,F>> {
		P p_;
		F f_;
	public:
		constexpr catching(P p,F f):p_(p),f_(f) {

		}
		constexpr ret<source_type_t<P>, result_type_t<P>> parse(source_type_t<P>&& src)const {
			auto m = src;
			try {
				return p_(std::move(src));
			}
			catch (E ex) {
				return ret{m, f_(ex) };
			}
		}
	};


	template<class E,class P, class F>
	constexpr auto make_catching(P p, F f) {
		return catching<P, E, F>(p,f);
	}
	template<class Src,class R>
	class type_eraser :public parser<Src, R, type_eraser<Src,R> > {
		std::function<ret<Src, R>(Src&&)>p_;
	public:
		template<class P>
		constexpr type_eraser(P p) :p_(p) {
			
		}
		constexpr ret<Src,R> parse(Src&& src)const {
			return p_(std::move(src));
		}
	};
	
	template<class P,class R,class F>
	class many :public parser<source_type_t<P>, result_type_t<R>, many<P, R,F> > {
		P p_;
		F f_;
		R init_;
	public:
		constexpr many(R init, P p, F updateFn) :p_(p),f_(updateFn),init_(init) {

		}
		constexpr ret<source_type_t<P>, result_type_t<R>> parse(source_type_t<P>&& src)const {
			auto s = src;
			auto ri = init_(std::move(s));
			s = ri.itr();
			result_type_t<R> rv = ri.get();
			try {

				while (true) {
					auto r = p_(std::move(s));
					s = r.itr();
					auto rp = r.get();
					std::pair<bool, result_type_t<R>> uv = f_(std::move(rv), rp);
					if (uv.first) {
						return { s,uv.second };
					}
					else {
						rv = uv.second;
					}
				}
			}
			catch (parser_exception) {
				return { s,rv };
			}
		}
	};
	template<class P, class R, class F>
	class manyN :public parser<source_type_t<P>, result_type_t<R>, manyN<P, R, F> > {
		P p_;
		F f_;
		size_t n_;
		R init_;
	public:
		constexpr manyN(R init, P p,size_t n,  F updateFn) :n_(n),p_(p), f_(updateFn), init_(init) {

		}
		constexpr ret<source_type_t<P>, result_type_t<R>> parse(source_type_t<P>&& src)const {
			auto s = src;
			auto ri = init_(std::move(s));
			s = ri.itr();
			result_type_t<R> rv = ri.get();
			size_t cnt = 0;
			try {
				while (true) {
					auto r = p_(std::move(s));
					++cnt;
					s = r.itr();
					auto rp = r.get();
					std::pair<bool, result_type_t<R>> uv = f_(std::move(rv), rp);
					if (uv.first&&cnt>=n_) {
						return { s,uv.second };
					}
					else {
						rv = uv.second;
					}
				}
			}
			catch (parser_exception) {
				if (cnt >= n_) {
					return { s,rv };
				}
				throw;
			}
		}
	};
	template<class P, class R, class F>
	class manyNM :public parser<source_type_t<P>, result_type_t<R>, manyNM<P, R, F> > {
		P p_;
		F f_;
		size_t n_;
		size_t m_;
		R init_;
	public:
		constexpr manyNM(R init, P p, size_t n,size_t m, F updateFn) :n_(n),m_(m), p_(p), f_(updateFn), init_(init) {

		}
		constexpr ret<source_type_t<P>, result_type_t<R>> parse(source_type_t<P>&& src)const {
			auto s = src;
			auto ri = init_(std::move(s));
			s = ri.itr();
			result_type_t<R> rv = ri.get();
			size_t cnt = 0;
			try {
				while (cnt<=m_) {
					auto r = p_(std::move(s));
					++cnt;
					s = r.itr();
					auto rp = r.get();
					std::pair<bool, result_type_t<R>> uv = f_(std::move(rv), rp);
					if (uv.first&&cnt >= n_) {
						return { s,uv.second };
					}
					else {
						rv = uv.second;
					}
				}
				return { s,rv };

			}
			catch (parser_exception) {
				if (cnt >= n_) {
					return { s,rv };
				}
				throw;
			}
		}
	};
	template<class P,class S>
	constexpr auto to_uncatching_impl(P p,S&& s) {
		return p(std::move(s));
	}
	template<class P,class S,class Eh,class... Et>
	constexpr auto to_uncatching_impl(P p,S&& s) {
		try {
			return to_uncatching_impl<P, Et...>(p, std::move(s));
		}
		catch (const Eh& ex) {
			throw uncaught_parser_exception<Eh>(ex);
		}
	}
	template<class P,class... Es>
	class to_uncatching :public parser<source_type_t<P>,result_type_t<P>, to_uncatching<P,Es...>> {
		P p_;
	public:
		constexpr to_uncatching(P p):p_(p) {

		}
		constexpr auto parse(source_type_t<P>&& src)const{
			return to_uncatching_impl<P, source_type_t<P>, Es...>(p_,std::move(src));
		}
	};
	template<class... Es, class P>
	constexpr auto make_to_uncatching(P p) {
		return to_uncatching<P, Es...>(p);
	}
	

	template<class P, class S>
	constexpr auto to_catching_impl(P p, S&& s) {
		return p(std::move(s));
	}
	template<class P, class S, class Eh, class... Et>
	constexpr auto to_catching_impl(P p, S&& s) {
		try {
			return to_catching_impl<P, Et...>(p, std::move(s));
		}
		catch (const uncaught_parser_exception<Eh>& ex) {
			throw ex.get_reason_exception();
		}
	}
	template<class P, class... Es>
	class to_catching :public parser<source_type_t<P>, result_type_t<P>, to_catching<P, Es...>> {
		P p_;
	public:
		constexpr to_catching(P p) :p_(p) {

		}
		constexpr auto parse(source_type_t<P>&& src)const {
			return to_catching_impl<P, source_type_t<P>, Es...>(p_, std::move(src));
		}
	};
	template<class... Es, class P>
	constexpr auto make_to_catching(P p) {
		return to_catching<P, Es...>(p);
	}
	template<class P,class V,class F = decltype(always_true)>
	class option_or: public parser<source_type_t<P>, std::common_type_t<V, result_type_t<P>>, option_or<P,V, F>> {
		P p_;
		V v_;
		F f_;
	public:
		constexpr option_or(P p,V v,F f = always_true) :p_(p), f_(f),v_(v) {

		}

		constexpr ret<source_type_t<P>,std::common_type_t<V,result_type_t<P>>> parse(source_type_t<P>&& src)const {
			auto m = src;
			try {
				auto r = p_(std::move(src));
				if (f_(r.get())) {
					return { r.itr(), r.get() };
				}
				return { m, v_ };
			}
			catch (parser_exception) {
				return { m,v_ };
			}
		}
	};
	template<class P,class F=decltype(always_true)>
	class option :public parser<source_type_t<P>, std::optional<result_type_t<P>>, option<P,F>> {
		P p_;
		F f_;
	public:
		constexpr option(P p, F f = always_true) :p_(p),f_(f) {

		}
		template<class V>
		constexpr auto or (V v) {
			return option_or(p_, v, f_);
		}
		constexpr ret<source_type_t<P>,std::optional<result_type_t<P>>> parse(source_type_t<P>&& src)const {
			auto m = src;
			try {
				auto r = p_(std::move(src));
				if (f_(r.get())) {
					return { r.itr(), std::make_optional(r.get()) };
				}
				return { m, std::nullopt };
			}
			catch (parser_exception) {
				return { m,std::nullopt };
			}
		}
	};
	template<class P>
	class get0:public parser<source_type_t<P>, std::decay_t<decltype(std::get<0>(std::declval< result_type_t<P>>()))>,get0<P>> {
		P p_;
	public:
		constexpr get0(P p) :p_(p){

		}
		constexpr auto parse(source_type_t<P>&& src)const {
			auto r = p_(std::move(src));
			return ret{ r.itr(),std::get<0>(r.get()) };
		}
	};
	template<class From,class To>
	class parser_cast_impl:public parser<
		source_type_t<From>,
		To,
		parser_cast_impl<From,To>
	> {
		From p_;
	public:
		constexpr parser_cast_impl(From p) :p_(p) {

		}
		constexpr ret< source_type_t<From>,To> parse(source_type_t<From>&& src)const {
			auto r=p_(std::move(src));
			return {r.itr(),r.get()};
		}
	};
	template<class To,class From>
	constexpr static auto parser_cast(From f) {
		return parser_cast_impl<From, To>(f);
	}
	template<class P,class To>
	class to_collection_impl:parser<
		source_type_t<P>,
		To,
		to_collection_impl<P,To>
	> {
		P p_;
		constexpr to_collection_impl(P p) :p_(p) {

		}
		constexpr auto parse(source_type_t<P>&& s)const {
			auto&& r = p_(std::move(src));
			return ret<source_type_t<P>, To>{
				r.itr(),
				To{ r.get() }
			};
		}
	};
	template<template<class>class To,class P>
	constexpr auto to_collection(P p) {
		return to_collection_impl<P,To<result_type_t<P>>>(p);
	}

	template<class Op, class Term>
	union node_data_or_term_union;

	enum class node_type {
		leaf,node
	};
	template<class Op, class Term>
	class node {
		node_type type_;
		node_data_or_term_union<Op, Term> data_;
	protected:
		node(node_type ty, node_data_or_term_union<Op, Term>&& uni) :type_(ty) {
			switch (ty)
			{
			case node_type::leaf:
				new(&data_.term) Term(uni.term);
				break;
			case node_type::node:
				data_.node = new node_data<Op, Term>(*uni.node);
				break;
			default:
				break;
			}
		}
	public:
		node(const node<Op,Term>& n) :type_(n.type_) {
			switch (n.type_)
			{
			case node_type::leaf:
				new(&data_.term) Term(n.data_.term);
				break;
			case node_type::node:
				data_.node= new node_data<Op,Term>(*n.data_.node);
				break;
			default:
				break;
			}
		}

		node<Op,Term>& operator=(const node<Op, Term>& n)
		{
			type_ = n.type_;
			switch (n.type_)
			{
			case node_type::leaf:
				new(&data_.term) Term(n.data_.term);
				break;
			case node_type::node:
				data_.node = new node_data<Op, Term>(*n.data_.node);
				break;
			default:
				break;
			}
			return *this;
		}
		node_type type() {
			return type_;
		}

		node<Op, Term>& left() {
			if (type_ != node_type::node) {
				throw std::invalid_argument("");
			}
			return data_.node->left;
		}
		node<Op, Term>& right() {
			if (type_ != node_type::node) {
				throw std::invalid_argument("");
			}
			return data_.node->right;
		}

		Op& op() {
			if (type_ != node_type::node ) {
				throw std::invalid_argument("");
			}
			return data_.node->op;
		}

		Term& term() {
			if (type_ != node_type::leaf ) {
				throw std::invalid_argument("");
			}
			return data_.term;
		}
		~node() {

			switch (type_)
			{
			case node_type::leaf:
				data_.term.~Term();
				break;
			case node_type::node:
				delete data_.node;
				break;
			default:
				break;
			}
		}
		static node<Op,Term> make_node(node<Op, Term> left, Op op, node<Op, Term> right) {
			node_data_or_term_union<Op, Term> uni =node_data_or_term_union<Op, Term>{};
			uni.node=new node_data<Op, Term>{ op,left,right };
			return node{ node_type::node,std::move(uni) };
		}
		static node<Op, Term> make_leaf(Term t) {
			node_data_or_term_union<Op, Term> uni = node_data_or_term_union<Op, Term>{};
			new(&uni.term) Term(std::move(t));
			return node{ node_type::leaf,std::move(uni) };
		}
	};
	template<class Op, class Term>
	struct node_data {
		Op op;
		node<Op, Term> left;
		node<Op, Term> right;
	};
	template<class Op, class Term>
	union node_data_or_term_union
	{
		node_data<Op,Term>* node;
		Term term;
		node_data_or_term_union() {}
		~node_data_or_term_union(){}
	};


	template<class Term,class Op>
	class expression_left :public parser<
		source_type_t<Term>,
		node<result_type_t<Op>,result_type_t<Term>>,
		expression_left<Term, Op>
	> {
		Term term_;
		Op op_;

		using rt = node < result_type_t<Op>, result_type_t<Term>>;
	public:
		constexpr expression_left(Term term,Op op):term_(term),op_(op) {

		}
		constexpr auto parse(source_type_t<Term>&& src)const {
			static auto c=many(
				map(term_, [](auto&& e) {
					return rt::make_leaf(e);
				}),
				join(op_, term_),
				[](auto&& a, auto&& e) {
					return accm::contd(rt::make_node(a, std::get<0>(e), rt::make_leaf(std::get<1>(e))));
				}
			);
			return c(std::move(src));
		}
	};
	template<class Term, class Op>
	class expression_right :public parser<
		source_type_t<Term>,
		node<result_type_t<Op>, result_type_t<Term>>,
		expression_right<Term, Op>
	> {
		Term term_;
		Op op_;

		using rt = node < result_type_t<Op>, result_type_t<Term>>;
	public:
		constexpr expression_right(Term term, Op op) :term_(term), op_(op) {

		}
		constexpr auto parse(source_type_t<Term>&& src)const {
			static auto c = many(
				map(term_, [](auto&& e) {
					return rt::make_leaf(e);
				}),
				join(op_, term_),
				[](auto a, auto&& e) {
					auto* ref = &a;

					if (ref->type() == node_type::leaf) {
						return accm::contd(rt::make_node(a, std::get<0>(e), rt::make_leaf(std::get<1>(e))));
					}
					while (ref->right().type()==node_type::node) {
						ref =& ref->right();
					}
					auto l_leaf = ref->right();
					ref->right()=rt::make_node(l_leaf, std::get<0>(e), rt::make_leaf(std::get<1>(e)));
					return accm::contd(a);
				}
			);
			return c(std::move(src));
			
		}
	};
	namespace branch {
		template<class V,class P>
		constexpr auto value_with(V v,P p) {
			return [=](const auto& argV) {
				if (argV == v) {
					return std::make_optional(p);
				}
				return typing_nullopt<P>;
			};
		}
		template<class K, class F>
		constexpr auto end_with(K k, F f) {
			return [=](auto&& a, const auto& argK, auto&&e) {
				if (argK == k) {
					return accm::terminate(f(std::move(a),std::move(e)));
				}
				return accm::contd(f(std::move(a), std::move(e)));

			};
		}
	}
	template<size_t index, class Src, class Key, class Accm, class Tuple, class RT>
	constexpr ret<Src, std::decay_t<RT>> state_machine_parser_impl(Src&& s, Key k, Accm accm, Tuple t, RT&& rv, std::enable_if_t<std::tuple_size_v<Tuple> == index>* = nullptr) {
		throw all_of_parser_failed_exception();
	}
	template<size_t index,class Src,class Key,class Accm,class Tuple,class RT>
	constexpr ret<Src,std::decay_t<RT>> state_machine_parser_impl(Src&& s,Key k,Accm accm,Tuple t,RT&& rv,std::enable_if_t<std::tuple_size_v<Tuple> !=index>* =nullptr){
		auto p=std::get<index>(t)(k);
		if (!p) {
			return state_machine_parser_impl<index + 1>(std::move(s),k, accm, t,std::move(rv));
		}
		auto ns = s;
		try {
			auto pr =p.value()(std::move(s));
			auto nr = accm(std::move(rv), pr.get().first, pr.get().second);
			if (nr.first) {
				return ret<Src, std::decay_t<RT>>{pr.itr(), nr.second};
			}
			return state_machine_parser_impl<0>(pr.itr(), pr.get().first,accm,t, nr.second);
		}
		catch (parser_exception) {
			//do nothing
		}
		return state_machine_parser_impl<index + 1>(std::move(ns), k, accm, t, std::move(rv));

	}

	template<class Key,class Init,class Accm,class... Case>
	class state_machine_parser:public parser_def_helper<
		source_type_t<Init>,
		std::decay_t<typename result_type_t<Init>::second_type>,
		state_machine_parser<Key,Init,Accm,Case...>
	>{
		Init init_;
		Accm accm_;
		std::tuple<Case...> cs_;
	public:
		constexpr state_machine_parser(Init init,Accm accm,Case... cs) :init_(init),accm_(accm),cs_{cs...} {

		}
		constexpr ret<source_type_t<Init>,typename result_type_t<Init>::second_type> parse(source_type_t<Init>&& src)const {
			auto r = init_(std::move(src));
			return state_machine_parser_impl<0>(r.itr(), r.get().first,accm_,cs_,r.get().second);
		}
	};
	template<class Init, class Accm, class... Case>
	state_machine_parser(Init init, Accm accm, Case... cs)->state_machine_parser<
		typename result_type_t<Init>::first_type,
		Init,
		Accm,
		Case...
	>;
}