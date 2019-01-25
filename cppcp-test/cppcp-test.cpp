#include "gtest/gtest.h"
#include "cppcp.h"
#include <string>
template <class Itr>

class CppCPTest : public ::testing::Test {};
using sitr = std::string::const_iterator;
template<class T>
using vitr = typename std::vector<typename T>::const_iterator;

TEST(CppCP, itr_any )
{
	std::string target = "abcdef";
	auto&& r = tig::cppcp::itr::any<sitr>()(std::cbegin(target));
	EXPECT_EQ(r.get(), 'a');
	EXPECT_EQ(tig::cppcp::itr::any<sitr>()(std::move(r.itr())).get(), 'b');
	
}
TEST(CppCP, filter)
{
	std::string target = "abcdef";
	auto&& fn = tig::cppcp::filter(tig::cppcp::itr::any<sitr>(),[](auto&& c) {return c == 'a'; },std::optional<char>());
	auto&& r = fn(cbegin(target));
	EXPECT_EQ(r.get(), std::make_optional('a'));
	auto&& r2 = fn(r.itr());
	EXPECT_EQ(r2.get(),std::nullopt);
	EXPECT_EQ(tig::cppcp::itr::any<sitr>()(r2.itr()).get(), 'c');
}
TEST(CppCP, sup)
{
	std::vector<int> target{ 1,2,3,4,5 };
	auto&& fn = tig::cppcp::sup<vitr<int>, int>(6);
	auto&& r = fn(cbegin(target));
	EXPECT_EQ(r.get(), 6 );
	EXPECT_EQ(r.itr(), cbegin(target));
}
TEST(CppCP, sup_temp)
{
	std::vector<int> target{ 1,2,3,4,5 };
	auto&& fn = tig::cppcp::sup<vitr<int>, int,6>();
	auto&& r = fn(cbegin(target));
	EXPECT_EQ(r.get(), 6);
	EXPECT_EQ(r.itr(), cbegin(target));
}
TEST(CppCP, any_unless_end)
{
	std::vector<int> target{ 1,2,3,4,5 };
	auto fn=tig::cppcp::itr::any_unless_end<vitr<int>>(cend(target));
	auto r=fn(cbegin(target));
	EXPECT_EQ(r.get(), 1);
	EXPECT_EQ(r.itr(), next(cbegin(target)));
}
TEST(CppCP, fold)
{
	std::vector<int> target{ 1,2,3,4,5 };
	auto&& fn = tig::cppcp::fold{
		tig::cppcp::itr::any_unless_end<vitr<int>>(cend(target)),
		tig::cppcp::sup<vitr<int>,int>(6),
		 [](auto&& a, auto && v){
		if (v)
		{
			return tig::cppcp::accm::contd( a + v.value());
		}
		else
		{
			return tig::cppcp::accm::terminate(a);
		}
	}
	};
	auto&& r=fn(cbegin(target));
	EXPECT_EQ(r.get(),6+(target.at(0)+target.at(target.size()-1))*(target.size()/2.0));
	EXPECT_EQ(r.itr(), cend(target));
}
TEST(CppCP, foldAndExitIfNullOpt)
{
	std::vector<int> target{ 1,2,3,4,5 };
	auto&& fn = tig::cppcp::fold(
		tig::cppcp::itr::any_unless_end<vitr<int>>(cend(target)),
		tig::cppcp::sup<vitr<int>, int>(6),
		tig::cppcp::exit_if_nullopt<int,int>([](auto && a,auto&& e) {
			return a + e;
		})
	);
	auto&& r = fn(cbegin(target));
	EXPECT_EQ(r.get(), 6 + (target.at(0) + target.at(target.size() - 1))*(target.size() / 2.0));
	EXPECT_EQ(r.itr(), cend(target));

}

TEST(CppCP, reduce)
{
	std::vector<int> target{ 1,2,3,4,5 };
	auto&& fn = tig::cppcp::reduce<vitr<int>, std::optional<int>>(
		tig::cppcp::itr::any_unless_end<vitr<int>>(cend(target)),
		[](auto&& a,auto&& v) {
			if (v)
			{
				return tig::cppcp::accm::contd(std::make_optional(a.value() + v.value()));
			}
			else
			{
				return tig::cppcp::accm::terminate(a);
			}
		}
		);
	auto && r = fn(cbegin(target));
	EXPECT_EQ(r.get().value(),(target.at(0) + target.at(target.size() - 1))*(target.size() / 2.0));
	EXPECT_EQ(r.itr(), cend(target));
}
TEST(CppCP, join0) {
	auto && target = std::vector<int>{ -1, 0, 1, 2, 3, 4, 5 };
	auto && r = tig::cppcp::join<vitr<int>>()(cbegin(target));
	EXPECT_EQ(r.get(),std::make_tuple());
	EXPECT_EQ(r.itr(), cbegin(target));
}
TEST(CppCP, join1)
{
	using namespace tig::cppcp;

	std::vector<int> target{ -1,0 ,1,2,3,4,5 };
	auto f = itr::any<vitr<int>>();
	auto&& fn = join(f );
	auto && r = fn(cbegin(target));
	EXPECT_EQ(r.get(), std::make_tuple(-1));
	EXPECT_EQ(r.itr(), std::next(cbegin(target), 1));

}
TEST(CppCP, join2)
{
	using namespace tig::cppcp;

	std::vector<int> target{-1,0 ,1,2,3,4,5 };
	auto an = itr::any<vitr<int>>();
	auto&& fn=join(an, fold(
		itr::any_unless_end(cend(target)),
		sup<vitr<int>,int>(6),
		exit_if_nullopt<int, int>([](auto && a, auto&& e) {
			return a + e;
		})
	));
	auto&& r = fn(cbegin(target));
	EXPECT_EQ(r.get(),std::make_tuple(-1,6+(target.at(1) + target.at(target.size() - 1))*((target.size()-1) / 2.0)));
	EXPECT_EQ(r.itr(), cend(target));

}

TEST(CppCP, join3)
{
	using namespace tig::cppcp;

	std::vector<int> target{ -2,-1,0 ,1,2,3,4,5 };
	auto&& fn = join(itr::any<vitr<int>>(), itr::any<vitr<int>>(),fold(
		itr::any_unless_end<vitr<int>>(cend(target)),
		sup<vitr<int>, int>(6),
		exit_if_nullopt<int, int>([](auto && a, auto&& e) {
			return a + e;
		})
	));
	auto &&r = fn(cbegin(target));
	EXPECT_EQ(r.get(), std::make_tuple(-2 ,-1, 6 + (target.at(2) + target.at(target.size() - 1))*((target.size() - 2) / 2.0)));
	EXPECT_EQ(r.itr(), cend(target));

}

TEST(CppCP, join1AndSkip)
{
	using namespace tig::cppcp;
	std::vector<int> target{ -1,0 ,1,2,3,4,5 };
	auto fn = join(skip(itr::any<vitr<int>>()));
	auto&& r = fn(cbegin(target));
	EXPECT_EQ(r.get(), std::make_tuple());
	EXPECT_EQ(r.itr(), std::next(cbegin(target), 1));

}
TEST(CppCP, join2AndSkip)
{
	using namespace tig::cppcp;
	std::vector<int> target{ -1,0 ,1,2,3,4,5 };
	auto&& fn = join(skip(itr::any<vitr<int>>()), skip(itr::any<vitr<int>>()));
	auto r = fn(cbegin(target));
	EXPECT_EQ(r.get(), std::make_tuple());
	EXPECT_EQ(r.itr(), std::next(cbegin(target), 2));
}
template<class T>
struct customSkip {
	static constexpr const bool value = true;
};
TEST(CppCP, join1AndSkipWithCustomSkip)
{
	using namespace tig::cppcp;
	std::vector<int> target{ -1,0 ,1,2,3,4,5 };

	auto&& fn = join_c<customSkip>::join(itr::any<vitr<int>>());
	auto && r = fn(cbegin(target));
	EXPECT_EQ(r.get(), std::make_tuple());
	EXPECT_EQ(r.itr(), std::next(cbegin(target), 1));

}

TEST(CppCP, join2AndSkipWithCustomSkip)
{
	using namespace tig::cppcp;
	std::vector<int> target{ -1,0 ,1,2,3,4,5 };
	auto&& fn = join_c<customSkip>::join(itr::any<vitr<int>>(), itr::any<vitr<int>>());
	auto&& r = fn(cbegin(target));
	EXPECT_EQ(r.get(), std::make_tuple());
	EXPECT_EQ(r.itr(), std::next(cbegin(target), 2));

}
TEST(CppCP, empty_tuple_as_skip_skipping)
{
	using namespace tig::cppcp;
	std::vector<int> target{ -1,0 ,1,2,3,4,5 };
	auto&& fn = empty_tuple_as_skip<vitr<int>>(join(itr::any<vitr<int>>(), itr::any<vitr<int>>()));
	auto &&r = fn(cbegin(target));
	EXPECT_EQ(r.itr(),std::next(cbegin(target),2));
}
TEST(CppCP, empty_tuple_as_skip_through)
{
	using namespace tig::cppcp;
	std::vector<int> target{ -1,0 ,1,2,3,4,5 };
	auto&& fn = empty_tuple_as_skip<vitr<int>>(join(itr::any<vitr<int>>(), itr::any<vitr<int>>()));
	auto r = fn(cbegin(target));
	EXPECT_EQ(r.get(), std::make_tuple(-1,0));
	EXPECT_EQ(r.itr(), std::next(cbegin(target), 2));

}

TEST(CppCP, skipNWithParser)
{
	using namespace tig::cppcp;
	std::vector<int> target{ -1,0 ,1,2,3,4,5 };
	auto&& fn = skipN(join(itr::any<vitr<int>>(), itr::any<vitr<int>>()),2);
	auto &&r = fn(cbegin(target));
	skip_tag tag = r.get();
	EXPECT_EQ(r.itr(), std::next(cbegin(target), 4));
}
TEST(CppCP, skipNWithCount)
{
	using namespace tig::cppcp;
	std::vector<int> target{ -1,0 ,1,2,3,4,5 };
	auto&& fn = skipN<vitr<int>>(3);
	auto &&r = fn(cbegin(target));
	skip_tag tag = r.get();

	EXPECT_EQ(r.itr(), std::next(cbegin(target), 3));
}
TEST(CppCP, map)
{
	using namespace std::literals::string_literals;
	using namespace tig::cppcp;
	std::vector<int> target{ -1,0 ,1,2,3,4,5 };
	auto f = itr::any<vitr<int>>();
	auto&& fn = map(f, [](auto && e) {return std::to_string(e); });
	auto &&r = fn(cbegin(target));
	EXPECT_EQ(r.get(), "-1"s);
	EXPECT_EQ(r.itr(), std::next(cbegin(target), 1));
}

TEST(CppCP, trys_true)
{
	using namespace std::literals::string_literals;
	using namespace tig::cppcp;
	std::vector<int> target{ -1,0 ,1,2,3,4,5 };
	auto f = itr::any<vitr<int>>();
	auto&& fn = trys( f);
	auto &&r = fn(cbegin(target));
	EXPECT_EQ(r.get(), -1);
	EXPECT_EQ(r.itr(), std::next(cbegin(target), 1));
}
TEST(CppCP, trys_secondry_true)
{
	using namespace std::literals::string_literals;
	using namespace tig::cppcp;
	std::vector<int> target{ -1,0 ,1,2,3,4,5 };
	auto f = itr::any<vitr<int>>();
	auto&& fn = trys([](auto e) {return e >= 0; }, f, map(f, [](auto && e) {return e + 1; }));
	auto &&r = fn(cbegin(target));
	EXPECT_EQ(r.get(), 0);
	EXPECT_EQ(r.itr(), std::next(cbegin(target), 1));
}

TEST(CppCP, trys_all_false)
{
	using namespace std::literals::string_literals;
	using namespace tig::cppcp;
	std::vector<int> target{ -1,0 ,1,2,3,4,5 };
	auto f = itr::any<vitr<int>>();
	auto&& fn = trys([](const auto& e) {return false; }, f, map(f, [](auto && e) {return e + 1; }));
	EXPECT_THROW(fn(cbegin(target)), tig::cppcp::all_of_parser_failed_exception);
}
TEST(CppCP, trys_variant_true)
{
	using namespace std::literals::string_literals;
	using namespace tig::cppcp;
	std::vector<int> target{ -1,0 ,1,2,3,4,5 };
	auto f = itr::any<vitr<int>>();
	auto&& fn = trys_variant([](int e) {return true; }, f,f);
	auto &&r = fn(cbegin(target));
	EXPECT_EQ(r.get(), std::variant<int>(-1));
	EXPECT_EQ(r.itr(), std::next(cbegin(target), 1));
}
TEST(CppCP, trys_variant_secondry_true)
{
	using namespace std::literals::string_literals;
	using namespace tig::cppcp;
	std::vector<int> target{ -1,0 ,1,2,3,4,5 };
	auto f = itr::any<vitr<int>>();
	auto&& fn = trys_variant(overloaded{
			[](auto arg) { return false; },
			[](const std::string& arg) { return true; },
		}, f, map(f, [](auto && e) {return  std::to_string(e); }));
	auto &&r = fn(cbegin(target));
	EXPECT_EQ(std::get<0>(r.get()),"-1"s);
	EXPECT_EQ(r.itr(), std::next(cbegin(target), 1));
}
TEST(CppCP, trys_variant_all_false)
{
	using namespace std::literals::string_literals;
	using namespace tig::cppcp;
	std::vector<int> target{ -1,0 ,1,2,3,4,5 };
	auto f = itr::any<vitr<int>>();
	auto&& fn = trys_variant([](const auto& e) {return false; }, f, map(f, [](auto && e) {return std::to_string(e); }));
	EXPECT_THROW(fn(cbegin(target)), tig::cppcp::all_of_parser_failed_exception);
}
TEST(CppCP, lazy)
{
	using namespace std::literals::string_literals;
	using namespace tig::cppcp;
	std::vector<int> target{ -1,0 ,1,2,3,4,5 };
	auto&& fn = lazy([]() {return itr::any<vitr<int>>(); });
	auto &&r = fn(cbegin(target));
	EXPECT_EQ(r.get(), -1);
	EXPECT_EQ(r.itr(), std::next(cbegin(target), 1));
}
TEST(CppCP, throwing)
{
	using namespace std::literals::string_literals;
	using namespace tig::cppcp;
	std::vector<int> target{ -1,0 ,1,2,3,4,5 };
	auto&& fn = throwing<vitr<int>,int>(parser_exception());
	EXPECT_THROW(fn(cbegin(target)), tig::cppcp::parser_exception);
}
TEST(CppCP, catching)
{
	using namespace std::literals::string_literals;
	using namespace tig::cppcp;
	std::vector<int> target{ -1,0 ,1,2,3,4,5 };
	auto&& fn2 = make_catching<parser_exception>(throwing<vitr<int>, int>(parser_exception()), [](const auto& e) {
		return 0;
	});
	EXPECT_EQ(fn2(cbegin(target)).get(), 0);
}
TEST(CppCP, type_eraser)
{
	using namespace std::literals::string_literals;
	using namespace tig::cppcp;
	std::vector<int> target{ -1,0 ,1,2,3,4,5 };
	auto&& fn = type_eraser<vitr<int>, int>(throwing<vitr<int>, int>(parser_exception()));
	EXPECT_THROW(fn(cbegin(target)), tig::cppcp::parser_exception);
}
TEST(CppCP, many)
{
	using namespace std::literals::string_literals;
	using namespace tig::cppcp;
	std::vector<int> target{ -1,0 ,1,2,3,4,5 };
	auto&& fn = many(itr::any<vitr<int>>(), sup<vitr<int>>(std::vector<int>()), [](auto&& list, auto v) {
		list.push_back(v);
		if (list.size() >= 4) {
			return accm::terminate(std::move(list));
		}
		return accm::contd(std::move(list));
	});
	auto expect = std::vector<int>{ -1, 0, 1, 2 };
	EXPECT_EQ(fn(cbegin(target)).get(), expect);

}
TEST(CppCP, manyN)
{
	using namespace std::literals::string_literals;
	using namespace tig::cppcp;
	std::vector<int> target{ -1,0 ,1,2,3,4,5 };
	auto&& fn = manyN(itr::any<vitr<int>>(),5, sup<vitr<int>>(std::vector<int>()), [](auto&& list, auto v) {
		list.push_back(v);
		if (list.size() >= 4) {
			return accm::terminate(std::move(list));
		}
		return accm::contd(std::move(list));
	});
	auto expect = std::vector<int>{ -1, 0, 1, 2,3 };
	EXPECT_EQ(fn(cbegin(target)).get(), expect);

}
TEST(CppCP, manyNM)
{
	using namespace std::literals::string_literals;
	using namespace tig::cppcp;
	std::vector<int> target{ -1,0 ,1,2,3,4,5 };
	auto&& fn = manyNM(itr::any<vitr<int>>(), 5,5, sup<vitr<int>>(std::vector<int>()), [](auto&& list, auto v) {
		list.push_back(v);
		if (list.size() >= 4) {
			return accm::terminate(std::move(list));
		}
		return accm::contd(std::move(list));
	});
	auto expect = std::vector<int>{ -1, 0, 1, 2,3 };
	EXPECT_EQ(fn(cbegin(target)).get(), expect);

}
TEST(CppCP, to_uncatching)
{
	using namespace std::literals::string_literals;
	using namespace tig::cppcp;
	std::vector<int> target{ -1,0 ,1,2,3,4,5 };
	auto&& fn = make_to_uncatching<parser_exception>(throwing<vitr<int>, int>(parser_exception()));
	EXPECT_THROW(fn(cbegin(target)).get(), tig::cppcp::uncaught_parser_exception<parser_exception>);

}
TEST(CppCP, to_catching)
{
	using namespace std::literals::string_literals;
	using namespace tig::cppcp;
	std::vector<int> target{ -1,0 ,1,2,3,4,5 };
	auto&& fn = make_to_catching<parser_exception>(
		make_to_uncatching<parser_exception>(throwing<vitr<int>, int>(parser_exception()))
	);
	EXPECT_THROW(fn(cbegin(target)).get(), parser_exception);

}

TEST(CppCP, option)
{
	using namespace std::literals::string_literals;
	using namespace tig::cppcp;
	std::vector<int> target{ -1,0 ,1,2,3,4,5 };
	auto&& fn =option( make_to_catching<parser_exception>(
		make_to_uncatching<parser_exception>(throwing<vitr<int>, int>(parser_exception()))
		));
	EXPECT_EQ(fn(cbegin(target)).get(), std::nullopt);

}

TEST(CppCP, option_or)
{
	using namespace std::literals::string_literals;
	using namespace tig::cppcp;
	std::vector<int> target{ -1,0 ,1,2,3,4,5 };
	auto&& fn = option_or(make_to_catching<parser_exception>(
		make_to_uncatching<parser_exception>(throwing<vitr<int>, int>(parser_exception()))
		),-999);
	EXPECT_EQ(fn(cbegin(target)).get(), -999);

}