#include "gtest/gtest.h"
#include "../Parser2/Parser2.h"
#include <string>
template <class Itr>

class Parser2Test : public ::testing::Test {
	

};
using sitr = std::string::const_iterator;
template<class T>
using vitr = typename std::vector<typename T>::const_iterator;

TEST(Parser2, any )
{
	std::string target = "abcdef";
	auto&& r = tig::parser::any<sitr>()(std::cbegin(target));
	EXPECT_EQ(r.get(), 'a');
	EXPECT_EQ(tig::parser::any<sitr>()(std::move(r.itr())).get(), 'b');
	
}
TEST(Parser2, satisfy)
{
	std::string target = "abcdef";
	auto&& fn = tig::parser::satisfy<sitr>([](auto&& c) {return c == 'a'; });
	auto&& r = fn(cbegin(target));
	EXPECT_EQ(r.get(), std::make_optional('a'));
	auto&& r2 = fn(r.itr());
	EXPECT_EQ(r2.get(),std::nullopt);
	EXPECT_EQ(tig::parser::any<sitr>()(r2.itr()).get(), 'c');
}
TEST(Parser2, sup)
{
	std::vector<int> target{ 1,2,3,4,5 };
	auto&& fn = tig::parser::sup<vitr<int>, int>(6);
	auto&& r = fn(cbegin(target));
	EXPECT_EQ(r.get(), 6 );
	EXPECT_EQ(r.itr(), cbegin(target));
}
TEST(Parser2, any_unless_end)
{
	std::vector<int> target{ 1,2,3,4,5 };
	auto fn=tig::parser::any_unless_end<vitr<int>>(cend(target));
	fn(cbegin(target));
	//EXPECT_EQ(r.get(), 6 + (target.at(0) + target.at(target.size() - 1))*(target.size() / 2.0));
	//EXPECT_EQ(r.itr(), cend(target));
}
TEST(Parser2, fold)
{
	std::vector<int> target{ 1,2,3,4,5 };
	auto&& fn = tig::parser::fold{
		tig::parser::any_unless_end<vitr<int>>(cend(target)),
		tig::parser::sup<vitr<int>,int>(6),
		 [](auto&& a, auto && v){
		if (v)
		{
			return std::make_pair(true, a + v.value());
		}
		else
		{
			return std::make_pair(false, a);
		}
	}
	};
	auto&& r=fn(cbegin(target));
	EXPECT_EQ(r.get(),6+(target.at(0)+target.at(target.size()-1))*(target.size()/2.0));
	EXPECT_EQ(r.itr(), cend(target));
}
TEST(Parser2, foldAndExitIfNullOpt)
{
	std::vector<int> target{ 1,2,3,4,5 };
	auto&& fn = tig::parser::fold(
		tig::parser::any_unless_end<vitr<int>>(cend(target)),
		tig::parser::sup<vitr<int>, int>(6),
		tig::parser::exit_if_nullopt<int,int>([](auto && a,auto&& e) {
			return a + e;
		})
	);
	auto&& r = fn(cbegin(target));
	EXPECT_EQ(r.get(), 6 + (target.at(0) + target.at(target.size() - 1))*(target.size() / 2.0));
	EXPECT_EQ(r.itr(), cend(target));

}

TEST(Parser2, reduce)
{
	std::vector<int> target{ 1,2,3,4,5 };
	auto&& fn = tig::parser::reduce<vitr<int>, std::optional<int>>(
		tig::parser::any_unless_end<vitr<int>>(cend(target)),
		[](auto&& a,auto&& v) {
			if (v)
			{
				return std::make_pair(true, std::make_optional(a.value() + v.value()));
			}
			else
			{
				return std::make_pair(false, a);
			}
		}
		);
	auto && r = fn(cbegin(target));
	EXPECT_EQ(r.get().value(),(target.at(0) + target.at(target.size() - 1))*(target.size() / 2.0));
	EXPECT_EQ(r.itr(), cend(target));
}
TEST(Parser2, join0) {
	auto && target = std::vector<int>{ -1, 0, 1, 2, 3, 4, 5 };
	auto && r = tig::parser::join<vitr<int>>()(cbegin(target));
	EXPECT_EQ(r.get(),std::make_tuple());
	EXPECT_EQ(r.itr(), cbegin(target));
}
TEST(Parser2, join1)
{
	using namespace tig::parser;

	std::vector<int> target{ -1,0 ,1,2,3,4,5 };
	auto f = any<vitr<int>>();
	typename parser_type_traits<decltype(f)>::source_type x = target.cbegin();
	auto&& fn = join(f );
	auto && r = fn(cbegin(target));
	EXPECT_EQ(r.get(), std::make_tuple(-1));
	EXPECT_EQ(r.itr(), std::next(cbegin(target), 1));

}
TEST(Parser2, join2)
{
	using namespace tig::parser;

	std::vector<int> target{-1,0 ,1,2,3,4,5 };
	auto&& fn=join(any<vitr<int>>(), fold(
		any_unless_end(cend(target)),
		sup<vitr<int>,int>(6),
		exit_if_nullopt<int, int>([](auto && a, auto&& e) {
			return a + e;
		})
	));
	auto&& r = fn(cbegin(target));
	EXPECT_EQ(r.get(),std::make_tuple(-1,6+(target.at(1) + target.at(target.size() - 1))*((target.size()-1) / 2.0)));
	EXPECT_EQ(r.itr(), cend(target));

}

TEST(Parser2, join3)
{
	using namespace tig::parser;

	std::vector<int> target{ -2,-1,0 ,1,2,3,4,5 };
	auto&& fn = join(any<vitr<int>>(), any<vitr<int>>(),fold(
		any_unless_end<vitr<int>>(cend(target)),
		sup<vitr<int>, int>(6),
		exit_if_nullopt<int, int>([](auto && a, auto&& e) {
			return a + e;
		})
	));
	auto &&r = fn(cbegin(target));
	EXPECT_EQ(r.get(), std::make_tuple(-2 ,-1, 6 + (target.at(2) + target.at(target.size() - 1))*((target.size() - 2) / 2.0)));
	EXPECT_EQ(r.itr(), cend(target));

}
/*
TEST(Parser2, join1AndSkip)
{
	using namespace tig::parser;
	std::vector<int> target{ -1,0 ,1,2,3,4,5 };
	auto&& fn = join(skip(any<vitr<int>>()));
	auto&& r = fn(cbegin(target));
	EXPECT_EQ(r.get(), std::make_tuple());
	EXPECT_EQ(r.itr(), std::next(cbegin(target), 1));

}
TEST(Parser2, join2AndSkip)
{
	using namespace tig::parser;
	std::vector<int> target{ -1,0 ,1,2,3,4,5 };
	auto&& fn = join<vitr<int>,skip_tag,skip_tag>(skip(any<vitr<int>>()), skip(any<vitr<int>>()));
	auto r = fn(cbegin(target));
	EXPECT_EQ(r.get(), std::make_tuple());
	EXPECT_EQ(r.itr(), std::next(cbegin(target), 2));

}
template<class T>
struct customSkip {
	static constexpr const bool value = true;
};
TEST(Parser2, join1AndSkipWithCustomSkip)
{
	using namespace tig::parser;
	std::vector<int> target{ -1,0 ,1,2,3,4,5 };

	auto&& fn = join<vitr<int>, customSkip,int>(any<vitr<int>>());
	auto && r = fn(cbegin(target));
	EXPECT_EQ(r.get(), std::make_tuple());
	EXPECT_EQ(r.itr(), std::next(cbegin(target), 1));

}

TEST(Parser2, join2AndSkipWithCustomSkip)
{
	using namespace tig::parser;
	std::vector<int> target{ -1,0 ,1,2,3,4,5 };
	auto&& fn = join<vitr<int>, customSkip,int,int>(any<vitr<int>>(), any<vitr<int>>());
	auto&& r = fn(cbegin(target));
	EXPECT_EQ(r.get(), std::make_tuple());
	EXPECT_EQ(r.itr(), std::next(cbegin(target), 2));

}
TEST(Parser2, empty_tuple_as_skip_skipping)
{
	using namespace tig::parser;
	std::vector<int> target{ -1,0 ,1,2,3,4,5 };
	auto&& fn = empty_tuple_as_skip<vitr<int>>(join<vitr<int>, customSkip, int, int>(any<vitr<int>>(), any<vitr<int>>()));
	auto &&r = fn(cbegin(target));
	EXPECT_EQ(r.itr(),std::next(cbegin(target),2));
}
TEST(Parser2, empty_tuple_as_skip_through)
{
	using namespace tig::parser;
	std::vector<int> target{ -1,0 ,1,2,3,4,5 };
	auto&& fn = empty_tuple_as_skip<vitr<int>>(join<vitr<int>, int, int>(any<vitr<int>>(), any<vitr<int>>()));
	auto r = fn(cbegin(target));
	EXPECT_EQ(r.get(), std::make_tuple(-1,0));
	EXPECT_EQ(r.itr(), std::next(cbegin(target), 2));

}

TEST(Parser2, skipNWithParser)
{
	using namespace tig::parser;
	std::vector<int> target{ -1,0 ,1,2,3,4,5 };
	auto&& fn = skipN(join<vitr<int>, int, int>(any<vitr<int>>(), any<vitr<int>>()),2);	
	auto &&r = fn(cbegin(target));
	skip_tag tag = r.get();
	EXPECT_EQ(r.itr(), std::next(cbegin(target), 4));
}
TEST(Parser2, skipNWithCount)
{
	using namespace tig::parser;
	std::vector<int> target{ -1,0 ,1,2,3,4,5 };
	auto&& fn = skipN<vitr<int>>(3);
	auto &&r = fn(cbegin(target));
	skip_tag tag = r.get();

	EXPECT_EQ(r.itr(), std::next(cbegin(target), 3));
}
TEST(Parser2, map)
{
	using namespace std::literals::string_literals;

	using namespace tig::parser;
	std::vector<int> target{ -1,0 ,1,2,3,4,5 };
	auto&& fn = map<vitr<int>,int,std::string>(any<vitr<int>>(), [](auto && e) {return std::to_string(e); });
	auto &&r = fn(cbegin(target));
	EXPECT_EQ(r.get(), "-1"s);
	EXPECT_EQ(r.itr(), std::next(cbegin(target), 1));
}*/
/*
エラー	C3848	型 
'const tig::parser::parser<Src,std::optional<int>,tig::parser::any_unless_end<Src>>' を含む式は、
'tig::parser::ret<Src,R> tig::parser::parser<Src,R,tig::parser::any_unless_end<Src>>::operator ()(Src &&)' 
を呼び出すためにいくつかの const volatile 修飾子を失う可能性があります。

*/