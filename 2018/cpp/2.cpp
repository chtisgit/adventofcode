#include <cctype>
#include <cstdio>
#include <cstring>
#include <array>
#include <algorithm>
#include <set>
#include <vector>

constexpr int MAXLEN = 40;

template<typename ForwardIterator, typename Func>
void consecutive_f(ForwardIterator begin, ForwardIterator end, Func F, const std::set<int>& coeff)
{
	auto prev = *begin;
	int count = 1;
	std::set<int> done;

	for(++begin; begin != end; ++begin){
		if(prev == *begin){
			count++;
		}else{
			if(coeff.count(count) != 0 && done.count(count) == 0){
				F(count);
				done.insert(count);
			}
			prev = *begin;
			count = 1;
		}
	}
	if(coeff.count(count) != 0 && done.count(count) == 0){
		F(count);
		done.insert(count);
	}
}

template<typename ForwardIterator1, typename ForwardIterator2>
int elementwise_compare(ForwardIterator1 b1, ForwardIterator1 e1, ForwardIterator2 b2, ForwardIterator2 e2)
{
	int positions = 0;

	for(; b1 != e1 && b2 != e2; ++b1, ++b2){
		if(*b1 == *b2)
			positions++;
	}
	if(!(b1 == e1 && b2 == e2)){
		throw "not equal length!";
	}
	return positions;
}

template<typename Seq>
std::string common(Seq s1, Seq s2) {
	size_t i, j;
	std::string s;
	for(i = j = 0; i < s1.size(); i++){
		if(s1[i] == s2[i]){
			s += s1[i];
		}
	}
	return s;
}

int main()
{
	const std::set<int> coeff{2,3};
	std::vector<std::array<char, MAXLEN>> ids;
	std::array<char, MAXLEN> boxId;
	std::array<int, MAXLEN> C;
	auto doneSearching = false;
	C.fill(0);

	while(nullptr != fgets(boxId.data(), boxId.size(), stdin)){
		auto len = strlen(boxId.data());
		while(len > 0 && isspace(boxId[len-1]))
			len--;
		boxId[len] = '\0';

		if(!doneSearching){
			for(const auto& a : ids) {
				int eq = elementwise_compare(begin(boxId), begin(boxId)+len, begin(a), begin(a)+len);
				printf("equal elements %d\n", eq);
				if(eq == len-1) {
					printf("found correct: %s \n", common(boxId,a).c_str());
					doneSearching = true;
					break;
				}
			}
			ids.push_back(boxId);
		}

		std::sort(begin(boxId), begin(boxId) + len);
		consecutive_f(begin(boxId), begin(boxId) + len, [&](int i){
			C[i]++;
		}, coeff);
	}

	int product = 1;
	for(int i = 0; i < C.size(); i++){
		if(i > 1 && C[i] != 0)
			product *= C[i];
	}

	printf("product: %d\n", product);
	return 0;
}

