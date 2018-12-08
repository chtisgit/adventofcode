#include <cstdio>
#include <set>
#include <vector>

template<typename RandomAccessIterator>
std::pair<RandomAccessIterator,int> sumMeta(RandomAccessIterator begin, RandomAccessIterator end)
{
	if(begin == end)
		return std::make_pair(begin, 0);
	auto children = *begin++;
	auto metadata = *begin++;
	auto sum = 0;

	printf("mychildren:%d metadata:%d\n",children, metadata);
	for(;children != 0; --children){
		auto p = sumMeta(begin, end - metadata);
		begin = p.first;
		sum += p.second;
	}
	for(;metadata != 0; --metadata){
		sum += *begin++;
	}
	return std::make_pair(begin, sum);
}

int main()
{
	std::vector<int> input;
	for (;;) {
		int a;
		if (scanf("%d", &a) != 1)
			break;
		input.push_back(a);
	}
	
	auto p = sumMeta(input.cbegin(), input.cend());
	printf("metadata sum: %d\n", p.second);

	return 0;
}
