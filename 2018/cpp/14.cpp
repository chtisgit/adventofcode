#include <algorithm>
#include <array>
#include <cstdio>
#include <numeric>
#include <vector>
#include <cassert>

template <typename Func> void breakUpNumber(long a, Func f)
{
	long d = 1;
	while (a / d != 0)
		d *= 10;
	d /= 10;
	while (d >= 1) {
		f((a / d) % 10);
		d /= 10;
	}
}

template <typename It> void nextStep(std::vector<long> &A, It begin, It end)
{
	long sum = 0;
	std::for_each(begin, end, [&](long i) { sum += A[i]; });
	breakUpNumber(sum, [&A](long x) { A.push_back(x); });
	for (; begin != end; ++begin) {
		*begin = (*begin + 1 + A[*begin]) % A.size();
	}
}

void print(const std::vector<long> &A, long pos1, long pos2)
{
	for (auto x : A) {
		const char *fmt = "%zd ";
		if(pos1 == 0 && pos2 == 0)
			fmt = "[(%zd)] ";
		else if(pos1 == 0)
			fmt = "(%zd) ";
		else if(pos2 == 0)
			fmt = "[%zd] ";
		printf(fmt, x);
		--pos1;
		--pos2;
	}
	printf("\n");
}

template<typename It>
std::string ten(It begin, It end)
{
	char buf[10];
	int i = 0;
	for(; i < 10 && begin != end; ++begin){
		buf[i++] = char(*begin+'0');
	}
	return std::string(buf, buf+10);
}

int main()
{
	std::vector<long> A{3, 7};
	std::array<long, 2> elves{0, 1};
	int from;

	printf("> ");
	fflush(stdout);
	scanf("%d", &from);

	for (int i = 0; A.size() < from + 10 || A.size() < 2028; i++) {
		nextStep(A, elves.begin(), elves.end());
	}
	printf("last ten scores: %s\n", ten(A.begin()+from, A.end()).c_str());

	assert(ten(A.begin()+5, A.end()) == "0124515891");
	assert(ten(A.begin()+9, A.end()) == "5158916779");
	assert(ten(A.begin()+18, A.end()) == "9251071085");
	assert(ten(A.begin()+2018, A.end()) == "5941429882");

	return 0;
}
