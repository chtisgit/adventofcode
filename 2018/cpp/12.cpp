#include <algorithm>
#include <array>
#include <cassert>
#include <cstdio>
#include <map>
#include <string>
#include <unistd.h>
#include <vector>

size_t parse(std::vector<char> &P, std::map<std::string, char> &rules)
{
	std::array<char, 10000> buf;
	size_t zero = 0;

	if (fgets(buf.data(), buf.size(), stdin) == nullptr)
		return 1;
	std::copy(buf.begin() + 15, buf.end(), buf.begin());

	for (size_t i = 0; i < buf.size(); i++) {
		if (buf[i] == '\n' || buf[i] == '\0') {
			zero = i;
			P.resize(i * 4 + 3);
			std::fill(P.begin(), P.end(), '.');
			std::copy(buf.begin(), buf.begin() + i,
				  P.begin() + zero);
			P[zero + 2 * i + 2] = '\0';
			break;
		}
	}

	for (;;) {
		char buf[10], r;
		if (scanf("%s => %c\n", buf, &r) != 2)
			break;
		rules.emplace(buf, r);
	}

	return zero;
}

long plantSum(const std::vector<char>& P, long zero, long gen = 0, long calcGen = 0)
{
	long sum = 0;
	for (size_t i = 0; i < P.size(); i++) {
		if (P[i] == '#')
			sum += i + (calcGen - gen) - zero;
	}
	return sum;
}

int main(int argc, char **argv)
{
	long calcGen = -1;
	if (argc < 2) {
		printf("usage: %s <generation>\n\n", argv[0]);
		return 1;
	}
	calcGen = atol(argv[1]);

	std::vector<char> P, Pnew;
	std::map<std::string, char> rules;
	auto zero = parse(P, rules);

	assert(!P.empty());
	assert(!rules.empty());

	Pnew.resize(P.size());

	long gen = 1;
	for (; gen <= calcGen; gen++) {
		std::fill(Pnew.begin(), Pnew.end(), '.');
		for (int i = 2; i < P.size() - 3; i++) {
			std::string trans(&P[i - 2], &P[i + 3]);

			if (rules.find(trans) != rules.end())
				Pnew[i] = rules[trans];
		}

		// printf("%02d: %s\n", gen, P.data() + zero);

		printf("[%ld] plant sum: %zd\n", gen, plantSum(Pnew, zero));
		if(std::equal(Pnew.cbegin()+1, Pnew.cend(), P.cbegin())){
			printf("shift detected (gen=%ld)\n", gen);
			printf("[%ld] plant sum: %zd\n", calcGen, plantSum(Pnew, zero, gen, calcGen));
			break;
		}

		std::swap(P, Pnew);
	}

	return 0;
}
