#include <algorithm>
#include <array>
#include <cassert>
#include <cstdio>
#include <vector>

int rackId(int x, int y)
{
	return x + 10;
}

int powerLevel(int x, int y, int serial)
{
	int power = rackId(x, y) * y;
	power += serial;
	power *= rackId(x, y);
	power /= 100;
	power %= 10;
	if (power < 0)
		power += 10;
	power -= 5;
	return power;
}

constexpr int W = 300, H = 300;

template <typename Func>
void acc(const int x0, const int y0, const int from, const int len, Func f)
{
	for (int y = y0; y < y0 + len; y++) {
		for (int x = x0; x < x0 + len; x++) {
			if (x - x0 >= from || y - y0 >= from)
				f(x, y);
		}
	}
}

struct Result {
	int x, y, power, len;
};

Result searchMaxPower(int serial, int from, int to)
{
	int x, y, len;
	std::array<int, W * H> grid;

	std::fill(grid.begin(), grid.end(), 0);
	for (y = 0; y < H; y++) {
		for (x = 0; x < W; x++) {
			int P = powerLevel(x, y, serial);
			acc(x, y, 0, from - 1,
			    [P, serial, &grid](int x, int y) {
				    if (x < 0 || y < 0 || x >= W || y >= H)
					    return;
				    grid[y * W + x] += P;
			    });
		}
	}

	std::vector<Result> res(to - from + 1);
	for (len = from; len <= to; len++) {
		printf("len=%d\n", len);
		for (y = 0; y < H; y++) {
			for (x = 0; x < W; x++) {
				int P = powerLevel(x, y, serial);
				acc(x, y, len - 1, len,
				    [P, serial, &grid](int x, int y) {
					    if (x >= W || y >= H)
						    return;
					    grid[y * W + x] += P;
				    });
			}
		}

		auto maxel = std::max_element(grid.cbegin(), grid.cend());
		auto off = maxel - grid.cbegin();
		res[len - from] = Result{
		    .x = off % W - len+1,
		    .y = off / W - len+1,
		    .power = *maxel,
		    .len = len,
		};
	}

	return *std::max_element(
	    res.cbegin(), res.cend(),
	    [](const auto &l, const auto &r) { return l.power < r.power; });
}

int main()
{
	int gridSerial;

	// plausibility checks
	assert(powerLevel(122, 79, 57) == -5);
	assert(powerLevel(217, 196, 39) == 0);
	assert(powerLevel(101, 153, 71) == 4);

	int test = 0;
	acc(10, 12, 1, 3, [&test](int x, int y) { test += x + y; });
	//assert(test == 10 * 8 + 12 * 8);

	printf("> ");
	if (scanf("%d", &gridSerial) != 1)
		return 1;

	auto res = searchMaxPower(gridSerial, 3, 3);
	printf("maximum power: %d\n", res.power);
	printf("top-left corner: %d,%d\n", res.x, res.y);


	int upto;
	printf("scan up to (2-300): ");
	if (scanf("%d", &upto) != 1 || upto < 2 || upto > 300)
		return 1;
	res = searchMaxPower(gridSerial, 1, upto);
	printf("maximum power: %d\n", res.power);
	printf("top-left corner: %d,%d,%d \n", res.x, res.y, res.len);
	return 0;
}
