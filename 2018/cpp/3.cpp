#include <algorithm>
#include <cstdio>
#include <vector>

struct Claim {
	int id;
	int x1, y1;
	int x2, y2;

	bool overlaps(const Claim &other) const
	{
		return x1 < other.x2 && x2 > other.x1 && y1 < other.y2 &&
		       y2 > other.y1;
	}
};

struct Fabric {
	std::vector<Claim> claims;
	int width = 0, height = 0;

	auto xy(int x, int y) const
	{
		return std::vector<Claim>::size_type(y*(width+1)+x);
	}

	void draw() const
	{
		std::vector<char> buf(height * (width+1));
		std::fill(buf.begin(), buf.end(), '.');

		for(const auto& claim : claims) {
			for (auto y = claim.y1; y < claim.y2; y++) {
				for (auto x = claim.x1; x < claim.x2; x++) {
					buf[xy(x,y)] = buf[xy(x,y)] == '.' ? (claim.id % 10)+'0' : '#';
				}
			}
		}
		printf("%dx%d:\n", width, height);
		for (auto y = 0; y < height; y++) {
			buf[xy(width,y)] = '\0';
			printf("%s\n", &buf[xy(0,y)]);
		}
	}

	Claim findNonOverlappingClaim() const
	{
		auto from = claims.cbegin(), to = claims.cend();
		std::vector<bool> overlaps(claims.size());

		for (auto i = from; i != to; i++) {
			auto j = i;
			for (++j; j != to; j++) {
				if (i->overlaps(*j)) {
					overlaps[i - from] = true;
					overlaps[j - from] = true;
				}
			}
			if (!overlaps[i - from])
				return *i;
		}
		throw "not found";
	}

	int countDoublyClaimed() const
	{
		std::vector<int> coord(height * (width+1));
		std::fill(coord.begin(), coord.end(), 0);

		std::for_each(
		    claims.cbegin(), claims.cend(), [&](const auto &claim) {
			    for (auto y = claim.y1; y < claim.y2; y++) {
				    for (auto x = claim.x1; x < claim.x2; x++) {
					    coord[xy(x,y)]++;
				    }
			    }
		    });
		return std::count_if(
		    coord.cbegin(), coord.cend(),
		    [&](auto count) {
			    return count >= 2;
		    });
	}

	void addClaim(Claim&& claim)
	{
		// plausibility checks
		if (claim.x2 < claim.x1 || claim.y2 < claim.y1) {
			throw "width or height is smaller than zero";
		}
		if (claim.x1 < 0 || claim.y1 < 0) {
			throw "top-left corner is out of bounds";
		}
		if (claim.x2 > 50000 || claim.y2 > 50000) {
			throw "bottom-right corner is probably out of bounds";
		}
		if (claim.x2 > width)
			width = claim.x2 + 1;
		if (claim.y2 > height)
			height = claim.y2 + 1;
		claims.push_back(std::move(claim));
	}
};

int main()
{
	Fabric F;

	for (;;) {
		int x, y, w, h, id;
		if (scanf("#%d @ %d,%d: %dx%d\n", &id, &x, &y, &w, &h) != 5) {
			printf("EOF. Read %zd claims.\n", F.claims.size());
			break;
		}
		F.addClaim(Claim{
		    .id = id,
		    .x1 = x,
		    .y1 = y,
		    .x2 = x + w,
		    .y2 = y + h,
		});
	}

	if(F.width < 50 && F.height < 30){
		F.draw();
	}
	printf("\n");
	printf("Counted overlaps: %d\n", F.countDoublyClaimed());
	printf("Non-overlapper: %d\n", F.findNonOverlappingClaim().id);

	return 0;
}
