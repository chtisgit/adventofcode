
#include <algorithm>
#include <array>
#include <cstdio>
#include <unistd.h>
#include <vector>

// don't stop searching for the message until the framebuffer
// I need to draw is smaller than MAXALLOC (1 MB).
constexpr int MAXALLOC = 1000000;

struct Point {
	int x, y;
	int vx, vy;
	int t = 0;

	void step(int delta)
	{
		x += vx * delta;
		y += vy * delta;
		t += delta;
	}
};

struct Rect {
	int x, y, w, h;
};

template <typename ForwardIterator>
Rect bounds(ForwardIterator begin, ForwardIterator end)
{
	Rect r;
	auto xit = std::minmax_element(
	    begin, end, [](const auto &l, const auto &r) { return l.x < r.x; });
	auto yit = std::minmax_element(
	    begin, end, [](const auto &l, const auto &r) { return l.y < r.y; });
	r.x = xit.first->x;
	r.w = xit.second->x - r.x + 1;
	r.y = yit.first->y;
	r.h = yit.second->y - r.y + 1;
	return r;
}

template <typename ForwardIterator>
void draw(Rect r, ForwardIterator begin, ForwardIterator end)
{
	std::vector<char> buf((r.w + 1) * r.h);
	std::fill(buf.begin(), buf.end(), ' ');

	std::for_each(begin, end, [r, &buf](const Point &p) {
		auto index = (p.y-r.y) * (r.w + 1) + p.x-r.x;
		if (index >= 0 && index < buf.size())
			buf[index] = 'X';
	});
	for (int y = 1; y < r.h; y++) {
		buf[y * (r.w + 1) - 1] = '\n';
	}
	buf[r.h * (r.w + 1) - 1] = '\0';
	printf("%s\n", buf.data());
}

int main()
{
	std::vector<Point> P;
	for (;;) {
		Point p;
		if (scanf("position=< %d, %d> velocity=< %d, %d>\n", &p.x, &p.y,
			  &p.vx, &p.vy) != 4)
			break;
		P.push_back(p);
	}

	auto b = bounds(P.cbegin(), P.cend());
	for (;;) {
		std::for_each(P.begin(), P.end(), [](auto &p) { p.step(1); });
		auto newb = bounds(P.cbegin(), P.cend());
		if(newb.w*newb.h < MAXALLOC && newb.w*newb.h > b.w*b.h){
			std::for_each(P.begin(), P.end(), [](auto &p) { p.step(-1); });
			break;
		}
		b = newb;
	}
	draw(b, P.begin(), P.end());

	printf("\nTime elapsed: %d\n", P.front().t);

	return 0;
}
