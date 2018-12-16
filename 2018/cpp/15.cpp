
#include <algorithm>
#include <array>
#include <cassert>
#include <cstdio>
#include <vector>

struct Coord;
struct Map;
struct Unit;

struct Coord {
	int x, y;
	Coord(int x, int y) : x(x), y(y)
	{
	}

	bool operator<(const Coord &rhs) const
	{
		return y < rhs.y || (y == rhs.y && x < rhs.x);
	}

	bool operator==(const Coord &rhs) const
	{
		return x == rhs.x && y == rhs.y;
	}

	std::array<Coord, 4> adjacent() const
	{
		return std::array<Coord, 4>{Coord(x, y - 1), Coord(x, y + 1),
		                            Coord(x - 1, y), Coord(x + 1, y)};
	}
	int distance(const Coord &other)
	{
		return abs(x - other.x) + abs(y - other.y);
	}
};

struct Unit {
	char type;
	Coord pos;
	int hp = 200, ap = 3;

	bool alive() const;
	void die(Map &);
	bool attack(Map &);
	void move(Map &, const Coord &to);
	void next(Map &);
};

struct Map {
	int w = 0, h = 0;
	std::vector<char> data;
	std::vector<Unit> units;

	char &C(const Coord &c)
	{
		return data[c.y * w + c.x];
	}

	Unit &findUnit(const Coord &c)
	{
		return *std::find_if(
		    units.begin(), units.end(),
		    [c](const Unit &u) { return u.alive() && u.pos == c; });
	}

	int victory()
	{
		auto goblinsalive = std::count_if(
		    units.cbegin(), units.cend(),
		    [](const Unit &u) { return u.type == 'G' && u.alive(); });
		auto elvesalive = std::count_if(
		    units.cbegin(), units.cend(),
		    [](const Unit &u) { return u.type == 'E' && u.alive(); });
		if (goblinsalive == 0)
			return 'E';
		else if (elvesalive == 0)
			return 'G';
		return 0;
	}

	std::vector<Coord> targets(char type)
	{
		std::vector<Coord> res;
		for (const Unit &u : units) {
			if (u.type != type || !u.alive())
				continue;
			auto adj = u.pos.adjacent();
			for (const auto &c : adj) {
				if (C(c) == '.') {
					res.push_back(c);
				}
			}
		}
		return res;
	}

	std::pair<int, bool> reachable(const Coord &from, const Coord &to)
	{
		if (C(from) != '.')
			return std::make_pair(0, false);
		if (from == to)
			return {0, true};

		auto save = C(from);
		C(from) = '*';

		auto res = std::make_pair(100000, false);
		auto adj = from.adjacent();
		for (const auto &x : adj) {
			auto tmp = reachable(x, to);
			if (tmp.second && tmp.first + 1 < res.first) {
				res.first = tmp.first + 1;
				res.second = true;
			}
		}

		C(from) = save;
		return res;
	}

	void next()
	{
		std::sort(units.begin(), units.end());
		std::for_each(units.begin(), units.end(),
		              [this](Unit &u) { u.next(*this); });
	}

	void draw()
	{
		int x, y;
		for (y = 0; y < h; y++) {
			for (x = 0; x < w; x++) {
				printf("%c", C(Coord(x, y)));
			}
			printf("\n");
		}
		printf("\n");
	}

	void stats()
	{
		for (const auto &u : units) {
			printf("%c(%d,%d)  %03d HP\n", u.type, u.pos.x, u.pos.y,
			       u.hp);
		}
	}
};

bool operator<(const Unit &lhs, const Unit &rhs)
{
	return lhs.pos < rhs.pos;
}

bool Unit::alive() const
{
	return hp > 0;
}
void Unit::die(Map &m)
{
	m.C(pos) = '.';
}

bool Unit::attack(Map &m)
{
	const auto enemy = type == 'E' ? 'G' : 'E';
	auto adj = pos.adjacent();
	auto last =
	    std::partition(adj.begin(), adj.end(), [enemy, &m](const Coord &c) {
		    return m.C(c) == enemy;
	    });
	if (last == adj.begin())
		return false;
	auto t = *std::min_element(adj.begin(), last, [&m](const Coord &lhs, const Coord& rhs) {
		const auto& l = m.findUnit(lhs);
		const auto& r = m.findUnit(rhs);
		if(l.hp != r.hp)
			return l.hp < r.hp;
		return lhs < rhs;
	});
	auto &attacked = m.findUnit(t);

	attacked.hp -= ap;
	if (attacked.hp <= 0)
		attacked.die(m);
	return true;
}

void Unit::move(Map &m, const Coord &to)
{
	m.C(pos) = '.';
	m.C(pos = to) = type;
}

void Unit::next(Map &m)
{
	// do nothing if dead
	if (!alive())
		return;

	// can attack?
	if (attack(m))
		return;

	const auto enemy = type == 'E' ? 'G' : 'E';

	// in range
	auto target = m.targets(enemy);
	if (target.empty())
		return;

	// reachable
	auto last = std::partition(
	    target.begin(), target.end(),
	    [this, &m](const Coord &c) { return m.reachable(pos, c).second; });

	// nearest
	std::sort(target.begin(), target.end(),
	          [this](const Coord &lhs, const Coord &rhs) {
		          return pos.distance(lhs) < pos.distance(rhs);
	          });
	auto D = pos.distance(target.front());
	last = std::find_if(
	    target.begin(), target.end(),
	    [this, D](const Coord &c) { return pos.distance(c) != D; });

	// read-order
	auto t = *std::min_element(target.begin(), last);

	printf("%c(%d,%d) moves towards (%d,%d)\n", type, pos.x, pos.y, t.x,
	       t.y);

	auto adj = pos.adjacent();
	// find first step
	auto step =
	    *std::min_element(adj.begin(), adj.end(),
	                      [&m, t](const Coord &lhs, const Coord &rhs) {
		                      if (m.C(lhs) == '.' && m.C(rhs) != '.')
			                      return true;
		                      else if (m.C(lhs) != '.')
			                      return false;

		                      auto l = m.reachable(lhs, t);
		                      auto r = m.reachable(rhs, t);
		                      if (l.second && !r.second) {
			                      return true;
		                      } else if (!l.second) {
			                      return false;
		                      }
		                      if (l.first != r.first) {
			                      return l.first < r.first;
		                      }
		                      return lhs < rhs;
	                      });
	if (m.C(step) == '.') {
		move(m, step);
		attack(m);
	}
}

Map parse()
{
	Map m;
	int x = 0, y = 0;
	for (;;) {
		int c = getchar();
		if (c == EOF)
			break;

		switch (c) {
		case 'G':
		case 'E':
			m.units.push_back(
			    Unit{.type = char(c), .pos = Coord(x, y)});
		case '#':
		case '.':
			++x;
			m.data.push_back(c);
			break;
		case '\n':
		case '\r':
			if (m.w != 0 && m.w != x) {
				printf("error");
				std::terminate();
			}
			m.w = x;
			++y;
			x = 0;
			break;
		default:
			printf("ignoring garbage '%c'", c);
			break;
		}
	}
	m.h = y;
	return m;
}

int main()
{
	auto map = parse();

	map.draw();

	int round = 1;
	while (map.victory() == 0) {
		map.next();
		printf("After round %d\n", round);
		map.draw();
		map.stats();
		round++;
	}

	printf("\n%c won\n", map.victory());

	return 0;
}
