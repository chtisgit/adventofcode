#include <algorithm>
#include <array>
#include <cassert>
#include <cstdio>
#include <deque>
#include <vector>

#define DEBUG_EN false
#define DEBUG(...)                                                             \
	if (DEBUG_EN) {                                                        \
		do {                                                           \
			printf(__VA_ARGS__);                                   \
		} while (false);                                               \
	}

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
	int distance(const Coord &other) const
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
	int round = 0;
	int w = 0, h = 0;
	std::vector<char> data;
	std::vector<Unit> units;

	char &C(const Coord &c)
	{
		return data[c.y * w + c.x];
	}

	int countDead(char type = 0)
	{
		int dead = 0;
		std::for_each(units.cbegin(), units.cend(), 
			[&dead, type](const Unit& u){
				if(!u.alive() && (type == 0 || type == u.type)) dead++;
			});
		return dead;
	}

	Unit &findUnit(const Coord &c)
	{
		return *std::find_if(
		    units.begin(), units.end(),
		    [c](const Unit &u) { return u.alive() && u.pos == c; });
	}

	void setAttackPower(char type, int ap)
	{
		for(auto& u : units) {
			if(u.type == type){
				u.ap = ap;
			}
		}
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
		if (from == to) {
			if (C(from) != '.')
				return {0, false};
			return {0, true};
		}

		std::vector<std::pair<Coord, char>> marks;
		auto adj = from.adjacent();
		std::deque<std::pair<Coord, int>> fifo;
		for (const auto &c : adj) {
			fifo.push_back({c, 1});
		}

		auto res = std::make_pair(w * h, false);
		while (!fifo.empty()) {
			auto p = fifo.front();
			fifo.pop_front();

			if (C(p.first) != '.')
				continue;

			if (p.first == to) {
				res = {p.second, true};
				break;
			}

			assert(C(p.first) != '*');
			marks.push_back({p.first, C(p.first)});
			C(p.first) = '*';

			auto adj = p.first.adjacent();
			for (const auto &a : adj) {
				fifo.push_back({a, p.second + 1});
			}
		}
		for (const auto &mark : marks) {
			C(mark.first) = mark.second;
		}
		return res;
	}

	auto nearestComparator(const Coord &target)
	{
		return [this, target](const Coord &lhs, const Coord &rhs) {
			auto l = this->reachable(lhs, target);
			auto r = this->reachable(rhs, target);
			return l.first < r.first;
		};
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
		printf("After round %d\n", round);
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
			if (u.alive()) {

				printf("%c(%d,%d)  %03d HP\n", u.type, u.pos.x,
				       u.pos.y, u.hp);
			}
		}
	}

	int outcome() const
	{
		int hp = 0;
		std::for_each(units.cbegin(), units.cend(),
			      [&hp](const Unit &u) {
				      if (u.alive())
					      hp += u.hp;
			      });
		return hp * round;
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
	auto t = *std::min_element(adj.begin(), last,
				   [&m](const Coord &lhs, const Coord &rhs) {
					   const auto &l = m.findUnit(lhs);
					   const auto &r = m.findUnit(rhs);
					   if (l.hp != r.hp)
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
	if (attack(m)) {
		DEBUG("%c(%d,%d): attacked\n", type, pos.x, pos.y);
		return;
	}

	const auto enemy = type == 'E' ? 'G' : 'E';

	// in range
	auto target = m.targets(enemy);
	if (target.empty()) {
		DEBUG("%c(%d,%d): no targets\n", type, pos.x, pos.y);
		return;
	}

	auto adj = pos.adjacent();
	struct A {
		Coord source;
		Coord target;
		std::pair<int, bool> steps;
	};
	std::vector<A> reach;
	for (size_t j = 0; j < adj.size(); j++) {
		if (m.C(adj[j]) != '.')
			continue;
		for (size_t i = 0; i < target.size(); i++) {

			auto r = m.reachable(adj[j], target[i]);
			DEBUG("(%d,%d)->(%d,%d)  steps:%d reachable:%d\n",
			      adj[j].x, adj[j].y, target[i].x, target[i].y,
			      r.first, r.second);

			reach.push_back(A{
			    .source = adj[j],
			    .target = target[i],
			    .steps = r,
			});
		}
	}

	// reachable
	auto last = std::partition(reach.begin(), reach.end(),
				   [](const A &a) { return a.steps.second; });
	if (last == reach.begin()) {
		DEBUG("%c(%d,%d): no targets reachable\n", type, pos.x, pos.y);
		return;
	}

	// nearest
	auto nearestSteps =
	    std::min_element(reach.begin(), last,
			     [](const A &lhs, const A &rhs) {
				     return lhs.steps.first < rhs.steps.first;
			     })
		->steps.first;
	last = std::partition(reach.begin(), last, [nearestSteps](const A &a) {
		return a.steps.first == nearestSteps;
	});
	assert(last != reach.begin());
	for (auto x = reach.begin(); x != last; ++x) {
		DEBUG("nearest: (%d,%d) steps: %d\n", x->source.x, x->source.y,
		      x->steps.first);
	}

	// read-order
	auto step = std::min_element(reach.begin(), last,
				     [](const A &lhs, const A &rhs) {
					     return lhs.source < rhs.source;
				     })
			->source;

	DEBUG("%c(%d,%d) -> (%d,%d)\n", type, pos.x, pos.y, step.x, step.y);

	move(m, step);
	attack(m);
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

// copy map
Map simulate(Map map, int elvesAP = 3)
{
	map.draw();
	map.setAttackPower('E', elvesAP);

	while (map.victory() == 0) {
		++map.round;
		map.next();
		map.draw();
		//map.stats();
	}

	return std::move(map);
}

int main(int argc, char **argv)
{
	auto map = parse();

	auto task1 = simulate(map);
	printf("\n%c won after %d rounds\n", task1.victory(), task1.round);
	printf("outcome: %d\n", task1.outcome());

	if (task1.victory() != 'E' && argc >= 2 && atoi(argv[1]) == 2) {
		// check if the elves can win with more attack power...
		for (int ap = 3; ap < 9000; ++ap) {
			auto task2 = simulate(map, ap);
			if(task2.victory() == 'E' && task2.countDead('E') == 0){
				printf("the elves can win the battle with ap=%d\n", ap);
				break;
			}
		}
	}

	return 0;
}
