#include <algorithm>
#include <array>
#include <cstdio>
#include <cstring>
#include <map>
#include <vector>

enum Reason { BEGIN_SHIFT, FALL_ASLEEP, WAKE_UP };

struct LogEntry {
	enum { YEAR, MONTH, DAY, HOUR, MINUTE, T_SIZE };
	int t[T_SIZE];
	Reason reason;
	int guard;

	bool operator<(const LogEntry &rhs) const
	{
		for (auto i = 0; t[i] <= rhs.t[i] && i < T_SIZE; i++) {
			if (t[i] < rhs.t[i])
				return true;
		}
		return false;
	}

	auto operator-(const LogEntry &rhs) const
	{
		constexpr int mult[] = {1, 60, 60 * 24};
		constexpr int param[] = {MINUTE, HOUR, DAY};
		constexpr int TO = sizeof(mult) / sizeof(*mult);
		static_assert(sizeof(param) / sizeof(*param) == TO, "");

		int diff = 0;
		for (int i = 0; i < TO; ++i) {
			diff += (t[param[i]] - rhs.t[param[i]]) * mult[i];
		}
		return diff;
	}

	template <typename Container>
	void sprint(Container &c) const
	{
		auto off =
		    snprintf(c.data(), c.size(), "[%04d-%02d-%02d %02d:%02d] ",
		             t[YEAR], t[MONTH], t[DAY], t[HOUR], t[MINUTE]);
		if (reason == WAKE_UP)
			snprintf(off + c.data(), c.size() - off, "wakes up");
		else if (reason == FALL_ASLEEP)
			snprintf(off + c.data(), c.size() - off,
			         "falls asleep");
		else
			snprintf(off + c.data(), c.size() - off,
			         "guard #%d begins shift", guard);
	}

	auto Minute() const
	{
		return t[MINUTE];
	}
};
using Log = std::vector<LogEntry>;

struct Guard {
	int nr;
	int minutes_asleep;

	Log collectInfo(const Log &original) const
	{
		Log copy(original.size());
		size_t count = 0;
		std::copy_if(original.cbegin(), original.cend(), copy.begin(),
		             [this, &count](const LogEntry &e) {
			             count++;
			             return e.guard == nr;
		             });
		copy.resize(count);

		return copy;
	}

	// returns a pair of the minute (first) and how often the guard was
	// asleep on that minute (second).
	std::pair<int, int> sleepiestMinute(const Log &original) const
	{
		std::array<int, 60> m;
		auto log = collectInfo(original);
		std::fill(m.begin(), m.end(), 0);

		for (auto it = log.begin(); it != log.end(); ++it) {
			auto &e = *it;
			if (e.reason == WAKE_UP) {
				auto &fell_asleep = *(it - 1);
				auto asleep = *it - *(it - 1);
				auto roundtrips = asleep / 60;
				std::for_each(
				    m.begin(), m.end(),
				    [roundtrips](int &x) { x += roundtrips; });
				for (int i = fell_asleep.Minute();
				     i != e.Minute(); i = (i + 1) % 60) {
					m[i]++;
				}
			}
		}
		auto x = std::max_element(m.cbegin(), m.cend()) - m.cbegin();
		return std::make_pair(x, m[x]);
	}
};

void parse(Log &log)
{
	std::array<char, 100> buf;
	for (;;) {
		int Y, M, D, h, m, g = -1;
		Reason reason;
		if (scanf("[%d-%d-%d %d:%d]", &Y, &M, &D, &h, &m) != 5)
			break;
		if (fgets(buf.data(), buf.size(), stdin) == nullptr)
			break;

		auto hash = strchr(buf.data(), '#');
		if (hash != nullptr) {
			g = atoi(hash + 1);
			reason = BEGIN_SHIFT;
		} else {
			reason = WAKE_UP;
			if (strstr(buf.data(), "fall") != nullptr)
				reason = FALL_ASLEEP;
		}

		log.push_back(LogEntry{
		    .t = {Y, M, D, h, m}, .reason = reason, .guard = g});
	}
}

void printLog(const Log &l)
{
	std::array<char, 200> line;
	for (const auto &e : l) {
		e.sprint(line);
		printf("%s\n", line.data());
	}
}

int main()
{
	Log log;
	parse(log);
	std::sort(log.begin(), log.end());
	// printLog(log);

	int guard = -1;
	std::map<int, Guard> guards;

	for (auto it = log.begin(); it != log.end(); ++it) {
		auto &e = *it;
		if (e.reason == BEGIN_SHIFT)
			guard = e.guard;
		e.guard = guard;
		if (e.reason == WAKE_UP) {
			guards[e.guard].nr = guard;
			guards[e.guard].minutes_asleep += *it - *(it - 1);
		}
	}

	std::map<int, std::pair<int, int>> sleepiest_per_guard;
	for (const auto &p : guards) {
		sleepiest_per_guard[p.first] = p.second.sleepiestMinute(log);
	}

	auto &sleepiest1 =
	    std::max_element(guards.cbegin(), guards.cend(),
	                     [](const auto &l, const auto &r) {
		                     return l.second.minutes_asleep <
		                            r.second.minutes_asleep;
	                     })
	        ->second;
	auto sleepiestMinute1 = sleepiest_per_guard[sleepiest1.nr].first;
	printf("sleepiest guard: guard %d is asleep %d minutes, mostly in "
	       "minute %d --> Product: %d\n",
	       sleepiest1.nr, sleepiest1.minutes_asleep, sleepiestMinute1,
	       sleepiestMinute1 * sleepiest1.nr);

	auto &sleepiest2 = *std::max_element(
	    sleepiest_per_guard.begin(), sleepiest_per_guard.end(),
	    [](const auto &l, const auto &r) {
		    return l.second.second < r.second.second;
	    });

	printf("sleepiest guard: guard %d is asleep %d times on minute %d --> "
	       "Product: %d\n",
	       sleepiest2.first, sleepiest2.second.second,
	       sleepiest2.second.first,
	       sleepiest2.second.first * sleepiest2.first);

	return 0;
}
