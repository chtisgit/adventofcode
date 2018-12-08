#include <algorithm>
#include <array>
#include <cstdio>
#include <cstring>
#include <vector>

enum Reason { BEGIN_SHIFT, FALL_ASLEEP, WAKE_UP };

struct LogEntry {
	enum {
		YEAR, MONTH, DAY, HOUR, MINUTE, T_SIZE
	};
	int t[T_SIZE];
	Reason reason;
	int guard;

	bool operator<(const LogEntry &rhs) const
	{
		for(auto i = 0; t[i] <= rhs.t[i] && i < T_SIZE; i++){
			if(t[i] < rhs.t[i])
				return true;
		}
		return false;
	}

	template <typename Container> void sprint(Container &c) const
	{
		auto off =
		    snprintf(c.data(), c.size(), "[%04d-%02d-%02d %02d:%02d] ",
			     t[YEAR], t[MONTH], t[DAY], t[HOUR], t[MINUTE]);
		if (reason == WAKE_UP)
			snprintf(off+c.data(), c.size() - off, "wakes up");
		else if (reason == FALL_ASLEEP)
			snprintf(off+c.data(), c.size() - off,
				 "falls asleep");
		else
			snprintf(off+c.data(), c.size() - off,
				 "guard #%d begins shift", guard);
	}
};
using Log = std::vector<LogEntry>;

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

		log.push_back(LogEntry{.t = {Y,M,D,h,m},
				       .reason = reason,
				       .guard = g});
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
	printLog(log);
	return 0;
}
