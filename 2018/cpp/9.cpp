#include <algorithm>
#include <cstdio>
#include <list>
#include <vector>

void printCircle(const std::vector<int> &circle, int p, int m)
{
	printf("[%d]", p);
	for (auto x : circle) {
		printf(m == x ? " (%d)" : " %d", x);
	}
	printf("\n");
}

template <int O, typename I>
I moveIt(I it, I begin, I end)
{
	auto incr = true;
	auto off = O;
	if (off < 0) {
		off = -off;
		incr = false;
	}
	for (int i = 0; i < off; i++) {
		if (incr){
			if(it == end)
				it = begin;
			++it;
		}else{
			if(it == begin)
				it = end;
			--it;
		}
	}
	return it;
}

// P is the number of players, M the number of marbles
long winningScore(int P, int M)
{
	std::vector<long> player(P);

	// I am a huge fan of arrays, but linked list is hugely faster
	// in this case. Probably because all operations that happen on the
	// container are near a local working point that does not change fast.
	std::list<int> circle{0};
	auto current = circle.begin();

	int p = 0;
	for (int m = 1; m != M; m++, p = (p + 1) % P) {
		auto next = current;
		if (m % 23 == 0) {
			// lucky player
			player[p] += m;

			next = moveIt<-7>(next, circle.begin(), circle.end());

			player[p] += *next;
			next = circle.erase(next);
		} else {
			// place marble in circle
			next = moveIt<2>(next, circle.begin(), circle.end());
			if (next == circle.begin())
				next = circle.end();

			next = circle.insert(next, m);
		}
		current = next;
	}

	return *std::max_element(player.cbegin(), player.cend());
}

int main()
{
	int P, M;
	if (2 !=
	    scanf("%d players; last marble is worth %d points\n", &P, &M)) {
		return 1;
	}
	printf("Winning Score: %zd\n", winningScore(P, M));
	return 0;
}
