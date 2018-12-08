#include <cstdio>
#include <memory>
#include <numeric>
#include <set>
#include <vector>

struct Tree {
	struct Node {
		Tree &tree;
		std::vector<int> children;
		std::vector<int> metadata;

		Node(Tree &tree) : tree(tree)
		{
		}

		int sum() const
		{
			int sum = std::accumulate(metadata.begin(),
			                          metadata.end(), 0);
			for (const auto &child : children) {
				sum += tree.nodes[child].sum();
			}
			return sum;
		}

		int value() const
		{
			if (children.empty()) {
				return std::accumulate(metadata.begin(),
				                       metadata.end(), 0);
			}
			int sum = 0;
			for (auto child : metadata) {
				if (child >= 1 && child <= children.size()) {
					sum += tree.nodes[children[child - 1]]
					           .value();
				}
			}
			return sum;
		}
	};

	std::vector<Node> nodes;

	auto sum()
	{
		return nodes[0].sum();
	}

	auto value()
	{
		return nodes[0].value();
	}

	template <typename RandomAccessIterator>
	std::pair<RandomAccessIterator, int>
	buildTree(RandomAccessIterator begin, RandomAccessIterator end)
	{
		auto children = *begin++;
		auto metadata = *begin++;

		auto index = nodes.size();
		nodes.emplace_back(*this);

		for (; children != 0; --children) {
			auto p = buildTree(begin, end - metadata);
			begin = p.first;
			nodes[index].children.push_back(p.second);
		}
		for (; metadata != 0; --metadata)
			nodes[index].metadata.push_back(*begin++);
		return make_pair(begin, index);
	}

	Tree(std::vector<int> &input)
	{
		buildTree(input.cbegin(), input.cend());
	}
};

int main()
{
	std::vector<int> input;
	for (;;) {
		int a;
		if (scanf("%d", &a) != 1)
			break;
		input.push_back(a);
	}
	Tree tree(input);

	printf("metadata sum: %d\n", tree.sum());
	printf("value of root: %d\n", tree.value());

	return 0;
}
