#include <iostream>
#include <vector>
#include <algorithm>

const int num = 5;
const int maxDepth = 7;
std::vector<std::pair<char, char> > ans;
std::vector<std::vector<int> > leaf;

bool isDistinguishable(int depth, std::vector<std::vector<int> > perms, int id)
{
	if (depth == 0)
	{
		ans[id] = std::make_pair(-1, perms.size());
		if (!perms.empty())
		{
			leaf[id] = perms[0];
		}
		return true;
	}
	int maxCnt = 1 << (depth - 1);
	for (int i = 0; i < num - 1; ++i)
	{
		for (int j = i + 1; j < num; ++j)
		{
			std::vector<std::vector<int> > perms1;
			std::vector<std::vector<int> > perms2;
			for (std::vector<int> perm : perms)
			{
				if (perm[i] < perm[j])
				{
					perms1.push_back(perm);
				}
				else
				{
					perms2.push_back(perm);
				}
			}
			if (perms1.size() <= maxCnt && perms2.size() <= maxCnt &&
				isDistinguishable(depth - 1, perms1, id << 1) && isDistinguishable(depth - 1, perms2, id << 1 | 1))
			{
				ans[id] = std::make_pair(i, j);
				return true;
			}
		}
	}
	return false;
}

int main()
{
	std::vector<int> a(num);
	std::vector<std::vector<int> > perms;
	for (int i = 0; i < num; ++i)
	{
		a[i] = i;
	}
	do
	{
		perms.push_back(a);
	}
	while (std::next_permutation(a.begin(), a.end()));
	ans.resize(1 << (maxDepth + 1));
	leaf.resize(1 << (maxDepth + 1));
	if (isDistinguishable(maxDepth, perms, 1))
	{
		std::cout << "YES" << std::endl;
		for (int i = 1; i < (1 << maxDepth); ++i)
		{
			std::cout << i << ": if " << (char)('a' + ans[i].first) << '<' << (char)('a' + ans[i].second) <<
			" goto " << (i << 1) << " else goto " << (i << 1 | 1) << std::endl;
		}
		for (int i = (1 << maxDepth); i < (1 << (maxDepth + 1)); ++i)
		{
			std::cout << i << ":";
			if (ans[i].second)
			{
				for (int j = 0; j < num; ++j)
				{
					a[leaf[i][j]] = j;
				}
				for (int j = 0; j < num; ++j)
				{
					std::cout << " " << (char)('a' + a[j]);
				}
			}
			else
			{
				std::cout << " None";
			}
			std::cout << std::endl;
		}
	}
	else
	{
		std::cout << "NO" << std::endl;
	}
	return 0;
}
