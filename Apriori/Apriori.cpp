/*

Sample Input 1:

4
牛奶 面包 麦片
牛奶 面包 糖 鸡蛋
牛奶 面包 黄油
糖 鸡蛋
2

Sample Input 2:

9
I1 I2 I5
I2 I4
I2 I3
I1 I2 I4
I1 I3
I2 I3
I1 I3
I1 I2 I3 I5
I1 I2 I3
2

*/

#include <iostream>
#include <sstream>
#include <vector>
#include <map>
#include <algorithm>
#include <stdexcept>
#include <ctime>
#include <omp.h>

class Item
{

public:

	Item(int id, std::string str)
	{
		setId(id);
		setString(str);
	}

	Item()
	{

	}
	
	void setId(int id)
	{
		mId = id;
	}

	int getId()
	{
		return mId;
	}
	
	void setString(std::string str)
	{
		mString = str;
	}

	std::string getString()
	{
		return mString;
	}

private:

	int mId;
	std::string mString;
	
};

struct Node
{
	std::vector<std::pair<int, Node*> > children;
	Node* parent;
	int value;
	bool isMaximal;
	int support;
	int K;
};

class Apriori
{

public:

	Apriori(const std::vector<Item>& items, const std::vector<std::vector<int> >& data, const int& threshold) : mItems(items), mData(data), mThreshold(threshold)
	{
		for (int i = 0; i < mData.size(); ++i)
		{
			std::sort(mData[i].begin(), mData[i].end());
		}
	}

	void run()
	{
		for (int i = 0; i < mC.size(); ++i)
		{
			for (int j = 0; j < mC[i].size(); ++j)
			{
				delete mC[i][j];
			}
		}
		root = new Node;
		root->value = 0;
		root->K = 0;
		mC.clear();
		mC.resize(mItems.size() + 1);
		mItemData.clear();
		mItemData.resize(mItems.size());
		mC[0].push_back(root);
		int* cnt = new int[mItems.size()];
		for (int i = 0; i < mItems.size(); ++i)
		{
			cnt[i] = 0;
		}
		for (int i = 0; i < mData.size(); ++i)
		{
			for (int j = 0; j < mData[i].size(); ++j)
			{
				mItemData[mData[i][j]].push_back(std::make_pair(i, j));
				cnt[mData[i][j]]++;
			}
		}
		for (int i = 0; i < mItems.size(); ++i)
		{
			if (cnt[i] >= mThreshold)
			{
				Node* newNode = new Node;
				newNode->parent = root;
				newNode->value = i;
				newNode->isMaximal = true;
				newNode->support = cnt[i];
				newNode->K = 1;
				root->children.push_back(std::make_pair(i, newNode));
				mC[1].push_back(newNode);
			}
		}
		delete cnt;
		for (int K = 2; K <= mItems.size(); ++K)
		{
			#pragma omp parallel for
			for (int k = 0; k < mC[K - 2].size(); ++k)
			{
				Node *node = mC[K - 2][k];
				int* itemset = new int[K];
				Node** nodeset = new Node*[K - 2];
				Node** subnodeset = new Node*[K - 2];
				int* candidate = new int[node->children.size()];
				int pos = K - 3;
				Node* tmp = node;
				while (tmp != root)
				{
					itemset[pos] = tmp->value;
					nodeset[pos] = tmp = tmp->parent;
					for (int i = pos + 1; i < K - 2; ++i)
					{
						nodeset[pos] = findChild(nodeset[pos], itemset[i]).second;
					}
					pos--;
				}
				for (int i = 0; i < node->children.size(); ++i)
				{
					int candidateCnt = 0;
					for (int j = i + 1; j < node->children.size(); ++j)
					{
						candidate[candidateCnt++] = j;
					}
					for (int j = 0; j < K - 2 && candidateCnt > 0; ++j)
					{
						subnodeset[j] = findChild(nodeset[j], node->children[i].first).second;
						int it1 = 0;
						int it2 = 0;
						int it = 0;
						while (it1 < candidateCnt && it2 < subnodeset[j]->children.size())
						{
							if (node->children[candidate[it1]].first == subnodeset[j]->children[it2].first)
							{
								candidate[it++] = candidate[it1];
								it1++;
								it2++;
							}
							else if (node->children[candidate[it1]].first < subnodeset[j]->children[it2].first)
							{
								it1++;
							}
							else
							{
								it2++;
							}
						}
						candidateCnt = it;
					}
					itemset[K - 2] = node->children[i].first;
					for (int j = 0; j < candidateCnt; ++j)
					{
						itemset[K - 1] = node->children[candidate[j]].first;
						int support = getSupport(itemset, K);
						if (support >= mThreshold)
						{
							node->children[i].second->isMaximal = false;
							node->children[candidate[j]].second->isMaximal = false;
							for (int it = 0; it < K - 2; ++it)
							{
								findChild(subnodeset[it], node->children[candidate[j]].first).second->isMaximal = false;
							}
							Node* newNode = new Node;
							newNode->parent = node->children[i].second;
							newNode->value = node->children[candidate[j]].first;
							newNode->isMaximal = true;
							newNode->support = support;
							newNode->K = K;
							node->children[i].second->children.push_back(std::make_pair(node->children[candidate[j]].first, newNode));
							mC[K].push_back(newNode);
						}
					}
				}
				delete nodeset;
				delete subnodeset;
				delete candidate;
				delete itemset;
			}
		}
	}

	void printFrequentSet()
	{
		std::vector<Node*> C;
		for (int K = 1; K <= mItems.size(); ++K)
		{
			for (int j = 0; j < mC[K].size(); ++j)
			{
				C.push_back(mC[K][j]);
			}
		}
		std::sort(C.begin(), C.end(), nodeCompare);
		for (int i = 0; i < C.size(); ++i)
		{
			printNode(C[i]);
			std::cout << ' ';
		}
		std::cout << std::endl;
	}

	void printMaximalFrequentSet()
	{
		std::vector<Node*> C;
		for (int K = 1; K <= mItems.size(); ++K)
		{
			for (int j = 0; j < mC[K].size(); ++j)
			{
				if (mC[K][j]->isMaximal)
				{
					C.push_back(mC[K][j]);
				}
			}
		}
		std::sort(C.begin(), C.end(), nodeCompare);
		for (int i = 0; i < C.size(); ++i)
		{
			printNode(C[i]);
			std::cout << ' ';
		}
		std::cout << std::endl;
	}

	void printTopFrequentSet(int k)
	{
		std::vector<Node*> C;
		for (int K = 2; K <= mItems.size(); ++K)
		{
			for (int j = 0; j < mC[K].size(); ++j)
			{
				C.push_back(mC[K][j]);
			}
		}
		if (k < C.size())
		{
			std::nth_element(C.begin(), C.begin() + k, C.end(), nodeCompare);
			std::sort(C.begin(), C.begin() + k, nodeCompare);
		}
		else
		{
			std::sort(C.begin(), C.end(), nodeCompare);
		}
		for (int i = 0; i < k && i < C.size(); ++i)
		{
			printNode(C[i]);
			std::cout << ' ';
		}
		std::cout << std::endl;
	}

private:

	std::vector<Item> mItems;
	std::vector<std::vector<int> > mData;
	std::vector<std::vector<Node*> > mC;
	std::vector<std::vector<std::pair<int, int> > > mItemData;
	int mThreshold;
	Node* root;

	std::pair<int, Node*> findChild(Node* node, int value)
	{
		return *(--std::lower_bound(node->children.begin(), node->children.end(), std::make_pair(value + 1, (Node*)NULL)));
	}

	int getSupport(int* itemset, int K)
	{
		int support = 0;
		for (int it1 = 0, it2 = 0; it1 < mItemData[itemset[0]].size() && it2 < mItemData[itemset[1]].size(); )
		{
			if (mItemData[itemset[0]][it1].first == mItemData[itemset[1]][it2].first)
			{
				int pos = 2;
				for (int j = mItemData[itemset[1]][it2].second + 1; j < mData[mItemData[itemset[1]][it2].first].size(); ++j)
				{
					if (mData[mItemData[itemset[1]][it2].first][j] == itemset[pos])
					{
						pos++;
					}
					else if (mData[mItemData[itemset[1]][it2].first][j] > itemset[pos])
					{
						break;
					}
					if (pos == K)
					{
						support++;
						break;
					}
				}
				it1++;
				it2++;
			}
			else if (mItemData[itemset[0]][it1].first < mItemData[itemset[1]][it2].first)
			{
				it1++;
			}
			else
			{
				it2++;
			}
		}
		return support;
	}

	void printNode(Node* node)
	{
		int K = node->K;
		int support = node->support;
		int* itemset = new int[node->K];
		for (int i = 0; i < K; ++i)
		{
			itemset[K - i - 1] = node->value;
			node = node->parent;
		}
		for (int i = 0; i < K; ++i)
		{
			if (i == 0)
			{
				std::cout << '{';
			}
			else
			{
				std::cout << ',';
			}
			std::cout << mItems[itemset[i]].getString();
		}
		std::cout << '}' << '=' << support;
	}

	static bool nodeCompare(Node* const &a, Node* const &b)
	{
		return a->support > b->support || (a->support == b->support && a->K > b->K);
	}
	
};

int main()
{
	clock_t start, finish;
	start = clock();
	std::vector<Item> items;
	std::vector<std::vector<int> > data;
	int threshold;
	int databaseSize;
	// std::cout << "Input the number of database: ";
	std::cin >> databaseSize;
	std::map<std::string, int> ID;
	for (int i = 0; i < databaseSize; ++i)
	{
		// std::cout << "Input the items of itemset_" << i + 1 << " : ";
		data.push_back(std::vector<int>());
		std::stringstream ss;
		std::string s;
		while (s.empty())
		{
			getline(std::cin, s);
		}
		ss << s;
		while (ss >> s)
		{
			if (!ID.count(s))
			{
				int t = ID.size();
				ID[s] = t;
			}
			data[i].push_back(ID[s]);
		}
	}
	items.resize(ID.size());
	for (std::pair<std::string, int> item : ID)
	{
		items[item.second] = Item(item.second, item.first);
	}
	// std::cout << "Input the support threshold: ";
	std::cin >> threshold;
	Apriori apriori(items, data, threshold);
	apriori.run();
	std::cout << "Frequent set: " << std::endl;
	apriori.printFrequentSet();
	std::cout << "Maximal frequent set: " << std::endl;
	apriori.printMaximalFrequentSet();
	std::cout << "Top 10 frequent set (larger than 1): " << std::endl;
	apriori.printTopFrequentSet(10);
	finish = clock();
	std::cout << "Time: " << (double)(finish - start) / CLOCKS_PER_SEC << "s." << std::endl;
	return 0;
}
