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
				mItemData[mData[i][j]].push_back(i);
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
		int coreNum = omp_get_num_procs();
		Node*** subnodeset = new Node**[coreNum];
		Node*** nodeset = new Node**[coreNum];
		int** itemset = new int*[coreNum];
		int** candidate = new int*[coreNum];
		int** superset = new int*[coreNum];
		int** superset2 = new int*[coreNum];
		int** support = new int*[coreNum];
		std::vector<Node*>* coreC = new std::vector<Node*>[coreNum];
		std::vector<Node*>* coreUnmaximal = new std::vector<Node*>[coreNum];
		for (int i = 0; i < coreNum; ++i)
		{
			subnodeset[i] = new Node*[mItems.size() - 2];
			nodeset[i] = new Node*[mItems.size() - 2];
			itemset[i] = new int[mItems.size()];
			candidate[i] = new int[mItems.size()];
			superset[i] = new int[mData.size()];
			superset2[i] = new int[mData.size()];
			support[i] = new int[mItems.size()];
		}
		for (int K = 2; K <= mItems.size(); ++K)
		{
			std::cerr << K << std::endl;
#pragma omp parallel for
			for (int k = 0; k < mC[K - 2].size(); ++k)
			{
				int ogtn = omp_get_thread_num();
				Node* node = mC[K - 2][k];
				int pos = K - 3;
				Node* tmp = node;
				while (tmp != root)
				{
					itemset[ogtn][pos] = tmp->value;
					nodeset[ogtn][pos] = tmp = tmp->parent;
					for (int i = pos + 1; i < K - 2; ++i)
					{
						nodeset[ogtn][pos] = findChild(nodeset[ogtn][pos], itemset[ogtn][i]).second;
					}
					pos--;
				}
				int supersetNum = -1;
				if (K > 2)
				{
					for (int i = 0; i < K - 2; ++i)
					{
						supersetNum = merge(superset[ogtn], supersetNum, mItemData[itemset[ogtn][i]], superset[ogtn]);
					}
				}
				for (int i = 0; i < node->children.size(); ++i)
				{
					int superset2Num = merge(superset[ogtn], supersetNum, mItemData[node->children[i].first], superset2[ogtn]);
					int candidateCnt = 0;
					for (int j = i + 1; j < node->children.size(); ++j)
					{
						candidate[ogtn][candidateCnt++] = j;
					}
					for (int j = 0; j < K - 2 && candidateCnt > 0; ++j)
					{
						subnodeset[ogtn][j] = findChild(nodeset[ogtn][j], node->children[i].first).second;
						int it1 = 0;
						int it2 = 0;
						int it = 0;
						while (it1 < candidateCnt && it2 < subnodeset[ogtn][j]->children.size())
						{
							if (node->children[candidate[ogtn][it1]].first == subnodeset[ogtn][j]->children[it2].first)
							{
								candidate[ogtn][it++] = candidate[ogtn][it1];
								it1++;
								it2++;
							}
							else if (node->children[candidate[ogtn][it1]].first < subnodeset[ogtn][j]->children[it2].first)
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
					for (int j = 0; j < candidateCnt; ++j)
					{
						support[ogtn][node->children[candidate[ogtn][j]].first] = 0;
					}
					for (int j = 0; j < superset2Num; ++j)
					{
						for (int t : mData[superset2[ogtn][j]])
						{
							support[ogtn][t]++;
						}
					}
					for (int j = 0; j < candidateCnt; ++j)
					{
						if (support[ogtn][node->children[candidate[ogtn][j]].first] >= mThreshold)
						{
							node->children[i].second->isMaximal = false;
							node->children[candidate[ogtn][j]].second->isMaximal = false;
							for (int it = 0; it < K - 2; ++it)
							{
								coreUnmaximal[ogtn].push_back(findChild(subnodeset[ogtn][it], node->children[candidate[ogtn][j]].first).second);
							}
							Node* newNode = new Node;
							newNode->parent = node->children[i].second;
							newNode->value = node->children[candidate[ogtn][j]].first;
							newNode->isMaximal = true;
							newNode->support = support[ogtn][node->children[candidate[ogtn][j]].first];
							newNode->K = K;
							node->children[i].second->children.push_back(std::make_pair(newNode->value, newNode));
							coreC[ogtn].push_back(newNode);
						}
					}
				}
			}
			for (int i = 0; i < coreNum; ++i)
			{
				for (Node* newNode : coreC[i])
				{
					mC[K].push_back(newNode);
				}
				for (Node* node : coreUnmaximal[i])
				{
					node->isMaximal = false;
				}
				coreC[i].clear();
				coreUnmaximal[i].clear();
			}
		}
		delete[] candidate;
		delete[] itemset;
		delete[] nodeset;
		delete[] subnodeset;
		delete[] superset;
		delete[] superset2;
		delete[] coreC;
		delete[] coreUnmaximal;
		delete cnt;
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
	std::vector<std::vector<int> > mItemData;
	int mThreshold;
	Node* root;

	std::pair<int, Node*> findChild(Node* node, int value)
	{
		return *(--std::lower_bound(node->children.begin(), node->children.end(), std::make_pair(value + 1, (Node*)NULL)));
	}

	int merge(int* A, int n, std::vector<int> B, int* C)
	{
		if (n == -1)
		{
			for (int i = 0; i < B.size(); ++i)
			{
				C[i] = B[i];
			}
			return B.size();
		}
		int it = 0;
		for (int it1 = 0, it2 = 0; it1 < n && it2 < B.size(); )
		{
			if (A[it1] == B[it2])
			{
				if (C != NULL)
				{
					C[it] = A[it1];
				}
				it++;
				it1++;
				it2++;
			}
			else if (A[it1] < B[it2])
			{
				it1++;
			}
			else
			{
				it2++;
			}
		}
		return it;
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
