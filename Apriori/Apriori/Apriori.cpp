// Apriori.cpp : 定义控制台应用程序的入口点。
//

#include "stdafx.h"
#include <iostream>
#include <fstream>
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
		Node** subnodeset = new Node*[mItems.size() - 2];
		Node** nodeset = new Node*[mItems.size() - 2];
		int* itemset = new int[mItems.size()];
		int* candidate = new int[mItems.size()];
		int* superset = new int[mData.size()];
		int* superset2 = new int[mData.size()];
		for (int K = 2; K <= mItems.size(); ++K)
		{
			for (Node* node : mC[K - 2])
			{
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
				int supersetNum = -1;
				if (K > 2)
				{
					for (int i = 0; i < K - 2; ++i)
					{
						supersetNum = merge(superset, supersetNum, mItemData[itemset[i]], superset);
					}
				}
				for (int i = 0; i < node->children.size(); ++i)
				{
					int superset2Num = merge(superset, supersetNum, mItemData[node->children[i].first], superset2);
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
					for (int j = 0; j < candidateCnt; ++j)
					{
						cnt[node->children[candidate[j]].first] = 0;
					}
					for (int j = 0; j < superset2Num; ++j)
					{
						for (int t : mData[superset2[j]])
						{
							cnt[t]++;
						}
					}
					for (int j = 0; j < candidateCnt; ++j)
					{
						if (cnt[node->children[candidate[j]].first] >= mThreshold)
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
							newNode->support = cnt[node->children[candidate[j]].first];
							newNode->K = K;
							node->children[i].second->children.push_back(std::make_pair(newNode->value, newNode));
							mC[K].push_back(newNode);
						}
					}
				}
			}
		}
		delete[] candidate;
		delete[] itemset;
		delete[] nodeset;
		delete[] subnodeset;
		delete[] superset;
		delete[] superset2;
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
	std::ifstream fin("F:\\DataStructure\\Apriori_bak\\T40I10D100K.dat");
	std::cin.rdbuf(fin.rdbuf());
	std::ofstream fout("F:\\DataStructure\\Apriori_bak\\T40I10D100K.out");
	std::cout.rdbuf(fout.rdbuf());
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
	system("pause");
	return 0;
}
