#include <iostream>
#include <stdexcept>

class IntQueue
{

public:

	IntQueue(size_t size) : mSize(size)
	{
		mArray = new int[mSize];
		mFlag = true;
		mFront = 0;
		mRare = 0;
	}

	~IntQueue()
	{
		delete[] mArray;
	}

	void push(int elem)
	{
		if (mFront != mRare || mFlag)
		{
			mArray[mRare] = elem;
			mRare = (mRare + 1) % mSize;
			mFlag = false;
		}
		else
		{
			throw std::length_error("The queue is full!");
		}
	}

	void pop()
	{
		if (mFront != mRare || !mFlag)
		{
			mFront = (mFront + 1) % mSize;
			mFlag = true;
		}
		else
		{
			throw std::out_of_range("The queue is empty!");
		}
	}

	int front()
	{
		if (mFront != mRare || !mFlag)
		{
			return mArray[mFront];
		}
		else
		{
			throw std::out_of_range("The queue is empty!");
		}
	}

	friend std::ostream& operator << (std::ostream& out, IntQueue& iq)
	{
		out << "**";
		if (iq.mFront != iq.mRare || !iq.mFlag)
		{
			int it = iq.mFront;
			do
			{
				out << ' ' << iq.mArray[it];
				it = (it + 1) % iq.mSize;
			}
			while (it != iq.mRare);
		}
		return out;
	}

private:

	int* mArray;
	size_t mSize;
	int mFront;
	int mRare;
	bool mFlag;
	
};

int main()
{
	IntQueue iq(100);
	while (true)
	{
		int n;
		std::cin >> n;
		if (n == 0)
		{
			break;
		}
		else if (n == -1)
		{
			try
			{
				iq.pop();
			}
			catch (std::exception& e)
			{
				std::cout << e.what() << std::endl;
			}
		}
		else
		{
			try
			{
				iq.push(n);
			}
			catch (std::exception& e)
			{
				std::cout << e.what() << std::endl;
			}
		}
		std::cout << iq << std::endl;
	}
	return 0;
}
