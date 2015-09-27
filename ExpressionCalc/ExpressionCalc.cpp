#include <iostream>
#include <algorithm>
#include <vector>
#include <stdexcept>

const int MAXN = 100000;

class RationalNumber
{

public:

	RationalNumber(long long numerator = 0, long long denominator = 1) throw(std::domain_error) : numerator(numerator), denominator(denominator)
	{
		if (denominator == 0)
		{
			throw std::domain_error("Denominator cannot be zero!");
		}
		reduce();
	}
	
	RationalNumber operator + (const RationalNumber& B) const
	{
		return RationalNumber(numerator * B.denominator + denominator * B.numerator, denominator * B.denominator);
	}
	
	RationalNumber operator - (const RationalNumber& B) const
	{
		return RationalNumber(numerator * B.denominator - denominator * B.numerator, denominator * B.denominator);
	}
	
	RationalNumber operator * (const RationalNumber& B) const
	{
		return RationalNumber(numerator * B.numerator, denominator * B.denominator);
	}
	
	RationalNumber operator / (const RationalNumber& B) const throw(std::domain_error)
	{
		if (B.numerator == 0)
		{
			throw std::domain_error("Division by zero!");
		}
		return RationalNumber(numerator * B.denominator, denominator * B.numerator);
	}

	RationalNumber operator ^ (const RationalNumber& B) const throw(std::invalid_argument)
	{
		if (B.isNonnegativeInteger())
		{
			throw std::invalid_argument("Invalid exponent!");
		}
		RationalNumber S(1, 1);
		RationalNumber A = *this;
		long long n = B.numerator;
		while (n)
		{
			if (n & 1)
			{
				S = S * A;
			}
			A = A * A;
			n >>= 1;
		}
		return S;
	}

	friend std::ostream& operator << (std::ostream& out, RationalNumber rn)
	{
		out << rn.numerator;
		if (rn.denominator != 1)
		{
			out << '/' << rn.denominator;
		}
		return out;
	}

	bool isZero()
	{
		return numerator == 0;
	}

	bool isInteger()
	{
		return denominator == 1;
	}

	bool isNonnegativeInteger() const
	{
		return denominator == 1 && numerator >= 0;
	}

	long long getNumerator() const
	{
		return numerator;
	}

private:

	long long numerator;
	long long denominator;

	void reduce()
	{
		if (denominator < 0)
		{
			denominator = -denominator;
		}
		long long gcd = std::__gcd(numerator, denominator);
		numerator /= gcd;
		denominator /= gcd;
	}

};

enum PriorityDirection
{
	LEFT,
	RIGHT
};

template<class T>
class Stack
{

public:

	Stack(int size = MAXN) throw(std::bad_alloc) : mSize(size), topPos(-1)
	{
		array = new T[size];
		if (array == NULL)
		{
			throw std::bad_alloc();
		}
	}

	~Stack()
	{
		delete[] array;
	}
	
	T top() throw(std::out_of_range)
	{
		if (topPos < 0)
		{
			throw std::out_of_range("The stack is empty!");
		}
		return array[topPos];
	}

	void push(T elem) throw(std::length_error)
	{
		if (topPos + 1 >= mSize)
		{
			throw std::length_error("The stack is full!");
		}
		array[++topPos] = elem;
	}

	bool isEmpty()
	{
		return topPos == -1;
	}

	void pop() throw(std::out_of_range)
	{
		if (topPos < 0)
		{
			throw std::out_of_range("The stack is empty!");
		}
		topPos--;
	}

private:

	T *array;
	size_t mSize;
	int topPos;

};

class Polynomial
{

public:

	Polynomial(bool x = false)
	{
		if (x)
		{
			array.resize(2);
			array[1] = RationalNumber(1);
		}
	}

	Polynomial(RationalNumber rn)
	{
		array.resize(1);
		array[0] = rn;
	}
	
	Polynomial operator + (const Polynomial& B) const
	{
		Polynomial poly;
		poly.array.resize(std::max(size(), B.size()));
		for (int i = 0; i < std::max(size(), B.size()); ++i)
		{
			if (i < size() && i < B.size())
			{
				poly.array[i] = array[i] + B.array[i];
			}
			else if (i < size())
			{
				poly.array[i] = array[i];
			}
			else
			{
				poly.array[i] = B.array[i];
			}
		}
		poly.autoResize();
		return poly;
	}
	
	Polynomial operator - (const Polynomial& B) const
	{
		Polynomial poly;
		poly.array.resize(std::max(size(), B.size()));
		for (int i = 0; i < std::max(size(), B.size()); ++i)
		{
			if (i < size() && i < B.size())
			{
				poly.array[i] = array[i] - B.array[i];
			}
			else if (i < size())
			{
				poly.array[i] = array[i];
			}
			else
			{
				poly.array[i] = B.array[i] * RationalNumber(-1);
			}
		}
		poly.autoResize();
		return poly;
	}
	
	Polynomial operator * (const Polynomial& B) const
	{
		Polynomial poly;
		poly.array.resize(size() + B.size() - 1);
		for (int i = 0; i < size(); ++i)
		{
			for (int j = 0; j < B.size(); ++j)
			{
				poly.array[i + j] = poly.array[i + j] + array[i] * B.array[j];
			}
		}
		return poly;
	}
	
	Polynomial operator / (const Polynomial& B) const throw(std::domain_error)
	{
		if (B.size() > 1)
		{
			throw std::domain_error("Division by polynomial!");
		}
		else if (B.size() == 0)
		{
			throw std::domain_error("Division by 0!");
		}
		Polynomial poly;
		poly.array.resize(size());
		for (int i = 0; i < size(); ++i)
		{
			poly.array[i] = array[i] / B.array[0];
		}
		poly.autoResize();
		return poly;
	}

	Polynomial operator ^ (const Polynomial& B) const throw(std::invalid_argument)
	{
		Polynomial S(RationalNumber(1));
		if (B.size() == 0)
		{
			return S;
		}
		if (B.size() > 1 || !B.array[0].isNonnegativeInteger())
		{
			throw std::invalid_argument("Invalid exponent!");
		}
		Polynomial A = *this;
		long long n = B.array[0].getNumerator();
		while (n)
		{
			if (n & 1)
			{
				S = S * A;
			}
			A = A * A;
			n >>= 1;
		}
		return S;
	}

	friend std::ostream& operator << (std::ostream& out, Polynomial poly)
	{
		bool first = true;
		for (int i = poly.size() - 1; i >= 0; --i)
		{
			if (!poly.array[i].isZero())
			{
				if (poly.array[i].getNumerator() > 0)
				{
					if (!first)
					{
						out << '+';
					}
					if (!(poly.array[i].getNumerator() == 1 && poly.array[i].isInteger() && i > 0))
					{
						out << poly.array[i];
					}
				}
				else
				{
					if (!(poly.array[i].getNumerator() == -1 && poly.array[i].isInteger() && i > 0))
					{
						out << poly.array[i];
					}
					else
					{
						out << '-';
					}
				}
				if (i >= 2)
				{
					out << "x^" << i;
				}
				else if (i == 1)
				{
					out << 'x';
				}
				first = false;
			}
		}
		if (first)
		{
			out << '0';
		}
		return out;
	}

	int size() const
	{
		return array.size();
	}

private:

	std::vector<RationalNumber> array;

	void autoResize()
	{
		for (int i = size() - 1; i >= -1; --i)
		{
			if (i == -1 || !array[i].isZero())
			{
				array.resize(i + 1);
				break;
			}
		}
	}

};

class Operator;

class OperatorStack : public Stack<Operator*>
{

public:

	OperatorStack(int size = MAXN) throw(std::bad_alloc) : Stack(size)
	{
		
	}

};

class PolynomialStack : public Stack<Polynomial>
{

public:

	PolynomialStack(int size = MAXN) throw(std::bad_alloc) : Stack(size)
	{
		
	}

};

class Operator
{

public:

	Operator()
	{

	}

	virtual ~Operator()
	{

	}

	virtual int getPriorityValue()
	{
		return 0;
	}

	virtual PriorityDirection getPriorityDirection()
	{
		return LEFT;
	}

	virtual void deal(OperatorStack& os, PolynomialStack& ps)
	{

	}

	virtual void calculate(PolynomialStack& ps)
	{

	}
	
};

class ArithmeticOperator : public Operator
{
public:

	virtual PriorityDirection getPriorityDirection()
	{
		return LEFT;
	}

	virtual void deal(OperatorStack& os, PolynomialStack& ps)
	{
		Operator *O = os.top();
		if (getPriorityValue() < O->getPriorityValue() || (getPriorityValue() == O->getPriorityValue() && getPriorityDirection() == LEFT))
		{
			while (!os.isEmpty() && os.top()->getPriorityValue() >= getPriorityValue())
			{
				Operator *O = os.top();
				os.pop();
				O->calculate(ps);
				delete O;
			}
		}
		os.push(this);
	}
	
};

class AdditionOperator : public ArithmeticOperator
{

public:

	virtual int getPriorityValue()
	{
		return 2;
	}

	virtual void calculate(PolynomialStack& ps)
	{
		Polynomial B = ps.top();
		ps.pop();
		Polynomial A = ps.top();
		ps.pop();
		ps.push(A + B);
	}
	
};

class SubtractionOperator : public ArithmeticOperator
{

public:

	virtual int getPriorityValue()
	{
		return 2;
	}

	virtual void calculate(PolynomialStack& ps)
	{
		Polynomial B = ps.top();
		ps.pop();
		Polynomial A = ps.top();
		ps.pop();
		ps.push(A - B);
	}
	
};

class MultiplicationOperator : public ArithmeticOperator
{

public:

	virtual int getPriorityValue()
	{
		return 3;
	}

	virtual void calculate(PolynomialStack& ps)
	{
		Polynomial B = ps.top();
		ps.pop();
		Polynomial A = ps.top();
		ps.pop();
		ps.push(A * B);
	}
	
};

class DivisionOperator : public ArithmeticOperator
{

public:

	virtual int getPriorityValue()
	{
		return 3;
	}

	virtual void calculate(PolynomialStack& ps)
	{
		Polynomial B = ps.top();
		ps.pop();
		Polynomial A = ps.top();
		ps.pop();
		ps.push(A / B);
	}
	
};

class ExponentiationOperator : public ArithmeticOperator
{

public:

	virtual int getPriorityValue()
	{
		return 4;
	}

	virtual PriorityDirection getPriorityDirection()
	{
		return RIGHT;
	}

	virtual void calculate(PolynomialStack& ps)
	{
		Polynomial B = ps.top();
		ps.pop();
		Polynomial A = ps.top();
		ps.pop();
		ps.push(A ^ B);
	}
	
};

class LeftBracketOperator : public Operator
{

public:

	virtual int getPriorityValue()
	{
		return 1;
	}

	virtual PriorityDirection getPriorityDirection()
	{
		return LEFT;
	}

	virtual void deal(OperatorStack& os, PolynomialStack& ps)
	{
		os.push(this);
	}

	virtual void calculate(PolynomialStack& ps)
	{
		throw std::invalid_argument("Invalid expression!");
	}
	
};

class RightBracketOperator : public Operator
{

public:

	virtual int getPriorityValue()
	{
		return 1;
	}

	virtual PriorityDirection getPriorityDirection()
	{
		return LEFT;
	}

	virtual void deal(OperatorStack& os, PolynomialStack& ps)
	{
		while (os.top()->getPriorityValue() > getPriorityValue())
		{
			Operator *O = os.top();
			os.pop();
			O->calculate(ps);
			delete O;
		}
		delete os.top();
		os.pop();
	}

	virtual void calculate(PolynomialStack& ps)
	{
		throw std::invalid_argument("Invalid expression!");
	}
	
};

class EndOperator : public Operator
{

public:

	virtual int getPriorityValue()
	{
		return 0;
	}

	virtual PriorityDirection getPriorityDirection()
	{
		return LEFT;
	}

	virtual void deal(OperatorStack& os, PolynomialStack& ps)
	{
		while (os.top()->getPriorityValue() > getPriorityValue())
		{
			Operator *O = os.top();
			os.pop();
			O->calculate(ps);
			delete O;
		}
		delete os.top();
		os.pop();
	}
	
};

class NegativeOperator : public Operator
{

public:

	virtual int getPriorityValue()
	{
		return 4;
	}

	virtual PriorityDirection getPriorityDirection()
	{
		return RIGHT;
	}

	virtual void deal(OperatorStack& os, PolynomialStack& ps)
	{
		os.push(this);
	}

	virtual void calculate(PolynomialStack& ps)
	{
		Polynomial A = ps.top();
		ps.pop();
		A = A * Polynomial(RationalNumber(-1));
		ps.push(A);
	}
	
};

class PositiveOperator : public Operator
{

public:

	virtual int getPriorityValue()
	{
		return 4;
	}

	virtual PriorityDirection getPriorityDirection()
	{
		return RIGHT;
	}

	virtual void deal(OperatorStack& os, PolynomialStack& ps)
	{
		os.push(this);
	}
	
};

class Status
{

public:
	
	virtual void dealPolynomial(Status*& status, OperatorStack& os, PolynomialStack& ps, Polynomial rn) = 0;

	virtual void dealOperator(Status*& status, OperatorStack& os, PolynomialStack& ps, Operator* op) = 0;

	virtual void dealAmbiguous(Status*& status, OperatorStack& os, PolynomialStack& ps, Operator* op1, Operator* op2) = 0;
	
	virtual void dealLeftBracket(Status*& status, OperatorStack& os, PolynomialStack& ps) = 0;

	virtual void dealRightBracket(Status*& status, OperatorStack& os, PolynomialStack& ps) = 0;

};

class PolynomialStatus : public Status
{

public:
	
	virtual void dealPolynomial(Status*& status, OperatorStack& os, PolynomialStack& ps, Polynomial rn);

	virtual void dealOperator(Status*& status, OperatorStack& os, PolynomialStack& ps, Operator* op);

	virtual void dealAmbiguous(Status*& status, OperatorStack& os, PolynomialStack& ps, Operator* op1, Operator* op2);
	
	virtual void dealLeftBracket(Status*& status, OperatorStack& os, PolynomialStack& ps);

	virtual void dealRightBracket(Status*& status, OperatorStack& os, PolynomialStack& ps);

	static Status* getInstance();

private:

	static Status* mStatus;
	
};

class OperatorStatus : public Status
{

public:
	
	virtual void dealPolynomial(Status*& status, OperatorStack& os, PolynomialStack& ps, Polynomial rn);

	virtual void dealOperator(Status*& status, OperatorStack& os, PolynomialStack& ps, Operator* op);

	virtual void dealAmbiguous(Status*& status, OperatorStack& os, PolynomialStack& ps, Operator* op1, Operator* op2);
	
	virtual void dealLeftBracket(Status*& status, OperatorStack& os, PolynomialStack& ps);

	virtual void dealRightBracket(Status*& status, OperatorStack& os, PolynomialStack& ps);

	static Status* getInstance();

private:

	static Status* mStatus;
	
};

Status* PolynomialStatus::mStatus = NULL;

void PolynomialStatus::dealPolynomial(Status*& status, OperatorStack& os, PolynomialStack& ps, Polynomial rn)
{
	(new MultiplicationOperator)->deal(os, ps);
	ps.push(rn);
}

void PolynomialStatus::dealOperator(Status*& status, OperatorStack& os, PolynomialStack& ps, Operator* op)
{
	op->deal(os, ps);
	status = OperatorStatus::getInstance();
}

void PolynomialStatus::dealAmbiguous(Status*& status, OperatorStack& os, PolynomialStack& ps, Operator* op1, Operator* op2)
{
	op1->deal(os, ps);
	status = OperatorStatus::getInstance();
	delete op2;
}

void PolynomialStatus::dealLeftBracket(Status*& status, OperatorStack& os, PolynomialStack& ps)
{
	(new MultiplicationOperator)->deal(os, ps);
	(new LeftBracketOperator)->deal(os, ps);
	status = OperatorStatus::getInstance();
}

void PolynomialStatus::dealRightBracket(Status*& status, OperatorStack& os, PolynomialStack& ps)
{
	(new RightBracketOperator)->deal(os, ps);
}

Status* PolynomialStatus::getInstance()
{
	if (mStatus == NULL)
	{
		mStatus = new PolynomialStatus();
	}
	return mStatus;
}

Status* OperatorStatus::mStatus = NULL;

void OperatorStatus::dealPolynomial(Status*& status, OperatorStack& os, PolynomialStack& ps, Polynomial rn)
{
	ps.push(rn);
	status = PolynomialStatus::getInstance();
}

void OperatorStatus::dealOperator(Status*& status, OperatorStack& os, PolynomialStack& ps, Operator* op)
{
	throw std::invalid_argument("Invalid expression!");
}

void OperatorStatus::dealAmbiguous(Status*& status, OperatorStack& os, PolynomialStack& ps, Operator* op1, Operator* op2)
{
	op2->deal(os, ps);
	delete op1;
}

void OperatorStatus::dealLeftBracket(Status*& status, OperatorStack& os, PolynomialStack& ps)
{
	(new LeftBracketOperator)->deal(os, ps);
}

void OperatorStatus::dealRightBracket(Status*& status, OperatorStack& os, PolynomialStack& ps)
{
	throw std::invalid_argument("Invalid expression!");
}

Status* OperatorStatus::getInstance()
{
	if (mStatus == NULL)
	{
		mStatus = new OperatorStatus();
	}
	return mStatus;
}

class Expression
{

public:

	Expression(std::string str = std::string()) : str(str)
	{

	}

	friend std::istream& operator >> (std::istream& in, Expression& exp)
	{
		std::cout << " In[" << exp.mHistory.size() << "] = ";
		return getline(in, exp.str);
	}

	friend std::ostream& operator << (std::ostream& out, Expression& exp)
	{
		return out << "Out[" << exp.mHistory.size() - 1 << "] = " << exp.mHistory.back();
	}

	Expression& calculate()
	{
		OperatorStack os(str.size() + 1);
		PolynomialStack ps(str.size() + 1);
		os.push(new EndOperator());
		Status* status = OperatorStatus::getInstance();
		try
		{
			for (std::string::iterator it = str.begin(); it != str.end(); ++it)
			{
				if (*it == ' ')
				{
					continue;
				}
				if (*it == '$')
				{
					int id = 0;
					++it;
					while (it != str.end())
					{
						if (isdigit(*it))
						{
							id *= 10;
							id += *it - '0';
						}
						else
						{
							break;
						}
						++it;
					}
					--it;
					if (id >= mHistory.size())
					{
						throw std::out_of_range("Out of range!");
					}
					status->dealPolynomial(status, os, ps, mHistory[id]);
				}
				else if (isdigit(*it) || *it == '.')
				{
					RationalNumber rn;
					long long denominator = 1;
					bool hasDot = false;
					while (it != str.end())
					{
						if (isdigit(*it))
						{
							if (hasDot)
							{
								denominator *= 10;
								rn = rn + RationalNumber(*it - '0', denominator);
							}
							else
							{
								rn = rn * RationalNumber(10);
								rn = rn + RationalNumber(*it - '0');
							}
						}
						else if (*it == '.' && !hasDot)
						{
							hasDot = true;
						}
						else
						{
							break;
						}
						++it;
					}
					--it;
					status->dealPolynomial(status, os, ps, Polynomial(rn));
				}
				else if (*it == 'x')
				{
					status->dealPolynomial(status, os, ps, Polynomial(true));
				}
				else
				{
					if (*it == '+' || *it == '-')
					{
						if (*it == '+')
						{
							status->dealAmbiguous(status, os, ps, new AdditionOperator(), new PositiveOperator());
						}
						else
						{
							status->dealAmbiguous(status, os, ps, new SubtractionOperator(), new NegativeOperator());
						}
					}
					else if (*it == '(')
					{
						status->dealLeftBracket(status, os, ps);
					}
					else if (*it == ')')
					{
						status->dealRightBracket(status, os, ps);
					}
					else
					{
						Operator *O;
						if (*it == '^')
						{
							O = new ExponentiationOperator();
						}
						else if (*it == '*')
						{
							if (it + 1 != str.end() && *(it + 1) == '*')
							{
								O = new ExponentiationOperator();
								it++;
							}
							else
							{
								O = new MultiplicationOperator();
							}
						}
						else if (*it == '/')
						{
							O = new DivisionOperator();
						}
						else
						{
							throw std::invalid_argument("Invalid expression!");
						}
						status->dealOperator(status, os, ps, O);
					}
				}
			}
			status->dealOperator(status, os, ps, new EndOperator());
			Polynomial ans = ps.top();
			ps.pop();
			if (!os.isEmpty() || !ps.isEmpty())
			{
				throw std::invalid_argument("Invalid expression!");
			}
			mHistory.push_back(ans);
			return *this;
		}
		catch (std::bad_alloc& e)
		{
			throw;
		}
		catch (std::domain_error& e)
		{
			throw;
		}
		catch (std::invalid_argument& e)
		{
			throw;
		}
		catch (std::length_error& e)
		{
			throw;
		}
		catch (std::exception& e)
		{
			throw std::invalid_argument("Invalid expression!");
		}
	}

private:

	std::string str;
	std::vector<Polynomial> mHistory;
	
};

int main()
{
	Expression exp;
	while (std::cin >> exp)
	{
		try
		{
			std::cout << exp.calculate() << std::endl;
		}
		catch (std::exception& e)
		{
			std::cerr << e.what() << std::endl;
		}
	}
	return 0;
}
