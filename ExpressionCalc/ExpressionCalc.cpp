#include <iostream>
#include <algorithm>
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
		return RationalNumber(B.numerator * denominator + B.denominator * numerator, B.denominator * denominator);
	}
	
	RationalNumber operator - (const RationalNumber& B) const
	{
		return RationalNumber(B.numerator * denominator - B.denominator * numerator, B.denominator * denominator);
	}
	
	RationalNumber operator * (const RationalNumber& B) const
	{
		return RationalNumber(B.numerator * numerator, B.denominator * denominator);
	}
	
	RationalNumber operator / (const RationalNumber& B) const throw(std::domain_error)
	{
		if (numerator == 0)
		{
			throw std::domain_error("Division by zero!");
		}
		return RationalNumber(B.numerator * denominator, B.denominator * numerator);
	}

	RationalNumber operator ^ (const RationalNumber& B) const throw(std::invalid_argument)
	{
		if (denominator != 1 || numerator < 0)
		{
			throw std::invalid_argument("Invalid exponent!");
		}
		RationalNumber S(1, 1);
		RationalNumber A = B;
		long long n = numerator;
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

	Stack(int size = MAXN) throw(std::bad_alloc) : size(size), topPos(-1)
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
		if (topPos + 1 >= size)
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
	size_t size;
	int topPos;

};

class Operator;

class OperatorStack : public Stack<Operator*>
{

public:

	OperatorStack(int size = MAXN) throw(std::bad_alloc) : Stack(size)
	{
		
	}

};

class RationalNumberStack : public Stack<RationalNumber>
{

public:

	RationalNumberStack(int size = MAXN) throw(std::bad_alloc) : Stack(size)
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

	virtual int getPriorityDirection()
	{
		return LEFT;
	}

	virtual void deal(OperatorStack& os, RationalNumberStack& rns)
	{

	}

	virtual void calculate(RationalNumberStack& rns)
	{

	}
	
};

class ArithmeticOperator : public Operator
{
public:

	virtual int getPriorityDirection()
	{
		return LEFT;
	}

	virtual void deal(OperatorStack& os, RationalNumberStack& rns)
	{
		Operator *O = os.top();
		if (getPriorityValue() < O->getPriorityValue() || (getPriorityValue() == O->getPriorityValue() && getPriorityDirection() == LEFT))
		{
			while (!os.isEmpty() && os.top()->getPriorityValue() >= getPriorityValue())
			{
				Operator *O = os.top();
				os.pop();
				O->calculate(rns);
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

	virtual void calculate(RationalNumberStack& rns)
	{
		RationalNumber A = rns.top();
		rns.pop();
		RationalNumber B = rns.top();
		rns.pop();
		rns.push(A + B);
	}
	
};

class SubtractionOperator : public ArithmeticOperator
{

public:

	virtual int getPriorityValue()
	{
		return 2;
	}

	virtual void calculate(RationalNumberStack& rns)
	{
		RationalNumber A = rns.top();
		rns.pop();
		RationalNumber B = rns.top();
		rns.pop();
		rns.push(A - B);
	}
	
};

class MultiplicationOperator : public ArithmeticOperator
{

public:

	virtual int getPriorityValue()
	{
		return 3;
	}

	virtual void calculate(RationalNumberStack& rns)
	{
		RationalNumber A = rns.top();
		rns.pop();
		RationalNumber B = rns.top();
		rns.pop();
		rns.push(A * B);
	}
	
};

class DivisionOperator : public ArithmeticOperator
{

public:

	virtual int getPriorityValue()
	{
		return 3;
	}

	virtual void calculate(RationalNumberStack& rns)
	{
		RationalNumber A = rns.top();
		rns.pop();
		RationalNumber B = rns.top();
		rns.pop();
		rns.push(A / B);
	}
	
};

class ExponentiationOperator : public ArithmeticOperator
{

public:

	virtual int getPriorityValue()
	{
		return 4;
	}

	virtual int getPriorityDirection()
	{
		return RIGHT;
	}

	virtual void calculate(RationalNumberStack& rns)
	{
		RationalNumber A = rns.top();
		rns.pop();
		RationalNumber B = rns.top();
		rns.pop();
		rns.push(A ^ B);
	}
	
};

class LeftBracketOperator : public Operator
{

public:

	virtual int getPriorityValue()
	{
		return 1;
	}

	virtual int getPriorityDirection()
	{
		return LEFT;
	}

	virtual void deal(OperatorStack& os, RationalNumberStack& rns)
	{
		os.push(this);
	}
	
};

class RightBracketOperator : public Operator
{

public:

	virtual int getPriorityValue()
	{
		return 1;
	}

	virtual int getPriorityDirection()
	{
		return LEFT;
	}

	virtual void deal(OperatorStack& os, RationalNumberStack& rns)
	{
		while (os.top()->getPriorityValue() > getPriorityValue())
		{
			Operator *O = os.top();
			os.pop();
			O->calculate(rns);
			delete O;
		}
		delete os.top();
		os.pop();
	}
	
};

class EndOperator : public Operator
{

public:

	virtual int getPriorityValue()
	{
		return 0;
	}

	virtual int getPriorityDirection()
	{
		return LEFT;
	}

	virtual void deal(OperatorStack& os, RationalNumberStack& rns)
	{
		while (os.top()->getPriorityValue() > getPriorityValue())
		{
			Operator *O = os.top();
			os.pop();
			O->calculate(rns);
			delete O;
		}
		delete os.top();
		os.pop();
	}
	
};

class Expression
{

public:

	Expression(std::string str = std::string()) : str(str)
	{

	}

	friend std::istream& operator >> (std::istream& in, Expression& exp)
	{
		return getline(in, exp.str);
	}

	RationalNumber calculate()
	{
		OperatorStack os(str.size());
		RationalNumberStack rns(str.size());
		os.push(new EndOperator());
		try
		{
			for (std::string::iterator it = str.begin(); it != str.end(); ++it)
			{
				if (*it == ' ')
				{
					continue;
				}
				if (isdigit(*it) || *it == '.')
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
					rns.push(rn);
				}
				else
				{
					Operator *O;
					if (*it == '(')
					{
						O = new LeftBracketOperator();
					}
					else if (*it == ')')
					{
						O = new RightBracketOperator();
					}
					else if (*it == '#')
					{
						O = new EndOperator();
					}
					else if (*it == '+')
					{
						O = new AdditionOperator();
					}
					else if (*it == '-')
					{
						O = new SubtractionOperator();
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
					O->deal(os, rns);
				}
			}
			if (!os.isEmpty())
			{
				throw std::invalid_argument("Invalid expression!");
			}
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
		return rns.top();
	}

private:

	std::string str;
	
};

int main()
{
	Expression exp;
	std::cin >> exp;
	try
	{
		std::cout << exp.calculate() << std::endl;
	}
	catch (std::exception& e)
	{
		std::cerr << e.what() << std::endl;
	}
	return 0;
}
