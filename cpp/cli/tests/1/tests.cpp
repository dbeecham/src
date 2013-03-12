#include <iostream>
#include <fstream>
#include <string>
#include <typeinfo>

using namespace std;

// Class
class getit {
	public:
		int x;
		int i;

		getit() { i = 0; }

		getit(int x) {
			this->x = x;
		}

		// Operator overloading
		int operator+(int x) {
			return this->i + x;
		}

		int operator()(int x) {
			return x*x;
		}

		getit operator = (int x) {
			this->i = x;
			return *this;
		}

};

// Structures
struct struct1 { int a; };
struct struct2 { 
	int a; 

	void operator = (struct1 s1) {
		a = s1.a;
	}
};

// Operator overloading outside class definition
int operator << (getit s, int x) {
	return 1;
}

int a() { return 8; }

// Referencevariable to a function
int (&b)() = a;

// Pointervariable to a function
int (*c)() = &a;

int function() { return 9; }

typedef int (*lnk)();

void increment(int &i) {
	i++;
}

int main() {

	getit i;
	getit a;
	i = a = 8;
	int x = 0;
	// The true operator calling
	cout << "Testing operators." << endl;
	(cout.operator<<(i + 2)).operator<<(endl);
	cout << (a << 3) << endl;
	cout << a(3) << endl;


	int j = 0;
	cout << "Typeid of j (which is an int): " << typeid(j).name() << endl;
	int &ra = j;
	int *pa = &j;

	// Calling reference
	cout << "reference b: " << b() << endl;

	// Normal variable
	cout << " j: " << j << " and address: " << &j << endl;
	// Reference to variable
	cout << "ra: " << ra << " and address: " << &ra << endl;
	// Pointer to variable
	cout << "pa: " << pa << " and address: " << &pa << endl;

	// Do loops have their own scope?
	cout << "Testing loop scope..." << endl;
	int scopetest = 1;
	int iterator;
	cout << "Outside the loop scopetest is: " << scopetest << endl;
	for (iterator = 0; iterator == 0; iterator++) {
		int scopetest = 2;
		cout << "Inside the loop scopetest is: " << scopetest << endl;
		scopetest++;
	}
	cout << "Outsid the loop again scopetest is: " << scopetest << endl;

	{ int scopetest = 3; scopetest++; }
	cout << "After a separate scope scopetest is: " << scopetest << endl;


	// Function addresses ISNOTWORKING
	cout << "Address of function a: " << &a << endl;
	cout << "Address of function b: " << &b << endl;
	cout << "Address of function c: " << &c << endl;
	cout << "Address of function c: " << &(*c) << endl;
	cout << "Address of dereferenced function c: " << *c << endl;

	// Declare pointer to function from type definition
	lnk two;
	two = &function;
	cout << two() << endl;


	// Calculating length by addresses.
	int array1[] = {1,2,3,4};
	int array2[] = {1,2,3,4};
	int array3[][4] = {{1,2,3,4},{1,2,3,4}};

	cout << "Length of array1: " << &array1[4] - array1 << endl;
	cout << "array1 - array2?: " << array1 - array2 << endl; // << This is bad behaviour.

	struct1 s1;
	s1.a = 2;
	struct2 s2;
	s2 = s1;
	cout << "s1.a = 2; s2 = s1; s2.a = " << s2.a << endl;

	int incrementer = 0;
	increment(incrementer); // This is no macro, it's a real function.
	cout << "Incrementede i, it's now " << incrementer << endl;;

}
