#include <iostream>

template <class T> T printType(T a) {
	std::cout << a << std::endl;
};

template <class A> class coordinate {
	public:
		A X;
		A Y;
		coordinate(A newX, A newY) {
			X = newX;
			Y = newY;
		}
		int nullify(A);
};

template <> class coordinate <char> {
	public:
		int X;
		int Y;
		coordinate(char newX, char newY) {
			X = (int)newX;
			Y = (int)newY;
		}
		int nullify(char);
};

template <class A> int coordinate<A>::nullify(A) {
	return 0;
}

int coordinate<char>::nullify(char a) {
	return 0;
}

int main() {
	printType(1);
	printType(0.1);
	printType('a');
	printType("abc");
	coordinate <int> coord (1, 2);
	coordinate <char> coord2 ('a', 'b');
}
