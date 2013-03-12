#include <iostream>

class coordinate {
	private:
		int i;
	protected:
		int j;
	public:
		int k;
};

class pinpoint: public coordinate {
	public:
		pinpoint(int newj, int newk) {
			j = newj;
			k = newk;
		}
};

class pinpointed: private coordinate {
	public:
		pinpointed(int newi, int newj, int newk) {
			i = newi;
			j = newj;
			k = newk;
		}
};

int main() {
}
