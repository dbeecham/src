#include <iostream>
#include <vector>

using namespace std;

int main(int args, char *argv[]) {
	vector< vector<int> > vec;

	vec.push_back(vector<int>());
	vec[0].push_back(1);

	cout << vec[0][0] << endl;
}
