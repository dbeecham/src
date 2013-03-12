#include <iostream>
#include <vector>

using namespace std;

class node {
	public:
		int content;
		vector <node> children;

		node() {
			this->content = 2;
		}

		node(int content) {
			this->content = content;
		}
		
		void createNewChild(int content) {
			this->children.push_back(node(content));
		}

};

int main() {
	node tmpnode;
	cout << tmpnode.content << endl;
	tmpnode.createNewChild(3);
	cout << tmpnode.children[0].content << endl;

	tmpnode.children[0].createNewChild(4);
	cout << tmpnode.children[0].children[0].content << endl;


	// and so forth. A tree, where every node can have as many children
	// as memory allows it to.
}
