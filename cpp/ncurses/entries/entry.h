void *netupdate(void *);

struct entry {
	int i;
	struct entry *next;
};

class everything {
	public:
		int i;
		struct entry *e;
};
