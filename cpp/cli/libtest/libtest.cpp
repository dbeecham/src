class test {
	private:
		int y;
		int x;
	public:
		int i;
		int j;
		int returny() {
			return this->y;
		}
		int returnx() {
			return this->x;
		}
		test() {
			this->y = 0;
			this->x = 1;
		}
}
