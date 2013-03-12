typedef void (*functionPointer)();

void printMemory();
void printAc();

functionPointer functions[] = {
	printAc,
	printMemory
};
