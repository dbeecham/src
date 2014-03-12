#include <stdio.h>
#include <limits.h>
#define loop for(;;)

int main(int argc, const char *argv[]) {
    int arr[] = {1, 2, 3, 4, 5, 6};
    int largest = arr[0];

    int i = 0;
    loop {
        if ((sizeof(arr) / sizeof(int)) == i) break;
        if (arr[i] > largest) largest = arr[i];
        i++;
    }

    printf("Largest element is %i\n", largest);
    
    return 0;
}
