#include <stdio.h>
#define ilen(arr) (sizeof(arr)/sizeof(int))

void reverse(int * arr, int len) {
    int tmp;
    for (int i = 0; i < (len / 2); i++) {
        tmp = arr[len - i];
        arr[len - i] = arr[i];
        arr[i] = tmp;
    }
}

int main(int argc, const char *argv[]) {
    int arr[] = {1, 2, 3, 4, 5, 6};
    reverse(arr, ilen(arr));
    for (int i = 0; i < ilen(arr); i++) {
        printf("%i\n", arr[i]);
    }
    return 0;
}
