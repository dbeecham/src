#include <stdio.h>

#define SWAP(a,b) (((a) ^ (b)) && ((b) ^= (a) ^= (b), (a) ^= (b))) 

int partition (
    int * arr,
    int low,
    int high
)
{

    int pivot = arr[high];
    int i = (low - 1);

    for (int j = low; j <= high; j++) {
        if (arr[j] < pivot) {
            i++;
            SWAP(arr[i], arr[j]);
        }
    }

    SWAP(arr[i + 1], arr[high]);
    return i + 1;
}

void quickSort (
    int * arr,
    int low,
    int high
)
{
    if (low < high) {
        int pi = partition(arr, low, high);
        quickSort(arr, low, pi - 1);
        quickSort(arr, pi + 1, high);
    }
}


void printArray(int * arr, int arr_len) {
    for (int i = 0; i < arr_len; i++) {
        printf(" %i ", arr[i]);
    }
    printf("\n");
}


int main() {
    int arr[] = { 110, 3, 83, 73, 63, 81, 112, 100 };
    quickSort(arr, 0, 7);
    printArray(arr, 7);
}
