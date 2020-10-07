#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

void radixsort256 (
    unsigned int * arr,
    int arr_len
)
{
    unsigned int * output = malloc(sizeof(unsigned int) * arr_len);
    int count[256];

    // just to keep track of which the original array was
    unsigned int * original_arr = arr;

    for (int shift = 0, s = 0; shift < 4; shift += 1, s += 8) {

        // zero the counts
        for (int i = 0; i < 256; i++) {
            count[i] = 0;
        }

        // store the occurrences in count[]
        for (int i = 0; i < arr_len; i++) {
            count[(arr[i] >> s) & 0xff]++;
        }

        // change count[i] so that count[i] now contains actual position of the
        // digit in output
        for (int i = 1; i < 256; i++) {
            count[i] += count[i - 1];
        }

        // build the output array
        for (int i = arr_len - 1; i >= 0; i--) {

            // precalculate the offset as it's a few instructions
            int index = (arr[i] >> s) & 0xff;

            // subtract from the count and store the value
            output[--count[index]] = arr[i];
        }

        // copy the output array to input[], so that input[] is sorted
        // according to the current digit
        // swap arr and output
        unsigned int * tmp = arr;
        arr = output;
        output = tmp;
    }

    // if we swiched pointers an odd number of times, copy the result over to
    // the input array before freeing the output
    if (original_arr == output) {
        // swap arr and output
        unsigned int * tmp = arr;
        arr = output;
        output = tmp;

        for (int i = 0; i < arr_len; i++) {
            arr[i] = output[i];
        }
    }

    free(output);

}


void printArray(unsigned int * arr, int arr_len) {
    for (int i = 0; i < arr_len; i++) {
        printf(" %i ", arr[i]);
    }
    printf("\n");
}


int main() {
    unsigned int arr[] = { 110, 3, 83, 73, 63, 81, 112, 100 };
    radixsort256(arr, 8);
    printArray(arr, 8);
}
