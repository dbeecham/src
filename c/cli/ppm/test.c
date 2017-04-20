#include <stdio.h>  /* printf */
#include <math.h>   /* lroundf, sinf, cosf */
#include <stdlib.h> /* abs */

short transform(float x, float min, float max) {
    return (short)lroundf(100 * ((x + fabsf(min)) / (max - min)));
}

short r(float x, float y) {
    float r = 1 + sinf(x) * sinf(y);
    return transform(r, 0, 2);
}
short g(float x, float y) {
    float r = (sinf(x) * cosf(y)) / 2;
    return transform(r, -1, 1);
}
short b(float x, float y) {
    float r = expf( -1 * (fabsf(x - 50)/100 + fabsf(y - 50)/100) );
    return transform(r, 0, 1);
}

int main(int args, char ** argv) {

    const unsigned int width = 100;
    const unsigned int height = 100;

    const float x_max = 10;
    const float y_max = 10;

    const float dx = x_max / width;
    const float dy = y_max / height;

    /* open file */
    FILE * fh = fopen("out.ppm", "w");
    fprintf(fh, "P3\n%i %i\n100\n", width, height);
    {
        short red;
        short green;
        short blue;
        for (float x = 0; x < x_max; x += dx) {
            for (float y = 0; y < y_max; y += dy) {
                red = r(x,y);
                green = g(x,y);
                blue = b(x,y);
                fprintf(fh, "%hi %hi %hi ", red, green, blue);
            }
            fprintf(fh, "\n");
        }
    }



}
