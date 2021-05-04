#include <SDL.h>
#include <SDL_image.h>
#include <stdio.h>
#include <stdlib.h>

int main() {
	if ((SDL_Init(SDL_INIT_VIDEO|SDL_INIT_TIMER)) == -1) {
		return 1;
	}

	SDL_Surface *screen;
	screen = SDL_SetVideoMode(350, 117, 24, SDL_SWSURFACE);
	if (!screen) return 2;

	SDL_Surface *image;
	image = IMG_Load("linux_is_cancer.png");
	SDL_BlitSurface(image, NULL, screen, NULL);
	SDL_UpdateRect(screen, 0, 0, image->w, image->h);
	SDL_FreeSurface(image);

	sleep(10);

	return 0;
}
