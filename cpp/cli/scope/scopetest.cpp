/*
 * This file is part of scopetest.
 *
 * scopetest is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * scopetest is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with scopetest.  If not, see <http://www.gnu.org/licenses/>.
 */

#include <stdio.h>

int testScope(int i) {
	printf("Current i: %d\n", i);
	i++;
	printf("I altered: %d\n", i);
	::i++;
	printf("Global I Altered: %d\n", i);
}

int i = 10;

int main() {
	testScope(i);
}
